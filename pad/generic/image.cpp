/*
"(c) Copyright 1993-1997 Pad++ Consortium {University of New Mexico (UNM),
and New York University (NYU)}, All Rights Reserved."  
Licensee can not remove or obscure any of the
copyright or trademark notices in this software.

IN NO EVENT SHALL THE AUTHORS OR DISTRIBUTORS BE LIABLE TO ANY PARTY
FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
ARISING OUT OF THE USE OF THIS SOFTWARE, ITS DOCUMENTATION, OR ANY
DERIVATIVES THEREOF, EVEN IF THE AUTHORS HAVE BEEN ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.

THE AUTHORS AND DISTRIBUTORS SPECIFICALLY DISCLAIM ANY WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE, AND NON-INFRINGEMENT.  THIS SOFTWARE
IS PROVIDED ON AN "AS IS" BASIS, AND THE AUTHORS AND DISTRIBUTORS HAVE
NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
MODIFICATIONS.

See the file "License" for general information on usage and
redistribution, and the file "LicenseTerms" for the specific license
agreement on usage and redistribution of this file, and the Pad++
software in general.
*/

#include "defs.h"
#include "pad-string.h"
#include "pad.h"
#include "view.h"
#include "point.h"
#include "image.h"
#include "restorer.h"
#include "renderer.h"
#include "global.h"
#include "win.h"
#include "imagedata.h"

#include <math.h>
#include <string.h>
#include <fstream>

using namespace std;

#define IMAGE_DEFAULT_DITHER        "refinedither"
#define IMAGE_DEFAULT_IMAGEDATA     NULL
#define IMAGE_DEFAULT_WRITEFORMAT   PAD_WRITE_REFERENCE

//////////////////////////////////////////////
//              Image definitions
//////////////////////////////////////////////  


Pad_Image::~Pad_Image() {
    Generate_delete();

    if (image) {
        image->Remove_object(this);
        image = NULL;
    }
}

Pad_Image::Pad_Image(Pad *pad) :
    Pad_Object(pad) {
    Init();
}

void
Pad_Image::Init(void) {
    _type = PAD_IMAGE;
    imageFlags = PAD_NO_MASK;
    image = NULL;
    rotImage = NULL;
    name = NULL;
    Set_writeformat_default();
    Set_dither_default();
}

//
// Image data has been deleted, so don't access it any more.
//
void
Pad_Image::Remove_image_data(void) {
    Damage();
    image = NULL;
    if (rotImage) {
        delete rotImage;
        rotImage = NULL;
        _Set_angle_data(0.0);
    }
}

//
// Setters and Getters for the writeformat option
//
Pad_Bool
Pad_Image::Set_writeformat(int new_format) {
    int rc = TRUE;
    imageFlags |= IMAGE_WRITEFORMAT_SET;
    switch (new_format) {
        case PAD_WRITE_COPY:
        case PAD_WRITE_REFERENCE:
            writeFormat = new_format;
            break;
        default:
            rc = FALSE;
            Pad_errorString = "invalid writeformat. Try \"PAD_WRITE_COPY\" or \"PAD_WRITE_REFERENCE\"";
    }

    return rc;
}


void
Pad_Image::Set_writeformat_default(void) {
    Set_writeformat(IMAGE_DEFAULT_WRITEFORMAT);
    imageFlags &= ~IMAGE_WRITEFORMAT_SET;
}

int
Pad_Image::Get_writeformat(void) {
    return writeFormat;
}


//
// Setters and Getters for the dither flag
//
Pad_Bool
Pad_Image::Set_dither(const char *new_dither) {
    int rc = TRUE;

    imageFlags |= IMAGE_DITHER_SET;
    if (image && (image->rgb == FALSE)) {
        dither = NO_DITHER;
    } else {
        if (!strcmp(new_dither, "nodither")) {
            dither = NO_DITHER;
        } else if (!strcmp(new_dither, "refinedither")) {
            dither = REFINE_DITHER;
        } else if (!strcmp(new_dither, "dither")) {
            dither = DITHER;
        } else {
            rc = FALSE;
            Pad_errorString = "invalid dither.  Try \"dither\", \"refinedither\", or \"nodither\"";
        }
    }
    Damage();

    return (rc);
}

void
Pad_Image::Set_dither_default(void) {
    Set_dither(IMAGE_DEFAULT_DITHER);
    imageFlags &= ~IMAGE_DITHER_SET;
}

const char *
Pad_Image::Get_dither(void) {
    const char *result;

    if (image && (image->rgb == FALSE)) {
        result = "nodither";
    } else {
        switch (dither) {
            case NO_DITHER:
                result = "nodither";
                break;
            case REFINE_DITHER:
                result = "refinedither";
                break;
            case DITHER:
                result = "dither";
                break;
            default:
                result = "??";
        }
    }

    return (result);
}

//
// Return scale factor that will transform object so height of object
// fits within height of frame.
//
float
Pad_Image::Compute_scale_within_frame(float *frame) {
    float s;
    Pad_ImageData *imageData;

    imageData = Get_imagedata();
    if (imageData) {
        s = (frame[YMAX] - frame[YMIN]) / imageData->height;
    } else {
        s = 1.0;
    }

    return (s);
}

void
Pad_Image::Compute_bounding_box(void) {
    Pad_ImageData *imageData;

    imageData = Get_imagedata();

    if (!(optionFlags & PAD_WIDTH_SET)) {
        if (imageData) {
            Set_bbox_xmin(0.0);
            Set_bbox_xmax(imageData->width);
        } else {
            Set_bbox_xmin(0.0);
            Set_bbox_xmax(0.0);
        }
    }
    if (!(optionFlags & PAD_HEIGHT_SET)) {

        if (imageData) {
            Set_bbox_ymin(0.0);
            Set_bbox_ymax(imageData->height);
        } else {
            Set_bbox_ymin(0.0);
            Set_bbox_ymax(0.0);
        }
    }

    // Need to update this now because otherwise when rotating
    // an image that is a member of a group, the position will
    // be incorrect when computing the groups bbox...
    if (group) {
        Set_position_from_anchor();
    }

    Pad_Object::Compute_bounding_box();
}

//
// Return true to continue rendering other objects, or false if rendering
// was interrupted.
//
Pad_Bool
Pad_Image::Render(void) {
    Pad_Bool rc = TRUE;
    float bb[4];
    Pad_Bool dither_flag;
    Pad_ImageData *imageData;

    imageData = Get_imagedata();
    if (imageData) {
        if (dither == NO_DITHER || imageData->dpy->truecolor) {
            dither_flag = FALSE;
        } else if (dither == REFINE_DITHER) {
            dither_flag = (pad->level == 0) ? FALSE : TRUE;
        } else {
            dither_flag = TRUE;
        }

        Get_bbox(bb);

        // If image is not cached out then do the normal rendering.
        // Otherwise, just render a filled rectangle (the real rendering
        // will be done when the scene is more stable).

        if (!(flags & PAD_MASK_CACHEDOUT)) {
            Pad_renderer->Draw_image(imageData, bb[XMIN], bb[YMIN], bb[XMAX],
                                     bb[YMAX], dither_flag);
        } else {
            Pad_renderer->Set_color(&Pad_Color::gray);
            Pad_renderer->Draw_filled_box(bb[XMIN], bb[YMIN], bb[XMAX], bb[YMAX]);
        }
    }

    return (rc);
}

//
// This gets called whenever the system is slow, and the object
// should render itself extra fast - even if that makes it ugly.
//
Pad_Bool
Pad_Image::Render_fast(void) {
    float bb[4];

    Get_bbox(bb);
    Pad_renderer->Set_color(&Pad_Color::gray);
    Pad_renderer->Draw_filled_box(bb[XMIN], bb[YMIN], bb[XMAX], bb[YMAX]);

    return (TRUE);
}

//
// After checking for normal rendering inform the cache manager
// about whether the image can be cached (only cache in for non-zero
// render level (so caching in has minimal effect while the user is
// changing the view).
//
Pad_Bool
Pad_Image::Check_for_render(void) {
    Pad_Bool rc = Pad_Object::Check_for_render();

    return rc;
}

///////////////////////////////////////
//
//    Rotation code
//
///////////////////////////////////////
// The following functions are for rotating images.
// Is_rotatable: simply returns true. It tells the  group class that
// images are rotatable, for the case that an image is part of
// a group being rotated.
// Rotate(float): Calls Rotate(float, Pad_Point), passing
// to it the anchor of the image data, to be used the point of rotation.
// Rotate(float Theta, Pad_Point &center): does the work of rotation. 
// It expands the image size to accomodate the new bounding box size, determines 
// which pixels belong to the rotation of the original image, and masks the other
// pixels. The actual size of the final image is the size of its bounding box.
//
Pad_Bool
Pad_Image::Is_rotatable(void) {
    return (TRUE);
}

Pad_Bool
Pad_Image::Rotate(float dtheta) {
    // center point must be in global coordinates,
    // so convert anchorpt from local*transform to global
    Pad_Point center;
    center = anchorpt;
    transform.Invert(center);
    Local_to_screen(center);

    return (Rotate(dtheta, center));
}

Pad_Bool
Pad_Image::Rotate(float dtheta, Pad_Point &center) {
    unsigned long *newRGBData;          // Holds the new RGB data
    unsigned char *newMask;             // Holds the new bitmask
    Pixel *newXpixels;          // holds the 8bit rotated image
    unsigned char *newXpixels8;         // holds the > 8bit rotated image
    int newlen;              // length of array of rotated image data (newheitht*newwidth)
    int row, col;             // loop variables
    int width, height, newwidth, newheight; // dimensions of the former and rotated images (in pixels)
    float sinTheta, cosTheta; // used to avoid recalculating sin and cos of the angle
    float leftTopRow, leftTopCol; // position of the leftTop corner, of the original figure, in the new image
    double row2, col2;          // used in rotation loop
    int mappedRow, mappedCol; // where a pixel in the new image transforms to in the reverse rotation process.
    // Used to decide if a pixel in the new image has a pixel from the original image
    // mappped to it, and if yes, which one.

    _Set_angle_data(Get_angle() + dtheta);              // Causes theta to be added to the present image angle theta
    _Set_angle_data(fmod(Get_angle(), 360.0f));   // Make it so:  0 < imageAngle < 360
    optionFlags |= PAD_ANGLE_SET; // Set bit specifying that angle has been set

    // If no image data, then can't rotate
    if (!image) {
        return (TRUE);
    }

    if (rotImage) {
        delete rotImage;
        rotImage = NULL;
    }

    if (Get_angle() == 0) {
        Update();
        return (TRUE);
    }

    sinTheta = sin(DEG2RAD(Get_angle()));
    cosTheta = cos(DEG2RAD(Get_angle()));

    width = image->width;
    height = image->height;

    // Calculate the position of the top left hand corner of the original
    // image when placed into the new grid (newRGBData), and calculate
    // the height and width of the new image (newwidth, newheight)
    if (sinTheta >= 0 && cosTheta >= 0) { // If first quadrant rotation
        leftTopRow = (width - 1) * sinTheta;
        leftTopCol = 0.0;
        newwidth = (int) (floor((height * sinTheta) + (width * cosTheta)));
        newheight = (int) (floor((width * sinTheta) + (height * cosTheta)));

    } else if (sinTheta >= 0.0 && cosTheta < 0.0) { // Second quadrant
        newheight = (int) (floor((width * sinTheta) + (height * (-cosTheta))));
        newwidth = (int) (floor((height * sinTheta) + (width * (-cosTheta))));
        leftTopRow = (newheight - 1);
        leftTopCol = (width - 1) * (-cosTheta);

    } else if (sinTheta < 0.0 && cosTheta < 0.0) { // Third quadrant
        newheight = (int) (floor((width * (-sinTheta)) + (height * (-cosTheta))));
        newwidth = (int) (floor((height * (-sinTheta)) + (width * (-cosTheta))));
        leftTopRow = ((height - 1) * -cosTheta);
        leftTopCol = (newwidth - 1);

    } else if (sinTheta < 0.0 && cosTheta >= 0.0) { // Forth quadrant
        leftTopRow = 0.0;
        leftTopCol = (height - 1) * -sinTheta;
        newwidth = (int) (floor((height * (-sinTheta)) + (width * cosTheta)));
        newheight = (int) (floor((width * (-sinTheta)) + (height * cosTheta)));
    }

    newlen = newwidth * newheight;// Size, in pixels, of new arrays

    // Allocate new data
    if (image->rgbData != NULL) {
        newRGBData = new unsigned long[newlen];
    } else {
        newRGBData = NULL;
    }
    if (image->xpixels8 != NULL) {
        newXpixels8 = new unsigned char[newwidth * newheight];
    } else {
        newXpixels8 = NULL;
    }
    if (image->xpixels != NULL) {
        newXpixels = new Pixel[newwidth * newheight];
    } else {
        newXpixels = NULL;
    }
    newMask = new unsigned char[newlen];

    // Reverse-Transform each pixel of the new image to see if it  
    // falls within the pixel space of the original. If yes, map the proper
    // pixel from the original to the new image
    for (row = 0; row < newheight; row++) {
        for (col = 0; col < newwidth; col++) {

            row2 = row - leftTopRow;
            col2 = col - leftTopCol;

            mappedRow = (int) ((row2 * cosTheta) + (col2 * sinTheta));
            mappedCol = (int) ((col2 * cosTheta) - (row2 * sinTheta));

            if ((0 <= mappedRow) && (mappedRow <= height - 1) &&
                (0 <= mappedCol) && (mappedCol <= width - 1)) {

                if (image->mask != NULL && image->mask[mappedRow * width + mappedCol] != 0) {
                    newMask[row * newwidth + col] = image->mask[mappedRow * width + mappedCol];
                } else {
                    newMask[row * newwidth + col] = 0;
                }

                if (newRGBData != NULL) { // Map original data to new image data if "data" exists
                    newRGBData[row * newwidth + col] = image->rgbData[mappedRow * width + mappedCol];
                }
                if (newXpixels != NULL) {// Map original inage_pixels to new xpixels if "xpixels" exist
                    newXpixels[row * newwidth + col] =
                        image->xpixels[mappedRow * width + mappedCol];
                }
                if (newXpixels8 != NULL) {// Map original inage_pixels8 to new xpixels8 if "xpixels8" exist
                    newXpixels8[row * newwidth + col] =
                        image->xpixels8[mappedRow * width + mappedCol];
                }
            } else {
                // Set background mask
                newMask[row * newwidth + col] = 255;
                // Set background pixels to black
                if (newRGBData != NULL) {
                    newRGBData[row * newwidth + col] = 0;
                }
                if (newXpixels != NULL) {
                    newXpixels[row * newwidth + col] = 0;
                }
                if (newXpixels8 != NULL) {
                    newXpixels8[row * newwidth + col] = 0;
                }
            }
        }
    }

    // Create rotated image data, filling in data slots
    rotImage = new Pad_ImageData(pad->view->win->dpy);
    rotImage->width = newwidth;
    rotImage->height = newheight;
    rotImage->rgbData = newRGBData;
    rotImage->xpixels = newXpixels;
    rotImage->xpixels8 = newXpixels8;
    rotImage->mask = newMask;

    // Move center point to same coord system as anchorpt.
    // This is local * transform.
    Pad_Point centerCopy = center;
    Screen_to_local(centerCopy);
    transform.Apply(centerCopy);
    anchorpt.Rotate(dtheta, centerCopy); // Causes the image to rotate about a point other than the anchor of the image

    Update();

    return (TRUE);
}
// End Rotation Code

//
// Returns true if object needs more refinement
//
Pad_Bool
Pad_Image::Continue_refinement(void) {
    Pad_ImageData *imageData;

    if ((flags & PAD_MASK_CACHEDOUT) && (pad->level == 0)) {
        return (TRUE);
    }

    imageData = Get_imagedata();

    // Carry on refining if image requested to dither on refinement
    if ((dither == REFINE_DITHER) && imageData && imageData->rgb && !imageData->dpy->truecolor) {
        return (pad->level == 0);
    } else {
        return (FALSE);
    }
}

void
Pad_Image::Set_imagedata(Pad_ImageData *new_image) {
    imageFlags |= IMAGE_IMAGEDATA_SET;
    if (image) {
        image->Remove_object(this);
        image = NULL;
        name = NULL;
    }
    if (rotImage) {
        delete rotImage;
        rotImage = NULL;
    }
    image = new_image;
    if (image) {
        image->Add_object(this);
        if (!(optionFlags & PAD_MAXSIZE_SET)) {
            maxSize = (int) (500 * MAX(image->width, image->height));
        }
        name = image->name;
    }

    Update();

    if (Get_angle() != 0) {
        Rotate(0);
    }
}

void
Pad_Image::Set_imagedata_default(void) {
    Set_imagedata(IMAGE_DEFAULT_IMAGEDATA);
    imageFlags &= ~IMAGE_IMAGEDATA_SET;
}

Pad_ImageData *
Pad_Image::Get_imagedata(void) {
    if (Get_angle() == 0) {
        return (image);
    } else {
        return (rotImage);
    }
}

//
//  Return the name is result
//

Pad_Bool
Pad_Image::Get_name(Pad_String &result) {

    result = "";
    result = name;
    return (TRUE);

}

//
// Determine if the event is within <halo> of image bounding box.
// If yes, then determine if the chosen pixel has been masked.
// If masked, no picking. If mask for that pixel is zero pick.
//
Pad_Bool
Pad_Image::Pick(Pad_Event *event, float halo) {
    float nearDist;
    float dx1, dy1, dx2, dy2;
    float bb[4];
    Pad_Bool rc;
    int pixCol, pixRow; // Column and row in image cooresponding to event position

    Get_bbox(bb);

    nearDist = halo / event->mag;
    dx1 = (bb[XMIN] - nearDist) - event->pt.x;
    dy1 = (bb[YMIN] - nearDist) - event->pt.y;
    dx2 = event->pt.x - (bb[XMAX] + nearDist);
    dy2 = event->pt.y - (bb[YMAX] + nearDist);

    if ((dx1 < 0) && (dy1 < 0) && (dx2 < 0) && (dy2 < 0)) {
        if (Get_angle() != 0) { // If rotated, look at "rotImage" for data to make picking decision
            pixCol = int(event->pt.x - bb[XMIN]);
            pixCol = MAX(0, pixCol);
            pixCol = MIN(rotImage->height - 1, pixCol);
            pixRow = int(rotImage->height - (event->pt.y - bb[YMIN]));
            pixRow = MAX(0, pixRow);
            pixRow = MIN(rotImage->width - 1, pixRow);

            if (rotImage->mask == NULL) {
                return TRUE;
            } else if (rotImage->mask[pixRow * rotImage->width + pixCol] != 0) {
                rc = FALSE;
            } else {
                rc = TRUE;
            }
        } else { // If not rotated look to at "image" for data to make picking decision
            pixCol = int(event->pt.x - bb[XMIN]);
            pixCol = MAX(0, pixCol);
            pixCol = MIN(image->height - 1, pixCol);
            pixRow = int(image->height - (event->pt.y - bb[YMIN]));
            pixRow = MAX(0, pixRow);
            pixRow = MIN(image->width - 1, pixRow);

            if (image->mask == NULL) {
                rc = TRUE;
            } else if (image->mask[pixRow * image->width + pixCol] != 0) {
                rc = FALSE;
            } else {
                rc = TRUE;
            }
        }
    } else {
        rc = FALSE;
    }

    return (rc);
}
