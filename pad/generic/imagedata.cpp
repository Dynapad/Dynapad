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
#include "colorcube.h"  // must put first, since in PAD_WIN, it include "tkWinInt.h" #include "defs.h"
#include "imagedata.h"

#include "pad-string.h"
#include "display.h"
#include "renderer.h"
#include "object.h"
#include "image.h"
#include "win.h"
#include "global.h"
#include "pad.h"

#include <string.h>

#include "imagemagick.h"

unsigned long *Pad_LoadPNM(char *filename, int* ret_width, int* ret_height);
unsigned long *Pad_LoadBMP(char *filename, int* ret_width, int* ret_height);
unsigned long *Pad_LoadPNG(char *filename, int* ret_width, int* ret_height);

////////////////////////////////////////////////////////////////////////
// 
//   Pad_ImageData
//
////////////////////////////////////////////////////////////////////////

//
// Every Pad_ImageData is indexed by two hashtables.
// Pad_imageTokenTable hashes from image tokens to Pad_ImageDatas.
// Pad_imageNameTable hashes from image file names to Pad_ImageDatas.
// Each Pad_ImageData also keeps a list of of all the Pad_Image objects
// that use that image.
//
// Warning: Do not use Pad_ImageData's directly.  Only use them via
//          Pad_Renderer.
//

Pad_ImageData::~Pad_ImageData()
{
    Pad_Object *obj;
    Pad_Iterator oi;

    DOLIST(oi, objects, Pad_Object, obj) {
	((Pad_Image *)obj)->Remove_image_data();
    }
    dpy->Remove_image_data(this);

    Pad_imageTokenTable.Remove((void *) token);
    Pad_imageNameTable.Remove((void *) name);

    dpy = NULL;    
    if (rgbData) {
	delete [] rgbData;
	rgbData = NULL;
    }
    if (xpixels) {
	delete [] xpixels;
	xpixels = NULL;
    }
    if (xpixels8) {
	delete [] xpixels8;
	xpixels8 = NULL;
    }
    if (mask) {
	delete mask;
	mask = NULL;
    }
}

Pad_ImageData::Pad_ImageData(Pad_Display *new_dpy, Pad_Uid newName)
{
    Init(new_dpy);
    name = newName;
    rgbData = Pad_LoadIM(newName, &width, &height);

    if (rgbData && width && height) {
	Compute_pixel_data();
    }

    Pad_imageNameTable.Set((void *) name, this);
    Pad_imageTokenTable.Set((void *) token, this);
}

//
// Constructor that takes a data stream in a standard format
// (i.e., gif, tiff, or jpeg)
//
Pad_ImageData::Pad_ImageData(Pad_Display *new_dpy, unsigned char *formattedData, int len)
{
    Init(new_dpy);

    name = token;
    rgbData = Pad_LoadIM(formattedData, len);

    if (rgbData && width && height) {
	Compute_pixel_data();
    }

    Pad_imageNameTable.Set((void *) name, this);
    Pad_imageTokenTable.Set((void *) token, this);
}

//
// Constructor that takes raw color data and makes an imagedata
// out of it.  There are two versions that take either 8 bit colormap data, 
// or 24 bit rgb data.  <bpp> specifies the bits per pixel of <rawData>.
//
// Warning: This takes responsibility for the data passed in and then
//          frees it with the delete operator, so the data must be
//          allocated with the new operator and then given to this method.
//
Pad_ImageData::Pad_ImageData(Pad_Display *new_dpy, unsigned char *rawData, int newWidth, int newHeight)
{
    Init(new_dpy);

    width = newWidth;
    height = newHeight;
    name = token;

    rgb = FALSE;
    xpixels8 = rawData;

    Pad_imageTokenTable.Set((void *) token, this);
    Pad_imageNameTable.Set((void *) name, this);
}

Pad_ImageData::Pad_ImageData(Pad_Display *new_dpy, unsigned long *rawData, int newWidth, int newHeight)
{
    Init(new_dpy);

    width = newWidth;
    height = newHeight;
    name = token;

    rgbData = rawData;
    Compute_pixel_data();

    Pad_imageTokenTable.Set((void *) token, this);
    Pad_imageNameTable.Set((void *) name, this);
}

Pad_ImageData::Pad_ImageData(Pad_Display *new_dpy)
{
    Init(new_dpy);
    Pad_imageTokenTable.Set((void *) token, this);
    // warning: update Pad_imageNameTable as soon as name is known
    name = token;
    Pad_imageNameTable.Set((void *) name, this);
}

void
Pad_ImageData::Init(Pad_Display *new_dpy)
{
    static int imageDataNum = 1;

    Pad_String tokenName;

    dpy = new_dpy;
    name = NULL;
    tile = 0;
    width = 0;
    height = 0;
    count = 1;
    rgbData = NULL;
    mask = NULL;
    xpixels = NULL;
    xpixels8 = NULL;
    rgb = TRUE;	// By default, store rgb data so image can be dithered
    dpy->Add_image_data(this);

    tokenName = "image";
    tokenName += imageDataNum++;
    token = Pad_GetUid(tokenName.Get());
    userdata = NULL;
}

//
// Set the dimensions of an imagedata.
// This initializes the image, and sets the entire
// image to the specified (r, g, b) color.
// r, g, b are floats in the range [0.0, 1.0]
//
void
Pad_ImageData::Set_dimensions(int newWidth, int newHeight, float red, float green, float blue)
{
    unsigned long d;
    int ir, ig, ib;
    int i, numPixels;

    width = newWidth;
    height = newHeight;

    if ((width > 0) && (height > 0)) {
				// First, free up any existing memory
	if (rgbData) {
	    delete [] rgbData;
	    rgbData = NULL;
	}
	if (xpixels) {
	    delete [] xpixels;
	    xpixels = NULL;
	}
	if (xpixels8) {
	    delete [] xpixels8;
	    xpixels8 = NULL;
	}
	if (mask) {
	    delete mask;
	    mask = NULL;
	}
				// Then allocate and initialize new memory
	rgbData = new unsigned long[width * height];
	ir = (int)(red * 255);
	ig = (int)(green * 255);
	ib = (int)(blue * 255);
	d = CC_RGBA(ir, ig, ib, 0);

        numPixels = width * height;
        for(i = 0; i < numPixels; i++) {
	    rgbData[i] = d;
	}
        
        Compute_pixel_data();
    }
}

//
// Free RGB data so image can no longer be dithered.
//
void
Pad_ImageData::Free_rgb(void)
{
    Pad_Object *obj;
    Pad_Iterator oi;

    if (rgb) {
	rgb = FALSE;
	if (rgbData) {
	    delete [] rgbData;
	    rgbData = NULL;
	}

	DOLIST(oi, objects, Pad_Object, obj) {
	    obj->Damage();
	}
    }
}

//
// Update the pixel values since the colormap has changed,
// and re-render every object that uses this image.
//
void 
Pad_ImageData::Cc_changed(void)
{
    Pad_Object *obj;
    Pad_Iterator oi;

    Compute_pixel_data();

    DOLIST(oi, objects, Pad_Object, obj) {
	obj->Damage();
    }
}

const char *
Pad_ImageData::Get_token(void)
{
    return(token);
}

//
// Add <obj> to the list of Pad_Objects that use this image data.
//
void
Pad_ImageData::Add_object(Pad_Object *obj)
{
    objects.Push_new_last(obj);
}

//
// Remove <obj> from the list of Pad_Objects that use this image data.
//
void
Pad_ImageData::Remove_object(Pad_Object *obj)
{
    objects.Remove(obj);
}

//
// Pad_ComputePixelData
//
//   Convert image rgb data to X colormapped Pixel entries.
//
void
Pad_ImageData::Compute_pixel_data(void)
{
    int i, len;
    int j;
    Pad_ColorCube *cc = dpy->Get_colorcube();
    unsigned long d;

    if (!rgbData) {		// Make sure image has been allocated.
	return;
    }

    if (xpixels) {
	delete [] xpixels;
	xpixels = NULL;
    }
    if (xpixels8) {
	delete [] xpixels8;
	xpixels8 = NULL;
    }
    if (mask) {
	delete [] mask;
    }
    len = width * height;
    if (dpy->depth == 8) {
	xpixels8 = new unsigned char[len];
    } else {
	xpixels = new Pixel[len];
    }
    mask = NULL;

    for (i=0; i<len; i++) {
	d = rgbData[i];
	if (d & 0xff000000) {
	    if (!mask) {
		mask = new unsigned char[len];
		for (j = 0; j < i; j++) {
		    mask[j] = 0;
		}
	    }
	    mask[i] = 255;
	    if (dpy->depth == 8) {
		xpixels8[i] = 0;
	    } else {
		xpixels[i] = 0;
	    }
	} else {
	    if (mask) mask[i] = 0;
	    if (dpy->depth == 8) {
		// On 8 bit X servers, pixels are only 8 bits
		xpixels8[i] = (unsigned char)cc->Get_pixel(d);   
	    } else {
		xpixels[i] = cc->Get_pixel(d);
	    }
	}
    }

}

//
// Set the pixel at [row, col] to the specified (r, g, b) value
// specified as floats in the range [0.0, 1.0]
//
void
Pad_ImageData::Set_pixel(int row, int col, float red, float green, float blue)
{
    unsigned long d;
    int ir, ig, ib;

    if ((row >= 0) && (row < height) &&
	(col >= 0) && (col < width) && 
	rgbData)
	{
	    ir = (int)(red * 255);
	    ig = (int)(green * 255);
	    ib = (int)(blue * 255);
	    d = CC_RGBA(ir, ig, ib, 0);
	    rgbData[(row * width) + col] = d;
	    Compute_pixel_data(row, col);
	}
}

//
// Set the pixel at [row, col] to the specified (r, g, b) value
// specified as floats in the range [0.0, 1.0]
//
void
Pad_ImageData::Set_pixel(int row, int col, float red, float green, float blue, float alpha)
{
    unsigned long d;
    int ir, ig, ib, ia;

    if ((row >= 0) && (row < height) &&
	(col >= 0) && (col < width) && 
	rgbData)
	{
	    ir = (int)(red * 255);
	    ig = (int)(green * 255);
	    ib = (int)(blue * 255);
	    ia = (int)(alpha * 255);
	    d = CC_RGBA(ir, ig, ib, ia);
	    rgbData[(row * width) + col] = d;
	    Compute_pixel_data(row, col);
	}
}

//
// Get the pixel at [row, col]
// returned as integers in the range [0, 255]
//
Pad_Bool
Pad_ImageData::Get_pixel(int row, int col, int* red, int* green, int* blue)
{
    unsigned long d;

    if ((row >= 0) && (row < height) &&
	(col >= 0) && (col < width) && 
	rgbData)
	{
	    d = rgbData[(row * width) + col];
            *red   = CC_RED(d);
            *green = CC_GREEN(d);
            *blue  = CC_BLUE(d);
            return TRUE;
	} else {
            return FALSE;
        }
}

//
// Recompute the X pixel at the specified (row, col) position.
// This is necessary if the underlying RGB data for that pixel
// has been changed.
//
void
Pad_ImageData::Compute_pixel_data(int row, int col)
{
    int i;
    int len;
    unsigned long d;
    Pad_ColorCube *cc = dpy->Get_colorcube();

    if ((row >= 0) && (row < height) &&
	(col >= 0) && (col < width) && 
	rgbData &&
	(((dpy->depth == 8) && xpixels8) ||
	 ((dpy->depth != 8) && xpixels)))
	{
	    i = (row * width) + col;
	    d = rgbData[i];

	    if (d & 0xff000000) {
		if (!mask) {
		    len = width * height;
		    mask = new unsigned char[len];
		    int j;
		    for (j = 0; j < i; j++) {
			mask[j] = 0;
		    }
		}
		mask[i] = 255;
		if (dpy->depth == 8) {
		    xpixels8[i] = 0;
		} else {
		    xpixels[i] = 0;
		}
	    } else {
		if (mask) mask[i] = 0;
		if (dpy->depth == 8) {
				// On 8 bit X servers, pixels are only 8 bits
		    xpixels8[i] = (unsigned char)cc->Get_pixel(d);   
		} else {
		    xpixels[i] = cc->Get_pixel(d);
		}
	    }
	}
}
