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

#ifndef PADIMAGE_H
#define PADIMAGE_H

#include "defs.h"
#include "object.h"
#include "point.h"
#include "display.h"
#include <X11/X.h>

#define NO_DITHER       0
#define REFINE_DITHER   1
#define DITHER          2

#define IMAGE_DITHER_SET       (1<<0)  // Set if -dither has been configured
#define IMAGE_IMAGEDATA_SET    (1<<1)  // Set if -image  has been configured
#define IMAGE_WRITEFORMAT_SET  (1<<2)  // Set if -writeformat has been configured

//
// Images
//
class Pad_String;

class Pad_Image : public Pad_Object
{
  public:
    unsigned char  imageFlags;	// OR'd combination of flags
    Pad_ImageData *image;	// Original unrotated image
    Pad_ImageData *rotImage;    // Rotated version, if angle != 0
    unsigned char  dither;
    Pad_Uid         name;
    int            writeFormat;

            Pad_Bool Check_for_render(void);
    virtual void     Compute_bounding_box(void);
    virtual float    Compute_scale_within_frame(float *frame);
    virtual Pad_Bool Continue_refinement(void);
    virtual const char * Get_dither(void);
    Pad_ImageData *  Get_imagedata(void);
    virtual Pad_Bool Get_name(Pad_String &string);
    virtual int      Get_writeformat(void);
            void     Init(void);
    virtual Pad_Bool Is_rotatable(void);
    virtual Pad_Bool Pick(Pad_Event *event, float halo);
            void     Remove_image_data(void);
    virtual Pad_Bool Render(void);
    virtual Pad_Bool Render_fast(void);
            int      Render_image(Pad_View &pad_view, int start_row, int end_row, 
				  int start_col, int end_col, int step);
    virtual Pad_Bool Rotate(float theta);
    virtual Pad_Bool Rotate(float theta, Pad_Point &center);
    virtual Pad_Bool Set_dither(const char *new_dither);
    virtual void     Set_dither_default(void);
            void     Set_imagedata(Pad_ImageData *new_image);
            void     Set_imagedata_default(void);
    virtual Pad_Bool Set_writeformat(int);
    virtual void     Set_writeformat_default(void);

    virtual ~Pad_Image();
    Pad_Image(Pad *pad);

    PAD_TYPE("image");
};

#endif
