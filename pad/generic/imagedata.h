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

#ifndef IMAGEDATA_H
#define IMAGEDATA_H 1

#include "defs.h"

class Pad;
class Pad_Object;
class Pad_Display;

typedef unsigned long Pixel;

//
// Pad_ImageData
//
//   Allocated image
//
class Pad_ImageData
{
  public:
    Pad_Display *  dpy;           // Display imagedata is associated with
    Pad_Uid         name;          // Filename of imagedata
    Pad_Uid         token;	  // Name of imagedata token
    int            count;         // Number of allocations of this imagedata
    Pad_List       objects;	  // List of Pad_Object's that use this imagedata
    int            width;	  // Width of imagedata in pixels
    int            height;        // Height of imagedata in pixels
    Pad_Bool       rgb;		  // True if imagedata supports dithered rgb
    unsigned long *rgbData;	  // Only used when dithering turned on.  Contains rgb
    unsigned char *xpixels8;      // Array of X pixels for 8 bit X servers
    Pixel         *xpixels;       // Array of X pixels for > 8 bit X servers
    unsigned char *mask;	  // Mask of which pixels get rendered
    Pixmap         tile;          // Image as a Pixmap 
    void *         userdata;      // whatever ... Scheme_Object * for drscheme

    void  Add_object(Pad_Object *obj);
    void  Cc_changed(void);
    void  Compute_pixel_data(void);
    void  Compute_pixel_data(int row, int col);
    void  Free_rgb(void);
    char *Get_token(void);
    void  Init(Pad_Display *dpy);
    void  Remove_object(Pad_Object *obj);
    void  Set_dimensions(int width, int height, float red, float green, float blue);
    void  Set_pixel(int row, int col, float red, float green, float blue);
    void  Set_pixel(int row, int col, float red, float green, float blue, float alpha);
    Pad_Bool Get_pixel(int row, int col, int* red, int* green, int* blue);

    Pad_ImageData(Pad_Display *dpy, Pad_Uid filename);
    Pad_ImageData(Pad_Display *dpy, unsigned char *formattedData, int len);
    Pad_ImageData(Pad_Display *dpy, unsigned char *rawData, int width, int height);
    Pad_ImageData(Pad_Display *dpy, unsigned long *rawData, int width, int height);
    Pad_ImageData(Pad_Display *dpy);
    ~Pad_ImageData();
};

#endif
