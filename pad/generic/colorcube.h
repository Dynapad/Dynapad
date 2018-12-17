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

#ifndef COLORCUBE_H
#define COLORCUBE_H 1

#include "defs.h"

typedef unsigned long Pixel;

class Pad_Display;
class Pad_Color;

//
// RGBA pixel management
//
#define CC_RED(rgba)   ((rgba) & 0xff)
#define CC_GREEN(rgba) (((rgba) >> 8) & 0xff)
#define CC_BLUE(rgba)  (((rgba) >> 16) & 0xff)
#define CC_ALPHA(rgba) ((rgba) >> 24)
#define CC_RGBA(r,g,b,a) ((r) | ((g) << 8) | ((b) << 16) | ((a) << 24))

class Pad_ColorCube {
  public:

    Pad_ColorCube(Pad_Display *dpy);
    ~Pad_ColorCube();

			// Attempts to allocate a colorcube of (nlevels*nlevels*nlevels) colors.
			// Returns true if OK, false if fails.
    Pad_Bool Allocate(int nlevels, float gamma);

    inline unsigned long Get_pixel(int r, int g, int b) {
	if (truecolor) {
	    return reds[r] | greens[g] | blues[b];
	} else {
	    if (r == g && g == b) {
		return xpixel_map[grays[r]];
	    } else {
		return xpixel_map[reds[r] + greens[g] + blues[b]];
	    }
	}
    }

			// Given a device-dependent pixel, return rgb
    void Get_rgb(unsigned long pixel, unsigned int &red, unsigned int &green, unsigned int &blue);

    inline unsigned long Get_pixel(unsigned long rgba) {
	return Get_pixel(CC_RED(rgba), CC_GREEN(rgba), CC_BLUE(rgba));
    }

			// Returns the xpixel_map. This is used by drawimage to
			// dither images efficiently.
    unsigned long *Get_device_pixel_map(int &numreds, int &numgreens, int &numblues);

			// Returns the array of X colors used by the pad color cube.
			// Other languages such as Java need this.
    XColor **Get_xcolor_map(void);

			// Returns the size of the colormap
    int Get_colormap_size(void) {return(size);}

  private:
    Pad_Bool _Allocate_TrueColor(void);
    void     _Free_colors(void);

    XColor **xcolor_map;        // Internal color cube index -> XColor
    unsigned long *xpixel_map;          // Internal color cube index -> X pixel

    //
    // Used by Get_color to map between color and index into cube
    //
    Pixel grays[256];		// gray level 0-255 -> cube index
    Pixel reds[256];    
    Pixel greens[256];
    Pixel blues[256];

    unsigned char _redshift, _greenshift, _blueshift;
    unsigned int  _redcolors, _greencolors, _bluecolors;

    Pad_Display *_dpy;

    int truecolor; // true if a TrueColor or DirectColor visual

    int size;			// total number of colors allocated
    int nreds, ngreens, nblues, ngrays;
};

#endif
