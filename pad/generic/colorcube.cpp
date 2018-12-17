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
#include "colorcube.h"
#include "color.h"
#include "display.h"
#include "renderer.h"

#include <math.h>
#include <iostream>
using namespace std;

typedef XColor* XColorPtr;

/* gamma correction value */
#define GAMMA(v) MIN(255, (int)(0.5 + 255 * pow( v / 255.0, 1.0/gamma)))

Pad_ColorCube::Pad_ColorCube(Pad_Display *dpy)
{
    xcolor_map = NULL;
    xpixel_map = NULL;
    size = 0;
    _dpy = dpy;
    _redshift = 0;
    _greenshift = 0;
    _blueshift = 0;
    _redcolors = 0;
    _greencolors = 0;
    _bluecolors = 0;
    nreds = ngreens = nblues = ngrays = 0;
}

Pad_ColorCube::~Pad_ColorCube()
{
    _Free_colors();
}


//
// Free up resources associated with colorcube.
//
void
Pad_ColorCube::_Free_colors(void)
{
    int i;

    if (_dpy->display && size) {
	for (i=0; i<size; i++) {
	    _dpy->Free_color(xcolor_map[i]);
	}
	if (xcolor_map) {
	    delete [] xcolor_map;
	    xcolor_map = NULL;
	}
	if (xpixel_map) {
	    delete [] xpixel_map;
	    xpixel_map = NULL;
	}
    }
    ngrays = 0;
    size = 0;
}

void
Pad_ColorCube::Get_rgb(unsigned long pixel, unsigned int &red, unsigned int &green, unsigned int &blue)
{
    Visual *visual = _dpy->visual;

    red = ((((pixel & visual->red_mask) >> _redshift) * 256) / _redcolors);
    green = ((((pixel & visual->green_mask) >> _greenshift) * 256) / _greencolors);
    blue = ((((pixel & visual->blue_mask) >> _blueshift) * 256) / _bluecolors);
}

Pad_Bool
Pad_ColorCube::_Allocate_TrueColor(void)
{
	int a;
	Pixel           pixval;
	XColor          xcolor;
	unsigned int    redcolors, greencolors, bluecolors;
	unsigned int    redstep, greenstep, bluestep;
	unsigned int    redbottom, greenbottom, bluebottom;
	unsigned int    redtop, greentop, bluetop;
	unsigned char shift, redshift = 255, greenshift = 255, blueshift = 255;

	xcolor.flags = DoRed | DoGreen | DoBlue;
	/*
	 * calculate number of distinct colors in each band
	 */

	redcolors = greencolors = bluecolors = 1;
	for (shift = 0, pixval = 1; pixval; shift++, pixval <<= 1) {
		if (pixval & _dpy->visual->red_mask) {
			redcolors <<= 1;
			if (redshift == 255) redshift = shift;
		}
		if (pixval & _dpy->visual->green_mask) {
			greencolors <<= 1;
			if (greenshift == 255) greenshift = shift;
		}
		if (pixval & _dpy->visual->blue_mask) {
			bluecolors <<= 1;
			if (blueshift == 255) blueshift = shift;
		}
	}

	/*
	 * sanity check
	 */

	if ((redcolors > (unsigned int)_dpy->visual->map_entries) ||
	    (greencolors > (unsigned int)_dpy->visual->map_entries) ||
	    (bluecolors > (unsigned int)_dpy->visual->map_entries)) {
	  fprintf(stderr, "Pad_ColorCube::Allocate - inconsistency in RGB color information?\n");
	}

	_redshift = redshift;
	_greenshift = greenshift;
	_blueshift = blueshift;
	_redcolors = redcolors;
	_greencolors = greencolors;
	_bluecolors = bluecolors;

	redstep = 256 / redcolors;
	greenstep = 256 / greencolors;
	bluestep = 256 / bluecolors;

	redbottom = greenbottom = bluebottom = 0;

	for (a = 0; a < _dpy->visual->map_entries; a++) {
	    if (redbottom < 256) {
	        redtop = redbottom + redstep;
	    }
	    if (greenbottom < 256) {
	        greentop = greenbottom + greenstep;
	    }
	    if (bluebottom < 256) {
	        bluetop = bluebottom + bluestep;
	    }

	    xcolor.red = (redtop - 1) * 257;
	    xcolor.green = (greentop - 1) * 257;
	    xcolor.blue = (bluetop - 1) * 257;

	    if (!XAllocColor(_dpy->display, _dpy->colormap, &xcolor)) {
	        fprintf(stderr, "XAllocColor failed on a TrueColor/DirectColor visual\n");
		    // Not clear what to do here
	    }

				// Fill in reds/greens/blues array
	    while ((redbottom < 256) && (redbottom < redtop)) {
	        reds[redbottom++] = xcolor.pixel & _dpy->visual->red_mask;
	    }
	    while ((greenbottom < 256) && (greenbottom < greentop)) {
	        greens[greenbottom++] = xcolor.pixel & _dpy->visual->green_mask;
	    }
	    while ((bluebottom < 256) && (bluebottom < bluetop)) {
	        blues[bluebottom++] = xcolor.pixel & _dpy->visual->blue_mask;
	    }
	}

	nreds   = redcolors;
	ngreens = greencolors;
	nblues  = bluecolors;

	return TRUE;
}

//
// Allocate X colors for the color cube
// Return TRUE if successful,
// or FALSE if failed.
//
Pad_Bool 
Pad_ColorCube::Allocate(int nlevels, float gamma)
{
    int i, r, g, b;
    int nlevelsq, ncolors;
    int grayoffset;
    float ramp;
				// Check for TrueColor visuals
    if (_dpy->visual->c_class == TrueColor || _dpy->visual->c_class == DirectColor) {
	truecolor = TRUE;
	return _Allocate_TrueColor();
    } else {	
	truecolor = FALSE;
    }
				// Require a colorcube at least 2x2x2
    if (nlevels <= 1) {
	return FALSE;
    }
    if (nlevels > 16) {
	nlevels = 16;
    }

    nlevelsq = nlevels * nlevels;
    ncolors = nlevelsq * nlevels;

    _Free_colors();

    ngrays = nlevelsq;    
    if (ngrays > 30) ngrays = 30;

    xcolor_map = new XColorPtr[ncolors + ngrays];
    xpixel_map = new unsigned long[ncolors + ngrays];

    if (!xcolor_map || !xpixel_map) {
        ngrays = 0;
        return FALSE;
    }
  			// First allocate a color cube
    ramp = 255.0 / (nlevels - 1);

    nreds = nlevels;
    ngreens = nlevels;
    nblues = nlevels;
    i = 0;
    for (r = 0; r < nreds; r++) {
	for (g = 0; g < ngreens; g++) {
	  for (b = 0; b < nblues; b++) {
	    int red   =  GAMMA(r * ramp);
	    int green =  GAMMA(g * ramp);
	    int blue  =  GAMMA(b * ramp);
	    xcolor_map[i] = _dpy->Alloc_color(red, green, blue);
	    xpixel_map[i] = xcolor_map[i]->pixel;
	    i++;
	  }
	}
    }
    ncolors = i;

				// Try to allocate a gray ramp
    grayoffset = ncolors;

    ramp = 255.0 / (ngrays - 1);

    for (i = 0; i < ngrays; i++) {
      int val = GAMMA(i * ramp);
      xcolor_map[i + grayoffset] = _dpy->Alloc_color(val, val, val);
      xpixel_map[i + grayoffset] = xcolor_map[i+grayoffset]->pixel;
    }

				// Done - now remember some details about the color cube
    size = ncolors + ngrays;

				// Setup the lookup tables used by Get_color
    for (i = 0; i < 256; i++) {
	float v = i / 255.0;
	reds[i]   = (int)(.5 + v * (nreds-1)) * ngreens * nblues;
	greens[i] = (int)(.5 + v * (ngreens-1)) * nblues;
	blues[i]  = (int)(.5 + v * (nblues-1));

	if (ngrays) 
	  grays[i]  = (int)(.5 + v * (ngrays-1)) + grayoffset;	  
	else
	  grays[i]  = reds[i] + greens[i] + blues[i];
    }

    return TRUE;
}

//
// Returns the table used to map between rgb colors and their pixel values,
// as well as the size of each component in the table.
//
unsigned long *
Pad_ColorCube::Get_device_pixel_map(int &numreds, int &numgreens, int &numblues)
{
    numreds = nreds;
    numgreens = ngreens;
    numblues = nblues;
    return xpixel_map;
}

//
// Returns the array of X colors used by the pad color cube.
// Other languages such as Java need this.
//
XColor **
Pad_ColorCube::Get_xcolor_map(void)
{
    return xcolor_map;
}
