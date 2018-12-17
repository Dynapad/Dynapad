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


// 
// JM - Visual effects: dissolve between drawables
//


#include "defs.h"
#include "effect.h"



//
// Pad_Dissolve - 
//     dissolves -src- onto -dst-. freq is 1, 2, or 3
//     indicating the speed of the dissolve. 
//

Pad_Effect::Pad_Effect(Display *display, Drawable dbl, 
		       int swidth, int sheight, int depth)
{
    XGCValues vals;

    dpy = display;
    dst = dbl;

    // a single line of the dither pattern that we are using
    mask = XCreatePixmap(dpy, dst, swidth, 1, depth);
    
    // a temporary copy of a line of -src- in, combined
    // with -mask- before copying to -result-
    strip = XCreatePixmap(dpy, dst, swidth, 1, depth);
    
    // a copy of -dst- used to double buffer the dissolve to make it smoother 
    result = XCreatePixmap(dpy, dst, swidth, sheight, depth);


    // Create GC's
    vals.foreground = 0xffffffff;
    vals.background = 0;
    vals.line_style = LineDoubleDash;
    vals.graphics_exposures = False;
    gc = XCreateGC(dpy, dst, GCLineStyle|GCForeground|GCBackground|GCGraphicsExposures, &vals);
    
    // used to combine -mask- and -strip- 
    vals.function = GXand;
    vals.graphics_exposures = False;
    and_gc = XCreateGC(dpy, dst, GCFunction|GCGraphicsExposures, &vals);
    
    // used to clear pixels in -result- 
    vals.function = GXandInverted;
    vals.graphics_exposures = False;
    and_inv_gc = XCreateGC(dpy, dst, GCFunction|GCGraphicsExposures, &vals);
    
    // used to combine -strip- and -result- 
    vals.function = GXor;
    vals.graphics_exposures = False;
    or_gc = XCreateGC(dpy, dst, GCFunction|GCGraphicsExposures, &vals);
}

Pad_Effect::~Pad_Effect()
{
				// free resources
    XFreeGC(dpy, gc);
    XFreeGC(dpy, and_gc);
    XFreeGC(dpy, or_gc);
    XFreeGC(dpy, and_inv_gc);
    
    XFreePixmap(dpy, strip);
    XFreePixmap(dpy, mask);
    XFreePixmap(dpy, result);
}

void 
Pad_Effect::Dissolve(Drawable src,
		     int sx, int sy, int swidth, int sheight, 
		     int dx, int dy, int speed, Pad_Bool (*interrupt_p)())
{
    char dash_list[2];
    char *dither_matrix;
    int freq, f, i, j, y;

    //
    // Ordered dither matrices
    //

    static char dither_matrix2[4] = {
	0, 2,
	3, 1,
    };
    static char dither_matrix3[9] = {
	6, 8, 4,
	1, 0, 3,
	5, 2, 7,
    };
    static char dither_matrix4[16] = {
	0,  8,  2, 10,
	12, 4, 14,  6,
	3, 11,  1,  9,
	15, 7, 13,  5,
    };
				// select a dither matrix 
    switch (speed) {
      case 1: // fast
        freq = 2;
	dither_matrix = dither_matrix2;
	break;
      case 2: // medium 
        freq = 3;
	dither_matrix = dither_matrix3;
	break;
      default: // slow
        freq = 4;
	dither_matrix = dither_matrix4;
	break;
    }
    
				// set up the dash list 
    dash_list[0] = 1; dash_list[1] = freq - 1;

    // copy -dst- to a temporary pixmap -result- to double buffer operation 
    XCopyArea(dpy, dst, result, gc, sx, sy, swidth, sheight, 0, 0);
    
    for (f = 0; f < (freq*freq); f++) {
      for (i = 0; i < freq; i++) {
	for (j = 0; j < freq; j++) {
	  if (dither_matrix[i * freq + j] == f) {
	    int n = 0; 
	    // draw the clip mask 
	    XSetDashes(dpy, gc, freq - j, dash_list, 2);
	    XDrawLine(dpy, mask, gc, 0, 0, swidth, 0);
	    
	    for (y = sy + i; y < sy + sheight; y += freq) {
	      int yoffs = y - sy;
	      // straight copy from -src- to -strip- 
	      XCopyArea(dpy, src, strip, gc,
			sx, y, swidth, 1, 0, 0);
	      
	      // mask out unwanted bits in -strip- 
	      XCopyArea(dpy, mask, strip, and_gc,
			0, 0, swidth, 1, 0, 0);
	      
	      // mask out unwanted bits in -result-
	      XCopyArea(dpy, mask, result, and_inv_gc, 0, 0,
			swidth, 1, 0, yoffs);
	      
	      // merge the strip data to -result- 
	      XCopyArea(dpy, strip, result, or_gc, 0, 0,
			swidth, 1, 0, yoffs);
	      
	      if (n % 100 == 0 && interrupt_p && interrupt_p()) {
		// I was interrupted - finish now
		XCopyArea(dpy, src, dst, gc, 
			  sx, sy, swidth, sheight, dx, dy);
		XSync(dpy, False);
		return;
	      }
	      n++;
	    }		
 
	    // now copy -result- to -dst- 
	    XCopyArea(dpy, result, dst, gc, 0, 0, swidth, sheight,
		      dx, dy);
	    
	    // make sure the X Server keeps up 
	    XSync(dpy, False);
	    
	    // exit the i/j loops
	    i = j = freq;
	  }
	}
      }
    }

				// This is a hack to fix a bug in some X servers where
				// the dashed line doesn't seem to get generated correctly
				// so the full image is not always dissolved.  So, we make sure
				// we complete the dissolve by copying the entire image
				// after the dissolve is complete.  This was noticed in
				// Accelerated X 2.1 for 8 and 16 bit servers.
    XCopyArea(dpy, src, dst, gc, sx, sy, swidth, sheight, dx, dy);

    XSync(dpy, False);
}

