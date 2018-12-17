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


#include "view.h"
#include "display.h"
#include "list.h"
#include "fontcache.h"
#include "colorcube.h"
#include "imagedata.h"
#include "tkcolor.h"
#include "tkborder.h"
#include "fontdata.h"
#include "global.h"
#include "win.h"


// Stipples

Pad_Stipple pad_stipples[17];

//////////////////////////////////////////////
//              Pad_Display definitions
//////////////////////////////////////////////  

static Pad_List displays;

				// NOTE: BBB - This is currently never called.
Pad_Display::~Pad_Display() 
{
    int i;
    for (i = 0; i <= 16; i++) {
	if (ditherStipples[i] != None) {
	    XFreePixmap(display, ditherStipples[i]);
	    ditherStipples[i] = None;
	}
    }
    if (fontcachelist) {
	delete fontcachelist;
	fontcachelist = NULL;
    }
    if (colorcube) {
	delete colorcube;
	colorcube = NULL;
    }
    displays.Remove(this);
}

Pad_Display::Pad_Display(Display *disp, Screen *scr, Visual *vis, Colormap cmap, int dpth)
{
    int colonIndex;

    display = disp;
    screen = scr;
    visual = vis;
    name = DisplayString(display);
    colonIndex = name.Strchr(':');
    if (colonIndex >= 0) {
	name.Insert('\0', colonIndex);
    }
    colorcube = NULL;
    widthmmofscreen = 0;
    heightmmofscreen = 0;
    width = WidthOfScreen(scr);
    height = HeightOfScreen(scr);
    depth = dpth;
    colormap = cmap;
    fontcachelist = NULL;

    gamma = 1.0;

    switch (vis->c_class) {
      case TrueColor:
      case DirectColor:
	truecolor = TRUE;
	break;
      default:
	truecolor = FALSE;
	break;
    }

    //
    // rootDrawable is set to a drawable that can be used with CreateGC
    // to create GC's suitable for the display/depth
    //
    if (depth == DefaultDepthOfScreen(scr)) {
	rootDrawable = RootWindowOfScreen(scr);
    } else {
	rootDrawable = XCreatePixmap(display, RootWindowOfScreen(scr),
		1, 1, depth);
    }

    Setup_dither();
    displays.Push_last(this);

    Pad_FontData::Set_default_font_path();

    // default fontcache size is 1 in unix
    Set_fontcache_size(1);

    bindInfoStale = TRUE;
    modeModMask = 0;
    metaModMask = 0;
    altModMask = 0;
    lockUsage = LU_IGNORE;
    numModKeyCodes = 0;
    modKeyCodes = NULL;
    stressPtr = NULL;
}

//
// Used to locate a suitable Pad_Display 
// object for the window. If one of the
// appropriate specifications already exists, return it.
// Else, allocate a new one.
// 
Pad_Display *
Pad_Get_display(Display *display, Screen *screen, Visual *visual, Colormap colormap, int depth)
{
    Pad_Iterator li;
    Pad_Display *dpy;

    DOLIST(li, displays, Pad_Display, dpy) {
	if ((dpy->depth == depth) &&
	    (dpy->display == display) &&
	    (dpy->screen == screen) &&
	    (dpy->visual == visual) &&
	    (dpy->colormap == colormap)) {

	    return(dpy);
	}
    }

    dpy = new Pad_Display(display, screen, visual, colormap, depth);

    return(dpy);
}

//
// Creates 17 dither patterns - pattern 0 has all the bits turned off,
// pattern 16 has all the bits turned on. This initialized both 4x4 X Bitmaps
// used as stipples in GC's, and also Pad_Stipple objects used for images.
//

static int dither4x4[4][4] =  {
    {0,8,2,10},
    {12,4,14,6},
    {3,11,1,9},
    {15,7,13,5}
};

void
Pad_Display::Setup_dither()
{
    int i, x, y, bit;
    char data[8], *row;

    for (i = 0; i <= 16; i++) {
	DOTIMES(y, 4) {
	    data[y] = 0;
	    data[y+4] = 0;
	    DOTIMES(x, 4) {
		// Use the dither4x4 matrix to determine if this bit is on
		bit = (i >= dither4x4[x][y]) ? 1 : 0;

		// set the bit in the array used to make the X Bitmap
		// mirror the pattern in x & y to make an 8x8 bitmap
		if (bit) {
		    data[y] |= (1<<x);
		    data[y] |= (1<<(4 + x));
		    data[4+y] |= (1<<x);
		    data[4+y] |= (1<<(4 + x));
		}

		// now setup the Pad_Stipple object. Note that Pad_Stipple
		// objects are 8x8, so we tile the 4x4 pattern 4 times.
		row = pad_stipples[i].pattern[y];
		row[x] = bit;
		row[x + 4] = bit;

		row = pad_stipples[i].pattern[y+4];
		row[x] = bit;
		row[x + 4] = bit;
	    } 
	}
	// Create the X Bitmap
	ditherStipples[i] = XCreateBitmapFromData(display, rootDrawable, data, 8, 8);
    }
}

int
Pad_Display::Screen_number(void)
{
    return(XScreenNumberOfScreen(screen));
}

void
Pad_Display::Add_image_data(Pad_ImageData *imageData)
{
    imageDatas.Push_last(imageData);
}

void
Pad_Display::Remove_image_data(Pad_ImageData *imageData)
{
    imageDatas.Remove(imageData);
}

Pad_ColorCube *
Pad_Display::Get_colorcube() 
{
    if (!colorcube) {
	colorcube = new Pad_ColorCube(this);
	colorcube->Allocate(COLORCUBE_LEVELS, gamma);
    }
    return(colorcube);
}

void
Pad_Display::Set_gamma(float g)
{
    Pad_Iterator ii;
    Pad_ImageData *imageData;

    gamma = g;
				// Only reallocate the colorcube if it was already
				// allocated.  Else, just set gamma variable and wait
				// until the colorcube is requested.
    if (colorcube) {
	delete colorcube;
	colorcube = new Pad_ColorCube(this);
	colorcube->Allocate(COLORCUBE_LEVELS, gamma);

				// Update all the image data that use the colorcube
	DOLIST(ii, imageDatas, Pad_ImageData, imageData) {
	    imageData->Cc_changed();
	}
    }
}

float
Pad_Display::Get_gamma()
{
    return gamma;
}

void
Pad_Display::Set_fontcache_size(int size)
{

    if (fontcachelist) {
	delete fontcachelist;
	fontcachelist = NULL;
    }
    if (size > 0) {
	fontcachelist = new Pad_FontCacheList(display, size);
    } else {
	fontcachelist = NULL;
    }
}

int 
Pad_Display::Get_fontcache_size()
{
    return (fontcachelist ? fontcachelist->Length() : 0);
}

Pad_FontCache *
Pad_Display::Get_fontcache(int font_id, int font_size)
{
    return (fontcachelist ? fontcachelist->Get(font_id + font_size) : (Pad_FontCache *)NULL);
}

    // Colors
    
XColor *
Pad_Display::Alloc_color(int r, int g, int b)
{
    XColor xcol;
    xcol.red = r * 257;
    xcol.green = g * 257;
    xcol.blue = b * 257;
        
    return Pad_AllocXColor(this, &xcol);
}

void
Pad_Display::Free_color(XColor *color)
{
    Pad_FreeXColor(this, color);
}

Pad_3DBorder 
Pad_Display::Alloc_border(int r, int g, int b)
{
    return Pad_Alloc3DBorder(this, r, g, b);
}

void
Pad_Display::Free_border(Pad_3DBorder border)
{
    Pad_Free3DBorder(this, border);
}


void *
Pad_Display::Get_font_data(char *name, int style)
{
    return Pad_FontData::Get_font_data(name, style);
}

//
// Grab a portion of screen and return it as image data.
// The portion of the screen grabbed is the region as specified
// relative to the specified window.
//
// If dimensions are specified, then de-res the grabbed image
// into the specified resolution - even if the result is distorted.
//
Pad_ImageData *
Pad_Display::Grab(Window win, int x, int y, int w, int h)
{
    return(Grab(win, w, h, x, y, w, h));
}

Pad_ImageData *
Pad_Display::Grab(Window win, int returnWidth, int returnHeight, int x, int y, int w, int h)
{
    int len;
    int row, col;
    XImage *image;
    unsigned int winWidth, winHeight;
    Pad_ImageData *imageData;
    unsigned char *colormapData;
    unsigned long *rgbData;
    int *collut, *rowlut;	// Lookup tables for de-resing
    Pad_Bool deres = FALSE;     // True if we de-res grabbed image

    if (win == None) {
				// No window specified, so grab from root
	win = RootWindowOfScreen(screen);
	winWidth = width;
	winHeight = height;
	
				// Range Checking on screen
	if (x < 0) {
	    w += x;
	    x = 0; 
	}
	if (y < 0) {
	    h += y;
	    y = 0; 
	}
	if (x+w > width) {
	    w = width - x;
	}
	if (y+h > height) {
	    h = height - y;
	}
    } else {
				// Grab from specific window.
	int winx, winy;
	int rootx, rooty;
	unsigned int border, depth;
	Window root;
	Window returnWin;

	if (!XGetGeometry(display, win, &root, &winx, &winy, &winWidth, &winHeight, &border, &depth)) {
	    Pad_errorString = "Unable to get window attributes for clicked-on window\n";
	    return(NULL);
	}

	XTranslateCoordinates(display, win, RootWindowOfScreen(screen), x, y, &rootx, &rooty, &returnWin);

				// Range Checking on screen
	if (rootx < 0) {
	    w += rootx;
	    x -= rootx;
	}
	if (y < 0) {
	    h += rooty;
	    y -= rooty;
	}
	if (rootx+w > width) {
	    w = w - (rootx+w - width);
	}
	if (rooty+h > height) {
	    h = h - (rooty+h - height);
	}

				// Range Checking within window
	if (w > (signed int)winWidth) {
	    w = winWidth;
	}
	if (h > (signed int)winHeight) {
	    h = winHeight;
	}
    }



				// Nothing to grab
    if ((w <= 0) || (h <= 0)) {
	Pad_errorString = "Empty region, nothing grabbed";
	return(NULL);
    }

    image = XGetImage(display, win, x, y, (unsigned int)w, (unsigned int)h, AllPlanes, ZPixmap);
    if (!image || !image->data) {
	Pad_errorString.Printf("Unable to get image (%d,%d %dx%d) from display", x, y, w, h);
	return(NULL);
    }

				// Don't return more than was grabbed
    if (returnWidth > w) {
	returnWidth = w;
    }
    if (returnHeight > h) {
	returnHeight = h;
    }

				// If we need to de-res the image, then compute look-up tables
    if ((returnWidth < w) || (returnHeight < h)) {
	deres = TRUE;

	collut = new int[returnWidth];
	rowlut = new int[returnHeight];
	for (col=0; col<returnWidth; col++) {
	    collut[col] = col * w / returnWidth;
	}
	for (row=0; row<returnHeight; row++) {
	    rowlut[row] = row * h / returnHeight;
	}
    }

    len = returnWidth * returnHeight;
    if (image->depth == 8) {
				// PseudoColor - just use pixels
	colormapData = new unsigned char[len];
	
	if (!deres) {
				// Image requested at full size, just get the whole thing.
				// Rows comes from Server aligned on 4-byte paragraphs
	    if ((w % 4) == 0) {
		memcpy(colormapData, image->data, len);
	    } else {
		int row;
		int srcWidth = w + 4-(w % 4);
		for (row=0; row<h; row++) {
		    memcpy(&colormapData[row*w], &image->data[row*srcWidth], w);
		}
	    }
	} else {
	    unsigned char *rowPtr;
	    unsigned char *destPtr;
	    int srcWidth;
				// Else, de-res image
				// Rows comes from Server aligned on 4-byte paragraphs
				// so if width is no a multiple of 4, then grab rows
				// on multiples of 4.
	    if ((w % 4) == 0) {
		srcWidth = w;
	    } else {
		srcWidth = w + 4-(w % 4);
	    }
	    destPtr = colormapData;
	    for (row=0; row<returnHeight; row++) {
		rowPtr = (unsigned char *)&image->data[rowlut[row] * srcWidth];
		for (col=0; col<returnWidth; col++) {
		    *destPtr++ = rowPtr[collut[col]];
		}
	    }
	}
	imageData = new Pad_ImageData(this, colormapData, returnWidth, returnHeight);
    } else {
				// Else, Truecolor, etc. - get pixels from server
	unsigned long *destPtr;
	Pad_Display *winDisplay;
	XWindowAttributes attr;
	Status status;
	Pad_ColorCube *cc;
	Pixel pixel;
	unsigned int r, g, b;

	rgbData = new unsigned long[len];
	destPtr = rgbData;
	status = XGetWindowAttributes(display, win, &attr);
	if (!status) {
	    Pad_errorString = "Unable to get window status";
	    XDestroyImage(image);
	    return(NULL);
	}
	winDisplay = Pad_Get_display(display, screen, attr.visual, attr.colormap, attr.depth);
	cc = winDisplay->Get_colorcube();

	if (!deres) {

				// Image requested at full size, just get the whole thing.
	    for (row=0; row<returnHeight; row++) {
		for (col=0; col<returnWidth; col++) {
		    pixel = (unsigned long)XGetPixel(image, col, row);
		    cc->Get_rgb(pixel, r, g, b);
		    *destPtr++ = CC_RGBA(r, g, b, 0);
		}
	    }
	} else {
				// Else, de-res image
	    for (row=0; row<returnHeight; row++) {
		for (col=0; col<returnWidth; col++) {
		    pixel = (unsigned long)XGetPixel(image, collut[col], rowlut[row]);
		    cc->Get_rgb(pixel, r, g, b);
		    *destPtr++ = CC_RGBA(r, g, b, 0);
		}
	    }
	}
	imageData = new Pad_ImageData(this, rgbData, returnWidth, returnHeight);
    }

    XDestroyImage(image);   
    if (deres) {
	delete [] collut;
	delete [] rowlut;
    }

    return(imageData);
}
