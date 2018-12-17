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

#include "renderer.h"
#include "display.h"
#include "ZFontType1.h"
#include "transform.h"
#include "fontcache.h"
#include "stdio.h"
#include "global.h"



////////////////////////////////////////////////////////////////////////
// 
//   Pad_FontCache
//
////////////////////////////////////////////////////////////////////////

/* 

A simple caching scheme for zooming fonts.

Speeds up the situation where the same character is rendered
multiple times at the same size/font/transparency. This happens
e.g. when rendering a textfile item containing many lines of
text.
*/

Pad_FontCache::Pad_FontCache()
{
    bitmap = None;
    bitGC = 0;
    fillGC = 0;
    myGC = 0;

    display = NULL;
    Pad_renderer = NULL;

    drawable = 0;
    fgGC = 0;
    orig_stipple = None;
}

Pad_FontCache::~Pad_FontCache()
{
    if (bitmap)
      XFreePixmap(display, bitmap);

    if (fillGC)
      XFreeGC(display, fillGC);

    if (bitGC)
      XFreeGC(display, bitGC);
}

Pad_Bool
Pad_FontCache::Allocate(Display *disp)
{
    int i;
    XGCValues vals;
    Window root = RootWindowOfScreen(DefaultScreenOfDisplay(disp));

    display = disp;

    for (i = 0; i < PAD_FC_NUM_CHARS; i++) {
	cache[i].font = NULL;
	cache[i].x = i * PAD_FC_CHAR_WIDTH;
    }

    bitmap = XCreatePixmap(display, root, PAD_FC_CHAR_WIDTH * PAD_FC_NUM_CHARS, 
			   PAD_FC_CHAR_HEIGHT, 1);

    if (bitmap == None) {
	return FALSE;
    }

    vals.foreground = 1;
    bitGC = XCreateGC(display, bitmap, GCForeground, &vals);		      

    vals.foreground = 0;
    fillGC = XCreateGC(display, bitmap, GCForeground, &vals);

    myGC = 0;

    return TRUE;
}


void
Pad_FontCache::Setup(Pad_Renderer *r, Drawable dbl, GC gc, Pixmap stipple)
{
    // called before rendering some text

    Pad_renderer = r;
    drawable = dbl;
    fgGC = gc;

    if (!myGC) {
	myGC = XCreateGC(display, dbl, 0, NULL);
	XSetStipple(display, myGC, bitmap);
	XSetFillStyle(display, myGC, FillStippled);
    }

    XCopyGC(display, fgGC, GCClipMask|GCForeground, myGC);

    if (stipple != None) {
	XSetFillStyle(display, bitGC, FillStippled);
	XSetStipple(display, bitGC, stipple);
    } else {
	XSetFillStyle(display, bitGC, FillSolid);
    }
}

//
// Draw_char - draw a character, using the cache if appropriate
//
void
Pad_FontCache::Draw_char(ZFontPoly *font, int c, float xoffs, int mag, Pixmap stipple)
{
    Pad_FontCacheEntry *cacheEntry;
    Pad_Point point;

    cacheEntry = In_cache(font, c, mag, stipple);

    point.Set(xoffs, 0);
    Pad_renderer->Local_to_screen(point);

    if (!cacheEntry) {		// Not in cache, so ...

	cacheEntry = Update_cache(font, c, mag, xoffs, point, stipple);	

	if (!cacheEntry)
	  return; // Cannot be cached
    }
    
    // Character is in cache - draw from cache

    int x, y;
    x = ((int)point.x) + cacheEntry->xoffset; 
    y = ((int)point.y) + cacheEntry->yoffset;

    XSetTSOrigin(display, myGC, x - cacheEntry->x, y);
    XFillRectangle(display, drawable, myGC, x, y, 
		   cacheEntry->width, cacheEntry->height);
}


static float xoffset;
extern Pad_Renderer *Pad_renderer;
static void add_vertex(short x, short y, float theta) { 
    if (theta == 0.0) {
	Pad_renderer->V2f(x + xoffset, y); 
    } else {
	Pad_renderer->V2f(x + xoffset, y, theta); 
    }
}

Pad_FontCacheEntry *
Pad_FontCache::Update_cache(ZFontPoly *font, int c, int size, 
			    float xoffs, Pad_Point &origin, Pixmap stipple)
{
    int bc = c - PAD_FC_BASE_CHAR;
    int width, height;
    int i, xmin, ymin;
    short cxmin, cymin, cxmax, cymax;
    Pad_FontCacheEntry *cacheEntry;
    Pad_Point pt1, pt2;
    XPoint *points;
    int numPoints;

    if (bc < 0 || bc >= PAD_FC_NUM_CHARS || size >= PAD_FC_CHAR_HEIGHT) {
				// Cannot be cached
	Draw_direct(font, c, xoffs);
	return NULL; 
     }

    if (!font->char_bbox(c, cxmin, cymin, cxmax, cymax)) {
				// No character definition for this character
	return(NULL);
    }

    // Get the bounds for this character in device coordinates

    pt1.Set(cxmin + xoffs, cymax);
    Pad_renderer->Local_to_screen(pt1);

    pt2.Set(cxmax + xoffs, cymin);
    Pad_renderer->Local_to_screen(pt2);

    xmin = (int)pt1.x;
    ymin = (int)pt1.y;
    width = (int)pt2.x - xmin;
    height = (int)pt2.y - ymin;

    if (width >= PAD_FC_CHAR_WIDTH || height >= PAD_FC_CHAR_HEIGHT) {
				// To big - draw directly
	Draw_direct(font, c, xoffs);
	return NULL;
    }

    // OK - now add to cache

    cacheEntry = &cache[bc];
    cacheEntry->font = font;
    cacheEntry->width = width;
    cacheEntry->height = height;
    cacheEntry->size = size;
    cacheEntry->stipple = stipple;
    cacheEntry->xoffset = xmin - (int)origin.x;
    cacheEntry->yoffset = ymin - (int)origin.y;


/*
    printf("cache %d width=%d, height=%d, size=%d, xoffs=%d, yoffs=%d\n",
	   c, cacheEntry->width, cacheEntry->height,
	   cacheEntry->size, cacheEntry->xoffset, cacheEntry->yoffset);

*/

    xoffset = xoffs;
    Pad_renderer->Begin_polygon();
    font->Run(c, add_vertex);
    Pad_renderer->End_polygon();
    points = ((Pad_XRenderer*)Pad_renderer)->Get_device_points(numPoints);

    xmin -= cacheEntry->x;

    for (i = 0; i < numPoints; i++) {
	points[i].x -= xmin;
	points[i].y -= ymin;
    }

    // Must clear the bitmap
    XFillRectangle(display, bitmap, fillGC,
		   cacheEntry->x, 0, cacheEntry->width, cacheEntry->height);

    XFillPolygon(display, bitmap, bitGC,
		 points, numPoints, Nonconvex, CoordModeOrigin);

    return cacheEntry;
}

void
Pad_FontCache::Draw_direct(ZFontPoly *font, int c, float xoffs)
{
    xoffset = xoffs;
    Pad_renderer->Begin_polygon();
    font->Run(c, add_vertex);
    Pad_renderer->End_polygon();
}

Pad_FontCacheEntry *
Pad_FontCache::In_cache(ZFontPoly *font, int c, int size, Pixmap stipple)
{
    int bc = c - PAD_FC_BASE_CHAR;

    if (bc >= 0 && bc < PAD_FC_NUM_CHARS) {
	Pad_FontCacheEntry *e = &cache[bc];
	if ((e->font == font && 
	     e->size == size &&
	     e->stipple == stipple)) {
	    return e;
	}
    }
    return NULL;
}

Pad_FontCacheList::Pad_FontCacheList(Display *disp, int size)
{
    int i;
    length = size;
    list = new Pad_FontCache[length];

    for (i = 0; i < length; i++) {
	if (!(list[i].Allocate(disp))) {
	    // Could not allocate cache - 
	    // just use those we could allocate
	    cerr << "Error - ran out of space for fontcache" << endl;
	    length = i;
	    break;
	}
    }
}

Pad_FontCacheList::~Pad_FontCacheList()
{
    delete [] list;
    list = NULL;
}











