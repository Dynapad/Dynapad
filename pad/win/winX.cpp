/* 
 * Modified from tkWinDraw.c --
 *
 *	This file contains the Xlib emulation functions pertaining to
 *	actually drawing objects on a window.
 *
 * Copyright (c) 1995 Sun Microsystems, Inc.
 * Copyright (c) 1994 Software Research Associates, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * SCCS: @(#) tkWinDraw.c 1.18 96/03/01 17:43:41
 */

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
#include <windows.h>

#include "tkWinInt.h"
#include "winX.h"
#include "../generic/display.h"
#include  "../generic/colorcube.h"
#include "../generic/win.h"
#include "../generic/global.h"

#include <math.h>

class Pad_Display;
/*
 * These macros convert between X's bizarre angle units to radians.
 */

#define Pad_AngleToRadians(a) ((double)(a) / 64 * PI / 180);

/*
 * Translation table between X gc functions and Win32 raster op modes.
 */

static int ropModes[] = {
    R2_BLACK,			/* GXclear */
    R2_MASKPEN,			/* GXand */
    R2_MASKPENNOT,		/* GXandReverse */
    R2_COPYPEN,			/* GXcopy */
    R2_MASKNOTPEN,		/* GXandInverted */
    R2_NOT,			/* GXnoop */
    R2_XORPEN,			/* GXxor */
    R2_MERGEPEN,		/* GXor */
    R2_NOTMERGEPEN,		/* GXnor */
    R2_NOTXORPEN,		/* GXequiv */
    R2_NOT,			/* GXinvert */
    R2_MERGEPENNOT,		/* GXorReverse */
    R2_NOTCOPYPEN,		/* GXcopyInverted */
    R2_MERGENOTPEN,		/* GXorInverted */
    R2_NOTMASKPEN,		/* GXnand */
    R2_WHITE			/* GXset */
};

/*
 * Translation table between X gc functions and Win32 BitBlt op modes.  Some
 * of the operations defined in X don't have names, so we have to construct
 * new opcodes for those functions.  This is arcane and probably not all that
 * useful, but at least it's accurate.
 */

#define NOTSRCAND	(DWORD)0x00220326 /* dest = (NOT source) AND dest */
#define NOTSRCINVERT	(DWORD)0x00990066 /* dest = (NOT source) XOR dest */
#define SRCORREVERSE	(DWORD)0x00DD0228 /* dest = source OR (NOT dest) */
#define SRCNAND		(DWORD)0x007700E6 /* dest = NOT (source AND dest) */

static int bltModes[] = {
    BLACKNESS,			/* GXclear */
    SRCAND,			/* GXand */
    SRCERASE,			/* GXandReverse */
    SRCCOPY,			/* GXcopy */
    NOTSRCAND,			/* GXandInverted */
    PATCOPY,			/* GXnoop */
    SRCINVERT,			/* GXxor */
    SRCPAINT,			/* GXor */
    NOTSRCERASE,		/* GXnor */
    NOTSRCINVERT,		/* GXequiv */
    DSTINVERT,			/* GXinvert */
    SRCORREVERSE,		/* GXorReverse */
    NOTSRCCOPY,			/* GXcopyInverted */
    MERGEPAINT,			/* GXorInverted */
    SRCNAND,			/* GXnand */
    WHITENESS			/* GXset */
};

/*
 * The following raster op uses the source bitmap as a mask for the
 * pattern.  This is used to draw in a foreground color but leave the
 * background color transparent.
 */

#define MASKPAT		0x00E20746 /* dest = (src & pat) | (!src & dst) */

/*
 * The following two raster ops are used to copy the foreground and background
 * bits of a source pattern as defined by a stipple used as the pattern.
 */

#define COPYFG		0x00CA0749 /* dest = (pat & src) | (!pat & dst) */
#define COPYBG		0x00AC0744 /* dest = (!pat & src) | (pat & dst) */


/*
 * The followng typedef is used to pass Windows GDI drawing functions.
 */

typedef WINGDIAPI BOOL (WINAPI *WinDrawFunc) _ANSI_ARGS_((HDC dc,
			    CONST POINT* points, int npoints));

/*
 * Forward declarations for procedures defined in this file:
 */

static POINT *		ConvertPoints _ANSI_ARGS_((XPoint *points, int npoints,
			    int mode, RECT *bbox));
static void		DrawOrFillArc _ANSI_ARGS_((Display *display,
			    Drawable d, GC gc, int x, int y,
			    unsigned int width, unsigned int height,
			    int angle1, int angle2, int fill));
extern void		RenderObject _ANSI_ARGS_((HDC dc, GC gc,
			    XPoint* points, int npoints, int mode, HPEN pen,
			    WinDrawFunc func));


// For special reason, when call TkGetDrawableDC, it
// automatically select the windows palette to the dc,
// but it fails for the global Pad_prc->winDC and bufDC.
// so we have to force each X drawing function to call these
// two functions

static HPALETTE
Pad_SelectPalette(HDC dc) 
{
	Colormap colormap;
	HPALETTE palette;
	TkWinColormap *cmap;

    TkWinDrawable *twdPtr = (TkWinDrawable *)Pad_prc->win->id;
	TkWindow *winPtr = twdPtr->window.winPtr;
	if (winPtr == NULL) {
	    colormap = DefaultColormap(Pad_prc->win->dpy->display, DefaultScreen(Pad_prc->win->dpy->display));
	} else {
	    colormap = winPtr->atts.colormap;
	}
	cmap = (TkWinColormap *) colormap;
	palette = SelectPalette(dc, cmap->palette, FALSE);
    RealizePalette(dc);
	return palette;
}

static void
Pad_ReleasePalette(HDC dc, HPALETTE palette) { 
    SelectPalette(dc, palette, TRUE);
    RealizePalette(dc);
}

/*
 *----------------------------------------------------------------------
 *
 * PutPixel --
 *
 *	Set a single pixel in an image.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static int
PutPixel(XImage *image, int x, int y, unsigned long pixel)
{
    char *destPtr = &(image->data[(y * image->bytes_per_line)
    	+ (x * (image->bits_per_pixel >> 3))]);
    destPtr[0] = GetBValue(pixel);
    destPtr[1] = GetGValue(pixel);
    destPtr[2] = GetRValue(pixel);
    return 0;
}

/*
 *----------------------------------------------------------------------
 *
 * ConvertPoints --
 *
 *	Convert an array of X points to an array of Win32 points.
 *
 * Results:
 *	Returns the converted array of POINTs.
 *
 * Side effects:
 *	Allocates a block of memory that should not be freed.
 *
 *----------------------------------------------------------------------
 */

static POINT *
ConvertPoints(XPoint *points, int npoints, int mode, RECT *bbox)
{
    static POINT *winPoints = NULL; /* Array of points that is reused. */
    static int nWinPoints = -1;	    /* Current size of point array. */
    int i;

    /*
     * To avoid paying the cost of a malloc on every drawing routine,
     * we reuse the last array if it is large enough.
     */

    if (npoints > nWinPoints) {
	if (winPoints != NULL) {
	    delete winPoints;
	}
	winPoints = new POINT[npoints];
	if (winPoints == NULL) {
	    nWinPoints = -1;
	    return NULL;
	}
	nWinPoints = npoints;
    }

    bbox->left = bbox->right = points[0].x;
    bbox->top = bbox->bottom = points[0].y;
    
    if (mode == CoordModeOrigin) {
	for (i = 0; i < npoints; i++) {
	    winPoints[i].x = points[i].x;
	    winPoints[i].y = points[i].y;
	    bbox->left = MIN(bbox->left, winPoints[i].x);
	    bbox->right = MAX(bbox->right, winPoints[i].x);
	    bbox->top = MIN(bbox->top, winPoints[i].y);
	    bbox->bottom = MAX(bbox->bottom, winPoints[i].y);
	}
    } else {
	winPoints[0].x = points[0].x;
	winPoints[0].y = points[0].y;
	for (i = 1; i < npoints; i++) {
	    winPoints[i].x = winPoints[i-1].x + points[i].x;
	    winPoints[i].y = winPoints[i-1].y + points[i].y;
	    bbox->left = MIN(bbox->left, winPoints[i].x);
	    bbox->right = MAX(bbox->right, winPoints[i].x);
	    bbox->top = MIN(bbox->top, winPoints[i].y);
	    bbox->bottom = MAX(bbox->bottom, winPoints[i].y);
	}
    }
    return winPoints;
}


/*
 *----------------------------------------------------------------------
 *
 * XCreatePixmap --
 *
 *	Creates an in memory drawing surface.
 *
 * Results:
 *	Returns a handle to a new pixmap.
 *
 * Side effects:
 *	Allocates a new Win32 bitmap.
 *
 *----------------------------------------------------------------------
 */
 
Pixmap
Pad_CreatePixmap(Display* display, Drawable d, unsigned int width, 
			  unsigned int height, unsigned int depth)
{
    TkWinDrawable *newTwdPtr, *twdPtr;
    
    display->request++;

    newTwdPtr = (TkWinDrawable*) ckalloc(sizeof(TkWinDrawable));
    if (newTwdPtr == NULL) {
	return None;
    }
    newTwdPtr->type = TWD_BITMAP;
    newTwdPtr->bitmap.depth = depth;
    twdPtr = (TkWinDrawable *)d;
    if (twdPtr->type != TWD_BITMAP) {
	if (twdPtr->window.winPtr == NULL) {
	    newTwdPtr->bitmap.colormap = DefaultColormap(display,
		    DefaultScreen(display));
	} else {
	    newTwdPtr->bitmap.colormap = twdPtr->window.winPtr->atts.colormap;
	}
    } else {
	newTwdPtr->bitmap.colormap = twdPtr->bitmap.colormap;
    }
    newTwdPtr->bitmap.handle = CreateBitmap(width, height, 1, depth, NULL);

    if (newTwdPtr->bitmap.handle == NULL) {
	ckfree((char *) newTwdPtr);
	return (Pixmap)NULL;
    }
    
    return (Pixmap)newTwdPtr;
}

/*
 *----------------------------------------------------------------------
 *
 * XFreePixmap --
 *
 *	Release the resources associated with a pixmap.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Deletes the bitmap created by XCreatePixmap.
 *
 *----------------------------------------------------------------------
 */

void
Pad_FreePixmap(Display* display, Pixmap pixmap)
{
    TkWinDrawable *twdPtr = (TkWinDrawable *) pixmap;

    display->request++;
    if (twdPtr != NULL) {
	DeleteObject(twdPtr->bitmap.handle);
	ckfree((char *)twdPtr);
    }
}


/*
 *----------------------------------------------------------------------
 *
 * Pad_CopyArea --
 *
 *	Copies data from one drawable to another using block transfer
 *	routines.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Data is moved from a window or bitmap to a second window or
 *	bitmap.
 *
 *----------------------------------------------------------------------
 */

void
Pad_CopyArea(Display *display, Drawable src, Drawable dest, GC gc, 
			 int src_x, int src_y, unsigned int width, unsigned int height,
			 int dest_x, int dest_y)
{	
   	TkWinDCState state;
	HDC dc = TkWinGetDrawableDC(Pad_prc->win->dpy->display, Pad_prc->win->id, &state); 
    BitBlt(dc, dest_x, dest_y, width, height, Pad_prc->bufDC, src_x, src_y,
	   bltModes[gc->function]);
	TkWinReleaseDrawableDC(Pad_prc->win->id, dc, &state);
}

/*
 *----------------------------------------------------------------------
 *
 * Pad_DrawString --
 *
 *	Draw a single string in the current font.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Renders the specified string in the drawable.
 *
 *----------------------------------------------------------------------
 */

void
Pad_DrawString(Display *display, Drawable d, GC gc, int x, int y, 
			   _Xconst char* string, int length)
{
    HDC dc;
    HFONT oldFont;

    display->request++;

    if (d == None) {
	return;
    }

    dc = Pad_prc->bufDC;
	SetROP2(dc, ropModes[gc->function]);

            // added in case the SelectPalette funcition defined 
	    // in TkGetDrawableDc does not work well for the global
	    // dc, such as Pad_prc->bufDC
	HPALETTE palette = Pad_SelectPalette(dc);
    

	if ((gc->fill_style == FillStippled
	    || gc->fill_style == FillOpaqueStippled)
	    && gc->stipple != None) {
	TkWinDrawable *twdPtr = (TkWinDrawable *)gc->stipple;
	HBRUSH oldBrush, stipple;
	HBITMAP oldBitmap, bitmap;
	HDC dcMem;
	TEXTMETRIC tm;
	SIZE size;

	if (twdPtr->type != TWD_BITMAP) {
	//    panic("unexpected drawable type in stipple");
	}

	/*
	 * Select stipple pattern into destination dc.
	 */
	
	dcMem = CreateCompatibleDC(dc);

	stipple = CreatePatternBrush(twdPtr->bitmap.handle);
	SetBrushOrgEx(dc, gc->ts_x_origin, gc->ts_y_origin, NULL);
	oldBrush = SelectObject(dc, stipple);

	SetTextAlign(dcMem, TA_LEFT | TA_TOP);
	SetTextColor(dcMem, gc->foreground);
	SetBkMode(dcMem, TRANSPARENT);
	SetBkColor(dcMem, RGB(0, 0, 0));

	if (gc->font != None) {
	    oldFont = SelectObject(dcMem, (HFONT)gc->font);
	}

	/*
	 * Compute the bounding box and create a compatible bitmap.
	 */

	GetTextExtentPoint(dcMem, string, length, &size);
	GetTextMetrics(dcMem, &tm);
	size.cx -= tm.tmOverhang;
	bitmap = CreateCompatibleBitmap(dc, size.cx, size.cy);
	oldBitmap = SelectObject(dcMem, bitmap);

	/*
	 * The following code is tricky because fonts are rendered in multiple
	 * colors.  First we draw onto a black background and copy the white
	 * bits.  Then we draw onto a white background and copy the black bits.
	 * Both the foreground and background bits of the font are ANDed with
	 * the stipple pattern as they are copied.
	 */

	PatBlt(dcMem, 0, 0, size.cx, size.cy, BLACKNESS);
	TextOut(dcMem, 0, 0, string, length);
	BitBlt(dc, x, y - tm.tmAscent, size.cx, size.cy, dcMem,
		0, 0, 0xEA02E9);
	PatBlt(dcMem, 0, 0, size.cx, size.cy, WHITENESS);
	TextOut(dcMem, 0, 0, string, length);
	BitBlt(dc, x, y - tm.tmAscent, size.cx, size.cy, dcMem,
		0, 0, 0x8A0E06);

	/*
	 * Destroy the temporary bitmap and restore the device context.
	 */

	if (gc->font != None) {
	    SelectObject(dcMem, oldFont);
	}
	SelectObject(dcMem, oldBitmap);
	DeleteObject(bitmap);
	DeleteDC(dcMem);
	SelectObject(dc, oldBrush);
	DeleteObject(stipple);
    } else {
	SetTextAlign(dc, TA_LEFT | TA_BASELINE);
	SetTextColor(dc, gc->foreground);
	SetBkMode(dc, TRANSPARENT);
	if (gc->font != None) {
	    oldFont = SelectObject(dc, (HFONT)gc->font);
	}
	TextOut(dc, x, y, string, length);
	if (gc->font != None) {
	    SelectObject(dc, oldFont);
	}
    }

	Pad_ReleasePalette(dc, palette);
}

/*
 *----------------------------------------------------------------------
 *
 * Pad_FillRectangles --
 *
 *	Fill multiple rectangular areas in the given drawable.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Draws onto the specified drawable.
 *
 *----------------------------------------------------------------------
 */

void
Pad_FillRectangles(Display *display, Drawable d, GC gc, 
				   XRectangle *rectangles, int nrectangles)
{
    HDC dc;
    HBRUSH brush;
    int i;
    RECT rect;

    if (d == None) {
	return;
    }

    dc = Pad_prc->bufDC;
	    // added in case the SelectPalette funcition defined 
	    // in TkGetDrawableDc does not work well for the global
	    // dc, such as Pad_prc->bufDC
    HPALETTE palette = Pad_SelectPalette(dc);

    SetROP2(dc, ropModes[gc->function]);
    brush = CreateSolidBrush(gc->foreground);
    if ((gc->fill_style == FillStippled
	    || gc->fill_style == FillOpaqueStippled)
	    && gc->stipple != None) {
	TkWinDrawable *twdPtr = (TkWinDrawable *)gc->stipple;
	HBRUSH oldBrush, stipple;
	HBITMAP oldBitmap, bitmap;
	HDC dcMem;
	HBRUSH bgBrush = CreateSolidBrush(gc->background);

	if (twdPtr->type != TWD_BITMAP) {
//	    panic("unexpected drawable type in stipple");
	}

	/*
	 * Select stipple pattern into destination dc.
	 */
	
	stipple = CreatePatternBrush(twdPtr->bitmap.handle);
	SetBrushOrgEx(dc, gc->ts_x_origin, gc->ts_y_origin, NULL);
	oldBrush = SelectObject(dc, stipple);
	dcMem = CreateCompatibleDC(dc);

	/*
	 * For each rectangle, create a drawing surface which is the size of
	 * the rectangle and fill it with the background color.  Then merge the
	 * result with the stipple pattern.
	 */

	for (i = 0; i < nrectangles; i++) {
	    bitmap = CreateCompatibleBitmap(dc, rectangles[i].width,
		    rectangles[i].height);
	    oldBitmap = SelectObject(dcMem, bitmap);
	    rect.left = 0;
	    rect.top = 0;
	    rect.right = rectangles[i].width;
	    rect.bottom = rectangles[i].height;
	    FillRect(dcMem, &rect, brush);
	    BitBlt(dc, rectangles[i].x, rectangles[i].y, rectangles[i].width,
		    rectangles[i].height, dcMem, 0, 0, COPYFG);
	    if (gc->fill_style == FillOpaqueStippled) {
		FillRect(dcMem, &rect, bgBrush);
		BitBlt(dc, rectangles[i].x, rectangles[i].y,
			rectangles[i].width, rectangles[i].height, dcMem,
			0, 0, COPYBG);
	    }
	    SelectObject(dcMem, oldBitmap);
	    DeleteObject(bitmap);
	}
	
	DeleteDC(dcMem);
	SelectObject(dc, oldBrush);
	DeleteObject(stipple);
    } else {
	for (i = 0; i < nrectangles; i++) {
	    rect.left = rectangles[i].x;
	    rect.top = rectangles[i].y;
	    rect.right = rect.left + rectangles[i].width;
	    rect.bottom = rect.top + rectangles[i].height;
	    FillRect(dc, &rect, brush);
	}
    }
    DeleteObject(brush);

    Pad_ReleasePalette(dc, palette);
}

/*
 *----------------------------------------------------------------------
 *
 * RenderObject --
 *
 *	This function draws a shape using a list of points, a
 *	stipple pattern, and the specified drawing function.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static void
RenderObject(HDC dc, GC gc, XPoint *points, int npoints, int mode, 
			 HPEN pen, WinDrawFunc func)
{
    RECT rect;
    HPEN oldPen;
    HBRUSH oldBrush;
    POINT *winPoints = ConvertPoints(points, npoints, mode, &rect);
		
	if ((gc->fill_style == FillStippled
	    || gc->fill_style == FillOpaqueStippled)
	    && gc->stipple != None) {

	TkWinDrawable *twdPtr = (TkWinDrawable *)gc->stipple;
	HDC dcMem;
	LONG width, height;
	HBITMAP oldBitmap;
	int i;
	HBRUSH oldMemBrush;
	
	if (twdPtr->type != TWD_BITMAP) {
//	    panic("unexpected drawable type in stipple");
	}

    // changed by " + 1.2 " for the transparency to work well.
	width = rect.right - rect.left + 1.2;
	height = rect.bottom - rect.top + 1.2;

	/*
	 * Select stipple pattern into destination dc.
	 */
	
	oldBrush = SelectObject(dc, CreatePatternBrush(twdPtr->bitmap.handle));
	SetBrushOrgEx(dc, gc->ts_x_origin, gc->ts_y_origin, NULL);

	/*
	 * Create temporary drawing surface containing a copy of the
	 * destination equal in size to the bounding box of the object.
	 */
	
	dcMem = CreateCompatibleDC(dc);
	oldBitmap = SelectObject(dcMem, CreateCompatibleBitmap(dc, width,
		height));
	oldPen = SelectObject(dcMem, pen);
	BitBlt(dcMem, 0, 0, width, height, dc, rect.left, rect.top, SRCCOPY);

	/*
	 * Translate the object to 0,0 for rendering in the temporary drawing
	 * surface. 
	 */

	for (i = 0; i < npoints; i++) {
	    winPoints[i].x -= rect.left;
	    winPoints[i].y -= rect.top;
	}

	/*
	 * Draw the object in the foreground color and copy it to the
	 * destination wherever the pattern is set.
	 */

	SetPolyFillMode(dcMem, (gc->fill_rule == EvenOddRule) ? ALTERNATE
		: WINDING);
	oldMemBrush = SelectObject(dcMem, CreateSolidBrush(gc->foreground));
	(*func)(dcMem, winPoints, npoints);
	BitBlt(dc, rect.left, rect.top, width, height, dcMem, 0, 0, COPYFG);

	/*
	 * If we are rendering an opaque stipple, then draw the polygon in the
	 * background color and copy it to the destination wherever the pattern
	 * is clear.
	 */

	if (gc->fill_style == FillOpaqueStippled) {
	    DeleteObject(SelectObject(dcMem,
		    CreateSolidBrush(gc->background)));
	    (*func)(dcMem, winPoints, npoints);
	    BitBlt(dc, rect.left, rect.top, width, height, dcMem, 0, 0,
		    COPYBG);
	}

	SelectObject(dcMem, oldPen);
	DeleteObject(SelectObject(dcMem, oldMemBrush));
	DeleteObject(SelectObject(dcMem, oldBitmap));
	DeleteDC(dcMem);
    } else {
	oldPen = SelectObject(dc, pen);
	oldBrush = SelectObject(dc, CreateSolidBrush(gc->foreground));
	SetROP2(dc, ropModes[gc->function]);

	SetPolyFillMode(dc, (gc->fill_rule == EvenOddRule) ? ALTERNATE
		: WINDING);

	(*func)(dc, winPoints, npoints);

	SelectObject(dc, oldPen);
    }
    DeleteObject(SelectObject(dc, oldBrush));
}

/*
 *----------------------------------------------------------------------
 *
 * Pad_DrawLines --
 *
 *	Draw connected lines.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Renders a series of connected lines.
 *
 *----------------------------------------------------------------------
 */

void
Pad_DrawLines(Display *display, Drawable d, GC gc, XPoint *points, 
			  int npoints, int mode)
{
    HPEN pen;
	HDC dc = Pad_prc->bufDC;
    
    if (d == None) {
	return;
    }

    pen = CreatePen(PS_SOLID, gc->line_width, gc->foreground);
		
            // added in case the SelectPalette funcition defined 
	    // in TkGetDrawableDc does not work well for the global
	    // dc, such as Pad_prc->bufDC
    HPALETTE palette = Pad_SelectPalette(dc);
    RenderObject(dc, gc, points, npoints, mode, pen, Polyline);
    DeleteObject(pen);
    Pad_ReleasePalette(dc, palette);
 
}

/*
 *----------------------------------------------------------------------
 *
 * Pad_FillPolygon --
 *
 *	Draws a filled polygon.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Draws a filled polygon on the specified drawable.
 *
 *----------------------------------------------------------------------
 */

void
Pad_FillPolygon(Display *display, Drawable d, GC gc, XPoint *points, 
				int npoints, int shape, int mode)
{
    HPEN pen;
    HDC dc;
    if (d == None) {
	return;
    }

    dc = Pad_prc->bufDC;
    pen = GetStockObject(NULL_PEN);
            // added in case the SelectPalette funcition defined 
	    // in TkGetDrawableDc does not work well for the global
	    // dc, such as Pad_prc->bufDC
    HPALETTE palette = Pad_SelectPalette(dc);
    RenderObject(dc, gc, points, npoints, mode, pen, Polygon);
    Pad_ReleasePalette(dc, palette);
}

/*
 *----------------------------------------------------------------------
 *
 * Pad_DrawRectangle --
 *
 *	Draws a rectangle.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Draws a rectangle on the specified drawable.
 *
 *----------------------------------------------------------------------
 */

void
Pad_DrawRectangle(Display *display, Drawable d, GC gc, int x, int y, 
				  unsigned int width, unsigned int height)
{
    HPEN pen, oldPen;
    HBRUSH oldBrush;
    HDC dc;

    if (d == None) {
	return;
    }

    dc = Pad_prc->bufDC;

    pen = CreatePen(PS_SOLID, gc->line_width, gc->foreground);
    oldPen = SelectObject(dc, pen);
    oldBrush = SelectObject(dc, GetStockObject(NULL_BRUSH));
    SetROP2(dc, ropModes[gc->function]);

    Rectangle(dc, x, y, x+width+1, y+height+1);

    DeleteObject(SelectObject(dc, oldPen));
    SelectObject(dc, oldBrush);
}

/*
 *----------------------------------------------------------------------
 *
 * Pad_DrawArc --
 *
 *	Draw an arc.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Draws an arc on the specified drawable.
 *
 *----------------------------------------------------------------------
 */

void
Pad_DrawArc(Display *display, Drawable d, GC gc, int x, int y, 
			unsigned int width, unsigned int height, int angle1, int angle2)
{
    display->request++;

    DrawOrFillArc(display, d, gc, x, y, width, height, angle1, angle2, 0);
}

/*
 *----------------------------------------------------------------------
 *
 * Pad_FillArc --
 *
 *	Draw a filled arc.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Draws a filled arc on the specified drawable.
 *
 *----------------------------------------------------------------------
 */

void
Pad_FillArc(Display *display, Drawable d, GC gc, int x, int y, 
			unsigned int width, unsigned int height, int angle1, int angle2)
{
    display->request++;

    DrawOrFillArc(display, d, gc, x, y, width, height, angle1, angle2, 1);
}

/*
 *----------------------------------------------------------------------
 *
 * DrawOrFillArc --
 *
 *	This procedure handles the rendering of drawn or filled
 *	arcs and chords.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Renders the requested arc.
 *
 *----------------------------------------------------------------------
 */

static void
DrawOrFillArc(Display *display, Drawable d, GC gc, int x, int y, 
			  unsigned int width, unsigned int height, 
			  int angle1, int angle2, int fill)
{
    HDC dc;
    HBRUSH brush, oldBrush;
    HPEN pen, oldPen;
    int xr, yr, xstart, ystart, xend, yend;
    double radian_start, radian_end, radian_tmp;

    if (d == None) {
	return;
    }

    dc = Pad_prc->bufDC;
    SetROP2(dc, ropModes[gc->function]);

            // added in case the SelectPalette funcition defined 
	    // in TkGetDrawableDc does not work well for the global
	    // dc, such as Pad_prc->bufDC
    HPALETTE palette = Pad_SelectPalette(dc);
    

    /*
     * Convert the X arc description to a Win32 arc description.
     */

    xr = (width % 2) ? (width / 2) : ((width - 1) / 2);
    yr = (height % 2) ? (height / 2) : ((height - 1) / 2);

    radian_start = Pad_AngleToRadians(angle1);
    radian_end = Pad_AngleToRadians(angle1+angle2);
    if( angle2 < 0 ) {
	radian_tmp = radian_start;
	radian_start = radian_end;
	radian_end = radian_tmp;
    }

    xstart = x + (int) ((double)xr * (1+cos(radian_start)) + 0.5);
    ystart = y + (int) ((double)yr * (1-sin(radian_start)) + 0.5 );
    xend = x + (int) ((double)xr * (1+cos(radian_end)) + 0.5 );
    yend = y + (int) ((double)yr * (1-sin(radian_end)) + 0.5 );

    /*
     * Now draw a filled or open figure.
     */

    if (!fill) {
	pen = CreatePen(PS_SOLID, gc->line_width, gc->foreground);
	oldPen = SelectObject(dc, pen);
	oldBrush = SelectObject(dc, GetStockObject(NULL_BRUSH));
	Arc(dc, x, y, x + width, y + height, xstart, ystart, xend, yend);
	DeleteObject(SelectObject(dc, oldPen));
	SelectObject(dc, oldBrush);
    } else {
	brush = CreateSolidBrush(gc->foreground);
	oldBrush = SelectObject(dc, brush);
	oldPen = SelectObject(dc, GetStockObject(NULL_PEN));
	if (gc->arc_mode == ArcChord) {
	    Chord(dc, x, y, x + width, y + height, xstart, ystart, xend, yend);
	} else if ( gc->arc_mode == ArcPieSlice ) {
	    Pie(dc, x, y, x + width, y + height, xstart, ystart, xend, yend);
	}
	DeleteObject(SelectObject(dc, oldBrush));
	SelectObject(dc, oldPen);
    }
	
    Pad_ReleasePalette(dc, palette);
}

/*
 *----------------------------------------------------------------------
 *
 * Pad_DrawLine --
 *
 *	Draw a single line between two points in a given drawable. 
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Draws a single line segment.
 *
 *----------------------------------------------------------------------
 */

void
Pad_DrawLine(Display *display, Drawable d, GC gc, 
			 int x1, int y1, int x2, int y2)
{
    XPoint points[2];

    points[0].x = x1;
    points[0].y = y1;
    points[1].x = x2;
    points[1].y = y2;
    Pad_DrawLines(display, d, gc, points, 2, CoordModeOrigin);
}

/*
 *----------------------------------------------------------------------
 *
 * Pad_FillRectangle --
 *
 *	Fills a rectangular area in the given drawable.  This procedure
 *	is implemented as a call to Pad_FillRectangles.
 *
 * Results:
 *	None
 *
 * Side effects:
 *	Fills the specified rectangle.
 *
 *----------------------------------------------------------------------
 */

void
Pad_FillRectangle(Display *display, Drawable d, GC gc, int x, int y, 
				  unsigned int width, unsigned int height)
{
    XRectangle rectangle;
    rectangle.x = x;
    rectangle.y = y;
    rectangle.width = width;
    rectangle.height = height;
    Pad_FillRectangles(display, d, gc, &rectangle, 1);
}


void
Pad_DestroyImage(XImage *imagePtr)    
{
    if (imagePtr != (XImage *) NULL) {
		if (imagePtr->data != NULL) {
			//ckfree((char *)imagePtr->data);
			imagePtr->data = NULL;
		}
		ckfree((char *)imagePtr);
    }
    
}

void
Pad_CopyGC (Display * display, GC srcGC, unsigned long mask, GC destGC) 
{
    destGC->function = 		(mask & GCFunction) 	?srcGC->function	:destGC->function;
    destGC->plane_mask = 	(mask & GCPlaneMask) 	?srcGC->plane_mask 	:destGC->plane_mask;
    destGC->foreground = 	(mask & GCForeground) 	?srcGC->foreground 	:destGC->foreground;
    destGC->background = 	(mask & GCBackground) 	?srcGC->background 	:destGC->background;
    destGC->line_width = 	(mask & GCLineWidth)	?srcGC->line_width	:destGC->line_width;	
    destGC->line_style = 	(mask & GCLineStyle)	?srcGC->line_style	:destGC->line_style;
    destGC->cap_style =  	(mask & GCCapStyle)	?srcGC->cap_style		:destGC->cap_style;
    destGC->join_style = 	(mask & GCJoinStyle)	?srcGC->join_style	:destGC->join_style;
    destGC->fill_style =  	(mask & GCFillStyle)	?srcGC->fill_style	:destGC->fill_style;
    destGC->fill_rule =  	(mask & GCFillRule)	?srcGC->fill_rule		:destGC->fill_rule;
    destGC->arc_mode = 		(mask & GCArcMode)	?srcGC->arc_mode		:destGC->arc_mode;
    destGC->tile = 		(mask & GCTile)		?srcGC->tile		:destGC->tile;
    destGC->stipple = 		(mask & GCStipple)	?srcGC->stipple		:destGC->stipple;
    destGC->ts_x_origin = 	(mask & GCTileStipXOrigin)	?srcGC->ts_x_origin:destGC->ts_x_origin;
    destGC->ts_y_origin = 	(mask & GCTileStipYOrigin)	?srcGC->ts_y_origin:destGC->ts_y_origin;
    destGC->font = 		(mask & GCFont)		?srcGC->font		:destGC->font;
    destGC->subwindow_mode = 	(mask & GCSubwindowMode)?srcGC->subwindow_mode	:destGC->subwindow_mode;
    destGC->graphics_exposures = (mask & GCGraphicsExposures)?srcGC->graphics_exposures:destGC->graphics_exposures;
    destGC->clip_x_origin = 	(mask & GCClipXOrigin)	?srcGC->clip_x_origin	:destGC->clip_x_origin;
    destGC->clip_y_origin = 	(mask & GCClipYOrigin)	?srcGC->clip_y_origin	:destGC->clip_y_origin;
    destGC->clip_mask = 	(mask & GCClipMask)	?srcGC->clip_mask		:destGC->clip_mask;
    destGC->dash_offset = 	(mask & GCDashOffset)	?srcGC->dash_offset	:destGC->dash_offset;
    destGC->dashes = 		(mask & GCDashList)	?srcGC->dashes		:destGC->dashes;

}

void 
Pad_SetDashes(Display *display, GC gc, int dash_offset, _Xconst char* dash_list, int n)
{
}

void 
Pad_ShapeCombineMask (Display*, Window, int, int , int, Pixmap,int) 
{
}

Bool 
Pad_CheckIfEvent(Display *, XEvent *, Bool (*) (Display *, XEvent *, XPointer), XPointer)
{
    return 0;
}
                
Bool 
Pad_QueryExtension(Display *, _Xconst char *, int *, int *, int *) 
{
    return 0;
}

int
Pad_ScreenNumberOfScreen(Screen *)
{
    return 0;
}

void
Pad_SetFont(Display*, GC, Font) {}

void 
Pad_SetTile(Display *, GC, Pixmap) {}
 
/*
 *----------------------------------------------------------------------
 *
 * Pad_PutImage --
 *
 *	Copies a subimage from an in-memory image to a rectangle of
 *	of the specified drawable.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Draws the image on the specified drawable.
 *
 *----------------------------------------------------------------------
 */

void
Pad_PutImage(unsigned long *colors, int ncolors, Display *display, 
			 Drawable d, GC gc, XImage *image, int src_x, int src_y, 
			 int dest_x, int dest_y, unsigned int width, unsigned int height)
{
	
    HDC dc, dcMem;
 
    BITMAPINFO *infoPtr;
    HBITMAP bitmap, oldBitmap;
    char *data;

    dc = Pad_prc->bufDC;
    display->request++;

            // added in case the SelectPalette funcition defined 
	    // in TkGetDrawableDc does not work well for the global
	    // dc, such as Pad_prc->bufDC
	HPALETTE palette = Pad_SelectPalette(dc);
	
    TkWinColormap *cmap = (TkWinColormap *) Pad_prc->win->dpy->colormap;

    dcMem = CreateCompatibleDC(dc);

    if (image->bits_per_pixel == 1) {
	data = TkAlignImageData(image, sizeof(WORD), MSBFirst);
	bitmap = CreateBitmap(width, height, 1, 1, data);
	SetTextColor(dc, gc->foreground);
	SetBkColor(dc, gc->background);
	ckfree(data);
    } else {    
	int i, usePalette;

	/*
	 * Do not use a palette for TrueColor images.
	 */
	
	usePalette = (image->bits_per_pixel < 24);
	
	if (usePalette) {
	    infoPtr = (BITMAPINFO*) ckalloc(sizeof(BITMAPINFOHEADER)
		    + sizeof(DWORD)*ncolors);
		
	} else {
	    infoPtr = (BITMAPINFO*) ckalloc(sizeof(BITMAPINFOHEADER));
	}
	
	infoPtr->bmiHeader.biSize = sizeof(BITMAPINFOHEADER);
	infoPtr->bmiHeader.biWidth = image->width;

	/*
	 * The following code works around a bug in Win32s.  CreateDIBitmap
	 * fails under Win32s for top-down images.  So we have to reverse the
	 * order of the scanlines.  If we are not running under Win32s, we can
	 * just declare the image to be top-down.
	 */

	if ((GetVersion() & 0x80000000)) {
	    int y;
	    char *srcPtr, *dstPtr, *temp;

	    temp = (char *) ckalloc((unsigned)image->bytes_per_line);
	    srcPtr = image->data;
	    dstPtr = image->data+(image->bytes_per_line * (image->height - 1));
	    for (y = 0; y < (image->height/2); y++) {
		memcpy(temp, srcPtr, image->bytes_per_line);
		memcpy(srcPtr, dstPtr, image->bytes_per_line);
		memcpy(dstPtr, temp, image->bytes_per_line);
		srcPtr += image->bytes_per_line;
		dstPtr -= image->bytes_per_line;
	    }
	    ckfree(temp);
	    infoPtr->bmiHeader.biHeight = image->height; /* Bottom-up order */
	} else {
	    infoPtr->bmiHeader.biHeight = -image->height; /* Top-down order */
	}
	infoPtr->bmiHeader.biPlanes = 1;
	infoPtr->bmiHeader.biBitCount = image->bits_per_pixel;
	infoPtr->bmiHeader.biCompression = BI_RGB;
	infoPtr->bmiHeader.biSizeImage = 0;
	infoPtr->bmiHeader.biXPelsPerMeter = 0;
	infoPtr->bmiHeader.biYPelsPerMeter = 0;
	infoPtr->bmiHeader.biClrImportant = 0;

	if (usePalette) {
		
	    infoPtr->bmiHeader.biClrUsed = ncolors;
	    for (i = 0; i < ncolors; i++) {
			
		   infoPtr->bmiColors[i].rgbRed = GetRValue(colors[i]);
		   infoPtr->bmiColors[i].rgbGreen = GetGValue(colors[i]);
		   infoPtr->bmiColors[i].rgbBlue = GetBValue(colors[i]);
		   infoPtr->bmiColors[i].rgbReserved = 0;
	
	    }
		
	} else {
	    infoPtr->bmiHeader.biClrUsed = 0;
	}
	bitmap = CreateDIBitmap(dc, &infoPtr->bmiHeader, CBM_INIT,
		image->data, infoPtr, DIB_RGB_COLORS);
	if (!usePalette) {
    	 ckfree((char *)infoPtr);
	}
    }
    oldBitmap = SelectObject(dcMem, bitmap);
    BitBlt(dc, dest_x, dest_y, width, height, dcMem, src_x, src_y, SRCCOPY);
    DeleteObject(SelectObject(dcMem, oldBitmap));
    DeleteDC(dcMem);

    Pad_ReleasePalette(dc, palette);   
}


/*
 *----------------------------------------------------------------------
 *
 * Pad_CreateImage --
 *
 *	Allocates storage for a new XImage.
 *
 * Results:
 *	Returns a newly allocated XImage.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

XImage *
Pad_CreateImage(Display *display, Visual *visual, 
				unsigned int depth, int format, int offset, 
				char *data, unsigned int width, 
				unsigned int height, int bitmap_pad, 
				int bytes_per_line)
{
    XImage* imagePtr = (XImage *) ckalloc(sizeof(XImage));
    imagePtr->width = width;
    imagePtr->height = height;
    imagePtr->xoffset = offset;
    imagePtr->format = format;
    imagePtr->data = data;
    imagePtr->byte_order = LSBFirst;
    imagePtr->bitmap_unit = 32;
    imagePtr->bitmap_bit_order = LSBFirst;
    imagePtr->bitmap_pad = bitmap_pad;
    imagePtr->depth = depth;

    /*
     * Round to the nearest word boundary.
     */
    
    imagePtr->bytes_per_line = bytes_per_line ? bytes_per_line
 	: ((depth * width + 31) >> 3) & ~3;

    /*
     * If the screen supports TrueColor, then we use 3 bytes per pixel, and
     * we have to install our own pixel routine.
     */
  
   if (visual->c_class == TrueColor) {
	HDC dc = GetDC(NULL);
	imagePtr->bits_per_pixel = GetDeviceCaps(dc, BITSPIXEL);
	ReleaseDC(NULL, dc);
	imagePtr->f.put_pixel = PutPixel;
    } else {
	imagePtr->bits_per_pixel = depth;
	imagePtr->f.put_pixel = NULL;
    }
    imagePtr->red_mask = visual->red_mask;
    imagePtr->green_mask = visual->green_mask;
    imagePtr->blue_mask = visual->blue_mask;
    imagePtr->f.create_image = NULL;
    imagePtr->f.destroy_image = NULL;
    imagePtr->f.get_pixel = NULL;
    imagePtr->f.sub_image = NULL;
    imagePtr->f.add_pixel = NULL;
    
    return imagePtr;
}




void
Pad_QueryColors(Display *dpy, Colormap colormap, XColor *defs_in_out, int ncolors)
{
	int i;

      TkWinColormap *cmap = (TkWinColormap *) colormap;
      PALETTEENTRY closeEntry;
 

      for (i=0; i<ncolors; i++) {
		GetPaletteEntries(cmap->palette, i, 1, &closeEntry);
		defs_in_out[i].red = closeEntry.peRed << 8;
		defs_in_out[i].green = closeEntry.peGreen << 8;
		defs_in_out[i].blue = closeEntry.peBlue << 8;
		defs_in_out[i].flags = DoRed | DoGreen | DoBlue;
		// in Win32, the color->pixel is PALETTERGB value, which is rgba 
		// mode, not 0-256 index, so redefine it
		defs_in_out[i].pixel = PALETTERGB(closeEntry.peRed,
			closeEntry.peGreen,closeEntry.peBlue);
      }
	
}

		 /*
 *----------------------------------------------------------------------
 *
 * Pad_DCDrawLines --
 *
 *	Draw connected lines.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Renders a series of connected lines.
 *
 *----------------------------------------------------------------------
 */

void
Pad_DCDrawLines(Display *display, HDC dc, GC gc, XPoint *points, 
			  int npoints, int mode)
{
    HPEN pen;
    
    if (dc == NULL) {
	return;
    }

    pen = CreatePen(PS_SOLID, gc->line_width, gc->foreground);
    RenderObject(dc, gc, points, npoints, mode, pen, Polyline);
    DeleteObject(pen);
}
/*
 *----------------------------------------------------------------------
 *
 * Pad_DCDrawLine --
 *
 *	Draw a single line between two points in a given drawable. 
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Draws a single line segment.
 *
 *----------------------------------------------------------------------
 */

void
Pad_DCDrawLine(Display *display, HDC dc, GC gc, 
			 int x1, int y1, int x2, int y2)
{
    XPoint points[2];

    points[0].x = x1;
    points[0].y = y1;
    points[1].x = x2;
    points[1].y = y2;
    Pad_DCDrawLines(display, dc, gc, points, 2, CoordModeOrigin);
}


/*
 *----------------------------------------------------------------------
 *
 * Pad_DCCopyArea --
 *
 *	Copies data from one device context to another using block transfer
 *	routines.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Data is moved from a window or bitmap to a second window or
 *	bitmap.
 *
 *----------------------------------------------------------------------
 */

void
Pad_DCCopyArea(Display *display, HDC srcDC, HDC destDC, GC gc, 
			 int src_x, int src_y, unsigned int width, unsigned int height,
			 int dest_x, int dest_y)
{	
    BitBlt(destDC, dest_x, dest_y, width, height, srcDC, src_x, src_y,
	   bltModes[gc->function]);
}


void
Pad_SetLineAttributes(Display *display, GC gc, unsigned int line_width, 
					  int line_style, int cap_style, int join_style)
{
	gc->line_width = line_width;
    gc->line_style = line_style;
    gc->cap_style = cap_style;
    gc->join_style = join_style;
}


