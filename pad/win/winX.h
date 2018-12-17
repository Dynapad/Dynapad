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

#ifndef WINX_H
#define WINX_H 1

extern "C" {
#  include <X11/Xlib.h>
}

#define XFree(data) {if ((data) != NULL) ckfree((char *) (data));}
#define XSynchronize(display, bool) {display->request++;}
#define XSync(display, bool) {display->request++;}
#define XVisualIDFromVisual(visual) (visual->visualid)

#ifdef XDestroyImage
#undef XDestroyImage
#endif

class Pad_Display;


#define XQueryColors	Pad_QueryColors
#define XAngleToRadians Pad_AngleToRadians
#define XCheckIfEvent   Pad_CheckIfEvent
#define XCopyArea       Pad_CopyArea
#define XCopyGC         Pad_CopyGC
#define XCreatePixmap   Pad_CreatePixmap
#define XCreateImage	Pad_CreateImage
#define XDestroyImage   Pad_DestroyImage
#define XDrawArc        Pad_DrawArc
#define XDrawLine       Pad_DrawLine
#define XDrawLines      Pad_DrawLines
#define XDrawRectangle  Pad_DrawRectangle
#define XDrawString     Pad_DrawString
#define XFillArc        Pad_FillArc
#define XFillPolygon    Pad_FillPolygon
#define XFillRectangle  Pad_FillRectangle
#define XFillRectangles Pad_FillRectangles
#define XFreePixmap     Pad_FreePixmap
#define XQueryExtension Pad_QueryExtension
#define XSetTile		Pad_SetTile
#define XSetLineAttributes Pad_SetLineAttributes
#define XScreenNumberOfScreen Pad_ScreenNumberOfScreen
#define XSetDashes      Pad_SetDashes
#define XSetFont		Pad_SetFont
#define XShapeCombineMask Pad_ShapeCombineMask

extern void 
Pad_CopyArea(Display *display, Drawable src, Drawable dest,
		GC gc, int src_x, int src_y, unsigned int width,
		unsigned int height, int dest_x, int dest_y);
		   
extern void 
Pad_CopyGC(Display * display, GC srcGC, unsigned long mask, GC destGC); 

extern void Pad_DrawString(Display* display, Drawable d, GC gc,
		int	x, int y, _Xconst char* string, int length);

extern void 
Pad_FillRectangle(Display* display, Drawable d, GC gc,
		int x, int y, unsigned int width, unsigned int height);

extern void 
Pad_FillRectangles(Display* display, Drawable d,GC gc,
		XRectangle*	rectangles, int nrectangles);

extern void 
Pad_DrawLine(Display* display, Drawable d, GC gc,
		int x1, int y1, int x2, int y2);

extern void 
Pad_DrawLines(Display* display, Drawable d, GC gc,
		XPoint*	points, int npoints, int mode);

extern void 
Pad_FillPolygon(Display* display, Drawable d, GC gc,
		XPoint* points, int npoints, int shape,int mode);

extern void 
Pad_DrawArc( Display* display, Drawable d, GC gc,
		int x, int y, unsigned int width,unsigned int height,
		int angle1, int angle2);

extern void 
Pad_FillArc(Display* display, Drawable d, GC gc, int x, int y, 
		unsigned int width, unsigned int height,
		int	angle1,int angle2);

extern int 
Pad_ScreenNumberOfScreen(Screen* screen);

extern void 
XSetFont(Display* display, GC gc, Font font);

extern Bool 
Pad_QueryExtension(Display *, _Xconst char *, int *, int *, int *);

extern Bool 
Pad_CheckIfEvent(Display *, XEvent *, Bool (*) (Display *, XEvent *, XPointer), XPointer);

extern void 
Pad_ShapeCombineMask (Display*, Window, int, int, int, Pixmap,int);

extern void 
Pad_SetDashes(Display *display, GC gc, int dash_offset, _Xconst char* dash_list, int n);

extern void Pad_CopyGC(Display *display, GC srcGC, unsigned long mask, GC destGC);

extern void
Pad_DestroyImage(XImage *imagePtr);

extern void	
Pad_PutImage(unsigned long *colors,int ncolors, 
		Display* display, Drawable d, GC gc, XImage* image, 
		int src_x, int src_y, int dest_x, int dest_y, 
		unsigned int width, unsigned int height);

extern XImage *
Pad_CreateImage(Display *display, Visual *visual, 
		unsigned int depth, int format, int offset, char *data, 
		unsigned int width, unsigned int height, 
		int bitmap_pad, int bytes_per_line);

extern Pixmap
Pad_CreatePixmap(Display* display, Drawable d, unsigned int width, 
		unsigned int height, unsigned int depth);

extern void
Pad_FreePixmap(Display* display, Pixmap pixmap); 

extern void
Pad_QueryColors(Display *dpy, Colormap colormap, XColor *defs_in_out, int ncolors);
  
extern void 
Pad_SetTile(Display*,GC, Pixmap);


extern void
Pad_DCDrawLine(Display *display, HDC dc, GC gc, 
		int x1, int y1, int x2, int y2);

extern void
Pad_DCDrawLines(Display *display, HDC dc, GC gc, XPoint *points, 
		int npoints, int mode);

extern void
Pad_DCCopyArea(Display *display, HDC srcDC, HDC destDC, GC gc, 
		int src_x, int src_y, unsigned int width, unsigned int height,
		int dest_x, int dest_y);

extern void
Pad_SetLineAttributes(Display *display, GC gc, unsigned int line_width, 
		int line_style, int cap_style, int join_style);

#endif  
