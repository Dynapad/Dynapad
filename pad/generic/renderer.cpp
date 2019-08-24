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
#include "renderer.h"
#include "display.h"
#include "line-font.h"
#include "ZFontType1.h"
#include "imagedata.h"
#include "drawimage.h"
#include "pad-string.h"
#include "win.h"
#include "restorer.h"
#include "effect.h"
#include "view.h"
#include "transform.h"
#include "fontcache.h"
#include "font.h"
#include "fontdata.h"
#include "colorcube.h"
#include "tkborder.h"
#include "global.h"

#include <string.h>
#include <assert.h>

#ifdef CYGWIN
#include <sys/cygwin.h>
#endif


static float xoffset;
static void add_rotated_vertex(short x, short y, float theta)
{
    Pad_renderer->V2f(x + xoffset, y, theta);
}

static void add_vertex(short x, short y, float )
{
    Pad_renderer->V2f(x + xoffset, y);
}

//
// Renderer destructor.  Can't define in header file
// due to bug with gcc in mklinux.
//
Pad_Renderer::~Pad_Renderer()
{
}


////////////////////////////////////////////////////////////////////////
//
//   X Renderer
//
////////////////////////////////////////////////////////////////////////

Pad_XRenderer::Pad_XRenderer(Pad_Display *dpy, Pad_Win *win)
{
    _dpy = dpy;
    _display = dpy->display;
    _drawable = 0;
    _xftdraw = NULL;
    _fgGC = 0;
    _width = 0;
    _height = 0;
    _win = win;
    _font = NULL;
    _fontHeightMult = 0;
    _fontData = NULL;
    _color = NULL;
    _border = NULL;
    _lineWidth = 0;
    _capStyle = CapButt;
    _joinStyle = JoinMiter;
    _concave = Complex;
    _transparency = 1.0;
    _ptIndex = 0;
    _ptNum = 1000;	 	    // Initial number of points
    _point = new XPoint[_ptNum];
    _flags = RENDERER_NO_FLAGS;
    _viewAppliedToTransform = FALSE;
    Pad_renderers.Push_last(this);
}


Pad_XRenderer::~Pad_XRenderer()
{
    Pad_Transform *transform;

				// Should free up fonts here
    Pad_renderers.Remove(this);
    delete [] _point;
    _point = NULL;
    while ((transform = (Pad_Transform *)_transform.Pop())) {
	delete transform;
    }
}


void
Pad_XRenderer::Set_device(Pad_RenderContext *c)
{
    _display  = c->win->dpy->display;
    _drawable = c->win->dbl;
    _fgGC     = c->win->fgGC;
    _width    = c->win->width;
    _height   = c->win->height;

    {
        int screen_num;
        screen_num = DefaultScreen(_display);
	if (_xftdraw) XftDrawDestroy(_xftdraw);
        _xftdraw  = XftDrawCreate(_display, _drawable,
            DefaultVisual(_display, screen_num),
	    DefaultColormap(_display, screen_num));
	if (!_xftdraw) {
	    fprintf(stderr, "XftDrawCreate failed\n");
	    exit(1);
	}
    }
}

//
// Return the current color
//
void
Pad_XRenderer::Get_color(Pad_String &name)
{
    if (_color) {
	_color->Get(name);
    } else {
	name = "none";
    }
}


//
// Make <color> be the current color for drawing.
//
void
Pad_XRenderer::Set_color(Pad_Color *color)
{
    XColor *xcol;
    Pixmap pixmap;

    _color = color;

    if (!color) {
	return;
    }

    xcol = (XColor*)color->Find(_dpy);

				// Set color
    XSetForeground(_display, _fgGC, xcol->pixel);

				// Set transparency (stipple pattern)
    pixmap = _Get_transparency_stipple(Get_transparency());
    if (pixmap) {
	XSetFillStyle(_display, _fgGC, FillStippled);
	XSetStipple(_display, _fgGC, pixmap);
    } else {
	XSetFillStyle(_display, _fgGC, FillSolid);
    }
}

Pixmap
Pad_XRenderer::_Get_transparency_stipple(float trans)
{
    Pixmap pixmap;

    if (!Pad_prc) {
	fprintf(stderr, "_Get_transparency_stipple called with NULL Pad_RenderContext\n");
	return(None);
    }

    if (trans == 1.0) {
	pixmap = None;
    } else {
	pixmap = Pad_prc->dpy->ditherStipples[(int)(trans * 16)];
    }

    return(pixmap);
}


//
// Make <border> be the current border for drawing.
//
void
Pad_XRenderer::Set_border(Pad_Border *border)
{
    _border = border;
    if (border) {
	_3dborder = (Pad_3DBorder)border->Find(_dpy);
    } else {
	_3dborder = NULL;
    }
}

//
// Return the name (in <borderName>) that was used to
// create this border.
//
void
Pad_XRenderer::Get_border(Pad_String &name)
{
    if (_border) {
	_border->Get(name);
    } else {
	name = "none";
    }
}

//
//  Set the following fields of a border GC to be current
//     - Clip mask
//     - stipple (for transparency)
//
void
Pad_XRenderer::_Update_border_gc(void)
{
    Pixmap pixmap;
    float trans;
    GC bgGC, darkGC, lightGC;

    if (Pad_prc) {
	bgGC    = Pad_3DBorderGC(_dpy, _3dborder, PAD_3D_FLAT_GC);
	darkGC  = Pad_3DBorderGC(_dpy, _3dborder, PAD_3D_DARK_GC);
	lightGC = Pad_3DBorderGC(_dpy, _3dborder, PAD_3D_LIGHT_GC);

	trans = Pad_renderer->Get_transparency();
	if (trans == 1.0) {
	    pixmap = None;
	} else {
	    pixmap = _dpy->ditherStipples[(int)(trans * 16)];
	}

				// Repeat this mess for each GC
	if (bgGC) {
	    XCopyGC(_display, _win->fgGC, GCClipMask, bgGC);
	    if (pixmap == None) {
		XSetFillStyle(_display, bgGC, FillSolid);
	    } else {
		XSetFillStyle(_display, bgGC, FillStippled);
		XSetStipple(_display, bgGC, pixmap);
	    }
	}

	if (darkGC) {
	    XCopyGC(_display, _win->fgGC, GCClipMask, darkGC);
	    if (pixmap == None) {
		XSetFillStyle(_display, darkGC, FillSolid);
	    } else {
		XSetFillStyle(_display, darkGC, FillStippled);
		XSetStipple(_display, darkGC, pixmap);
	    }
	}

	if (lightGC) {
	    XCopyGC(_display, _win->fgGC, GCClipMask, lightGC);
	    if (pixmap == None) {
		XSetFillStyle(_display, lightGC, FillSolid);
	    } else {
		XSetFillStyle(_display, lightGC, FillStippled);
		XSetStipple(_display, lightGC, pixmap);
	    }
	}
    }
}

//
// Restore border GC to no clipmask and no stipple
//
void
Pad_XRenderer::_Restore_border_gc(void)
{
    GC bgGC, darkGC, lightGC;

    if (Pad_prc) {
	bgGC    = Pad_3DBorderGC(_dpy, _3dborder, PAD_3D_FLAT_GC);
	darkGC  = Pad_3DBorderGC(_dpy, _3dborder, PAD_3D_DARK_GC);
	lightGC = Pad_3DBorderGC(_dpy, _3dborder, PAD_3D_LIGHT_GC);

	XSetClipMask(_display, bgGC, None);
	XSetClipMask(_display, darkGC, None);
	XSetClipMask(_display, lightGC, None);

	XSetFillStyle(_display, bgGC, FillSolid);
	XSetFillStyle(_display, darkGC, FillSolid);
	XSetFillStyle(_display, lightGC, FillSolid);
    }
}

//
// Draw a 3D rectangle
//
void
Pad_XRenderer::Draw_3d_rectangle(Pad_Point &min, Pad_Point &max, float borderWidth, int relief)
{
    _Draw_3d_rectangle(min, max, borderWidth, relief, FALSE);
}

//
// Draw a 3D rectangle with a filled background
//
void
Pad_XRenderer::Draw_filled_3d_rectangle(Pad_Point &min, Pad_Point &max, float borderWidth, int relief)
{
    _Draw_3d_rectangle(min, max, borderWidth, relief, TRUE);
}

void
Pad_XRenderer::_Draw_3d_rectangle(Pad_Point &mn, Pad_Point &mx, float borderWidth, int relief, Pad_Bool filled)
{
    int x1, y1, x2, y2, width, height, iBorderWidth;
    Pad_Transform transform;
    Pad_Point min, max;

    if (!_border) {
	return;
    }

    min = mn;
    max = mx;

    Get_transform(transform);
    _Transform_point(&transform, min, x1, y1);
    _Transform_point(&transform, max, x2, y2);
    width = x2 - x1 + 1;
    height = y1 - y2 + 1;
    iBorderWidth = (int)(borderWidth * transform.Get_scale());

				// Keep border width above minimum
    switch (relief) {
      case PAD_RELIEF_RIDGE:
      case PAD_RELIEF_GROOVE:
	iBorderWidth = MAX(iBorderWidth, 2);
	break;
      case PAD_RELIEF_RAISED:
      case PAD_RELIEF_SUNKEN:
	iBorderWidth = MAX(iBorderWidth, 1);
	break;
    }

    _Update_border_gc();
    if (filled) {
	Pad_Fill3DRectangle(_dpy, _drawable, _3dborder, x1, y2, width, height, iBorderWidth, relief);
    } else {
	Pad_Draw3DRectangle(_dpy, _drawable, _3dborder, x1, y2, width, height, iBorderWidth, relief);
    }
    _Restore_border_gc();
}

//
// Draw a 3D polygon
//
void
Pad_XRenderer::Draw_3d_polygon(Pad_PList &points, float polyBorderWidth, int leftRelief)
{
    _Draw_3d_polygon(points, polyBorderWidth, leftRelief, FALSE);
}

//
// Draw a 3D polygon with a filled background
//
void
Pad_XRenderer::Draw_filled_3d_polygon(Pad_PList &points, float polyBorderWidth, int leftRelief)
{
    _Draw_3d_polygon(points, polyBorderWidth, leftRelief, TRUE);
}

void
Pad_XRenderer::_Draw_3d_polygon(Pad_PList &points, float polyBorderWidth, int leftRelief, Pad_Bool filled)
{
    int i, x, y, iPolyBorderWidth;
    int npoints;
    XPoint *ipoints;
    Pad_Transform transform;
    Pad_Point pt;

    if (!_border) {
	return;
    }

    Get_transform(transform);

    npoints = points.Length();
    ipoints = new XPoint[npoints];
    DOTIMES(i, npoints) {
	pt = points.Nth(i);
	_Transform_point(&transform, pt, x, y);
	ipoints[i].x = x;
	ipoints[i].y = y;
    }
    iPolyBorderWidth = (int)(polyBorderWidth * transform.Get_scale());

				// Keep border width above minimum
    switch (leftRelief) {
      case PAD_RELIEF_RIDGE:
      case PAD_RELIEF_GROOVE:
	iPolyBorderWidth = MAX(iPolyBorderWidth, 2);
	break;
      case PAD_RELIEF_RAISED:
      case PAD_RELIEF_SUNKEN:
	iPolyBorderWidth = MAX(iPolyBorderWidth, 1);
	break;
    }

    _Update_border_gc();
    if (filled) {
	Pad_Fill3DPolygon(_dpy, _drawable, _3dborder, ipoints, npoints, iPolyBorderWidth, leftRelief);
    } else {
	Pad_Draw3DPolygon(_dpy, _drawable, _3dborder, ipoints, npoints, iPolyBorderWidth, leftRelief);
    }
    _Restore_border_gc();

    delete [] ipoints;
}

//
// Draw a vertical bevel
//
void
Pad_XRenderer::Draw_3d_vertical_bevel(Pad_Point &min, Pad_Point &max, Pad_Bool leftBevel, int relief)
{
    int x1, y1, x2, y2, width, height;
    Pad_Transform transform;

    if (!_border) {
	return;
    }

    Get_transform(transform);
    _Transform_point(&transform, min, x1, y1);
    _Transform_point(&transform, max, x2, y2);
    width = x2 - x1 + 1;
    height = y1 - y2 + 1;

    _Update_border_gc();
    Pad_3DVerticalBevel(_dpy, _drawable, _3dborder, x1, y2, width, height, leftBevel, relief);
    _Restore_border_gc();
}

//
// Draw a horizontal bevel
//
void
Pad_XRenderer::Draw_3d_horizontal_bevel(Pad_Point &min, Pad_Point &max, Pad_Bool leftIn, Pad_Bool rightIn,
					Pad_Bool topBevel, int relief)
{
    int x1, y1, x2, y2, width, height;
    Pad_Transform transform;

    if (!_border) {
	return;
    }

    Get_transform(transform);
    _Transform_point(&transform, min, x1, y1);
    _Transform_point(&transform, max, x2, y2);
    width = x2 - x1 + 1;
    height = y1 - y2 + 1;

    _Update_border_gc();
    Pad_3DHorizontalBevel(_dpy, _drawable, _3dborder, x1, y2, width, height, leftIn, rightIn, topBevel, relief);
    _Restore_border_gc();
}


//////////////////////////////////////////////
//              Line styles
//////////////////////////////////////////////

void
Pad_XRenderer::Set_line_width(float width)
{
    Set_abs_line_width(width * Get_mag());
}

float
Pad_XRenderer::Get_line_width(void)
{
    return(_lineWidth / Get_mag());
}

void
Pad_XRenderer::Set_abs_line_width(float width)
{
    _lineWidth = (int)width;

    if (_lineWidth <= 1) {
	_lineWidth = 0;	// Zero width X lines are most efficient
    }
				// Needs attention - BBB
				// fgGC should move into renderer
    XSetLineAttributes(_display, _fgGC, _lineWidth, LineSolid, _capStyle, _joinStyle);
}

void
Pad_XRenderer::Set_cap_style(const char *capStyle)
{
    unsigned char cap;

    if (!strcmp(capStyle, "butt")) {
	cap = CapButt;
    } else if (!strcmp(capStyle, "projecting")) {
	cap = CapProjecting;
    } else if (!strcmp(capStyle, "round")) {
	cap = CapRound;
    } else {
	return;
    }

    Set_cap_style(cap);
}

void
Pad_XRenderer::Set_cap_style(unsigned char capStyle)
{
    _capStyle = capStyle;
    XSetLineAttributes(_display, _fgGC, _lineWidth, LineSolid, _capStyle, _joinStyle);
}

unsigned char
Pad_XRenderer::Get_cap_style(void)
{
    return(_capStyle);
}

const char *
Pad_XRenderer::Get_cap_style_name(void)
{
    const char *name;

    switch(_capStyle) {
      case CapButt:
	name = "butt";
	break;
      case CapProjecting:
	name = "projecting";
	break;
      case CapRound:
	name = "round";
	break;
    }

    return(name);
}


void
Pad_XRenderer::Set_join_style(char *joinStyle)
{
    unsigned char join;

    if (!strcmp(joinStyle, "bevel")) {
	join = JoinBevel;
    } else if (!strcmp(joinStyle, "miter")) {
	join = JoinMiter;
    } else if (!strcmp(joinStyle, "round")) {
	join = JoinRound;
    } else {
	return;
    }

    Set_join_style(join);
}

void
Pad_XRenderer::Set_join_style(unsigned char joinStyle)
{
    _joinStyle = joinStyle;
    XSetLineAttributes(_display, _fgGC, _lineWidth, LineSolid, _capStyle, _joinStyle);
}

unsigned char
Pad_XRenderer::Get_join_style(void)
{
    return(_joinStyle);
}

const char *
Pad_XRenderer::Get_join_style_name(void)
{
    const char *name;

    switch(_joinStyle) {
      case JoinBevel:
	name = "bevel";
	break;
      case JoinMiter:
	name = "miter";
	break;
      case JoinRound:
	name = "round";
	break;
    }

    return(name);
}

void
Pad_XRenderer::Set_line_style(float width, unsigned char cap_style, unsigned char joinStyle,
	Pad_Bool absLineStyle)
{
    if (absLineStyle)
      Set_abs_line_style(width, cap_style, joinStyle);
    else
      Set_abs_line_style(width * Get_mag(), cap_style, joinStyle);
}

void
Pad_XRenderer::Set_abs_line_style(float width, unsigned char cap_style, unsigned char joinStyle)
{
    _lineWidth = (int)width;
    _capStyle = cap_style;
    _joinStyle = joinStyle;

    if (_lineWidth <= 1) {
	_lineWidth = 0;	// Zero width X lines are most efficient
    }
    XSetLineAttributes(_display, _fgGC, _lineWidth, LineSolid, _capStyle, _joinStyle);
}

//
// Set the current transparency (0-transparent, 1-opaque)
//
void
Pad_XRenderer::Set_transparency(float transparency)
{
    _transparency = transparency;
}

//
// Return the current transparency (0-transparent, 1-opaque)
//
float
Pad_XRenderer::Get_transparency(void)
{
    return(_transparency);
}

void
Pad_XRenderer::Init_transform_stack(void)
{
    Pad_Transform *transform;

    while ((transform = (Pad_Transform *)_transform.Pop())) {
	delete transform;
    }
    transform = new Pad_Transform();
    _transform.Push(transform);
    _views.Make_empty();
}

float
Pad_XRenderer::Get_mag(void)
{
    Pad_Transform transform;

    Get_transform(transform);
    return(transform.Get_scale());
}

//
// Make the front buffer look like back buffer with the given speed.
// If speed = 0, copy the back buffer directly to the front buffer.
// If speed > 0, dissolve the buffers with the window's current effect.
//
void
Pad_XRenderer::Swap_buffers(int speed)
{
    if (!Pad_prc) {
	fprintf(stderr, "Swap_buffers called with NULL Pad_RenderContext\n");
	return;
    }
    if (speed > 0 && Pad_prc->win->effect) {
        _Dissolve_buffers(speed);
    } else {
	XCopyArea(_display, _drawable, Pad_prc->win->id,
		  Pad_prc->win->copyGC, 0, 0, _width, _height,
		  0, 0);
	XSync(_display, FALSE);
    }
}

//
// Draw the entire clip region in white transparently.
//
void
Pad_XRenderer::Draw_clip_region(Pad_Restorer *restorer)
{
    Pad_ColorCube *cc = Pad_prc->dpy->Get_colorcube();

    int l = restorer->win->view->pad->level * 50;
    if (l > 255) l = 255;
    l = 255 - l;
    XSetFillStyle(Pad_prc->dpy->display, Pad_prc->win->fgGC, FillStippled);
    XSetStipple(Pad_prc->dpy->display, Pad_prc->win->fgGC,
		Pad_prc->dpy->ditherStipples[GRAY_50_STIPPLE]);
    XSetForeground(Pad_prc->dpy->display, Pad_prc->win->fgGC,
		   (cc->Get_pixel(l, l, l)));

    XFillRectangle(restorer->win->dpy->display, restorer->win->id, restorer->win->fgGC,
		   0, 0, (unsigned int)restorer->win->width, (unsigned int)restorer->win->height);

    XSetFillStyle(restorer->win->dpy->display, restorer->win->fgGC, FillSolid);
}

// Used as callback function by Dissolve mechansim
static Pad_Bool
is_interrupted()
{
    return(Pad_renderer->Is_interrupted(Pad_prc->win));
}

//
// Like Swap_buffers but uses a dissolve
//
void
Pad_XRenderer::_Dissolve_buffers(int speed)
{
    if (!Pad_prc) {
	fprintf(stderr, "_Dissolve_buffers called with NULL Pad_RenderContext\n");
	return;
    }
    // This operation is expensive so we find out what area of the window is
    // being swapped and then dissolve just that...

    int x, y, width, height;
    Pad_prc->win->activeRestorer->Get_clip_box(x, y, width, height);

    Pad_prc->win->effect->Dissolve(_drawable, x, y, width, height, x, y,
				 speed, is_interrupted);
}

//
// Make a copy of the specified transform, and push the copy
// onto the stack.
//
void
Pad_XRenderer::Push_transform(Pad_Transform &transform)
{
    Pad_Transform *newTransform;

    newTransform = new Pad_Transform(transform);
    _transform.Push(newTransform);
}

//
// Pop off and discard the top transformation on the stack.
//
void
Pad_XRenderer::Pop_transform(void)
{
    Pad_Transform *transform;

    if (_transform.Is_empty()) {
	cerr << "Pop_transform: popped too many transforms" << endl;
    } else {
	transform = (Pad_Transform *)_transform.Pop();
	delete transform;
    }
}

//
// Maintain stack of views
//
void
Pad_XRenderer::Push_view(Pad_View *view)
{
    _views.Push(view);
    _objects.Push(view);
    _viewAppliedToTransform = FALSE;
}

void
Pad_XRenderer::Pop_view(void)
{
    if (_views.Is_empty()) {
	cerr << "Pop_view: popped too many views" << endl;
    } else {
	_views.Pop();
	_objects.Pop();
	_viewAppliedToTransform = TRUE;
    }
}

//
// Return the top transformation on the stack
//
// Note: Because views are stored on a separate stack,
// if they have not yet been applied to the transform
// by Transform(), then we need to manually apply it here.
//
void
Pad_XRenderer::Get_transform(Pad_Transform &transform)
{
    Pad_View *view;
    Pad_BBox bb;

    transform = (Pad_Transform *)_transform.First();
    if (!_viewAppliedToTransform && !_views.Is_empty()) {
				// Get top view from view stack
	view = (Pad_View *)_views.First();
	view->Get_bbox(bb);
	transform.Translate((- view->xview * view->zoom) + bb.Xctr(),
			    (- view->yview * view->zoom) + bb.Yctr());
	transform.Scale(view->zoom);
    }
}

//
// Push object onto renderer stack, combining its transform
// with the current view on the renderer's view stack.
//
void
Pad_XRenderer::Push_object(Pad_Object *obj)
{
    float xoffset, yoffset, scale;
    float newXoffset, newYoffset, newScale;
    Pad_Transform transform;
    Pad_Transform *newTransform;
    Pad_View *view;
    Pad_Object *stackObj;
    Pad_BBox bb;

    newTransform = &obj->transform;
				// Access info from transforms
    newTransform->Get_offset(xoffset, yoffset);
    scale = newTransform->Get_scale();

				// Compute transformation based on new specified
				// transformation combined with current view.
				// It is important to combine them here rather
				// pushing the view first and then the transformation
				// because that way, there is a floating point explosion
				// because when pushing the view, you must calculate
				// (zoom * xview) which can become too large.
    stackObj = (Pad_Object *)_objects.First();
    if ((stackObj && (stackObj->Type() != PAD_VIEW) && (stackObj->Type() != PAD_PORTAL)) ||
	(_views.Is_empty())) {
				// If there is a non-view object on the stack
				// that means the view has already been incorporated
				// and we shouldn't add it in here.
	newXoffset = xoffset;
	newYoffset = yoffset;
	newScale = scale;
    } else {
	view = (Pad_View *)_views.First();

	view->Get_bbox(bb);
	newXoffset = view->zoom * (xoffset - view->xview) + bb.Xctr();
	newYoffset = view->zoom * (yoffset - view->yview) + bb.Yctr();
	newScale = view->zoom * scale;
    }

				// Combine computed transformation with transformation
				// that is currently on the renderer stack.
    _viewAppliedToTransform = TRUE;
    _objects.Push(obj);
    Get_transform(transform);

    transform.Translate(newXoffset, newYoffset);
    transform.Scale(newScale);
    Push_transform(transform);
}

void
Pad_XRenderer::Pop_object(void)
{
    Pad_Object *obj;

    _objects.Pop();
    obj = (Pad_Object *)_objects.First();
    if (obj && (obj->Type() != PAD_VIEW) && (obj->Type() != PAD_PORTAL)) {
	_viewAppliedToTransform = TRUE;
    } else {
	_viewAppliedToTransform = FALSE;
    }
    Pop_transform();
}

//
// Modify top transformation on stack by translation.
//
void
Pad_XRenderer::Translate(float x, float y)
{
    Pad_Transform *transform;

    transform = (Pad_Transform *)_transform.First();
    transform->Translate(x, y);
}

//
// Modify top transformation on stack by scale.
//
void
Pad_XRenderer::Scale(float s)
{
    Pad_Transform *transform;

    transform = (Pad_Transform *)_transform.First();
    transform->Scale(s);
}


//
// Reallocate the _point array of points, doubling its size,
// and copying over original array.
//
void
Pad_XRenderer::_Reallocate_points(void)
{
    int i;
    XPoint *new_point;


    _ptNum *= 2;
    new_point = new XPoint[_ptNum];
    DOTIMES(i, _ptIndex) {
	new_point[i] = _point[i];
    }
    delete [] _point;
    _point = new_point;
}

//
// Transform the point from local coordinates to screen coordinates
// based on the current renderer transformation.
//
void
Pad_XRenderer::Local_to_screen(Pad_Point &point)
{
    Pad_Transform transform;

    Get_transform(transform);
    transform.Apply(point);
    point.x = Pad_F2s(point.x);
    point.y = Pad_F2s(_height - point.y);
}

//
// Transform the point (x, y) by the current transformation,
// and return the result in (ix, iy).
//
void
Pad_XRenderer::_Transform_point(Pad_Point &point, int &ix, int &iy)
{
    Pad_Transform transform;

    Get_transform(transform);
    _Transform_point(&transform, point, ix, iy);
}

void
Pad_XRenderer::_Transform_point(float x, float y, int &ix, int &iy)
{
    Pad_Point point;

    point.Set(x, y);
    _Transform_point(point, ix, iy);
}

void
Pad_XRenderer::_Transform_point(Pad_Transform *transform, float x, float y, int &ix, int &iy)
{
    Pad_Point point;

    point.Set(x, y);
    _Transform_point(transform, point, ix, iy);
}

void
Pad_XRenderer::_Transform_point(Pad_Transform *transform, Pad_Point &point, int &ix, int &iy)
{
    transform->Apply(point);
    ix = Pad_F2rs(point.x, _lineWidth);
    iy = Pad_F2rs((_height - point.y), _lineWidth);
}

//
// Transform the point (x, y) by the current transformation,
// and add it to the list of points to render.
//
void
Pad_XRenderer::V2f(const float x, const float y)
{
    Pad_Transform transform;
    Pad_Point point;

    if (_ptIndex >= _ptNum) {
	_Reallocate_points();
    }

    point.Set(x, y);

    Get_transform(transform);
    transform.Apply(point);
    _point[_ptIndex].x = Pad_F2rs(point.x, _lineWidth);
    _point[_ptIndex].y = Pad_F2rs((_height - point.y), _lineWidth);

    _ptIndex++;
}

//
// Transform the point (x, y) by the current transformation,
// and add it to the list of points to render.
//
void
Pad_XRenderer::V2f(const float x, const float y, float theta)
{
    Pad_Transform transform;
    Pad_Point point;

    if (_ptIndex >= _ptNum) {
	_Reallocate_points();
    }

    Pad_Point tmppt;
    Pad_Point center;
    tmppt.x = x;
    tmppt.y = y;
    center.x = 0.0;
    center.y = 0.0;
    tmppt.Rotate(theta, center);
    point.Set(tmppt.x, tmppt.y);

    Get_transform(transform);
    transform.Apply(point);

    _point[_ptIndex].x = Pad_F2rs(point.x, _lineWidth);
    _point[_ptIndex].y = Pad_F2rs((_height - point.y), _lineWidth);

    _ptIndex++;
}

void
Pad_XRenderer::V2f(const float v[2])
{
    V2f(v[0], v[1]);
}

void
Pad_XRenderer::V2f(const Pad_Point &p)
{
    V2f(p.x, p.y);
}

//
// Transform the next four points of a spline by the current
// transformation, apply bezier smoothing, and add them to the
// list of points to render.
//
void
Pad_XRenderer::B8f(Pad_Point *ip0, Pad_Point *ip1, Pad_Point *ip2, Pad_Point *ip3)
{
    B8f(*ip0, *ip1, *ip2, *ip3);
}
void
Pad_XRenderer::B8f(Pad_Point &ip0, Pad_Point &ip1, Pad_Point &ip2, Pad_Point &ip3)
{
    Pad_Transform transform;
    Pad_Point p0, p1, p2, p3;

    p0 = ip0;
    p1 = ip1;
    p2 = ip2;
    p3 = ip3;
    Get_transform(transform);
    transform.Apply(p0);
    transform.Apply(p1);
    transform.Apply(p2);
    transform.Apply(p3);

    _Bezier(p0.x, p0.y, p1.x, p1.y, p2.x, p2.y, p3.x, p3.y);
}

void
Pad_XRenderer::_Bezier(const float x0, const float y0,
		       const float x1, const float y1,
		       const float x2, const float y2,
		       const float x3, const float y3)
{
    float midx, midy, dx, dy;
    int tx0, ty0, tx1, ty1;

    midx = (x0 + (3.0 * (x1 + x2)) + x3) / 8.0;
    midy = (y0 + (3.0 * (y1 + y2)) + y3) / 8.0;
    dx = midx - ((x0 + x3) / 2.0);
    dy = midy - ((y0 + y3) / 2.0);

    if (((dx * dx) + (dy * dy)) < 1.0) {
	if ((_ptIndex + 3) >= _ptNum) {
	    _Reallocate_points();
	}

	_ptIndex--;
	tx0 = _point[_ptIndex].x;
	ty0 = _point[_ptIndex].y;
	_ptIndex++;
	tx1 = Pad_F2rs(midx, _lineWidth);
	ty1 = Pad_F2rs(_height - midy, _lineWidth);
	if (tx0 != tx1 || ty0 != ty1) {
	    _point[_ptIndex].x = tx0 = tx1;
	    _point[_ptIndex].y = ty0 = ty1;
	    _ptIndex++;
	}
	tx1 = Pad_F2rs(x3, _lineWidth);
	ty1 = Pad_F2rs(_height - y3, _lineWidth);
	if (tx0 != tx1 || ty0 != ty1) {
	    _point[_ptIndex].x = tx1;
	    _point[_ptIndex].y = ty1;
	    _ptIndex++;
	}
    } else {
	_Bezier(x0, y0, (x0+x1)/2.0, (y0+y1)/2.0,
		(x0+x1+x1+x2)/4.0, (y0+y1+y1+y2)/4.0, midx, midy);
	_Bezier(midx, midy, (x1+x2+x2+x3)/4.0, (y1+y2+y2+y3)/4.0,
		(x2+x3)/2.0, (y2+y3)/2.0, x3, y3);
    }
}

float
Pad_XRenderer::Nearest_pixel_x(float x)
{
    short ix;
    Pad_Transform transform;

    Get_transform(transform);
    transform.Apply_x(x);
    ix = Pad_F2s(x);
    transform.Invert_x(ix);

    return(ix);
}

float
Pad_XRenderer::Nearest_pixel_y(float y)
{
    short iy;
    Pad_Transform transform;

    Get_transform(transform);
    transform.Apply_y(y);
    iy = Pad_F2s(y);
    transform.Invert_y(iy);

    return(iy);
}

void
Pad_XRenderer::Begin_line(void)
{
    _ptIndex = 0;
}

void
Pad_XRenderer::End_line(void)
{
    if (_ptIndex > 0) {
	if (_ptIndex == 1) {
	    _point[1] = _point[0];
	    _ptIndex++;
	}
	XDrawLines(_display, _drawable, _fgGC,
		   _point, _ptIndex, CoordModeOrigin);
    }
}

void
Pad_XRenderer::Begin_rectangle(void)
{
    _ptIndex = 0;
}

void
Pad_XRenderer::End_rectangle(void)
{
    int x, y, width, height;

    if (_ptIndex >= 2) {
	x = MIN(_point[0].x, _point[1].x);
	y = MIN(_point[0].y, _point[1].y);
	width = MAX(_point[0].x, _point[1].x) - x + 1;
	height = MAX(_point[0].y, _point[1].y) - y + 1;
	XFillRectangle(_display, _drawable, _fgGC, x, y, width, height);
    }
}

void
Pad_XRenderer::Begin_oval(void)
{
    _ptIndex = 0;
}

void
Pad_XRenderer::End_oval(Pad_Bool fill)
{
    int x, y, width, height;

    if (_ptIndex >= 2) {
	x = MIN(_point[0].x, _point[1].x);
	y = MIN(_point[0].y, _point[1].y);
	width = MAX(_point[0].x, _point[1].x) - x + 1;
	height = MAX(_point[0].y, _point[1].y) - y + 1;
	if( fill) {
	    XFillArc(_display, _drawable, _fgGC, x, y, width, height, 0, 360*64);
	} else {
	    XDrawArc(_display, _drawable, _fgGC, x, y, width, height, 0, 360*64);
	}
    }
}

void
Pad_XRenderer::Begin_polygon(void)
{
    _ptIndex = 0;
}

void
Pad_XRenderer::End_polygon(void)
{
    if (_ptIndex > 0) {
	XFillPolygon(_display, _drawable, _fgGC,
		     _point, _ptIndex, _concave, CoordModeOrigin);
    }
}


//
// Used in fontcache.c
//
XPoint *Pad_XRenderer::Get_device_points(int &numPoints)
{
    numPoints = _ptIndex;
    return _point;
}


void
Pad_XRenderer::Set_concave(Pad_Bool concave)
{
    _concave = (concave ? Nonconvex : Complex);
}

//
// Set_font
//
//   Sets the current font to the specified font.
//   The font is automatically loaded, and if it isn't found,
//   the system font is used instead.
//
//   Returns true if the font is successfully loaded.
//
void
Pad_XRenderer::Set_font(Pad_Font *font)
{
    if (font) {
	_font = font;
	_fontData = (Pad_FontData*)font->Find(_dpy);
	_fontData->Load();
    } else {
	_font     = &Pad_Font::lineFont;
	_fontData = &Pad_FontData::lineFontData;
    }
    _fontHeightMult = font->Get_size() / _fontData->Char_height();
}

void
Pad_XRenderer::Set_font_height(float height)
{
    _fontHeightMult = height / _fontData->Char_height();
}

void
Pad_XRenderer::Get_font(Pad_String &name)
{
    if (_font) {
	_font->Get(name);
    } else {
	name = "Line";
    }
}


//
// Draw_line
//
//   Draw a multi-segment line connecting the array of points.
//
void
Pad_XRenderer::Draw_line(Pad_PList &pts, Pad_Bool closed)
{
    Draw_line(pts.Length(), pts.Pointer(), closed);
}

void
Pad_XRenderer::Draw_line(int npts, Pad_Point *pts, Pad_Bool closed)
{
    int i;

    if (npts > 0) {
	Begin_line();
	for (i=0; i<npts; i++) {
	    V2f(pts[i]);
	}
	if (closed) {
	    V2f(pts[0]);
	}
	End_line();
    }
}

//
// Draw_polygon
//
//   Draw a closed polygon connecting the array of points.
//
void
Pad_XRenderer::Draw_polygon(Pad_PList &pts)
{
    Draw_polygon(pts.Length(), pts.Pointer());
}

void
Pad_XRenderer::Draw_polygon(int npts, Pad_Point *pts)
{
    int i;

    if (npts > 0) {
	Begin_polygon();
	for (i=0; i<npts; i++) {
	    V2f(pts[i]);
	}
	End_polygon();
    }
}

//
// Draw_box
//
//   Draw a box wire frame specified by a bounding box.
//
void
Pad_XRenderer::Draw_box(float xmin, float ymin, float xmax, float ymax)
{
    Pad_renderer->Begin_line();
    Pad_renderer->V2f(xmin, ymin);
    Pad_renderer->V2f(xmax, ymin);
    Pad_renderer->V2f(xmax, ymax);
    Pad_renderer->V2f(xmin, ymax);
    Pad_renderer->V2f(xmin, ymin);
    Pad_renderer->End_line();
}

void
Pad_XRenderer::Draw_box(Pad_Point *pt)
{
    Pad_renderer->Begin_line();
    Pad_renderer->V2f(pt[0].x, pt[0].y);
    Pad_renderer->V2f(pt[1].x, pt[1].y);
    Pad_renderer->V2f(pt[2].x, pt[2].y);
    Pad_renderer->V2f(pt[3].x, pt[3].y);
    Pad_renderer->V2f(pt[0].x, pt[0].y);
    Pad_renderer->End_line();
}

//
// Draw_filled_box
//
//   Draw a filled box specified by a bounding box.
//
void
Pad_XRenderer::Draw_filled_box(float xmin, float ymin, float xmax, float ymax)
{
    Pad_renderer->Begin_polygon();
    Pad_renderer->V2f(xmin, ymin);
    Pad_renderer->V2f(xmax, ymin);
    Pad_renderer->V2f(xmax, ymax);
    Pad_renderer->V2f(xmin, ymax);
    Pad_renderer->V2f(xmin, ymin);
    Pad_renderer->End_polygon();
}


//
// Font Substitution Support
//

#define XF_MIN_SIZE 8		// cutoff size below which X fonts are not used


#define XF_WEIGHT 1.5		// the current zoom level is multiplied by this factor to
				// get size of X font to use. Note that for other fonts
				// than the ones listed below, this may need to be tuned.

// -mode- used with Get_x_font:
#define XF_IF_LOADED 0          // Get font if it is already loaded.
#define XF_EXISTS 1             // Can you get the font?
#define XF_LOAD_NOW 2           // Get font, loading if necessary.
#define XF_LOAD_ALL 3           // Load every font size in a particular family/style

static int num_fonts = 0;       // number of font sizes per family to cache

typedef struct {
    const char *xlfd;           // X Logical Font Description string
    Font *fids;                 // X Font ids for loaded fonts
} FontStyle;

typedef struct {
    const char *name;           // family name
    FontStyle styles[4];
} FontFamily;

//
// List of font families which are speeded up using X Bitmap fonts.
// 
#define XF_NUM_FAMILIES 2
static FontFamily families[] = {
    { "Times", {
	// plain, bold, italic, bolditalic (in that order)
	{ "-adobe-times-medium-r-normal--%d-*-*-*-p-*-iso8859-1", NULL } ,
        { "-adobe-times-bold-r-normal--%d-*-*-*-p-*-iso8859-1", NULL },
	{ "-adobe-times-medium-i-normal--%d-*-*-*-p-*-iso8859-1", NULL },
	{ "-adobe-times-bold-i-normal--%d-*-*-*-p-*-iso8859-1", NULL }
    } },
    { "Helvetica", {
	// plain, bold, italic, bolditalic (in that order)
	{ "-adobe-helvetica-medium-r-normal--%d-*-*-*-p-*-iso8859-1", NULL },
	{ "-adobe-helvetica-bold-r-normal--%d-*-*-*-p-*-iso8859-1", NULL },
	{ "-adobe-helvetica-medium-o-normal--%d-*-*-*-p-*-iso8859-1", NULL },
	{ "-adobe-helvetica-bold-o-normal--%d-*-*-*-p-*-iso8859-1", NULL }
    } }
};



// Get_x_font()
//
// Takes the name of a font and a size, returns an X Font ID
// to use if there is one, or zero otherwise. If mode is
// XF_LOAD_NOW, then Get_x_font can use XLoadFont to obtain a new font, 
// otherwise this only queries the fonts that have already been
// loaded. If mode is XF_EXISTS, this returns a non-zero result if
// the font is one that X recognizes.
//
static Font Get_x_font(Display *dpy, char *name, int style,
		       float magnification, int mode)
{
    static char *lastFontName = NULL;
    static FontFamily *family = NULL;

    int size = (int)(magnification * XF_WEIGHT);
    int index = size - XF_MIN_SIZE;

	    // default font maxsize to 16
    Pad_Bool initDefaultFonts = FALSE;
    if (num_fonts == 0) {
	num_fonts = 16;
	initDefaultFonts = TRUE;
    }

    if (index < 0 || index >= num_fonts)
      return 0; // outside range of font sizes that use X fonts

    if (name != lastFontName) {
				// changed font - look for the new font name
				// in the list of families that have X fonts
	int i;
	static int inited = FALSE;
	if (!inited) {
	    for (i = 0; i < XF_NUM_FAMILIES; i++) {
		families[i].name = (char*)Pad_GetUid(families[i].name);
	    }

				// Init default fonts
	    if (initDefaultFonts) {
		void Pad_set_x_font_maxsize(Display *dpy, int size);
		Pad_set_x_font_maxsize(NULL, num_fonts);
	    }
	    inited = TRUE;
	}

	family = NULL;

	for (i = 0; i < XF_NUM_FAMILIES; i++) {
	    if (name == families[i].name) {
		// font the font
		family = &families[i];
		break;
	    }
	}
	lastFontName = name;
    }

    if (!family) {
	return 0; 		// no X font replacement for this family
    }

    FontStyle *fs = &family->styles[style];
    if (!fs->fids) {
        return(0L);
    }


    // -index- is in the range 0 to (XF_NUM_FONTS - 1)
    // indicating where in the list of fids the font for the
    // given -size- is.
				// now get the X Font
    switch (mode) {
      case XF_EXISTS:
	return (Font)1;   // yes - an X font exists for this font/size

      case XF_LOAD_NOW:
	if (fs->fids[index] == 0) {
				// font not yet loaded. Use XLoadFont
	    char fname[100];
	    sprintf(fname, fs->xlfd, size);
	    fs->fids[index] = XLoadFont(dpy, fname);
	}
	break;

      case XF_LOAD_ALL:
	int i;
	for (i = 0; i < num_fonts; i++) {
	    char fname[100];
	    sprintf(fname, fs->xlfd, i + XF_MIN_SIZE);
	    fs->fids[i] = XLoadFont(dpy, fname);
	}
	break;
    }

    return fs->fids[index];
}


//  
// Brute force approach to load all available bitmap fonts for a    
// given font family/style.
//
void Pad_load_x_font_bitmaps(Display *dpy, Pad_Font *font)
{
    Get_x_font(dpy, font->Name(), font->Get_style(), (XF_MIN_SIZE / XF_WEIGHT) + 1,
	       XF_LOAD_ALL);
}

//
// Sets the maximum size that X Bitmaps get used for. Notice that
// setting this to a smaller value doesn't cause X fonts to get
// unloaded.
//
void Pad_set_x_font_maxsize(Display *, int size)
{
    int i, j, k;

    for (i = 0; i < XF_NUM_FAMILIES; i++) {
	for (j = 0; j < 4; j++) {
	    FontStyle *fs = &families[i].styles[j];

	    Font *fids = new Font[size];

	    for (k = 0; k < size; k++) {
		// try to use previous font if its already been loaded
		fids[k] = (fs->fids && k < num_fonts) ? fs->fids[k] : 0;
	    }

	    // now set fs->fids
	    if (fs->fids) {
		delete fs->fids;
	    }
	    fs->fids = fids;

	}
    }

    num_fonts = size;

}

int Pad_get_x_font_maxsize(Display *)
{
    return num_fonts;
}


//
// Draw_string
//
//   Render a single-line string. 
//   Quality of rendering is controlled with level (0 - low, 1 - high)
//   Render string starting at pad-coordinate (xoff, yoff)
//
//   Returns TRUE if there is more quality available, and FALSE if the font 
//   is rendered at its highest quality (this is used by Continue_refinement).
//
#define SMALL_FONT 6

// different font rendering techniques
#define R_XFONT 0               // use a bitmap font
#define R_CACHE 1               // use a font cache
#define R_POLY 2                // just draw the polygon
#define R_POLYLINE 3            // draw the polygon using lines
#define R_HASH 4                // draw using hash marks (not rotated)
#define R_ROTATED_HASH 5        // draw using hash marks (rotated)
#define R_FREETYPE 6            // draw using freetype

typedef void (*pfss)(short, short, float);

Pad_Bool
Pad_XRenderer::Draw_string(char *string, int level,
			   float xoff, float yoff, float theta)
{
    float magnification;
    int tabpos;
    unsigned char c;
    float scaleMult = 1.0;
    Pad_Transform transform;
    Pad_Bool canRefine = FALSE;
    Pad_Bool rotated = (theta != 0.0);
    Pad_Point point;

    ZFontPoly *highRes;
    ZFontPoly *lowRes;
    ZFontPoly *poly;
    int globalLevel;
    Pixmap stipple;
    Pad_FontCache *cache;
    int renderType;
    Font xfont;
    int mag;
    float cwidth;
    pfss addVertex      = (rotated ? add_rotated_vertex : add_vertex);

    if (!string) {
	return FALSE;
    }

    Get_transform(transform);
    transform.Translate(xoff, yoff);
    transform.Scale(_fontHeightMult);
    Push_transform(transform);

    magnification = Get_mag();

    if (!_font || _fontData->useLineFont) {

	// Draw line font
	Set_line_style(0.13, CapButt, JoinMiter);

	if (level == 0) {
	    Pad_LineFont::DrawString_0(string, 0.0, 0.0);
	} else if (level == 1) {
	    if (rotated) {
		Pad_LineFont::DrawString_1(string, 0.0, 0.0, theta);
	    } else {
		Pad_LineFont::DrawString_1(string, 0.0, 0.0);
	    }
	} else {
	    if (rotated) {
		Pad_LineFont::DrawString_2(string, 0.0, 0.0, theta);
	    } else {
		Pad_LineFont::DrawString_2(string, 0.0, 0.0);
	    }
	}
	canRefine = (level < 2);
	goto done;
    }

    //
    // Draw Adobe Type1 Font
    //
    highRes  = _fontData->highResPoly;
    lowRes   = _fontData->lowResPoly;
    poly     = (level == 0 ? lowRes : highRes);
    globalLevel     = Pad_prc->win->activeRestorer->refinementLevel;
    stipple      = _Get_transparency_stipple(Get_transparency());
    cache = NULL;
    renderType = R_POLY;
    xfont = 0;

    magnification *= highRes->char_height('A');
    mag = (int)magnification;
    xoffset = 0;
    tabpos = 0;

    /*
    fprintf(stderr, "%d %d %d %s\n",
      level,
      _win->activeRestorer->mode,
      _win->refiningRestorer->mode,
      string);
    */
    if (Pad_FontData::xftenable == TRUE &&
        (_win->activeRestorer->mode == RESTORE_REFINE ||
         _win->activeRestorer->mode == RESTORE_REPAIR) &&
	3 < magnification && magnification < 800) {
	int screen_num = DefaultScreen(_display);
        XftColor color_fg;
	XColor dummyc;
    // [unused]: XColor fg;
    // [unused]: Status retval;
	XftPattern *pat;
	XGCValues xgcvalues;
	char *newstring, *p, *q;
	int i, len;

	len = strlen(string) + 1;
	for (p = string; *p; p++)
	    if (*p == '\t') len += 7;
	newstring = (char *)alloca(len);
	memset(newstring, 0, len);
	for (p = string, i = 0, q = newstring; *p; p++) {
	    if (*p == '\t') {
		*q = ' ';
		i++;
		for ( ; i%8 != 0; i++)
		  *q++ = ' ';
	        continue;
	    }
	    *q++ = *p;
	    i++;
	}

	renderType = R_FREETYPE;
        if (!_xftdraw) {
            fprintf(stderr, "No _xftdraw!\n");
            return FALSE;
        }

	XGetGCValues(_display, _fgGC, GCForeground, &xgcvalues);
	dummyc.pixel = xgcvalues.foreground;
	XQueryColor(_display, DefaultColormap(_display, screen_num), &dummyc);
        color_fg.color.red = dummyc.red;
        color_fg.color.green = dummyc.green;
        color_fg.color.blue = dummyc.blue;
        color_fg.color.alpha = 0xffff;
        color_fg.pixel = dummyc.pixel;

	if (magnification != _fontData->previousmag ||
	    _fontData->xftfactorchanged) {
	    if (_fontData->xftfont ) XftFontClose(_display, _fontData->xftfont);
	    pat = XftPatternBuild(
	        0,
	        XFT_FILE, XftTypeString, _fontData->filename.Get(),
	        XFT_PIXEL_SIZE, XftTypeDouble,
		    (double)1.4*magnification*_fontData->xftfactor,
	        NULL);
            _fontData->xftfont = XftFontOpenPattern(_display, pat);
            if (!_fontData->xftfont) {
                fprintf(stderr, "XftFontOpen failed\n");
	        return FALSE;
	    }
	    //XftPatternDestroy(pat);
	    _fontData->previousmag = magnification;
	    _fontData->xftfactorchanged = FALSE;
	}
	if (strcmp(newstring, "")) {
	    float xfactor;
        // [unused]: float yfactor;
	    float padfwidth, padfheight;
	    XGlyphInfo xglyphinfo;
	    float fwidth, fheight;
	    Pad_BBox bbox;
	    _fontData->String_extents(newstring, padfwidth, padfheight);
	    XftTextExtents8(_display, _fontData->xftfont,
	      (unsigned char*)newstring, strlen(newstring), &xglyphinfo);
	    transform.Invert(fwidth = (float)xglyphinfo.width);
	    transform.Invert(fheight = (float)xglyphinfo.height);
	    xfactor = fwidth/padfwidth;
	    /*
	    fprintf(stderr, "-%s- %f %f %f\n",
	      newstring, xfactor, padfwidth, fwidth);
	    */
	    if (xfactor < 0.98 || 1.00 < xfactor) {
	        _fontData->xftfactor += (0.99 - xfactor)*_fontData->xftfactor;
	        _fontData->xftfactorchanged = TRUE;
	    }
    }

	point.Set(xoffset, 0);
	Pad_renderer->Local_to_screen(point);
        XftDrawString8(_xftdraw, &color_fg, _fontData->xftfont,
	    (int)point.x, (int)point.y,
	    (unsigned char *) newstring, strlen(newstring));
        canRefine = FALSE;
	goto done;
    }

    if (level >= 2) {
	/*
	// Big enough or high enough render level to use high quality font.
	// See if we can use the font cache or a bitmap font.

	if (!rotated) {    // only use cache or bitmap fonts if NOT rotated
	    xfont = Get_x_font(_display, _fontData->name, _fontData->style,
			       magnification,
			       globalLevel  ? XF_LOAD_NOW
			                    : XF_IF_LOADED);
	    if (xfont) {
		// got a bitmap font
		renderType = R_XFONT;
		XSetFont(_display, _fgGC, xfont);
	    } else if (magnification > SMALL_FONT){
		// try the font cache
		cache = _dpy->Get_fontcache(_fontData->id, mag);
		if (cache) {
		    cache->Setup(this, _drawable, _fgGC, stipple);
		    renderType = R_CACHE;
		}
	    }
	}

	Set_concave(TRUE);
	*/
    } else {
	// Small text and low render level - revert to line font
	Set_abs_line_style(1, CapButt, JoinMiter);
	scaleMult = Pad_LineFont::CharHeight() / highRes->char_height('A');
	Pop_transform();
	transform.Scale(1.0/scaleMult);
	Push_transform(transform);
	renderType = rotated ? R_ROTATED_HASH : R_HASH;
	canRefine = TRUE;
    }

    // Now draw the characters

    for (c=*string; *string != 0; c=*++string) {
	switch (c) {
	  case '\t':
	    xoffset += (8 - (tabpos % 8)) * highRes->char_width(' ') * scaleMult;
	    tabpos = 0;
	    break;

	  case '\n':
	    xoffset = 0;
	    tabpos = 0;
	    Pop_transform();
	    transform.Translate(0.0, -1.4 * highRes->char_height('A'));
	    Push_transform(transform);
	    break;

	  case ' ':
	    tabpos++;
	    cwidth = highRes->char_width(c);
	    if ((renderType == R_HASH) || (renderType == R_ROTATED_HASH)) {
		cwidth *= scaleMult;
	    }
	    xoffset += cwidth;
	    break;

	  default:
	    tabpos++;
	    cwidth = highRes->char_width(c);

	    switch (renderType) {
	      case R_XFONT:
		// Use X Bitmap font
		point.Set(xoffset, 0);
		Pad_renderer->Local_to_screen(point);
		XDrawString(_display, _drawable, _fgGC,
			    (int)point.x, (int)point.y, (char*)&c, 1);
		break;

	      case R_CACHE:
		// Draw using font cache
		cache->Draw_char(highRes, c, xoffset, mag, stipple);
		break;

	      case R_POLY:
		// Draw using polygonal data
		Begin_polygon();
		poly->Run(c, addVertex, theta);
		End_polygon();
		break;

	      case R_POLYLINE:
		// Draw using polygonal data, but draw with lines
		Begin_line();
		Set_line_style(0, CapButt, JoinBevel);
		poly->Run(c, addVertex, theta);
		End_line();
		break;

	      case R_HASH:
		// Draw using hash marks (no rotation)
		Pad_LineFont::DrawChar_1(xoffset, 0.0);
		cwidth *= scaleMult;
		break;

	      case R_ROTATED_HASH:
		// Draw using hash marks with rotation
		Pad_LineFont::DrawChar_1(xoffset, 0.0, theta);
		cwidth *= scaleMult;
		break;
	    }
	    xoffset += cwidth;
	}
    }
    Set_concave(FALSE);

    // Now set canRefine

    switch (renderType) {
      case R_XFONT:  	    // Already using an X font, so no refining,
      case R_HASH:          // Line fonts don't get refined (unless tiny and substituting for another)
      case R_ROTATED_HASH:
        break;

      default:
	if (!rotated && _win->activeRestorer->mode != RESTORE_REFINE) {
	    canRefine = TRUE;
	} else {
	    // Are we at level 0?
	    canRefine = (level < 2);
	}
    }

  done:
    Pop_transform();
    /*
    fprintf(stderr, "%s %s %d\n",
      canRefine?"true":"false",
      rotated?"true":"false",
      renderType);
    */
    return canRefine;
}

//
// Get_char_width
//
//   Return width of specified character.
//
float
Pad_XRenderer::Get_char_width(char c)
{
    if (c == '\0') c = ' ';
    return _fontHeightMult * _fontData->Char_width(c);
}

//
// Get_char_height
//
//   Return height of specified character.
//
float
Pad_XRenderer::Get_char_height(char c)
{
    if (c == '\0') c = ' ';
    return _fontHeightMult * _fontData->Char_height(c);
}

//
// Get_font_height
//
//   Return height of line of text.
//
float
Pad_XRenderer::Get_font_height(void)
{
    return 1.4 * Get_char_height('A');
}

//
// Get_font_descender
//
//   Return amount lowest character goes below baseline.
//
float
Pad_XRenderer::Get_font_descender(void)
{
    return 0.3 * Get_char_height('A');
}

//
// Get_string_bbox
//
//   Return bounding box of specified string.
//
void
Pad_XRenderer::Get_string_bbox(char *string, Pad_BBox *bbox)
{
    float width, height;

    _fontData->String_extents(string, width, height);

    width *= _fontHeightMult;    // Normalize
    height *= 1.4;                 // Default line spacing
    height *= _fontHeightMult;   // Normalize

    bbox->Set(0.0, 0.0, width, height);
}

//
// Alloc_image
//
//   Allocate image for future rendering.
//   Given a filename, it returns a Pad_ImageData pointer,
//   or NULL if allocation failed.  
//   This does reference counting, so if the same image is
//   asked for more than once, only one is allocated.  The
//   image data isn't actually freed until the last reference
//   is removed.
//
Pad_ImageData *
Pad_XRenderer::Alloc_image(char *name, Pad_Bool *newflag)
{
    Pad_ImageData *image;
    Pad_String expandedName;
    Pad_Uid nameUid;
    // [unused]: char buf[1024]; // yes, someday it will be too small

#ifdef CYGWIN
    cygwin_conv_to_full_posix_path(name, buf);
    name = buf;
#endif
    // is there a better place and better way to make this test?
    FILE *fp = fopen(name,"r");
    if( !fp ) return NULL;
    fclose(fp);

    Pad_Expand_pathname(name, expandedName);
    nameUid = Pad_GetUid(expandedName.Get());

    image = (Pad_ImageData *)Pad_imageNameTable.Get((void *) nameUid);
    if (image) {
        *newflag = FALSE;
	image->count++;
    } else {
        *newflag = TRUE;
	image = new Pad_ImageData(_dpy, nameUid);
	if (!(image->rgbData && image->width && image->height)) {
            *newflag = FALSE;
	    delete image;
	    image = NULL;
	} else {
	    if (!Pad_rgb) {
                *newflag = FALSE;
		image->Free_rgb();
	    }
	}
    }

    return(image);
}

// JM's experimental tiling facility
void
Pad_XRenderer::Set_tile(Pad_ImageData *image1, Pad_ImageData *image2,
			long *tmp, float d, int tx, int ty)
{
    if (!image1) {
	XSetFillStyle(_display, _fgGC, FillSolid);
	XSetTSOrigin(_display, _fgGC, 0, 0);
    } else {

	if (image1->tile == None) {
	    // Make the tile

	    image1->tile = XCreatePixmap(_display, _dpy->rootDrawable,
					image1->width, image1->height,
					_dpy->depth);
	}
	int x, y;
	long src, dst, pix;
	void *data;
	static GC gc = None;

	if (d <= 0) {
	    data = image1->rgbData;
	} else if (d >= 1) {
	    data = image2->rgbData;
	} else {
	    for (y = 0; y < image1->height; y++) {
		int row = y * image1->width;
		for (x = 0; x < image1->width; x++) {
		    pix = x + row;
		    src = image1->rgbData[pix];
		    dst = image2->rgbData[pix];
		    tmp[pix] =
		      CC_RGBA(MAX(0, MIN(255, (int)LERP(d, CC_RED(src), CC_RED(dst)))),
			      MAX(0, MIN(255, (int)LERP(d, CC_GREEN(src), CC_GREEN(dst)))),
			      MAX(0, MIN(255, (int)LERP(d, CC_BLUE(src), CC_BLUE(dst)))),
			      0);

		}
	    }
	    data = tmp;
	}

	if (gc == None) gc = XCreateGC(_display, _dpy->rootDrawable, 0, NULL);

	Pad_DrawImage(_display, Pad_prc->win, image1->tile,
		      gc, data, NULL, image1->width, image1->height,
		      0, 0, image1->width-1, image1->height-1,
		      0,0, image1->width, image1->height,
		      TRUE, 1.0);
	XSetTile(_display, _fgGC, image1->tile);
	XSetFillStyle(_display, _fgGC, FillTiled);
	XSetTSOrigin(_display, _fgGC, tx, ty);
    }
}


//
// Alloc_image
//
//   Allocate image for future rendering.
//   Given data of a GIF image, it returns a Pad_ImageData pointer,
//   or NULL if allocation failed.
//
Pad_ImageData *
Pad_XRenderer::Alloc_image(unsigned char *data, int len)
{
    Pad_ImageData *image;

    image = new Pad_ImageData(_dpy, data, len);

    if (!(image->rgbData && image->width && image->height)) {
	delete image;
	image = NULL;
    } else {
	if (!Pad_rgb) {
	    image->Free_rgb();
	}
    }

    return(image);
}

//
// Alloc_image
//
//   Allocate image for future rendering.
//   It returns a Pad_ImageData pointer,
//   or NULL if allocation failed.
//
Pad_ImageData *
Pad_XRenderer::Alloc_image()
{
    Pad_ImageData *image;

    image = new Pad_ImageData(_dpy);
    return(image);
}

//
// Free image by name.  
// Return true if image succesfully freed.
//
Pad_Bool
Pad_XRenderer::Free_image(char *token)
{
    Pad_Bool rc;
    Pad_ImageData *imageData;

    imageData = (Pad_ImageData *)Pad_imageTokenTable.Get(Pad_GetUid(token));
    if (imageData) {
	rc = Free_image(imageData);
    } else {
	Pad_errorString = "Image not previously allocated: ";
	Pad_errorString += token;
	rc = FALSE;
    }

    return(rc);
}

//
// Free_image
//
//   Free an allocated image.
//   Perform reference counting, so don't really delete it
//   until the last reference has been removed.
//   Return true if image succesfully freed.
//
Pad_Bool
Pad_XRenderer::Free_image(Pad_ImageData *image)
{
    assert(image->count >= 1);

    if (image->count == 1) {
	delete image;
    } else {
	image->count--;
    }

    return(TRUE);
}

//
// Given a token name, return a pointer to a Pad_ImageData
// instantiating that image, if one exists.  Else,
// return NULL;
//
Pad_ImageData *
Pad_XRenderer::Get_image(char *token)
{
    Pad_ImageData *imageData;

    imageData = (Pad_ImageData *)Pad_imageTokenTable.Get(Pad_GetUid(token));

    return(imageData);
}

//
// Draw_image
//
//   Draw the specified image, magnifying or shinking as necessary to
//   fill the specified destination space.
//   If dither is true, then use dithering to render.
//   alpha is used to specify how opaque the image is to be drawn.
//   Destination parameters in local coordinates.
//
Pad_Bool
Pad_XRenderer::Draw_image(Pad_ImageData *image,
			  float dst_left, float dst_bot, float dst_right, float dst_top,
			  Pad_Bool dither)
{
    int rc = 1;
    float clipped[4], unclipped[4];
    float clipped_width, clipped_height;
    float unclipped_width, unclipped_height;
    float dst_bbox[4];
    XPoint img1, img2;		// Source image region
    XPoint scr1, scr2;		// Destination image region
    void *data;

    if (!Pad_prc) {
	fprintf(stderr, "Draw_image called with NULL Pad_RenderContext\n");
	return(FALSE);
    }

    dst_bbox[XMIN] = dst_left;
    dst_bbox[YMIN] = dst_bot;
    dst_bbox[XMAX] = dst_right;
    dst_bbox[YMAX] = dst_top;
    Pad_prc->win->activeRestorer->Get_bbox_screen_coords(dst_bbox, clipped, unclipped);
    clipped_width = clipped[XMAX] - clipped[XMIN];
    clipped_height = clipped[YMAX] - clipped[YMIN];
    unclipped_width = unclipped[XMAX] - unclipped[XMIN];
    unclipped_height = unclipped[YMAX] - unclipped[YMIN];

    if ((unclipped_width == 0) || (unclipped_height == 0)) {
	return(TRUE);
    }

    scr1.x = (int)clipped[XMIN];
    scr1.y = (int)clipped[YMIN];
    scr2.x = (int)clipped[XMAX];
    scr2.y = (int)clipped[YMAX];

    img1.x = (int)(((float)(clipped[XMIN] - unclipped[XMIN]) / unclipped_width) * image->width);
    img1.y = (int)(((float)(clipped[YMIN] - unclipped[YMIN]) / unclipped_height) * image->height);

    img2.x = (int)(((float)(clipped[XMIN] + clipped_width - unclipped[XMIN]) / unclipped_width) *
		   image->width);
    img2.y = (int)(((float)(clipped[YMIN] + clipped_height - unclipped[YMIN]) / unclipped_height) *
		   image->height);

    if (img1.x >= image->width) {img1.x = image->width - 1;}
    if (img2.x >= image->width) {img2.x = image->width - 1;}
    if (img1.y >= image->height) {img1.y = image->height - 1;}
    if (img2.y >= image->height) {img2.y = image->height - 1;}

				// Adjust for partial visibility of magnified pixels
    if (image->width < unclipped_width) {
	if (clipped[XMIN] != unclipped[XMIN]) {
	    scr1.x += (int)((unclipped[XMIN] - clipped[XMIN]) +
			    (img1.x * (unclipped_width / image->width)));
	}
	if (clipped[XMAX] != unclipped[XMAX]) {
	    scr2.x += (int)((unclipped[XMAX] - clipped[XMAX]) -
			    ((image->width - img2.x - 1) * (unclipped_width / image->width)));
	}
    }
    if (image->height < unclipped_height) {
	if (clipped[YMIN] != unclipped[YMIN]) {
	    scr1.y += (int)((unclipped[YMIN] - clipped[YMIN]) +
			    (img1.y * (unclipped_height / image->height)));
	}
	if (clipped[YMAX] != unclipped[YMAX]) {
	    scr2.y += (int)((unclipped[YMAX] - clipped[YMAX]) -
			    ((image->height - img2.y - 1) * (unclipped_height / image->height)));
	}
    }

    if (dither && image->rgb) {
	data = image->rgbData;
    } else {
	dither = FALSE;
	if (Pad_prc->dpy->depth == 8) {
	    data = image->xpixels8;
	} else {
	    data = image->xpixels;
	}
    }
    if (data) {
	rc = Pad_DrawImage(_display, Pad_prc->win, _drawable,
			   _fgGC, data, image->mask, image->width, image->height,
			   img1.x, img1.y, img2.x, img2.y,
			   scr1.x, scr1.y, scr2.x, scr2.y, dither, Get_transparency());
    }

    return(rc);
}

//
// This routine is used by Is_interrupted to check if the
// specified event is one that should interrupt refinement.  The following
// events will interrupt refinement:
//    ButtonPress
//    KeyPress
//
// It passes the result into the global variable Pad_EventCheckerResult.
//
static Bool
EventChecker(Display *, XEvent *event, char *)
{
    switch (event->type)
      {
	case ButtonPress:
	case KeyPress:
	case Expose:
	    Pad_eventCheckerResult = TRUE;
	  break;
	case MotionNotify:
	  if (((XMotionEvent *)event)->state & (Button1Mask | Button2Mask | Button3Mask)) {
	      Pad_eventCheckerResult = TRUE;
	  }
	  break;
	default:
	  break;
      }

    return(0);
}

//
// Predicate to be called during a render that returns true
// if there is an event that should interrupt the current render.
//
Pad_Bool
Pad_XRenderer::Is_interrupted(Pad_Win *win)
{
    Pad_Bool rc;
    XEvent event;

    if (win->interruptible) {
	Pad_eventCheckerResult = FALSE;
	XCheckIfEvent(_display, &event, EventChecker, 0);
	rc = Pad_eventCheckerResult;
    } else {
	rc = FALSE;
    }

    return(rc);
}
