/*
"(c) Copyright 1993-1997 Pad++ Consortium {University of New Mexico (UNM),
New York University (NYU), and Bell Communications Research (Bellcore)},
All Rights Reserved."  Licensee can not remove or obscure any of the
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


#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/extensions/shape.h>

#include "../generic/defs.h"
#include "../generic/point.h"
#include "../generic/list.h"

static void 
_ShapeWindow(Display *display, Window window,
	     XPoint *pts1, int npts1, XPoint *pts2, int npts2)
{
    Region r;

    if (npts1) {
	r = XPolygonRegion(pts1, npts1, EvenOddRule);
	XShapeCombineRegion(display, window, ShapeClip, 0,0, r, ShapeSet);
	XDestroyRegion(r);
    } else {
	XShapeCombineMask(display, window, ShapeClip, 0,0, None, ShapeSet);
    }

    if (npts2) {
	r = XPolygonRegion(pts2, npts2, EvenOddRule);
	XShapeCombineRegion(display, window, ShapeBounding, 0,0, r, ShapeSet);
	XDestroyRegion(r);
    } else {
	XShapeCombineMask(display, window, ShapeBounding, 0, 0, None, ShapeSet);
    }
}

//
// Converts Pad coordinates to coordinates from 0 to 1, Y down.
// 
void 
Pad_ComputeShapeCoords(Pad_List &pts)
{
    Pad_Point *p;
    Pad_Iterator li;
    float minx, maxx, maxy;
    Pad_Point *first;

    if (pts.Is_empty()) {
        return;
    }

    first = ((Pad_Point*)pts.First());

    // make coordinates to between 0 and N, inverting Y in the process
    minx = first->x;
    maxy = first->y;

    DOLIST(li, pts, Pad_Point, p) {
	if (p->x < minx) minx = p->x;
	if (p->y > maxy) maxy = p->y;
    }
    DOLIST(li, pts, Pad_Point, p) {
	p->x = p->x - minx;
	p->y = maxy - p->y;
    }

    // now normalize coordinates between 0 and 1
    maxx = first->x;
    maxy = first->y;

    DOLIST(li, pts, Pad_Point, p) {
	if (p->x > maxx) maxx = p->x;
	if (p->y > maxy) maxy = p->y;
    }
    DOLIST(li, pts, Pad_Point, p) {
	p->x /= maxx;
	p->y /= maxy;
    }
}

//
// Takes a list of (adjusted) coordinates. Makes a window take on that shape.
//
void 
Pad_ReshapeWindow(Display *display, Window window, int width, 
		  int height, Pad_List &pts, int ninner, int nouter)
{
    XPoint *in_points = NULL, *out_points = NULL;
    Pad_Point *p;
    Pad_Iterator li;
    int i;

    if (ninner) 
	    in_points = new XPoint[ninner];
    if (nouter) 
	    out_points = new XPoint[nouter];

    i = 0;
    DOLIST(li, pts, Pad_Point, p) {
	if (i < ninner) {
	    in_points[i].x = (int)(p->x * width);
	    in_points[i].y = (int)(p->y * height);
	} else {
	    out_points[i-ninner].x = (int)(p->x * width);
	    out_points[i-ninner].y = (int)(p->y * height);
	}
	i++;
    }

    _ShapeWindow(display, window, in_points, ninner, out_points, nouter);
    if (in_points) 
	    delete in_points;
    if (out_points) 
	    delete out_points;
}







