/*
 * winregion.h --
 *
 *	This file contains declarations that are shared among the
 *	Windows-specific parts of Tk, but aren't used by the rest of
 *	Tk.
 *
 * Copyright (c) 1995 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * SCCS: @(#) pad-region.h 1.17 96/07/18 17:51:55
 */


#include "../generic/tkInt.h"

extern "C" {
#  include <X11/Xutil.h>
}


				// First, define macros for region commands
				// that are defined by Tk
#define XCreateRegion					(Region) TkCreateRegion
#define XClipBox(rgn, rect)				TkClipBox((TkRegion) rgn, rect)
#define XDestroyRegion(rgn)				TkDestroyRegion((TkRegion) rgn)
#define XIntersectRegion(a, b, r)       TkIntersectRegion((TkRegion) a, \
											(TkRegion) b, (TkRegion) r)
#define XRectInRegion(r, x, y, w, h)	TkRectInRegion((TkRegion) r, x, y, w, h)
#define XUnionRectWithRegion(rect, src, ret)  TkUnionRectWithRegion(rect, \
											(TkRegion) src, (TkRegion) ret)

				// Then, define macros for region commands
				// that we define.
#define XEmptyRegion		Pad_EmptyRegion
#define XOffsetRegion		Pad_OffsetRegion
#define XPolygonRegion		Pad_PolygonRegion
#define XShapeCombineRegion	Pad_ShapeCombineRegion
#define XSubtractRegion		Pad_SubtractRegion
#define XUnionRegion		Pad_UnionRegion

				// Simulation of X region calls
extern Bool   Pad_EmptyRegion(Region);
extern Bool   Pad_OffsetRegion(Region, int, int);
extern Region Pad_PolygonRegion(XPoint *, int, int);
extern void   Pad_ShapeCombineRegion(Display *, Window, int, int, int, Region, int);
extern void   Pad_SubtractRegion(Region, Region, Region);
extern void   Pad_UnionRegion(Region, Region, Region);
