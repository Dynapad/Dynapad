/* 
 * winregion.c --
 *
 *	X Region emulation code.
 */

#include <windows.h>

extern "C" {
#  include <X11/Xlib.h>
}

#include "winregion.h"


/*
 *----------------------------------------------------------------------
 *
 * Pad_UnionRegion --
 *
 *	Compute the union of two regions.
 *
 * Results:
 *	Returns the result in the dr_return region.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

void
Pad_UnionRegion(Region sra, Region srb, Region dr_return)
{
    CombineRgn((HRGN) dr_return, (HRGN) sra, (HRGN) srb, RGN_OR);
}


/*
 *----------------------------------------------------------------------
 *
 * Pad_SubtractRegion --
 *
 *	Create a region consisting of the areas of region 1 (identified by sra) that
 *  are not part of region 2 (identified by srb)
 *
 * Results:
 *	Returns the result in the dr_return region.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

void
Pad_SubtractRegion(Region sra, Region srb, Region dr_return)
{
    CombineRgn((HRGN) dr_return, (HRGN) sra, (HRGN) srb, RGN_DIFF);
}



/*
 *----------------------------------------------------------------------
 *
 * Pad_PolygonRegion --
 *
 *	Create a polygonal region, the system will closes the polygon automatically,
 *  if necessary, by drawing a line from the last vertex to the first
 *
 * Results:
 *	Returns the result in the dr_return region.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

Region
Pad_PolygonRegion(XPoint *points, int num, int nMode)
{
  Region rgn;
  POINT *pt;
 
    pt = new POINT[num];
    for (int i=0; i < num; i++) {
  	pt[i].x = points[i].x;
  	pt[i].y = points[i].y;
    }

  	// casting points from XPoint to LPPOINT fails to make
  	// the polygon region properly, in that case, the region
	// is the bounding box of the polygon. Thus here, use
	// POINT *pt instead of (LPPOINT) points
    rgn = (Region) CreatePolygonRgn(pt, num, WINDING);
    delete [] pt;
    return rgn;
}


/*
 *----------------------------------------------------------------------
 *
 * Pad_EmptyRegion --
 *
 *	Create a polygonal region, the system will closes the polygon automatically,
 *  if necessary, by drawing a line from the last vertex to the first
 *
 * Results:
 *	Returns the result in the dr_return region.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

Bool
Pad_EmptyRegion(Region rgn)
{
    HRGN newRgn = CreateRectRgn(0, 0, 0, 0);
	
    return(EqualRgn((HRGN)rgn, (HRGN)newRgn));
}

/*
 *----------------------------------------------------------------------
 *
 * Pad_OffsetRegion --
 *
 *	Create a polygonal region, the system will closes the polygon automatically,
 *  if necessary, by drawing a line from the last vertex to the first
 *
 * Results:
 *	Returns the result in the dr_return region.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

Bool
Pad_OffsetRegion(Region rgn, int x, int y)
{
    return (Bool) OffsetRgn((HRGN) rgn, x, y);  
}

//
// Windows doesn't support non-square top-level windows
//
void Pad_ShapeCombineRegion(Display *dpy, Window dest, int destKind, int xOff, int yOff, Region r, int op) 
{
}
