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
#include "pad.h"
#include "point.h"
#include "line.h"
#include "view.h"
#include "pad-string.h"
#include "renderer.h"
#include "events.h"
#include "noisedata.h"
#include "misc.h"
#include "win.h"
#include "global.h"

#include <math.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#define LINE_DEFAULT_PEN         "black"
#define LINE_DEFAULT_PENWIDTH    1
#define LINE_DEFAULT_CAPSTYLE    "round"
#define LINE_DEFAULT_JOINSTYLE   "round"
#define LINE_DEFAULT_ARROW       ARROW_NONE
#define LINE_DEFAULT_ARROW_A     8
#define LINE_DEFAULT_ARROW_B     10
#define LINE_DEFAULT_ARROW_C     3
#define POLYGON_DEFAULT_FILL   "none"


//////////////////////////////////////////////
//              Line definitions
//////////////////////////////////////////////  

Pad_Line::~Pad_Line(void) {
    Generate_delete();
}

Pad_Line::Pad_Line(Pad *pad) :
    Pad_Object(pad) {
    _type = PAD_LINE;
    lineFlags = PAD_NO_MASK;
    maxSize = -1;  /* not 10000 */
    firstArrowPtr = NULL;
    lastArrowPtr = NULL;
    Set_pen_default();
    Set_penwidth_default();
    Set_joinstyle_default();
    Set_capstyle_default();
    Set_arrow_default();
    arrowShapeA = LINE_DEFAULT_ARROW_A;
    arrowShapeB = LINE_DEFAULT_ARROW_B;
    arrowShapeC = LINE_DEFAULT_ARROW_C;
}

//
// Gets called with string args when object is created.  This routine must 
// process any arguments up to, but not including the options.  (argc, argv) 
// point to parameters on the creation line just after 'create <type>'.
// This returns the number of args processed.
//
int
Pad_Line::Create_obj_args(int argc, char **argv) {
    int i, coords, pts;
    Pad_Point pt;

    // Process args to find how many coords are specified
    coords = 0;
    while ((coords < argc) &&
           ((argv[coords][0] != '-') || (isdigit(argv[coords][1])) || (argv[coords][1] == '.'))) {
        coords++;
    }
    pts = coords / 2;

    if ((coords % 2) == 1) {
        Pad_errorString += "wrong # args: need even number of points\n";
        return (-1);
    }

    // Add coords to this item
    DOTIMES(i, pts) {
        pt.x = ATOXF(pad->view->win, argv[0]);
        pt.y = ATOYF(pad->view->win, argv[1]);
        points.Push_last(&pt);
        argc -= 2;
        argv += 2;
    }
    Compute_bounding_box();
    // Don't want item to move when it changes.
    // So compute new anchor position so that item doesn't move.
    Set_anchor_from_position();
    Damage();

    return (coords);
}

//
// Rotate the object by theta.
//
// Rotate calls the utility function Pad_Rotate
// to do the work of transforming the
// positions of the objects coordinates.
// theta is expected to be in degrees
// 
Pad_Bool
Pad_Line::Is_rotatable(void) {
    return (TRUE);
}

Pad_Bool
Pad_Line::Rotate(float dtheta) {
    // center point must be in global coordinates,
    // so convert anchorpt from local*transform to global
    Pad_Point center;
    center = anchorpt;
    transform.Invert(center);
    Local_to_screen(center);

    return (Rotate(dtheta, center));
}

Pad_Bool
Pad_Line::Rotate(float dtheta, Pad_Point &center) {
    float curAngle;
    Pad_PList pts;

    curAngle = Get_angle();

    Get_coords(pts, FALSE);
    Pad_Rotate(pts, dtheta, center);
    Set_coords(pts, FALSE);

    _Set_angle_data(curAngle + dtheta);

    return (TRUE);
}

//
// Setters and getters for pen color
//
Pad_Bool
Pad_Line::Set_pen(const char *colorname) {
    lineFlags |= LINE_PEN_SET;
    penColor.Set(colorname);
    Damage();

    return (TRUE);
}

Pad_Bool
Pad_Line::Set_pen(int red, int green, int blue) {
    lineFlags |= LINE_PEN_SET;
    penColor.Set(red, green, blue);
    Damage();

    return (TRUE);
}

void
Pad_Line::Set_pen_default(void) {
    lineFlags &= ~LINE_PEN_SET;
    penColor.Set(LINE_DEFAULT_PEN);
    Damage();
}

void
Pad_Line::Get_pen_name(Pad_String &penname) {
    penColor.Get(penname);
}

//
// Setters and getters for pen width
//
Pad_Bool
Pad_Line::Set_penwidth(float penwidth, Pad_Bool abslinestyle) {
    lineFlags |= LINE_PENWIDTH_SET;
    lineWidth = penwidth;
    absLineStyle = abslinestyle;
    if (lineFlags & LINE_ARROW_SET) {
        Configure_arrows(Get_arrow());
    }
    Update();

    return (TRUE);
}

void
Pad_Line::Set_penwidth_default(void) {
    Set_penwidth(LINE_DEFAULT_PENWIDTH);
    lineFlags &= ~LINE_PENWIDTH_SET;
}

float
Pad_Line::Get_penwidth(void) {
    return (lineWidth);
}

Pad_Bool
Pad_Line::Get_abslinestyle(void) {
    return (absLineStyle);
}

Pad_Bool
Pad_Line::Set_joinstyle(const char *joinstyle) {
    if (!strcmp(joinstyle, "bevel")) {
        joinStyle = JoinBevel;
    } else if (!strcmp(joinstyle, "miter")) {
        joinStyle = JoinMiter;
    } else if (!strcmp(joinstyle, "round")) {
        joinStyle = JoinRound;
    } else {
        Pad_errorString += "Invalid joinstyle, try: bevel, miter or round\n";
        return (FALSE);
    }
    lineFlags |= LINE_JOINSTYLE_SET;

    Update();

    return (TRUE);
}

void
Pad_Line::Set_joinstyle_default(void) {
    Set_joinstyle(LINE_DEFAULT_JOINSTYLE);
    lineFlags &= ~LINE_JOINSTYLE_SET;
}

const char *
Pad_Line::Get_joinstyle(void) {
    const char *joinstyle = NULL;

    switch (joinStyle) {
        case JoinBevel:
            joinstyle = "bevel";
            break;
        case JoinMiter:
            joinstyle = "miter";
            break;
        case JoinRound:
            joinstyle = "round";
            break;
    }

    return (joinstyle);
}

Pad_Bool
Pad_Line::Set_capstyle(const char *capstyle) {
    if (!strcmp(capstyle, "butt")) {
        capStyle = CapButt;
    } else if (!strcmp(capstyle, "projecting")) {
        capStyle = CapProjecting;
    } else if (!strcmp(capstyle, "round")) {
        capStyle = CapRound;
    } else {
        Pad_errorString += "Invalid capstyle, try: butt, projecting or round\n";
        return (FALSE);
    }
    lineFlags |= LINE_CAPSTYLE_SET;

    Update();

    return (TRUE);
}

void
Pad_Line::Set_capstyle_default(void) {
    Set_capstyle(LINE_DEFAULT_CAPSTYLE);
    lineFlags &= ~LINE_CAPSTYLE_SET;
}

const char *
Pad_Line::Get_capstyle(void) {
    const char *capstyle = NULL;

    switch (capStyle) {
        case CapButt:
            capstyle = "butt";
            break;
        case CapProjecting:
            capstyle = "projecting";
            break;
        case CapRound:
            capstyle = "round";
            break;
    }

    return (capstyle);
}

void
Pad_Line::Set_arrow(int arrow) {
    lineFlags |= LINE_ARROW_SET;
    Configure_arrows(arrow);

    Update_and_compute_anchor();
}

void
Pad_Line::Set_arrow_default(void) {
    Set_arrow(LINE_DEFAULT_ARROW);
    lineFlags &= ~LINE_ARROW_SET;
}

int
Pad_Line::Get_arrow(void) {
    int arrow;

    if (firstArrowPtr) {
        if (lastArrowPtr) {
            arrow = ARROW_BOTH;
        } else {
            arrow = ARROW_FIRST;
        }
    } else {
        if (lastArrowPtr) {
            arrow = ARROW_LAST;
        } else {
            arrow = ARROW_NONE;
        }
    }

    return (arrow);
}

void
Pad_Line::Set_arrowshape(float a, float b, float c) {
    lineFlags |= LINE_ARROWSHAPE_SET;
    arrowShapeA = a;
    arrowShapeB = b;
    arrowShapeC = c;
    Configure_arrows(Get_arrow());

    Update_and_compute_anchor();
}

void
Pad_Line::Set_arrowshape_default(void) {
    float a, b, c;

    a = LINE_DEFAULT_ARROW_A;
    b = LINE_DEFAULT_ARROW_B;
    c = LINE_DEFAULT_ARROW_C;
    if (!Pad_coordFrames.Is_empty()) {
        a /= 100.0;
        b /= 100.0;
        c /= 100.0;
    }
    Set_arrowshape(UNITSTOVALUE(pad->view->win, a),
                   UNITSTOVALUE(pad->view->win, b),
                   UNITSTOVALUE(pad->view->win, c));
    lineFlags &= ~LINE_ARROWSHAPE_SET;
}

char *
Pad_Line::Get_arrowshape(void) {
    static char shapebuf[128];
    sprintf(shapebuf, "%f %f %f",
            VALUETOUNITS(pad->view->win, arrowShapeA),
            VALUETOUNITS(pad->view->win, arrowShapeB),
            VALUETOUNITS(pad->view->win, arrowShapeC));
    return shapebuf;
}

//
// Set the coordinates of an object.
// if <objectCoords> is true, then add them in local coordinates, otherwise,
// add them in global coordinates.
// Return TRUE if successful or FALSE otherwise
//
Pad_Bool
Pad_Line::Set_coords(Pad_PList &pts, Pad_Bool objectCoords) {
    if (Get_lock()) {
        return (TRUE);        // Can't modify if locked
    }

    _Set_angle_data(0);

    points.Make_empty();
    Pad_Line::Append_coords(pts, objectCoords);

    return (TRUE);
}

//
// Append the specified coordinates onto the existing list of coordinates.
// if <objectCoords> is true, then add them in local coordinates, otherwise,
// add them in global coordinates.
// Return TRUE if successful or FALSE otherwise
//
Pad_Bool
Pad_Line::Append_coords(Pad_PList &pts, Pad_Bool objectCoords) {
    int i;
    int orig_arrow;
    Pad_Point pt;
    Pad_Point *ppt;

    if (Get_lock()) {
        return (TRUE);        // Can't modify if locked
    }

    // Remember original arrow status
    orig_arrow = Get_arrow();
    // Delete any existing arrow points
    if (firstArrowPtr != NULL) {
        if (points.Length() > 0) {
            ppt = points.First();
            ppt->Set(firstArrowPtr[0].x, firstArrowPtr[0].y);
        }
        delete[] firstArrowPtr;
        firstArrowPtr = NULL;
    }
    if (lastArrowPtr != NULL) {
        if (points.Length() > 0) {
            ppt = points.Last();
            ppt->Set(lastArrowPtr[0].x, lastArrowPtr[0].y);
        }
        delete[] lastArrowPtr;
        lastArrowPtr = NULL;
    }


    DOTIMES(i, pts.Length()) {
        pt = pts.Nth(i);
        if (!objectCoords) {
            Screen_to_local(pt);
        }
        points.Push_last(&pt);
    }

    Configure_arrows(orig_arrow); // Restore arrow heads
    Update_and_compute_anchor();
    Generate_event(Pad_ModifyNotify, NULL, "coords");

    return (TRUE);
}

//
// Return the points of an object.  The points are returned
// in allocated memory and must be freed.
//
void
Pad_Line::Get_coords(Pad_PList &pts, Pad_Bool objectCoords) {
    int i, len;
    Pad_Point pt;
    Pad_Point *data;

    pts.Make_empty();
    len = points.Length();

    DOTIMES(i, len) {
        pt = points.Nth(i);
        if (!objectCoords) {
            Local_to_screen(pt);
        }
        pts.Push_last(&pt);
    }

    // If line has an arrow then make sure the original first and last points
    // are used (and not the ones recomputed due to arrows).
    data = pts.Pointer();
    if ((firstArrowPtr != NULL) && (len > 0)) {
        data[0].Set(firstArrowPtr[0].x, firstArrowPtr[0].y);
        if (!objectCoords) Local_to_screen(data[0]);
    }
    if ((lastArrowPtr != NULL) && (len > 0)) {
        data[len - 1].Set(lastArrowPtr[0].x, lastArrowPtr[0].y);
        if (!objectCoords) Local_to_screen(data[len - 1]);
    }
}

//
// Set the width of a line.  Really a wrapper around coords,
// maintaining anchor point.
//
Pad_Bool
Pad_Line::Set_width(float width) {
    int i;
    Pad_Point pt;
    Pad_PList pts;
    Pad_BBox bb;
    float x, scale, scalePt;

    // If no coords, then add some at the origin
    if (points.Is_empty()) {
        pt.Set(0.0, 0.0);
        pts.Push_last(&pt);
        pt.Set(0.0, 0.0);
        pts.Push_last(&pt);
        Set_coords(pts, FALSE);
    }

    // Figure where to scale around
    Get_global_bbox(bb);
    switch (anchor) {
        case PAD_ANCHOR_NW:
        case PAD_ANCHOR_W:
        case PAD_ANCHOR_SW:
            scalePt = bb.Xmin();
            break;
        case PAD_ANCHOR_N:
        case PAD_ANCHOR_CENTER:
        case PAD_ANCHOR_S:
            scalePt = 0.5 * (bb.Xmin() + bb.Xmax());
            break;
        case PAD_ANCHOR_NE:
        case PAD_ANCHOR_E:
        case PAD_ANCHOR_SE:
            scalePt = bb.Xmax();
            break;
    }

    // Scale coordinates
    scale = width / bb.Width();
    Get_coords(pts, FALSE);
    DOTIMES(i, pts.Length()) {
        x = pts.Nth(i)->x;
        x = scalePt + (x - scalePt) * scale;
        pts.Nth(i)->x = x;
    }
    Set_coords(pts, FALSE);

    return (TRUE);
}

//
// Set the width of a line.  Really a wrapper around coords,
// maintaining anchor point.
//
Pad_Bool
Pad_Line::Set_height(float height) {
    int i;
    Pad_Point pt;
    Pad_PList pts;
    Pad_BBox bb;
    float y, scale, scalePt;

    // If no coords, then add some at the origin
    if (points.Is_empty()) {
        pt.Set(0.0, 0.0);
        pts.Push_last(&pt);
        pt.Set(0.0, 0.0);
        pts.Push_last(&pt);
        Set_coords(pts, FALSE);
    }

    // Figure where to scale around
    Get_global_bbox(bb);
    switch (anchor) {
        case PAD_ANCHOR_NW:
        case PAD_ANCHOR_N:
        case PAD_ANCHOR_NE:
            scalePt = bb.Ymax();
            break;
        case PAD_ANCHOR_W:
        case PAD_ANCHOR_CENTER:
        case PAD_ANCHOR_E:
            scalePt = 0.5 * (bb.Ymin() + bb.Ymax());
            break;
        case PAD_ANCHOR_SW:
        case PAD_ANCHOR_S:
        case PAD_ANCHOR_SE:
            scalePt = bb.Ymin();
            break;
    }
    // Scale coordinates
    scale = height / bb.Height();
    Get_coords(pts, FALSE);
    DOTIMES(i, pts.Length()) {
        y = pts.Nth(i)->y;
        y = scalePt + (y - scalePt) * scale;
        pts.Nth(i)->y = y;
    }
    Set_coords(pts, FALSE);

    return (TRUE);
}

/*
 *--------------------------------------------------------------
 *
 * Configure_arrows --
 *
 *	If arrowheads have been requested for a line, this
 *	procedure makes arrangements for the arrowheads.
 *
 * Results:
 *	returns PAD_OK.
 *
 * Side effects:
 *	Information in linePtr is set up for one or two arrowheads.
 *	the firstArrowPtr and lastArrowPtr polygons are allocated
 *	and initialized, if need be, and the end points of the line
 *	are adjusted so that a thick line doesn't stick out past
 *	the arrowheads.
 *
 *--------------------------------------------------------------
 */

int
Pad_Line::Configure_arrows(int arrow) {
    Pad_Point *poly, *pt0, *pt1;
    double dx, dy, arrowLength, sinTheta, cosTheta, temp;
    double fracHeight;            /* Line width as fraction of
					 * arrowhead width. */
    double backup;            /* Distance to backup end points
					 * so the line ends in the middle
					 * of the arrowhead. */
    double vertX, vertY;        /* Position of arrowhead vertex. */
    double shapeA, shapeB, shapeC;    /* Adjusted coordinates (see
					 * explanation below). */
    double offset;                /* Hack as described below. */

    // Can't have arrow heads on a line with only one point
    if (points.Length() < 2) {
        return (PAD_OK);
    }

    /*
     * Setup arrowheads, if needed.  If arrowheads are turned off,
     * restore the line's endpoints (they were shortened when the
     * arrowheads were added).
     */

    if ((firstArrowPtr != NULL) && (points.Length() > 0)) {
        pt0 = points.First();
        pt0->Set(firstArrowPtr[0].x, firstArrowPtr[0].y);
        delete[] firstArrowPtr;
        firstArrowPtr = NULL;
    }
    if ((lastArrowPtr != NULL) && (points.Length() > 0)) {
        pt1 = points.Last();
        pt1->Set(lastArrowPtr[0].x, lastArrowPtr[0].y);
        delete[] lastArrowPtr;
        lastArrowPtr = NULL;
    }

    if ((points.Length() == 0) || (arrow == ARROW_NONE))
        return (PAD_OK);

    /*
     * The code below makes a tiny increase in the shape parameters
     * for the line.  This is a bit of a hack, but it seems to result
     * in displays that more closely approximate the specified parameters.
     * Without the adjustment, the arrows come out smaller than expected.
     */

    offset = 0.0001 * (arrowShapeA + arrowShapeB + arrowShapeC) / 3.0;
    shapeA = arrowShapeA + offset;
    shapeB = arrowShapeB + offset;
    shapeC = arrowShapeC + lineWidth / 2.0 + offset;

    /*
     * If there's an arrowhead on the first point of the line, compute
     * its polygon and adjust the first point of the line so that the
     * line doesn't stick out past the leading edge of the arrowhead.
     */

    fracHeight = (lineWidth / 2.0) / shapeC;
    backup = fracHeight * shapeB + shapeA * (1.0 - fracHeight) / 2.0;
    if (arrow != ARROW_LAST) {
        poly = firstArrowPtr;
        if (poly == NULL) {
            poly = new Pad_Point[PTS_IN_ARROW];
            firstArrowPtr = poly;
        }
        pt0 = points.First();
        poly[0] = pt0;
        poly[PTS_IN_ARROW - 1] = pt0;
        pt1 = points.Nth(1);
        dx = poly[0].x - pt1->x;
        dy = poly[0].y - pt1->y;
        arrowLength = hypot(dx, dy);
        if (arrowLength == 0) {
            sinTheta = cosTheta = 0.0;
        } else {
            sinTheta = dy / arrowLength;
            cosTheta = dx / arrowLength;
        }
        vertX = poly[0].x - shapeA * cosTheta;
        vertY = poly[0].y - shapeA * sinTheta;
        temp = shapeC * sinTheta;
        poly[1].x = poly[0].x - shapeB * cosTheta + temp;
        poly[4].x = poly[1].x - 2 * temp;
        temp = shapeC * cosTheta;
        poly[1].y = poly[0].y - shapeB * sinTheta - temp;
        poly[4].y = poly[1].y + 2 * temp;
        poly[2].x = poly[1].x * fracHeight + vertX * (1.0 - fracHeight);
        poly[2].y = poly[1].y * fracHeight + vertY * (1.0 - fracHeight);
        poly[3].x = poly[4].x * fracHeight + vertX * (1.0 - fracHeight);
        poly[3].y = poly[4].y * fracHeight + vertY * (1.0 - fracHeight);

        /*
         * Polygon done.  Now move the first point towards the second so
         * that the corners at the end of the line are inside the
         * arrowhead.
         */

        pt0->x = poly[0].x - backup * cosTheta;
        pt0->y = poly[0].y - backup * sinTheta;
    }

    /*
     * Similar arrowhead calculation for the last point of the line.
     */

    if (arrow != ARROW_FIRST) {
        pt0 = points.Nth(points.Length() - 2);
        pt1 = points.Last();
        poly = lastArrowPtr;
        if (poly == NULL) {
            poly = new Pad_Point[PTS_IN_ARROW];
            lastArrowPtr = poly;
        }
        poly[0].x = poly[5].x = pt1->x;
        poly[0].y = poly[5].y = pt1->y;
        dx = poly[0].x - pt0->x;
        dy = poly[0].y - pt0->y;
        arrowLength = hypot(dx, dy);
        if (arrowLength == 0) {
            sinTheta = cosTheta = 0.0;
        } else {
            sinTheta = dy / arrowLength;
            cosTheta = dx / arrowLength;
        }
        vertX = poly[0].x - shapeA * cosTheta;
        vertY = poly[0].y - shapeA * sinTheta;
        temp = shapeC * sinTheta;
        poly[1].x = poly[0].x - shapeB * cosTheta + temp;
        poly[4].x = poly[1].x - 2 * temp;
        temp = shapeC * cosTheta;
        poly[1].y = poly[0].y - shapeB * sinTheta - temp;
        poly[4].y = poly[1].y + 2 * temp;
        poly[2].x = poly[1].x * fracHeight + vertX * (1.0 - fracHeight);
        poly[2].y = poly[1].y * fracHeight + vertY * (1.0 - fracHeight);
        poly[3].x = poly[4].x * fracHeight + vertX * (1.0 - fracHeight);
        poly[3].y = poly[4].y * fracHeight + vertY * (1.0 - fracHeight);
        pt1->x = poly[0].x - backup * cosTheta;
        pt1->y = poly[0].y - backup * sinTheta;
    }

    return (PAD_OK);
}

void
Pad_Line::Compute_bounding_box(void) {
    int i;
    float hw;
    float xmin, xmax, ymin, ymax;
    float miterLength;
    Pad_Point p1, p2, p3;
    Pad_Point *pt0, *pt1, *pti, *ptlast;

    if (points.Length() > 0) {
        pt0 = points.First();
        xmin = xmax = pt0->x;
        ymin = ymax = pt0->y;
        Pad_NoiseData *nd = Get_noisedata();
        if (nd) {
            float bbox_ret[4];
            nd->Compute_bbox(points, FALSE, joinStyle == JoinMiter, lineWidth, bbox_ret);
            xmin = bbox_ret[XMIN];
            ymin = bbox_ret[YMIN];
            xmax = bbox_ret[XMAX];
            ymax = bbox_ret[YMAX];
        } else if ((joinStyle == JoinMiter) && (points.Length() > 1)) {
            pt1 = points.Nth(1);
            p1.x = pt0->x;
            p1.y = pt0->y;
            p2.x = pt1->x;
            p2.y = pt1->y;
            ptlast = points.Last();
            xmin = MIN(xmin, ptlast->x);
            ymin = MIN(ymin, ptlast->y);
            xmax = MAX(xmax, ptlast->x);
            ymax = MAX(ymax, ptlast->y);


            for (i = 1; i < (points.Length() - 1); i++) {
                pti = points.Nth(i + 1);
                p3.x = pti->x;
                p3.y = pti->y;

                miterLength = Pad_Miter_length(lineWidth, p1.x, p1.y, p2.x, p2.y, p3.x, p3.y);
                pti = points.Nth(i);
                if (pti->x < xmin) xmin = pti->x - miterLength;
                if (pti->y < ymin) ymin = pti->y - miterLength;
                if (pti->x > xmax) xmax = pti->x + miterLength;
                if (pti->y > ymax) ymax = pti->y + miterLength;

                p1.x = p2.x;
                p1.y = p2.y;
                p2.x = p3.x;
                p2.y = p3.y;
            }
        } else {
            for (i = 1; i < points.Length(); i++) {
                pti = points.Nth(i);
                if (pti->x < xmin) xmin = pti->x;
                if (pti->y < ymin) ymin = pti->y;
                if (pti->x > xmax) xmax = pti->x;
                if (pti->y > ymax) ymax = pti->y;
            }
        }

        if (capStyle == CapProjecting) {
            xmin -= 0.5 * lineWidth;
            ymin -= 0.5 * lineWidth;
            xmax += 0.5 * lineWidth;
            ymax += 0.5 * lineWidth;
        }

        if (firstArrowPtr) {
            if (nd) {
                float bbox_ret[4];
                nd->Compute_bbox(firstArrowPtr, PTS_IN_ARROW, FALSE, FALSE, 0, bbox_ret);
                if (bbox_ret[XMIN] < xmin) xmin = bbox_ret[XMIN];
                if (bbox_ret[YMIN] < ymin) ymin = bbox_ret[YMIN];
                if (bbox_ret[XMAX] > xmax) xmax = bbox_ret[XMAX];
                if (bbox_ret[YMAX] > ymax) ymax = bbox_ret[YMAX];
            } else {
                for (i = 0; i < PTS_IN_ARROW; i++) {
                    if (firstArrowPtr[i].x < xmin) xmin = firstArrowPtr[i].x;
                    if (firstArrowPtr[i].y < ymin) ymin = firstArrowPtr[i].y;
                    if (firstArrowPtr[i].x > xmax) xmax = firstArrowPtr[i].x;
                    if (firstArrowPtr[i].y > ymax) ymax = firstArrowPtr[i].y;
                }
            }
        }
        if (lastArrowPtr) {
            if (nd) {
                float bbox_ret[4];
                nd->Compute_bbox(lastArrowPtr, PTS_IN_ARROW, FALSE, FALSE, 0, bbox_ret);
                if (bbox_ret[XMIN] < xmin) xmin = bbox_ret[XMIN];
                if (bbox_ret[YMIN] < ymin) ymin = bbox_ret[YMIN];
                if (bbox_ret[XMAX] > xmax) xmax = bbox_ret[XMAX];
                if (bbox_ret[YMAX] > ymax) ymax = bbox_ret[YMAX];
            } else {
                for (i = 0; i < PTS_IN_ARROW; i++) {
                    if (lastArrowPtr[i].x < xmin) xmin = lastArrowPtr[i].x;
                    if (lastArrowPtr[i].y < ymin) ymin = lastArrowPtr[i].y;
                    if (lastArrowPtr[i].x > xmax) xmax = lastArrowPtr[i].x;
                    if (lastArrowPtr[i].y > ymax) ymax = lastArrowPtr[i].y;
                }
            }
        }

        hw = 0.6 * lineWidth;
        if (!(optionFlags & PAD_WIDTH_SET)) {
            if ((xmax - xmin + hw) == 0.0) {
                // Protect against vertical 0 width lines
                Set_bbox_xmin(xmin - (0.001 * (ymax - ymin)));
                Set_bbox_xmax(xmax + (0.001 * (ymax - ymin)));
            } else {
                Set_bbox_xmin(xmin - hw);
                Set_bbox_xmax(xmax + hw);
            }
        }
        if (!(optionFlags & PAD_HEIGHT_SET)) {
            if ((ymax - ymin + hw) == 0.0) {
                // Protect against horizontal 0 width lines
                Set_bbox_ymin(ymin - (0.001 * (xmax - xmin)));
                Set_bbox_ymax(ymax + (0.001 * (xmax - xmin)));
            } else {
                Set_bbox_ymin(ymin - hw);
                Set_bbox_ymax(ymax + hw);
            }
        }
    } else {
        if (!(optionFlags & PAD_WIDTH_SET)) {
            Set_bbox_xmin(0.0);
            Set_bbox_xmax(0.0);
        }
        if (!(optionFlags & PAD_HEIGHT_SET)) {
            Set_bbox_ymin(0.0);
            Set_bbox_ymax(0.0);
        }
    }

    Pad_Object::Compute_bounding_box();
}

//
// Return true to continue rendering other objects, or false if rendering
// was interrupted.
//
Pad_Bool
Pad_Line::Render(void) {
    if (penColor.Is_set()) {
        Pad_renderer->Set_color(penColor);
        Pad_renderer->Set_line_style(lineWidth, capStyle, joinStyle, absLineStyle);
        Render_line(FALSE);
    }

    return (TRUE);
}

void
Pad_Line::Render_line(Pad_Bool closed) {
    Pad_NoiseData *nd = Get_noisedata();
    if (nd) {            // Noisy line
        Pad_NoiseIterator ni;
        Pad_Point point;
        ni.Setup(nd, points, closed);
        Pad_renderer->Begin_line();
        while (ni.Next(point)) Pad_renderer->V2f(point);
        Pad_renderer->End_line();
    } else {
        Pad_renderer->Draw_line(points, closed);
    }


    if (firstArrowPtr) {
        if (nd) {        // Noisy line
            Pad_NoiseIterator ni;
            Pad_Point point;
            ni.Setup(nd, firstArrowPtr, PTS_IN_ARROW, FALSE);
            Pad_renderer->Begin_polygon();
            while (ni.Next(point)) Pad_renderer->V2f(point);
            Pad_renderer->End_polygon();
        } else {
            Pad_renderer->Draw_polygon(PTS_IN_ARROW, firstArrowPtr);
        }
    }

    if (lastArrowPtr) {
        if (nd) {        // Noisy line
            Pad_NoiseIterator ni;
            Pad_Point point;
            ni.Setup(nd, lastArrowPtr, PTS_IN_ARROW, FALSE);
            Pad_renderer->Begin_polygon();
            while (ni.Next(point)) Pad_renderer->V2f(point);
            Pad_renderer->End_polygon();
        } else {
            Pad_renderer->Draw_polygon(PTS_IN_ARROW, lastArrowPtr);
        }
    }
}

void
Pad_Line::Render_polygon(void) {
    Pad_NoiseData *nd = Get_noisedata();
    if (nd) {
        Pad_NoiseIterator ni;
        Pad_Point point;
        ni.Setup(nd, points, TRUE);
        Pad_renderer->Begin_polygon();
        while (ni.Next(point)) Pad_renderer->V2f(point);
        Pad_renderer->End_polygon();
    } else {
        Pad_renderer->Draw_polygon(points);
    }
}

//
// Determines if the specified point is within halo pixels of the line.
// Coordinates are local.
//
Pad_Bool
Pad_Line::Pick(Pad_Event *event, float halo) {
    Pad_Bool rc = FALSE;

    if (penColor.Is_set()) {
        Pad_NoiseData *nd = Get_noisedata();
        if (nd) {
            int i;
            Pad_NoiseIterator ni;
            Pad_Point pts[2], point;

            i = 0;
            ni.Setup(nd, points, ((Type() == PAD_LINE) ? FALSE : TRUE));
            while (ni.Next(point)) {
                if (i == 0) {
                    pts[0].x = point.x;
                    pts[0].y = point.y;
                } else {
                    pts[1].x = point.x;
                    pts[1].y = point.y;
                    rc = event->pt.On_line(2, pts,
                                           lineWidth + (2.0 * halo / event->mag),
                                           ((Type() == PAD_LINE) ? FALSE : TRUE));
                    if (rc) {
                        break;
                    }
                    pts[0].x = pts[1].x;
                    pts[0].y = pts[1].y;
                }
                i++;
            }
        } else {
            rc = event->pt.On_line(points,
                                   lineWidth + (2.0 * halo / event->mag),
                                   ((Type() == PAD_LINE) ? FALSE : TRUE));
        }

        // Check the arrows
        if (!rc && firstArrowPtr) {
            rc = event->pt.In_polygon(PTS_IN_ARROW, firstArrowPtr);
        }
        if (!rc && lastArrowPtr) {
            rc = event->pt.In_polygon(PTS_IN_ARROW, lastArrowPtr);
        }
    }

    return (rc);
}

//////////////////////////////////////////////
//              Spline definitions
//////////////////////////////////////////////  

Pad_Spline::~Pad_Spline(void) {
    Generate_delete();
}

Pad_Spline::Pad_Spline(Pad *pad) :
    Pad_Line(pad) {
    _type = PAD_SPLINE;
}

//
// Gets called with string args when object is created.  This routine must 
// process any arguments up to, but not including the options.  (argc, argv) 
// point to parameters on the creation line just after 'create <type>'.
// This returns the number of args processed.
//
int
Pad_Spline::Create_obj_args(int argc, char **argv) {
    int i, pts, coords;
    Pad_Point pt;

    // Process args to find how many coords are specified
    coords = 0;
    while ((coords < argc) &&
           ((argv[coords][0] != '-') || (isdigit(argv[coords][1])) || (argv[coords][1] == '.'))) {
        coords++;
    }
    pts = coords / 2;

    if ((pts > 0) && (pts < 3)) {
        Pad_errorString += "wrong # args: should be \"pathName create spline x1 y1 ?... xn yn option value ...?\"\n";
        return (-1);
    }

    if (((coords % 6) != 0) && ((coords % 6) != 2)) {
        Pad_errorString += "wrong # args: need 6n or 6n+2 points\n";
        return (-1);
    }

    // Add coords to this item
    DOTIMES(i, pts) {
        pt.x = ATOXF(pad->view->win, argv[0]);
        pt.y = ATOYF(pad->view->win, argv[1]);
        points.Push_last(&pt);
        argc -= 2;
        argv += 2;
    }
    Compute_bounding_box();
    // Don't want item to move when it changes.
    // So compute new anchor position so that item doesn't move.
    Set_anchor_from_position();
    Damage();

    return (coords);
}

//
// Return true to continue rendering other objects, or false if rendering
// was interrupted.
//
Pad_Bool
Pad_Spline::Render(void) {
    int i;
    Pad_Bool closed;

    if (!penColor.Is_set()) {
        return (TRUE);
    }

    closed = (points.Length() % 3) ? 0 : 1;

    Pad_renderer->Set_color(penColor);
    Pad_renderer->Set_line_style(lineWidth, capStyle, joinStyle, absLineStyle);

    Pad_renderer->Begin_line();

    Pad_renderer->V2f(points.Nth(0));

    for (i = 0; i < (points.Length() - 3); i += 3) {
        Pad_renderer->B8f(points.Nth(i), points.Nth(i + 1), points.Nth(i + 2), points.Nth(i + 3));
    }
    if (closed) {
        Pad_renderer->B8f(points.Nth(i), points.Nth(i + 1), points.Nth(i + 2), points.Nth(0));
    }
    Pad_renderer->End_line();

    if (firstArrowPtr) {
        Pad_renderer->Draw_polygon(PTS_IN_ARROW, firstArrowPtr);
    }

    if (lastArrowPtr) {
        Pad_renderer->Draw_polygon(PTS_IN_ARROW, lastArrowPtr);
    }

    return (TRUE);
}

//
// Determines if the specified point is within halo pixels of the spline.
// Coordinates are local.
//
Pad_Bool
Pad_Spline::Pick(Pad_Event *event, float halo) {
    Pad_Bool rc;

    if (!penColor.Is_set()) {
        rc = FALSE;
    } else {
        rc = event->pt.On_bezier(points,
                                 lineWidth + (2.0 * halo / event->mag),
                                 (points.Length() % 3) ? 0 : 1);

        // Check the arrows
        if (!rc && firstArrowPtr) {
            rc = event->pt.In_polygon(PTS_IN_ARROW, firstArrowPtr);
        }
        if (!rc && lastArrowPtr) {
            rc = event->pt.In_polygon(PTS_IN_ARROW, lastArrowPtr);
        }
    }

    return (rc);
}

//////////////////////////////////////////////
//              Polygon definitions
//////////////////////////////////////////////  

Pad_Polygon::~Pad_Polygon(void) {
    Generate_delete();
}

Pad_Polygon::Pad_Polygon(Pad *pad) :
    Pad_Line(pad) {
    _type = PAD_POLYGON;
    Set_fill_default();
}

//
// Return true to continue rendering other objects, or false if rendering
// was interrupted.
//
Pad_Bool
Pad_Polygon::Render(void) {
    if (fillColor.Is_set()) {
        Pad_renderer->Set_color(fillColor);
        Render_polygon();
    }
    if (penColor.Is_set()) {
        Pad_renderer->Set_color(penColor);
        Pad_renderer->Set_line_style(lineWidth, capStyle, joinStyle, absLineStyle);
        Render_line(TRUE);
    }

    return (TRUE);
}

//
// Determines if the specified point is within halo pixels of the polygon.
// Coordinates are local.
//
Pad_Bool
Pad_Polygon::Pick(Pad_Event *event, float halo) {
    Pad_Bool rc;

    if (!fillColor.Is_set()) {
        rc = Pad_Line::Pick(event, halo);
    } else {
        Pad_NoiseData *nd = Get_noisedata();
        if (nd) {
            Pad_Point *pts;
            int npts;
            pts = nd->Get_points(points, TRUE, npts);
            rc = event->pt.In_polygon(npts, pts);
            delete[] pts;
        } else {
            rc = event->pt.In_polygon(points);
        }
        if (!rc) {
            // Point not on polygon, might be on thick edge,
            // so check that.
            rc = Pad_Line::Pick(event, halo);
        }
    }

    return (rc);
}

//
// Setters and getters for fill color
//
Pad_Bool
Pad_Polygon::Set_fill(const char *colorname) {
    lineFlags |= LINE_FILL_SET;
    fillColor.Set(colorname);
    Damage();

    return (TRUE);
}

Pad_Bool
Pad_Polygon::Set_fill(int red, int green, int blue) {
    lineFlags |= LINE_FILL_SET;
    fillColor.Set(red, green, blue);
    Damage();

    return (TRUE);
}

void
Pad_Polygon::Set_fill_default(void) {
    lineFlags &= ~LINE_FILL_SET;
    fillColor.Set(POLYGON_DEFAULT_FILL);
    Damage();
}

void
Pad_Polygon::Get_fillname(Pad_String &fillname) {
    fillColor.Get(fillname);
}

//////////////////////////////////////////////
//              Rectangle definitions
//////////////////////////////////////////////  

Pad_Rectangle::~Pad_Rectangle(void) {
    Generate_delete();
}

Pad_Rectangle::Pad_Rectangle(Pad *pad) :
    Pad_Polygon(pad) {
    _type = PAD_RECTANGLE;
}

Pad_Rectangle::Pad_Rectangle(Pad *pad, float x1, float y1, float x2, float y2) :
    Pad_Polygon(pad) {
    Pad_PList pts;
    Pad_Point pt;

    _type = PAD_RECTANGLE;

    pt.Set(x1, y1);
    pts.Push_last(&pt);
    pt.Set(x2, y2);
    pts.Push_last(&pt);

    Set_coords(pts, TRUE);
}

//
// Gets called with string args when object is created.  This routine must 
// process any arguments up to, but not including the options.  (argc, argv) 
// point to parameters on the creation line just after 'create <type>'.
// This returns the number of args processed.
//
int
Pad_Rectangle::Create_obj_args(int argc, char **argv) {
    int coords;
    Pad_Point pt;
    Pad_PList pts;

    // Process args to find how many coords are specified
    coords = 0;
    while ((coords < argc) &&
           ((argv[coords][0] != '-') || (isdigit(argv[coords][1])) || (argv[coords][1] == '.'))) {
        coords++;
    }

    if ((coords != 0) && (coords != 4)) {
        Pad_errorString += "wrong # args: should be \"pathName create rectangle x1 y1 x2 y2 ?option value ...?\"\n";
        return (-1);
    }

    if (coords == 4) {
        pt.Set(ATOXF(pad->view->win, argv[0]), ATOYF(pad->view->win, argv[1]));
        pts.Push_last(&pt);
        pt.Set(ATOXF(pad->view->win, argv[2]), ATOYF(pad->view->win, argv[3]));
        pts.Push_last(&pt);

        Set_coords(pts, TRUE);
    }

    return (coords);
}

//
// Can't append onto rectangles, but derived types might
//
Pad_Bool
Pad_Rectangle::Append_coords(Pad_PList &, Pad_Bool) {
    return (FALSE);
}

//
// Set the coordinates of an object.
// Return TRUE if successful or FALSE otherwise
//
Pad_Bool
Pad_Rectangle::Set_coords(Pad_PList &pts, Pad_Bool objectCoords) {
    int i;
    Pad_Point pt;
    Pad_Point *ppt;

    if (Get_lock()) {
        return (TRUE);        // Can't modify if locked
    }

    if (pts.Length() != 2) {
        return (FALSE);        // Rectangles must have exactly two corner points
    }

    // Erase existing points
    points.Make_empty();
    // Set new points
    pt.Set(pts.Nth(0)->x, pts.Nth(0)->y);
    points.Push_last(&pt);
    pt.Set(pts.Nth(1)->x, pts.Nth(0)->y);
    points.Push_last(&pt);
    pt.Set(pts.Nth(1)->x, pts.Nth(1)->y);
    points.Push_last(&pt);
    pt.Set(pts.Nth(0)->x, pts.Nth(1)->y);
    points.Push_last(&pt);

    if (!objectCoords) {
        DOTIMES(i, 4) {
            ppt = points.Nth(i);
            Screen_to_local(*ppt);
        }
    }

    Update_and_compute_anchor();
    Generate_event(Pad_ModifyNotify, NULL, "coords");

    return (TRUE);
}

//
// Return the points of an object.  The points are returned
// in allocated memory and must be freed.
//
void
Pad_Rectangle::Get_coords(Pad_PList &pts, Pad_Bool objectCoords) {
    Pad_Point pt0, pt1;

    if (points.Length() == 0) {
        return;
    }

    pt0 = points.Nth(0);
    pt1 = points.Nth(2);
    if (!objectCoords) {
        Local_to_screen(pt0);
        Local_to_screen(pt1);
    }
    pts.Push_last(&pt0);
    pts.Push_last(&pt1);
}

//
// Return true to continue rendering other objects, or false if rendering
// was interrupted.
//
Pad_Bool
Pad_Rectangle::Render(void) {
    if (Get_angle() == 0) {
        if (fillColor.Is_set()) {
            Pad_renderer->Set_color(fillColor);
            Pad_renderer->Begin_rectangle();
            Pad_renderer->V2f(points.Nth(0));
            Pad_renderer->V2f(points.Nth(2));
            Pad_renderer->End_rectangle();
        }
        if (penColor.Is_set()) {
            Pad_renderer->Set_color(penColor);
            Pad_renderer->Set_line_style(lineWidth, capStyle, joinStyle, absLineStyle);
            Render_line(TRUE);
        }
    } else {
        Pad_Polygon::Render();
    }

    return (TRUE);
}

//
// Determines if the specified point is within halo pixels of the rectangle.
// Coordinates are local.
//
Pad_Bool
Pad_Rectangle::Pick(Pad_Event *event, float halo) {
    Pad_Bool rc;

    if (!fillColor.Is_set()) {
        rc = Pad_Line::Pick(event, halo);
    } else {
        rc = Pad_Object::Pick(event, halo);
    }

    return (rc);
}

//
// Must implement rotate here because otherwise, Pad_Line
// will call rectangles Set/Get_coords which only deals
// with two #s.
//
Pad_Bool
Pad_Rectangle::Rotate(float dtheta) {
    // center point must be in global coordinates,
    // so convert anchorpt from local*transform to global
    Pad_Point center;
    center = anchorpt;
    transform.Invert(center);
    Local_to_screen(center);

    return (Rotate(dtheta, center));
}

Pad_Bool
Pad_Rectangle::Rotate(float dtheta, Pad_Point &center) {
    float curAngle;
    Pad_PList pts;

    curAngle = Get_angle();

    Pad_Line::Get_coords(pts, FALSE);
    Pad_Rotate(pts, dtheta, center);
    Pad_Line::Set_coords(pts, FALSE);

    _Set_angle_data(curAngle + dtheta);

    return (TRUE);
}

//////////////////////////////////////////////
//              Oval definitions
//////////////////////////////////////////////  

#define X 0
#define Y 1
#define PTS 40

static float
    baseoval[PTS][2] = {
    {1.,               0.5},
    {0.993844170298,   0.57821723252},
    {0.975528258148,   0.654508497187},
    {0.945503262094,   0.72699524987},
    {0.904508497187,   0.793892626146},
    {0.853553390593,   0.853553390593},
    {0.793892626146,   0.904508497187},
    {0.72699524987,    0.945503262094},
    {0.654508497187,   0.975528258148},
    {0.57821723252,    0.993844170298},
    {0.5,              1.},
    {0.42178276748,    0.993844170298},
    {0.345491502813,   0.975528258148},
    {0.27300475013,    0.945503262094},
    {0.206107373854,   0.904508497187},
    {0.146446609407,   0.853553390593},
    {0.0954915028125,  0.793892626146},
    {0.0544967379058,  0.72699524987},
    {0.0244717418524,  0.654508497187},
    {0.00615582970243, 0.57821723252},
    {0.,               0.5},
    {0.00615582970243, 0.42178276748},
    {0.0244717418524,  0.345491502813},
    {0.0544967379058,  0.27300475013},
    {0.0954915028125,  0.206107373854},
    {0.146446609407,   0.146446609407},
    {0.206107373854,   0.0954915028125},
    {0.27300475013,    0.0544967379058},
    {0.345491502813,   0.0244717418524},
    {0.42178276748,    0.00615582970243},
    {0.5,              0.},
    {0.57821723252,    0.00615582970243},
    {0.654508497187,   0.0244717418524},
    {0.72699524987,    0.0544967379058},
    {0.793892626146,   0.0954915028125},
    {0.853553390593,   0.146446609407},
    {0.904508497187,   0.206107373854},
    {0.945503262094,   0.27300475013},
    {0.975528258148,   0.345491502813},
    {0.993844170298,   0.42178276748}};

Pad_Oval::~Pad_Oval(void) {
    Generate_delete();
}

Pad_Oval::Pad_Oval(Pad *pad) :
    Pad_Rectangle(pad) {
    _type = PAD_OVAL;
}

Pad_Oval::Pad_Oval(Pad *pad, float x1, float y1, float x2, float y2) :
    Pad_Rectangle(pad) {
    Pad_PList pts;
    Pad_Point pt;
    float dx, dy;

    _type = PAD_OVAL;

    pt.Set(x1, y1);
    pts.Push_last(&pt);
    pt.Set(x2, y2);
    pts.Push_last(&pt);

    Set_coords(pts, TRUE);

    renderpoints.Make_empty();
    dx = x2 - x1;
    dy = y2 - y1;
    for (int i = 0; i < PTS; i++) {
        pt.Set(x1 + baseoval[i][X] * dx, y1 + baseoval[i][Y] * dy);
        renderpoints.Push_last(&pt);
    }
}

//
// Set the coordinates of an object.
// Return TRUE if successful or FALSE otherwise
//
Pad_Bool
Pad_Oval::Set_coords(Pad_PList &pts, Pad_Bool objectCoords) {
    int i;
    float x0, y0;
    float dx0, dy0;
    float dx1, dy1;
    Pad_Point pt;
    Pad_Point *ppt;

    if (Get_lock()) {
        return (TRUE);           // Can't modify if locked
    }

    if (pts.Length() != 2) {
        return (FALSE);          // Ovals must have exactly two corner points
    }

    // Erase existing points
    points.Make_empty();
    // Set new points
    pt.Set(pts.Nth(0)->x, pts.Nth(0)->y);
    points.Push_last(&pt);
    pt.Set(pts.Nth(1)->x, pts.Nth(0)->y);
    points.Push_last(&pt);
    pt.Set(pts.Nth(1)->x, pts.Nth(1)->y);
    points.Push_last(&pt);
    pt.Set(pts.Nth(0)->x, pts.Nth(1)->y);
    points.Push_last(&pt);

    if (!objectCoords) {
        DOTIMES(i, 4) {
            ppt = points.Nth(i);
            Screen_to_local(*ppt);
        }
    }

    if (Get_angle() == 0) {
        x0 = points.Nth(0)->x;
        y0 = points.Nth(0)->y;
        dx0 = points.Nth(2)->x - points.Nth(0)->x;
        dy0 = points.Nth(2)->y - points.Nth(0)->y;

        renderpoints.Make_empty();
        for (i = 0; i < PTS; i++) {
            pt.Set(x0 + baseoval[i][X] * dx0, y0 + baseoval[i][Y] * dy0);
            renderpoints.Push_last(&pt);
        }
    } else {
        x0 = points.Nth(0)->x;
        y0 = points.Nth(0)->y;
        dx0 = points.Nth(3)->x - x0;
        dy0 = points.Nth(3)->y - y0;
        dx1 = points.Nth(1)->x - x0;
        dy1 = points.Nth(1)->y - y0;

        renderpoints.Make_empty();
        for (i = 0; i < PTS; i++) {
            pt.Set(x0 + baseoval[i][X] * dx0 + baseoval[i][Y] * dx1,
                   y0 + baseoval[i][X] * dy0 + baseoval[i][Y] * dy1);
            renderpoints.Push_last(&pt);
        }
    }

    Update_and_compute_anchor();
    Generate_event(Pad_ModifyNotify, NULL, "coords");

    return (TRUE);
}

//
// Return true to continue rendering other objects, or false if rendering
// was interrupted.
//
Pad_Bool
Pad_Oval::Render(void) {
    if (Get_angle() == 0) {
        // use XOval for unrotated ovals
        if (fillColor.Is_set()) {
            Pad_renderer->Set_color(fillColor);
            Pad_renderer->Begin_oval();
            Pad_renderer->V2f(points.Nth(0));
            Pad_renderer->V2f(points.Nth(2));
            Pad_renderer->End_oval(TRUE);

        }
        if (penColor.Is_set()) {
            Pad_renderer->Set_color(penColor);
            Pad_renderer->Set_line_style(lineWidth, capStyle, joinStyle, absLineStyle);
            Pad_renderer->Begin_oval();
            Pad_renderer->V2f(points.Nth(0));
            Pad_renderer->V2f(points.Nth(2));
            Pad_renderer->End_oval(FALSE);
        }
    } else {
        if (fillColor.Is_set()) {
            Pad_renderer->Set_color(fillColor);
            Pad_renderer->Draw_polygon(renderpoints);
        }
        if (penColor.Is_set()) {
            Pad_renderer->Set_color(penColor);
            Pad_renderer->Set_line_style(lineWidth, capStyle, joinStyle, absLineStyle);
            Pad_renderer->Draw_line(renderpoints, TRUE);
        }
    }

    return (TRUE);
}

//
// Determines if the specified point is within halo pixels of the oval.
// Coordinates are local.
//
Pad_Bool
Pad_Oval::Pick(Pad_Event *event, float halo) {
    Pad_Bool rc;

    if (!fillColor.Is_set()) {
        if (penColor.Is_set()) {
            rc = event->pt.On_line(renderpoints, lineWidth + (2.0 * halo / event->mag), TRUE);
        }

    } else {
        rc = event->pt.In_polygon(renderpoints);
        if (!rc) {
            // Point not on polygon, might be on thick edge,
            // so check that.
            if (penColor.Is_set()) {
                rc = event->pt.On_line(renderpoints,
                                       lineWidth + (2.0 * halo / event->mag), TRUE);
            }
        }

    }

    return (rc);
}

//
// Must implement rotate here because otherwise, Pad_Line
// will call rectangles Set/Get_coords which only deals
// with two #s.
//
Pad_Bool
Pad_Oval::Rotate(float dtheta) {
    // center point must be in global coordinates,
    // so convert anchorpt from local*transform to global
    Pad_Point center;
    center = anchorpt;
    transform.Invert(center);
    Local_to_screen(center);

    return (Rotate(dtheta, center));
}

Pad_Bool
Pad_Oval::Rotate(float dtheta, Pad_Point &center) {
    float curAngle;
    Pad_PList pts;
    Pad_Point pt;


    curAngle = Get_angle();

    Pad_Line::Get_coords(pts, FALSE);
    Pad_Rotate(pts, dtheta, center);

    float dx0, dy0, dx1, dy1, x0, y0;
    x0 = pts.Nth(0)->x;
    y0 = pts.Nth(0)->y;
    dx0 = pts.Nth(3)->x - x0;
    dy0 = pts.Nth(3)->y - y0;
    dx1 = pts.Nth(1)->x - x0;
    dy1 = pts.Nth(1)->y - y0;

    renderpoints.Make_empty();
    for (int i = 0; i < PTS; i++) {
        pt.Set(x0 + baseoval[i][X] * dx0 + baseoval[i][Y] * dx1,
               y0 + baseoval[i][X] * dy0 + baseoval[i][Y] * dy1);
        Screen_to_local(pt);
        renderpoints.Push_last(&pt);
    }

    // Need to reset transform here because
    // internal points have been set to global
    // coords, and bbox should get computed
    // relative to new points without old transform.
    transform.Set(0.0f, 0.0f, 1.0f);
    Pad_Line::Set_coords(pts, FALSE);
    _Set_angle_data(curAngle + dtheta);

    return (TRUE);
}

void
Pad_Oval::Compute_bounding_box(void) {
    int i;
    float hw;
    float xmin, xmax, ymin, ymax;
    Pad_Point *pt0, *pti;

    if (renderpoints.Length() > 0) {
        pt0 = renderpoints.First();
        xmin = xmax = pt0->x;
        ymin = ymax = pt0->y;
        for (i = 1; i < renderpoints.Length(); i++) {
            pti = renderpoints.Nth(i);
            if (pti->x < xmin) xmin = pti->x;
            if (pti->y < ymin) ymin = pti->y;
            if (pti->x > xmax) xmax = pti->x;
            if (pti->y > ymax) ymax = pti->y;
        }

        hw = 0.6 * lineWidth;
        if (!(optionFlags & PAD_WIDTH_SET)) {
            if ((xmax - xmin + hw) == 0.0) {
                // Protect against vertical 0 width lines
                Set_bbox_xmin(xmin - (0.001 * (ymax - ymin)));
                Set_bbox_xmax(xmax + (0.001 * (ymax - ymin)));
            } else {
                Set_bbox_xmin(xmin - hw);
                Set_bbox_xmax(xmax + hw);
            }
        }
        if (!(optionFlags & PAD_HEIGHT_SET)) {
            if ((ymax - ymin + hw) == 0.0) {
                // Protect against horizontal 0 width lines
                Set_bbox_ymin(ymin - (0.001 * (xmax - xmin)));
                Set_bbox_ymax(ymax + (0.001 * (xmax - xmin)));
            } else {
                Set_bbox_ymin(ymin - hw);
                Set_bbox_ymax(ymax + hw);
            }
        }
    } else {
        if (!(optionFlags & PAD_WIDTH_SET)) {
            Set_bbox_xmin(0.0);
            Set_bbox_xmax(0.0);
        }
        if (!(optionFlags & PAD_HEIGHT_SET)) {
            Set_bbox_ymin(0.0);
            Set_bbox_ymax(0.0);
        }
    }

    Pad_Object::Compute_bounding_box();
}
