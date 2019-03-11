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

/*
When a region of a Pad surface changes, Pad_Win creates a Pad_Restorer
corrosponding to the changed region. The Pad_Restorer oversee the
restoration of that region. You can think of a Pad_Restorer as a process
which is forked to repair part of a Pad surface.

The Pad_Win object is responsible for managing the restorers that are
currently active.
*/


#include "defs.h"
#include "view.h"
#include "object.h"
#include "pad.h"
#include "restorer.h"
#include "renderer.h"

#include "global.h"
#include "win.h"
#include "display.h"

#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <math.h>

#  include <unistd.h>

//
// Callback used to run the restoration process
//
static void
restoreCB(ClientData clientData)
{
    Pad_Restorer *restorer = (Pad_Restorer*)clientData;

    if (Pad_prc) {
				// Postpone any restorations that occur
				// during a render
	restorer->timer = Pad_CreateTimerHandler(IDLE_DELAY, restoreCB, clientData);
    } else {
	restorer->timer = NULL;
	restorer->Do_restore();
    }
}


////////////////////////////////////////////////////////////
//
// Pad_Restorer class
//
////////////////////////////////////////////////////////////
Pad_Restorer::~Pad_Restorer()
{
    XDestroyRegion(xregion);
    xregion = NULL;              // Nullify pointer for safety in case restorer gets
				 // deleted again by accident.
    if (timer) {
        Pad_DeleteTimerHandler(timer);
    }
    if (this == win->biddingRestorer) {
	win->biddingRestorer = NULL;
    }
    Reset_clip_stack();
}

Pad_Restorer::Pad_Restorer(Pad_Win *w, Pad_Bool is_full)
{
    win = w;
    refinementLevel = -1;
    timer = NULL;
    delay = 0;
    full = is_full;
    mode = RESTORE_UNDEFINED;
    should_flush = TRUE;
    flush_all = FALSE;
    xregion = XCreateRegion();
}

Pad_Restorer::Pad_Restorer(Pad_Restorer *restorer)
{
    void *region;		// Avoid incorrect DOLIST automatic casting for Region
    Pad_Iterator ri;

    win = restorer->win;
    refinementLevel = restorer->refinementLevel;
    timer = NULL;
    delay = restorer->delay;
    full = restorer->full;
    should_flush = TRUE;
    flush_all = FALSE;

    mode = RESTORE_UNDEFINED;	// Scheduler sets this
    xregion = XCreateRegion();
    XUnionRegion(xregion, restorer->xregion, xregion);

    DOLIST(ri, restorer->clip_stack, void, region) {
	Region new_region = XCreateRegion();
	XUnionRegion(new_region, (Region)region, new_region);
	clip_stack.Push_last(new_region);
    }
}

//
// Compute union of two restoration regions
//
Pad_Restorer &
Pad_Restorer::operator += (const Pad_Restorer &other)
{
    assert(win == other.win);

    if (other.full) {
        // Make this region full
        full = TRUE;
    } else if (full) {
        ; // this region already includes the other region
    } else {
        XUnionRegion(xregion, other.xregion, xregion);
    }

    return(*this);
}

//
// Compute union of two restoration regions
//
Pad_Restorer &
Pad_Restorer::operator += (XRectangle xrect)
{
    if (!full) {
	XUnionRectWithRegion(&xrect, xregion, xregion);
    }
    return(*this);
}


//
// Subtract <other> restorer's region from <this>
//
Pad_Restorer &
Pad_Restorer::operator -= (const Pad_Restorer &other)
{
    assert(win == other.win);

    if (other.full) {
        // Make this region empty
        XDestroyRegion(xregion);
        xregion = XCreateRegion();
        full = FALSE;
    } else {
	if (full) {
	    XDestroyRegion(xregion);
	    xregion = XCreateRegion();
	    full = FALSE;
	    Pad_AddRectToRegion(xregion, 0, 0, win->width, win->height);
	}
	XSubtractRegion(xregion, other.xregion, xregion);
    }
    return(*this);
}

//
// Compute intersection of restorer's regions.
//
Pad_Restorer &
Pad_Restorer::operator &= (const Pad_Restorer &other)
{
    assert(win == other.win);

    if (other.full)
        ; // Intersection with a full region leaves this unchanged
    else if (full) {
        // If I was full, I am now a copy of the other region
        full = FALSE;
        XDestroyRegion(xregion);
        xregion = XCreateRegion();
        XUnionRegion(xregion, other.xregion, xregion);
    } else {
        XIntersectRegion(xregion, other.xregion, xregion);
    }

    return(*this);
}


//
// Converts bounding box of object to screen coordinates
// Resulting coords will be slightly larger than bounding box.
// This insures that objects will not be clipped based
// on rounding errors when converting bounding boxes.
// Thus, this shouldn't be used when exact coordinates are needed.
// Must be called within render.
//
static void
bbox_to_X_rect(float bbox[4], XRectangle *rect)
{
    Pad_Point min, max;

    min.Set(bbox[XMIN], bbox[YMIN]);
    Pad_renderer->Local_to_screen(min);

    max.Set(bbox[XMAX], bbox[YMAX]);
    Pad_renderer->Local_to_screen(max);

    Pad_SetRoundedRect(rect, min.x, max.y, (max.x - min.x + 2), (min.y - max.y + 2));
}

//
// Get_visibility
//
//   Return the percentage of the object that was rounded.
//   (xmin, ymin) - (xmax, ymax) will be filled in with numbers between 0.0 and 1.0
//   that represent what portion of the object is visible.
//   (0, 0) represents the lower left corner of the object.
//   (1, 1) represents the upper right corner of the object.
//
void
Pad_Restorer::Get_visibility(Pad_Object *obj, float &xmin, float &ymin, float &xmax, float &ymax)
{
    float bb[4];
    float clipped[4], unclipped[4];
    float unclipped_width, unclipped_height;

    obj->Get_bbox(bb);
    Get_bbox_screen_coords(bb, clipped, unclipped);
    unclipped_width = unclipped[XMAX] - unclipped[XMIN];
    unclipped_height = unclipped[YMAX] - unclipped[YMIN];

    if ((unclipped_width == 0) || (unclipped_height == 0)) {
	xmin = 0.0;
	ymin = 0.0;
	xmax = 0.0;
	ymax = 0.0;
    } else {
	xmin = (clipped[XMIN] - unclipped[XMIN]) / unclipped_width;
	xmax = (clipped[XMAX] - unclipped[XMIN]) / unclipped_width;
	ymin = 1.0 - (clipped[YMAX] - unclipped[YMIN]) / unclipped_height;
	ymax = 1.0 - (clipped[YMIN] - unclipped[YMIN]) / unclipped_height;
    }
}

//
// Get_bbox_screen_coords
//
//   Return the bounding box of the object in current screen coordinates, and also the
//   intersection of the bounding box with the current clip region.
//   The caller must provide bbox's to return the clipped and unclipped values in.
//   Use the parent's coordinate system (must be called within a render)
//
//   Note that the clipped or unclipped width may be zero, so the caller
//   must check them if they are ever used in division.
//
void
Pad_Restorer::Get_bbox_screen_coords(float *bbox, float *clipped, float *unclipped)
{
    Pad_Point min, max;
    XRectangle clipped_rect;
    Pad_Transform transform;

				// Calculate (unclipped) screen bounding box
    min.Set(bbox[XMIN], bbox[YMIN]);
				// Transform from local to screen, but can't use Local_to_screen,
				// because that also clips.
    Pad_renderer->Get_transform(transform);
    transform.Apply(min);
    min.y = win->height - min.y;

    max.Set(bbox[XMAX], bbox[YMAX]);
    transform.Apply(max);
    max.y = win->height - max.y;

    unclipped[XMIN] = min.x;
    unclipped[YMIN] = max.y;
    unclipped[XMAX] = max.x;
    unclipped[YMAX] = min.y;

				// Calculate clipped screen bounding box
    XClipBox(Get_region(), &clipped_rect);
    clipped[XMIN] = MAX(clipped_rect.x, unclipped[XMIN]);
    clipped[YMIN] = MAX(clipped_rect.y, unclipped[YMIN]);
    clipped[XMAX] = MIN(clipped_rect.x + clipped_rect.width, unclipped[XMAX]);
    clipped[YMAX] = MIN(clipped_rect.y + clipped_rect.height, unclipped[YMAX]);
}

//
// Test if a bounding rectangle overlaps within a restorer's region.
// The bbox is transformed by the current matrix, so the direct bounding box of an
// object can be passed here (untransformed by any groups it might be a member of).
//
Pad_Bool
Pad_Restorer::Contains_rect(float bbox[4])
{
    int result;

    XRectangle rect;
    bbox_to_X_rect(bbox, &rect);
    result = XRectInRegion(Get_region(), rect.x, rect.y, rect.width, rect.height);
    
    if (result == RectangleOut) {
	return FALSE;
    } else {
	return TRUE;
    }
}

//
// Test if a bounding rectangle falls entirely within a restorer's region.
//
Pad_Bool
Pad_Restorer::Occludes_rect(float bbox[4])
{
    XRectangle rect;
    bbox_to_X_rect(bbox, &rect);
    return (XRectInRegion(Get_region(),
			  rect.x, rect.y, rect.width, rect.height) == RectangleIn);
}

//
// Add specified bounding box (pad pixels) to a restorer
//
void
Pad_Restorer::Add_rect(float xmin, float ymin, float xmax, float ymax)
{
    if (full) {
        return;
    } else {
	float x, y;
	XRectangle rect;

	x = xmin + (0.5 * win->width) - 0.5;
	y = (0.5 * win->height) - ymax - 0.5;
	Pad_SetRoundedRect(&rect, x - 2, y - 2, (xmax - xmin + 5.0), (ymax - ymin + 5.0));
	XUnionRectWithRegion(&rect, xregion, xregion);
    }
}

//
// Add specified bounding box (pad coordinates) to a restorer
// Used in render methods to add rects to a region. Uses current screen
// transform info. Doesn't clip against current screen mask
//
void
Pad_Restorer::Add_rect(float bbox[4])
{
    XRectangle rect;
    bbox_to_X_rect(bbox, &rect);
    XUnionRectWithRegion(&rect, xregion, xregion);
}

//
// Used in render method to add rects to a region. Uses current screen
// transform info. Clips against current screen mask
//
void
Pad_Restorer::Add_rect_to_refiner(float bbox[4])
{
    if (!full) {
				// Calculate screen coords
	XRectangle rect;
	bbox_to_X_rect(bbox, &rect);
	rect.width++;
	rect.height++;
				// Insure that the new rect is clipped
				// against the current clipping region
	Region region = XCreateRegion();
	XUnionRectWithRegion(&rect, region, region);
	XIntersectRegion(region, Pad_prc->win->activeRestorer->Get_region(), region);

				// Now add it to xregion
	XUnionRegion(region, xregion, xregion);
	XDestroyRegion(region);
    }
}

Region
Pad_Restorer::Get_region(void)
{
    if (clip_stack.Is_empty()) {
	if (full) {
	    return win->windowRestorer->xregion;
	} else {
	    return xregion;
	}
    } else {
	return ((Region)clip_stack.First());
    }
}

// Returns screen clip rectangle
void 
Pad_Restorer::Get_clip_box(int &x, int &y, int &width, int &height)
{
    XRectangle rect;

    if (full) {
	XClipBox(win->windowRestorer->xregion, &rect);
    } else {
	XClipBox(xregion, &rect);
    }
    x = rect.x;
    y = rect.y;
    width = rect.width;
    height = rect.height;
}

//
// On Windows, this
//   Initializes the DC's
//   and sets up the current clipping region
// On X, this
//   Sets up the current clipping region
//
void
Pad_Restorer::Initialize_clip_region(void)
{
    Set_gc_clip(win->fgGC);
    Set_gc_clip(win->copyGC);
}

void
Pad_Restorer::Finalize_clip_region(void)
{
    Unset_gc_clip(win->fgGC);
    Unset_gc_clip(win->copyGC);
}

//
// Set the clipmask of a GC to a region
//
void
Pad_Restorer::Set_gc_clip(GC gc)
{
	if (full) {
        XSetClipMask(win->dpy->display, gc, None);
    } else {
		XSetRegion(win->dpy->display, gc, xregion);
    }
}

void
Pad_Restorer::Unset_gc_clip(GC gc)
{
    XSetClipMask(win->dpy->display, gc, None);
}


//
// Set the clipmask of a GC to the intersection region and rectangle
//
void
Pad_Restorer::Push_clip(float bbox[4])
{
    Region region = XCreateRegion();

    if (!bbox) {
	Unset_gc_clip(win->fgGC);
    } else {
	XRectangle rect;
	
	bbox_to_X_rect(bbox, &rect);

	XUnionRectWithRegion(&rect, region, region);
    }
    Push_clip(region);
}

//
// JM - Set the clipmask to a polygonal shape. Used for nonrectangular portals.
//
void Pad_Restorer::Push_clip(Pad_PList &points)
{
    Push_clip(points.Pointer(), points.Length());
}

void Pad_Restorer::Push_clip(Pad_Point *points, int npoints)
{
    Pad_Point point;
    XPoint *xpts;
    Region r;
    int i;

    xpts = new XPoint[npoints];
    for (i = 0; i < npoints; i++) {
        point.Set(points[i].x, points[i].y);
        Pad_renderer->Local_to_screen(point);
        xpts[i].x = (short)point.x;
        xpts[i].y = (short)point.y;
    }
    r = XPolygonRegion(xpts, npoints, EvenOddRule);
    delete [] xpts;
    Push_clip(r);
}

//
// Underlying routine called by other Push_clip methods.
//
void Pad_Restorer::Push_clip(Region region)
{
    if (clip_stack.Is_empty()) {
	if (full) {
	    XIntersectRegion(win->windowRestorer->xregion, region, region);
	} else {
	    XIntersectRegion(xregion, region, region);
	}
    } else {
	XIntersectRegion((Region)clip_stack.First(), region, region);
    }

    XSetRegion(win->dpy->display, win->fgGC, region);

    clip_stack.Push((void*)region);
}

void
Pad_Restorer::Pop_clip()
{
    if (clip_stack.Is_empty()) {
	cerr << "pop_clip: stack empty" << endl;
    } else {
	XDestroyRegion((Region)clip_stack.Pop());
	if (clip_stack.Is_empty()) {
	    Set_gc_clip(win->fgGC);
	} else {
	    Region region = (Region)clip_stack.First();
	    XSetRegion(win->dpy->display, win->fgGC, region);
	}
	}
}

void
Pad_Restorer::Reset_clip_stack()
{
    Region region;
    while ((region = (Region)clip_stack.Pop())) {
	XDestroyRegion(region);
    }
}


//
// Method that actually does the work
//
void
Pad_Restorer::Do_restore(void)
{
    Pad_Restorer *newer;
    Pad_Restorer *r;
    Pad_Iterator ri;

    if (timer) {
	Pad_DeleteTimerHandler(timer);
	timer = NULL;
    }

				// If window is not mapped, don't restore.
    if (!win->Is_mapped()) {
	win->restorers.Remove((void*)this);
	delete this;
	return;
    }

    if (this == win->biddingRestorer) {
	win->biddingRestorer = NULL;     // Close bidding
    }

				// Go through all newer restorers in win->restorers,
				// subtracting their regions from this restorer's region.
    DOLIST(ri, win->restorers, Pad_Restorer, newer) {
	if (newer == this)
	  break;
	*this -= *newer;
    }

				// If this is not a refiner, then reschedule all refiners
				// to act a bit later.
    if (!Is_refiner()) {
	DOLIST(ri, win->restorers, Pad_Restorer, r) {
	    if (r->Is_refiner()) {
		r->Reschedule();
	    }
	}
    }

    if (!full) {
	XIntersectRegion(xregion, win->windowRestorer->xregion, xregion);
    }

				// Remove this restorer from the list of active restorers
    win->restorers.Remove((void*)this);

				// Use default refinement level if none was given.
    if (refinementLevel == -1) {
	refinementLevel = win->defaultRenderLevel;
    }

				// Render restorer
    if (!Is_empty()) {
	if (!win->view->Render(this)) {
				// Restorer failed to render fully because it was
				// interrupted.  Reschedule it to run later.
	    Reschedule_copy();
	}
    }
    delete this;
}


// Makes a copy of this and schedules it to refine
void
Pad_Restorer::Reschedule_copy(void)
{
    Pad_Restorer *new_restorer = new Pad_Restorer(this);
    Pad_Restore_Mode new_mode;

    // repairers stay repairers - all other things become refiners
    new_mode = ((mode == RESTORE_REPAIR) ? RESTORE_REPAIR : RESTORE_REFINE);
    new_restorer->delay = RESCHEDULE_DELAY;
    new_restorer->Schedule(new_mode, NULL);
}

// Reschedules this to run later
void
Pad_Restorer::Reschedule(void)
{
    if (timer) {
        Pad_DeleteTimerHandler(timer);
    }
    timer = Pad_CreateTimerHandler(RESCHEDULE_DELAY, restoreCB, (ClientData)this);
}

//
// Activating a restorer adds it to its win's list of active restorers, and schedules a
// callback to run the restore.
//
void
Pad_Restorer::Schedule(Pad_Restore_Mode newMode, Pad_Restorer *current)
{
    if (win->restorers.Member(this)) {
	if ((newMode == mode) && (mode == RESTORE_REPAIR || mode == RESTORE_REFINE)) {
	} else if (newMode != RESTORE_NOW) {
	    printf("Internal Error: Pad_Restorer::scedule() called twice, mode=%d\n",
		   mode);
	    return;
	}
    } else {
	win->restorers.Push((void*)this);
    }

    mode = newMode;

				// There could be an existing timer if this restorer
				// was rescheduled.
    if (timer) {
	Pad_DeleteTimerHandler(timer);
	timer = NULL;
    }

    switch (mode) {
      case RESTORE_NOW:
	Do_restore();
	break;
	
      case RESTORE_WHEN_IDLE:
				// Use timers instead of idle event
				// so that they happen before refinements.
	timer = Pad_CreateTimerHandler(IDLE_DELAY, restoreCB, (ClientData)this);
	break;
	
      case RESTORE_REPAIR:
	delay = REPAIR_DELAY;
	refinementLevel = win->highestRenderLevel;
	timer = Pad_CreateTimerHandler(delay, restoreCB, (ClientData)this);
	break;

      case RESTORE_REFINE:
	if (current) {
	    if (current->Is_refiner()) {
		delay = REFINE_DELAY;
	    } else {
		delay = win->refinementDelay;
	    }
	    refinementLevel = current->refinementLevel + 1;
	}

	timer = Pad_CreateTimerHandler(delay, restoreCB, (ClientData)this);
	break;

      default:
	printf("Internal Error: Pad_Restorer::Schedule() unknown mode\n");
    }
}

ostream &
operator << (ostream& os, Pad_Restorer &r)
{
    os << "<Pad_Restorer";
    os << " " << &r << ">";

    return os;
}

//
// Utility function to add rectangle to region (avoids building XRectangle)
//
void
Pad_AddRectToRegion(Region region, int x, int y, int width, int height)
{
    static XRectangle rect;

    rect.x = x;
    rect.y = y;
    rect.width = width;
    rect.height = height;

    XUnionRectWithRegion(&rect, region, region);
}

//
// Round rects properly to avoid overflow, since rects are implemented with shorts.
// Note that not only must we avoid rounding improperly, we must make sure that
// (x + width) and (y + height) are both within the range of a short.
//
void
Pad_SetRoundedRect(XRectangle *rect, float x, float y, float width, float height)
{
    float dx, dy;
    float rx, ry;

    rx = x;
    if (x < -32768)
      rx = -32768;
    else if (x > 32767)
      rx = 32767;

    ry = y;
    if (y < -32768)
      ry = -32768;
    else if (y > 32767)
      ry = 32767;

    rect->x = Pad_F2rs(x, 0);
    rect->y = Pad_F2rs(y, 0);
    dx = fabs(x - rx);
    dy = fabs(y - ry);
    if ((rect->x + width - dx) < 32767) {
	rect->width = Pad_F2s(width - dx);
    } else {
	rect->width = Pad_F2s(width - dx) - rect->x;
    }
    if ((rect->y + height - dy) < 32767) {
	rect->height = Pad_F2s(height - dy);
    } else {
	rect->height = Pad_F2s(height - dy) - rect->y;
    }
}
