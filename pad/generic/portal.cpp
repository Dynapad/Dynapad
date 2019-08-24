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
#include "list.h"
#include "pad.h"
#include "point.h"
#include "object.h"
#include "portal.h"
#include "object.h"
#include "line.h"
#include "win.h"
#include "text.h"
#include "restorer.h"
#include "renderer.h"
#include "group.h"
#include "events.h"
#include "global.h"

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/types.h>

#  include <unistd.h>

#define PORTAL_DEFAULT_BORDERWIDTH 2
#define PORTAL_DEFAULT_FONT        "Times-1"
#define PORTAL_DEFAULT_RELIEF      PAD_RELIEF_RIDGE
#define PORTAL_DEFAULT_TITLE       ""

//////////////////////////////////////////////
//              Portal definitions
//////////////////////////////////////////////

Pad_Portal::~Pad_Portal()
{
    Pad_Portal *portal;
    Pad_Iterator pi;

    Generate_delete();

    if (lookon && (lookon != win)) {
	lookon->lookonPortals.Remove(this);
    }
    win->viewPortals.Remove(this);
    win->view->visiblePortals.Remove(this);
    DOLIST(pi, win->viewPortals, Pad_Portal, portal) {
	portal->visiblePortals.Remove(this);
    }
    if (title) {
	delete title;
	title = NULL;
    }
}

Pad_Portal::Pad_Portal(Pad *pad) :
Pad_View(pad)
{
    _type = PAD_PORTAL;
    win = pad->view->win;
    portalFlags = PAD_NO_MASK;
    lookon = NULL;
    currentlyVisible = FALSE;
    title = NULL;
    recursiveRenderLevel = 0;
    serial = 0;
    rectangle = FALSE;
    
    Set_fill_default();
    Set_borderwidth_default();
    Set_pen_default();
    Set_font_default();
    Set_borderrelief_default();
    Set_title_default();

    xview = 0.0;
    yview = 0.0;
    zoom = 1.0;

    lookon = pad->view->win;
    win->viewPortals.Push_last(this);

    Compute_bounding_box();
    Compute_view_bounding_box();
				// Necessary to update view's bbox because otherwise the pad's 
				// global bbox will be messed up as a result of the view's contructor
				// getting called when this portal was created.
    pad->view->Compute_view_bounding_box();
}

//
// Gets called with string args when object is created.  This routine must 
// process any arguments up to, but not including the options.  (argc, argv) 
// point to parameters on the creation line just after 'create <type>'.
// This returns the number of args processed.
//
int
Pad_Portal::Create_obj_args(int argc, char **argv)
{
    int i, coords;
    Pad_PList pts;
    Pad_Point pt;
    Pad_Bool rc;
		
				// Process args to find how many coords are specified
    coords = 0;
    while ((coords < argc) &&
	   ((argv[coords][0] != '-') || (isdigit(argv[coords][1])) || (argv[coords][1] == '.'))) {
	    coords++;
    }

    if ((coords > 0) && (coords < 4)) {
	Pad_errorString = "wrong # args: should be \"pathName create portal x1 y1 ?... xn yn option value ...?\"";
	return(-1);
    }
		
    if ((coords % 2) != 0) {
	Pad_errorString = "wrong # args: need an event number of points";
	return(-1);
    }

				// Add coords to this item
    DOTIMES(i, coords/2) {
	pt.Set(ATOXF(pad->view->win, argv[0]), ATOYF(pad->view->win, argv[1]));
	pts.Push_last(&pt);
	argc -= 2;
	argv += 2;
    }
    rc = Set_coords(pts, TRUE);

    if (!rc) {
	Pad_errorString = "invalid coordinates";
	return(-1);
    }

    return(coords);
}

//
// Damage a portal.  Must define this to call the Pad_Object Damage()
// method directly or else the Pad_View Damage() will get called
// which would damage the entire scene.
//
void
Pad_Portal::Damage(void)
{
    Pad_Object::Damage();
}

//
// Setters and getters for view
//
Pad_Bool
Pad_Portal::Set_view(float newXview, float newYview, float newZoom, Pad_Bool renderNow)
{
    Pad_Bool rc;

    // don't change if locked
    if (Get_lock()) {
        return TRUE;
    }

    portalFlags |= PORTAL_VIEW_SET;

    rc = Pad_View::Set_view(newXview, newYview, newZoom, renderNow);

    return(rc);
}

void
Pad_Portal::Set_view_default(void)
{
    float xctr, yctr, newZoom;
    Pad_BBox bb;

    portalFlags &= ~PORTAL_VIEW_SET;
    
				// If portal looking on surface it is on,
				// then center view to be "underneath" portal
    if (lookon == pad->view->win) {
	Get_global_bbox(bb);
	xctr = 0.5 * (bb.Xmin() + bb.Xmax());
	yctr = 0.5 * (bb.Ymin() + bb.Ymax());
	newZoom = 1.0 / Get_global_size();
	Set_view(xctr, yctr, newZoom);
    } else {
				// If portal looking onto other surface,
				// then reset view to "0 0 1"
	Set_view(0.0, 0.0, 1.0);
    }
}

//
// Setters and getters for lookon
//
void
Pad_Portal::Set_lookon(Pad_Win *newWin)
{
    Pad_String layerString;

    portalFlags |= PORTAL_LOOKON_SET;
    if (lookon && (lookon != win)) {
	lookon->lookonPortals.Remove(this);
    }
    lookon = newWin;
    if (newWin) {
	if (newWin != win) {
	    newWin->lookonPortals.Push_last(this);
	}
	newWin->view->Update_visibility();
    }
    Update_visibility();
				// Update layers
    Update_layers();
    Get_visiblelayers(layerString);
    Set_visiblelayers(layerString.Get());

    Damage();
}

void
Pad_Portal::Set_lookon_default(void)
{
    Set_lookon(pad->view->win);
    portalFlags &= ~PORTAL_LOOKON_SET;
}

Pad_Win *
Pad_Portal::Get_lookon(void)
{
    return(lookon);
}

//
// Setters and getters for title
//
void
Pad_Portal::Set_title(const char *new_title)
{
    if (title) {
	    delete title;
	    title = NULL;
    }
        portalFlags |= PORTAL_TITLE_SET;
    if (new_title[0] != '\0') {
        title = new Pad_String(new_title);
    }
    Damage();
}

void
Pad_Portal::Set_title_default(void)
{
    Set_title(PORTAL_DEFAULT_TITLE);
    portalFlags &= ~PORTAL_TITLE_SET;
}

const char *
Pad_Portal::Get_title(void)
{
    if (title) {
	    return(title->Get());
    } else {
	    return("");
    }
}

//
// Setters and getters for pen color
//
Pad_Bool
Pad_Portal::Set_pen(const char *colorname)
{
    portalFlags |= PORTAL_PEN_SET;
    titleColor.Set(colorname);
    Damage();

    return(TRUE);
}

Pad_Bool
Pad_Portal::Set_pen(int red, int green, int blue)
{
    portalFlags |= PORTAL_PEN_SET;
    titleColor.Set(red, green, blue);
    Damage();

    return(TRUE);
}

void
Pad_Portal::Set_pen_default(void)
{
    intptr_t red, green, blue;
    portalFlags &= ~PORTAL_PEN_SET;

    border.Get(red, green, blue);
    titleColor.Set_contrasting(red, green, blue);
    Damage();
}

void
Pad_Portal::Get_penname(Pad_String &fillname)
{
    titleColor.Get(fillname);
}

//
// Setters and getters for fill color
//
Pad_Bool
Pad_Portal::Set_fill(const char *colorname)
{
    portalFlags |= PORTAL_FILL_SET;
    fillColor.Set(colorname);

				// If border is default, then set it to follow fill
    if (!(portalFlags & PORTAL_BORDER_SET)) {
	Set_border_default();
    }
				// If pen is default, then set it to follow fill
    if (!(portalFlags & PORTAL_PEN_SET)) {
	Set_pen_default();
    }
    Damage();

    return(TRUE);
}

Pad_Bool
Pad_Portal::Set_fill(int red, int green, int blue)
{
    portalFlags |= PORTAL_FILL_SET;
    fillColor.Set(red, green, blue);

				// If border is default, then set it to follow fill
    if (!(portalFlags & PORTAL_BORDER_SET)) {
	Set_border_default();
    }
				// If pen is default, then set it to follow fill
    if (!(portalFlags & PORTAL_PEN_SET)) {
	Set_pen_default();
    }
    Damage();

    return(TRUE);
}

void
Pad_Portal::Set_fill_default(void)
{
    Pad_String colorName;
    pad->view->win->background.Get(colorName);
    Set_fill(colorName.Get());
    portalFlags &= ~PORTAL_FILL_SET;
}

void
Pad_Portal::Get_fillname(Pad_String &fillname)
{
    fillColor.Get(fillname);
}

void
Pad_Portal::Set_border(char *bordername)
{
    portalFlags |= PORTAL_BORDER_SET;
    border.Set(bordername);
    Damage();
}

void
Pad_Portal::Set_border_default(void)
{
    Pad_String borderName;

    if (fillColor.Is_set()) {
	fillColor.Get(borderName);
    } else {
	borderName = "none";
    }
    
    Set_border(borderName.Get());
    portalFlags &= ~PORTAL_BORDER_SET;
}

void
Pad_Portal::Set_borderrelief(int new_relief)
{
    portalFlags |= PORTAL_RELIEF_SET;
    relief = new_relief;
    Damage();
}

void
Pad_Portal::Set_borderrelief_default(void)
{
    Set_borderrelief(PORTAL_DEFAULT_RELIEF);
    portalFlags &= ~PORTAL_RELIEF_SET;
}

void
Pad_Portal::Get_bordername(Pad_String &borderName)
{
    border.Get(borderName);
}

int
Pad_Portal::Get_borderrelief(void)
{
    return(relief);
}

void
Pad_Portal::Set_borderwidth(float borderWidth)
{
    portalFlags |= PORTAL_BORDERWIDTH_SET;
    lineWidth = borderWidth;
    Update();
}

void
Pad_Portal::Set_borderwidth_default(void)
{
    Set_borderwidth(PORTAL_DEFAULT_BORDERWIDTH);
    portalFlags &= ~PORTAL_BORDERWIDTH_SET;
}

float
Pad_Portal::Get_borderwidth(void)
{
    return(lineWidth);
}

void
Pad_Portal::Set_font(const char *fontname)
{
    portalFlags |= PORTAL_FONT_SET;
    font.Set(fontname);
    Damage();
}

void
Pad_Portal::Set_font_default(void)
{
    Set_font(PORTAL_DEFAULT_FONT);
    portalFlags &= ~PORTAL_FONT_SET;
}

void
Pad_Portal::Get_font(Pad_String &fontname)
{
    font.Get(fontname);
}

//
// Set the coordinates of the portal.
// Return TRUE if successful or FALSE otherwise
//
// If two points are specified, then they specify opposite sides of a rectangle.
// Otherwise, they specify the points of a polygon.
//
Pad_Bool 
Pad_Portal::Set_coords(Pad_PList &pts, Pad_Bool objectCoords)
{
    return(Set_coords(pts, objectCoords, TRUE));
}

Pad_Bool 
Pad_Portal::Set_coords(Pad_PList &pts, Pad_Bool objectCoords, Pad_Bool recomputeView)
{
    if (Get_lock()) {
	return(TRUE);		// Can't modify if locked
    }

    _Set_angle_data(0);
    points.Make_empty();
    return(Append_coords(pts, objectCoords, recomputeView));
}

Pad_Bool 
Pad_Portal::Append_coords(Pad_PList &pts, Pad_Bool objectCoords)
{
    return (Append_coords(pts, objectCoords, TRUE));
}

Pad_Bool 
Pad_Portal::Append_coords(Pad_PList &pts, Pad_Bool objectCoords, Pad_Bool recomputeView)
{
    int i;
    Pad_Point pt, *pt0, *pt1;
    float oldxctr, oldyctr;
    float xctr, yctr;
    float tmp;
    Pad_BBox bb;

    if (Get_lock()) {
	return(TRUE);		// Can't modify if locked
    }

    Get_global_bbox(bb);
    oldxctr = 0.5 * (bb.Xmin() + bb.Xmax());
    oldyctr = 0.5 * (bb.Ymin() + bb.Ymax());

    DOTIMES(i, pts.Length()) {
	pt = pts.Nth(i);
	if (!objectCoords) {
	    Screen_to_local(pt);
	}
	points.Push_last(&pt);
    }

				// Insure that rectangle's points are ordered properly
    if (pts.Length() == 2) {
	rectangle = TRUE;
	pt0 = points.First();
	pt1 = points.Last();
	if (pt0->x > pt1->x) {
	    tmp = pt0->x;
	    pt0->x = pt1->x;
	    pt1->x = tmp;
	}
	if (pt0->y > pt1->y) {
	    tmp = pt0->y;
	    pt0->y = pt1->y;
	    pt1->y = tmp;
	}
    } else {
	rectangle = FALSE;
    }
				// Set up new pad_view
    Update_and_compute_anchor();

    

    if (recomputeView == TRUE) {
	Get_global_bbox(bb);
	xctr = 0.5 * (bb.Xmin() + bb.Xmax());
	yctr = 0.5 * (bb.Ymin() + bb.Ymax());
	xview += (xctr - oldxctr) / (zoom * transform.Get_scale());
	yview += (yctr - oldyctr) / (zoom * transform.Get_scale());
    }

    Compute_view_bounding_box();

    Generate_event(Pad_ModifyNotify, NULL, "coords");

    return(TRUE);
}

//
// Return the points of the portal.  The points are returned
// in allocated memory and must be freed.
//
void
Pad_Portal::Get_coords(Pad_PList &pts, Pad_Bool objectCoords)
{
    int i, len;
    Pad_Point pt;

    len = points.Length();
    DOTIMES(i, len) {
	pt = points.Nth(i);
	if (!objectCoords) {
	    Pad_Object::Local_to_screen(pt);
	}
	pts.Push_last(&pt);
    }
}

//
// Set the width of a portal.  Really a wrapper around coords,
// maintaining anchor point.
//
Pad_Bool
Pad_Portal::Set_width(float width)
{
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
    switch(anchor) {
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

    return(TRUE);
}

//
// Set the width of a portal.  Really a wrapper around coords,
// maintaining anchor point.
//
Pad_Bool
Pad_Portal::Set_height(float height)
{
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
    switch(anchor) {
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

    return(TRUE);
}

//
// Determines if the specified point is within halo pixels of the portal.
// The event is in local coordinates.
//
Pad_Bool
Pad_Portal::Pick(Pad_Event *event, float halo)
{
    Pad_Bool rc;

    if (rectangle) {
	rc = Pad_Object::Pick(event, halo);
    } else {
				// Check first if it is on the border.
	rc = event->pt.On_line(points, lineWidth + (2.0 * halo / event->mag), TRUE);
	if (!rc) {
				// See if it's inside the portal
	    rc = event->pt.In_polygon(points);
	}
    }

    return(rc);
}

//
// Determines if the specified point is within halo pixels of the
// portal's border.  This assumes that the point is already determined to be
// within the object's bounding box.
// The event is in local coordinates.
//
Pad_Bool
Pad_Portal::Pick_border(Pad_Event *event, float halo)
{
    float min_dist;
    float titleheight;
    float bb[4];
    Pad_Bool rc;

    min_dist = lineWidth + (2.0 * halo / event->mag);
    if (rectangle) 
      {
	  if (title) {
	      titleheight = lineWidth * 5;
	  } else {
	      titleheight = 0;
	  }

	  Get_bbox(bb);
	  if ((event->pt.x <= (bb[XMIN] + min_dist)) ||
	      (event->pt.y <= (bb[YMIN] + min_dist)) ||
	      (event->pt.x >= (bb[XMAX] - min_dist)) ||
	      (event->pt.y >= (bb[YMAX] - min_dist - titleheight)))
	    {
		rc = TRUE;
	    }
	  else
	    {
		rc = FALSE;
	    }
      }
    else
      { 
	  rc = event->pt.On_line(points, min_dist, TRUE);
      }

    return(rc);
}

//
// Find all the portals visible within this portal
//
void
Pad_Portal::Update_visibility(void)
{
    float bb[4];
    Pad_Portal *portal;
    Pad_Iterator pi;

    visiblePortals.Make_empty();

    // Go through all the portals on the surface
    // that this is looking at.
    if (lookon) {
	DOLIST(pi, lookon->viewPortals, Pad_Portal, portal) {
	    portal->Get_global_bbox(bb);
	    if ((portal != this) && (Pad_Is_overlapping(bb, viewBBox.Get()))) {
		visiblePortals.Push_last(portal);
	    }
	}
    }

    dirtyPortals = FALSE;
}

//
// Pass the event through the portal
// modifying it by the portal's position, view, and transformation.
// Event starts out being in portal's coordinate system, and this
// method converts it to be in the coordinate system of the portal's
// lookon.
//
void
Pad_Portal::Pass_through(Pad_Event *event)
{
    Pad_BBox bb;
    float bbxctr, bbyctr, junk;
    float scale;

    junk = 1.0;
    Get_global_bbox(bb);
    bbxctr = 0.5 * (bb.Xmin() + bb.Xmax());
    bbyctr = 0.5 * (bb.Ymin() + bb.Ymax());
    scale = zoom;
    Local_to_screen(scale);
    event->pt.x = xview + ((event->pt.x - bbxctr) / scale);
    event->pt.y = yview + ((event->pt.y - bbyctr) / scale);
    event->mag *= scale;
}

//
// Pass the specified bounding box through the portal, modifying it 
// by the portal's position, view, and transformation.
// Then clip the bounding box to the portals.
//
void
Pad_Portal::Pass_through(float *bb)
{
    float obj_bbox[4];
    Pad_Object *obj;

				// Transform by portal position and view
    Get_bbox(obj_bbox);
    bb[XMIN] = (bb[XMIN] * zoom) + (-xview * zoom) + obj_bbox[XMIN] + 0.5 * (obj_bbox[XMAX] - obj_bbox[XMIN]);
    bb[YMIN] = (bb[YMIN] * zoom) + (-yview * zoom) + obj_bbox[YMIN] + 0.5 * (obj_bbox[YMAX] - obj_bbox[YMIN]);
    bb[XMAX] = (bb[XMAX] * zoom) + (-xview * zoom) + obj_bbox[XMIN] + 0.5 * (obj_bbox[XMAX] - obj_bbox[XMIN]);
    bb[YMAX] = (bb[YMAX] * zoom) + (-yview * zoom) + obj_bbox[YMIN] + 0.5 * (obj_bbox[YMAX] - obj_bbox[YMIN]);

				// Transform by portal's transform
    obj = this;
    do {
	obj->transform.Apply(bb);
	obj = obj->group;
    } while (obj);

    Get_global_bbox(obj_bbox);
    Clip_bbox(bb, obj_bbox, bb);
}

//
// Return true to continue rendering other objects, or false if rendering
// was interrupted.
//
Pad_Bool
Pad_Portal::Render(void)
{
    return(Render((Pad_Bool)FALSE));
}

//
// This gets called whenever the system is slow, and the object
// should render itself extra fast - even if that makes it ugly.
//
Pad_Bool
Pad_Portal::Render_fast(void)
{
    Render(TRUE);

    return(TRUE);
}

Pad_Bool
Pad_Portal::Render(Pad_Bool fast)
{
    float borderWidth;
    Pad_Bool rc = TRUE;
    Pad_BBox bb;
    Pad_Point *pt0, *pt1;
    Pad_Point min, max;

    if (points.Length() < 2) {
	return(TRUE);
    }

				// If portal doesn't have a lookon,
				// then draw portal as a shadow.
    if (!lookon) {
	Pad_renderer->Set_transparency(0.25);
	Pad_renderer->Set_color(&Pad_Color::black);
	if (rectangle) {
	    pt0 = points.First();
	    pt1 = points.Last();
	    Pad_renderer->Draw_filled_box(pt0->x, pt0->y, pt1->x, pt1->y);
	} else {
	    Pad_renderer->Draw_polygon(points);
	}
	return(TRUE);
    }

    recursiveRenderLevel++;
    if (recursiveRenderLevel <= 1) // Don't allow recursive portal rendering
      {
	  float clipbb[4];
	  float half_lw;
	  Pad_BBox save_bbox;
	
	  half_lw = lineWidth * 0.5;
          Get_bbox(bb);

	  if (!fast) {
				// Clip to portal
	      if (rectangle) {
		  clipbb[XMIN] = bb.Xmin() + half_lw;
		  clipbb[XMAX] = bb.Xmax() - half_lw;
		  clipbb[YMIN] = bb.Ymin() + half_lw;
		  clipbb[YMAX] = bb.Ymax() - half_lw;
		  Pad_prc->win->activeRestorer->Push_clip(clipbb);
	      } else {
		  Pad_prc->win->activeRestorer->Push_clip(points);
	      }
				// Update the list of portals currently being rendered through
	      Pad_prc->views.Push_last(this);
	  }
	
				// Draw portal background
	  if (fillColor.Is_set()) {
	      Pad_renderer->Set_color(fillColor);
	      Pad_renderer->Draw_filled_box(bb.Xmin(), bb.Ymin(), bb.Xmax(), bb.Ymax());
	  }

	  if (!fast) {
	      save_bbox = Pad_prc->activeBBox;
	      Clip_view_bbox(Pad_prc->activeBBox.Get());

	      Pad_renderer->Push_view(this);    // Store this view
	      rc = lookon->view->pad->Render(); // Render the surface that the portal is looking at (its lookon)
	      Pad_renderer->Pop_view();         // Restore view stack
	
	      Pad_prc->activeBBox = save_bbox; // Restore the activeBBox to what it was before
	      Pad_prc->win->activeRestorer->Pop_clip();
	      Pad_prc->views.Remove(this);
	  }
	
				// Draw border
	  borderWidth = lineWidth;
	  if (border.Is_set()) {
	      Pad_renderer->Set_border(border);
	      if (rectangle) {
		  pt0 = points.First();
		  pt1 = points.Last();
		  min = pt0;
		  max = pt1;
		  Pad_renderer->Draw_3d_rectangle(min, max, borderWidth, relief);
	      } else {
		  Pad_renderer->Draw_3d_polygon(points, borderWidth, relief);
	      }
	  }
				// Render Title Bar
	  if (title && titleColor.Is_set() && border.Is_set() && rectangle)
	    {
		float titleheight;
		float title_xmin, title_ymin, title_xmax, title_ymax;

		pt0 = points.First();
		pt1 = points.Last();
		
		titleheight = lineWidth * 5;
		if (titleheight == 0) {
		    titleheight = 0.05 * (pt1->y - pt0->y);
		}
		borderWidth = borderWidth / 2;
		title_xmin = MAX(pt0->x, pt0->x + borderWidth);
		title_ymin = MAX(pt0->y + borderWidth, pt1->y - borderWidth - titleheight);
		title_xmax = MIN(pt1->x, pt1->x - borderWidth);
		title_ymax = MIN(pt1->y, pt1->y - borderWidth);
		min.Set(title_xmin, title_ymin);
		max.Set(title_xmax, title_ymax);
		
		Pad_renderer->Set_border(border);
		Pad_renderer->Draw_filled_3d_rectangle(min, max, borderWidth, relief);

		clipbb[XMIN] = pt0->x + lineWidth;
		clipbb[XMAX] = pt1->x - 2 * lineWidth;
		clipbb[YMIN] = pt0->y + lineWidth;
		clipbb[YMAX] = pt1->y - lineWidth;
		Pad_prc->win->activeRestorer->Push_clip(clipbb);
		Pad_renderer->Set_color(titleColor);
		
		Pad_renderer->Set_font(font);
		Pad_renderer->Set_font_height(titleheight * 0.5);
		Pad_renderer->Draw_string(title->Get(), 2,
					  pt0->x + 4 * borderWidth / 1.0,
					  pt1->y - titleheight + (0.5 * lineWidth), 0.0);
		Pad_prc->win->activeRestorer->Pop_clip();
	    }
      }
    recursiveRenderLevel--;
    return(rc);
}

//
// Present to suppress strange C++ warnings related to virtual classes with
// the same name and different signatures with derived classes.
//
// Return true to continue rendering other objects, or false if rendering
// was interrupted.
//
Pad_Bool
Pad_Portal::Render(Pad_Restorer *)
{
    cerr << "Portal::render Pad_Restorer - This should never be called" << endl;
    return(TRUE);
}

//
// Portals get clipped if there is a title
// (or if width or height is set).
//
Pad_Bool
Pad_Portal::Check_render_clipped(void)
{
    Pad_Bool rc;
    Clipping renderClipped = Get_clipping();

    switch (renderClipped) {
      case CLIPPING_FALSE:
	rc = FALSE;
	break;
      case CLIPPING_TRUE:
	rc = TRUE;
	break;
      default:
	if (title ||
	    (optionFlags & PAD_WIDTH_SET) ||
	    (optionFlags & PAD_HEIGHT_SET)) 
	  {
	      rc = TRUE;
	  } else {
	      rc = FALSE;
	  }
	break;
    }

    return(rc);
}

//
// This is the bounding box of the portal as an object sitting
// on the pad.  Note that this code is duplicated from line.
void
Pad_Portal::Compute_bounding_box()
{
    int i;
    float hw;
    float xmin, xmax, ymin, ymax;
    float miter_length;
    Pad_Point p1, p2, p3;
    Pad_Point *pt0, *pt1, *pti;

    if (points.Is_empty()) {
	Set_bbox(0, 0, 0, 0);
	return;
    }

				// Simple case for rectangular portals
    if (rectangle) {
	pt0 = points.First();
	pt1 = points.Last();
	if (!(optionFlags & PAD_WIDTH_SET)) {
	    Set_bbox_xmin(pt0->x);
	    Set_bbox_xmax(pt1->x);
	}
	if (!(optionFlags & PAD_HEIGHT_SET)) {
	    Set_bbox_ymin(pt0->y);
	    Set_bbox_ymax(pt1->y);
	}
    } else {
				// Non-rectangular portals are more complicated
				// because the miter length must be taken into
				// account.
	pt0 = points.First();
	xmin = xmax = pt0->x;
	ymin = ymax = pt0->y;

	if (points.Length() > 1) {
	    pt1 = points.Nth(1);
	    p1.x = pt0->x;
	    p1.y = pt0->y;
	    p2.x = pt1->x;
	    p2.y = pt1->y;
	    pti = points.Last();
	    xmin = MIN(xmin, pti->x);
	    ymin = MIN(ymin, pti->y);
	    xmax = MAX(xmax, pti->x);
	    ymax = MAX(ymax, pti->y);
	    for (i=1; (i<points.Length()-1); i++) {
		p3 = points.Nth(i+1);
		
		miter_length = Pad_Miter_length(lineWidth, p1.x, p1.y, p2.x, p2.y, p3.x, p3.y);
		
		pti = points.Nth(i);
		if (pti->x < xmin) xmin = pti->x - miter_length;
		if (pti->y < ymin) ymin = pti->y - miter_length;
		if (pti->x > xmax) xmax = pti->x + miter_length;
		if (pti->y > ymax) ymax = pti->y + miter_length;
		
		p1.x = p2.x;
		p1.y = p2.y;
		p2.x = p3.x;
		p2.y = p3.y;
	    }
	}
	hw = 0.6 * lineWidth;
	if (!(optionFlags & PAD_WIDTH_SET)) {
	    Set_bbox_xmin(xmin - hw);
	    Set_bbox_xmax(xmax + hw);
	}
	if (!(optionFlags & PAD_HEIGHT_SET)) {
	    Set_bbox_ymin(ymin - hw);
	    Set_bbox_ymax(ymax + hw);
	}
    }
    
    Pad_Object::Compute_bounding_box();

    win->view->Pad_View::Update_visibility();   // Update list of visible portals
}

//
// This is the bounding box of what the portal sees.
//
void
Pad_Portal::Compute_view_bounding_box(void)
{
    float bbwidth, bbheight;
    float izoom;
    Pad_BBox bb;

    Get_bbox(bb);
    bbwidth = bb.Width();
    bbheight = bb.Height();
    izoom = 1.0 / zoom;

    viewBBox.Set(xview - izoom * (0.5 * bbwidth), yview - izoom * (0.5 * bbheight),
		  xview + izoom * (0.5 * bbwidth), yview + izoom * (0.5 * bbheight));
}

//
// Compute the intersection of bbox1 and bbox2.
// Return the result in result_bbox.
//
void
Pad_Portal::Clip_bbox(float *result_bbox, float *bbox1, float *bbox2)
{
    result_bbox[XMIN] = MAX(bbox1[XMIN], bbox2[XMIN]);
    result_bbox[YMIN] = MAX(bbox1[YMIN], bbox2[YMIN]);
    result_bbox[XMAX] = MIN(bbox1[XMAX], bbox2[XMAX]);
    result_bbox[YMAX] = MIN(bbox1[YMAX], bbox2[YMAX]);
    if (result_bbox[XMAX] < result_bbox[XMIN]) {
	result_bbox[XMAX] = result_bbox[XMIN];
    }
    if (result_bbox[YMAX] < result_bbox[YMIN]) {
	result_bbox[YMAX] = result_bbox[YMIN];
    }
}

//
// Clip portal's view bounding box to <clip_bbox>,
// and return value in that <clip_bbox>.
//
void
Pad_Portal::Clip_view_bbox(float *clip_bbox)
{
    float x1, y1, x2, y2;
    float vx1, vy1, vx2, vy2;
    float xfac, yfac;
    float bb[4];

    Get_global_bbox(bb);
    x1 = bb[XMIN];
    y1 = bb[YMIN];
    x2 = bb[XMAX];
    y2 = bb[YMAX];

    vx1 = viewBBox.Xmin();
    vy1 = viewBBox.Ymin();
    vx2 = viewBBox.Xmax();
    vy2 = viewBBox.Ymax();

    if ((x1 != x2) && (y1 != y2)) {
				// Compute factor between referent's bbox and object's bbox.
	xfac = (vx2 - vx1) / (x2 - x1);
	yfac = (vy2 - vy1) / (y2 - y1);
	
				// Clip portal's bounding box with viewer's bbox
	if (x1 < clip_bbox[XMIN]) {
	    vx1 += xfac * (clip_bbox[XMIN] - x1);
	}
	if (y1 < clip_bbox[YMIN]) {
	    vy1 += yfac * (clip_bbox[YMIN] - y1);
	}
	if (x2 > clip_bbox[XMAX]) {
	    vx2 -= xfac * (x2 - clip_bbox[XMAX]);
	}
	if (y2 > clip_bbox[YMAX]) {
	    vy2 -= yfac * (y2 - clip_bbox[YMAX]);
	}
    }

    clip_bbox[XMIN] = vx1;
    clip_bbox[YMIN] = vy1;
    clip_bbox[XMAX] = vx2;
    clip_bbox[YMAX] = vy2;
}

//
// Rotate the portal
//
// Rotate recieves the rotation angle (theta), the center of rotation,
// and a flag telling weather to use object coordinates or global.
// If the last flag is TRUE object coords are used, if FALSE, global coords.
// Rotate calls a utility function in misc.C, called Pad_Rotate, 
// to do the work of transforming the positions of the objects coordinates.
// theta is expected to be in degrees
// 
Pad_Bool
Pad_Portal::Is_rotatable(void)
{
    return(TRUE);
}

Pad_Bool
Pad_Portal::Rotate(float dtheta)
{
				// center point must be in global coordinates,
				// so convert anchorpt from local*transform to global
    Pad_Point center;
    center = anchorpt;
    transform.Invert(center);
    Local_to_screen(center);

    return(Rotate(dtheta, center));
}

Pad_Bool
Pad_Portal::Rotate(float dtheta, Pad_Point &center)
{
    int i;
    float     curAngle;
    Pad_PList pts, polyPts;
    Pad_Point pt;

    curAngle = Get_angle();

    _Set_angle_data(curAngle + dtheta);
    curAngle = Get_angle();
    _Set_angle_data(fmod(curAngle, 360.0f));
    optionFlags |= PAD_ANGLE_SET; // Set bit specifying that angle has been set

    Get_coords(pts, FALSE);
    
    if (pts.Length() == 2) { // i.e. if rectangle make rectangular poly 
	pt.Set(pts.Nth(0)->x, pts.Nth(0)->y);
	polyPts.Push_last(&pt);
	pt.Set(pts.Nth(1)->x, pts.Nth(0)->y);
	polyPts.Push_last(&pt);
	pt.Set(pts.Nth(1)->x, pts.Nth(1)->y);
	polyPts.Push_last(&pt);
	pt.Set(pts.Nth(0)->x, pts.Nth(1)->y);
	polyPts.Push_last(&pt);

	pts.Make_empty();
	DOTIMES(i, polyPts.Length()) {
	    pts.Push_last(polyPts.Nth(i));
	}
    }

    Pad_Rotate(pts, dtheta, center);

    Set_coords(pts, FALSE, FALSE);

    _Set_angle_data(curAngle + dtheta);
   
    return(TRUE);
}
