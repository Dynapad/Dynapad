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
#include "view.h"
#include "portal.h"
#include "object.h"
#include "line.h"
#include "win.h"
#include "text.h"
#include "restorer.h"
#include "renderer.h"
#include "callback.h"
#include "global.h"
#include "display.h"

#include <math.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/types.h>

#  include <unistd.h>


#define VIEW_DEFAULT_VISIBLELAYERS "all"

/*
The Pad_View specifies the view onto the pad, i.e., what portion of
the pad dataspace is being looked at.  This view is specified by the
variables, xview, yview, and zoom.

(xview, yview) specifies the centered point on the pad
zoom specifies the scale - always around xview, yview.
*/

Pad_View::~Pad_View()
{
    layers = 0;
    if (visibleLayers) {
	delete [] visibleLayers;
	visibleLayers = NULL;
    }
    if (invisibleLayers) {
	delete [] invisibleLayers;
	invisibleLayers = NULL;
    }
}

//
// Use this one to create a new top-level window
//
Pad_View::Pad_View(Pad_Win *newWin) :
Pad_Object()
{
    win = newWin;
    win->view = this;		// Must initialize here because making a new pad uses them
    layers = 0;
    visibleLayers = NULL;	// Must initialize here because making a new pad uses them
    invisibleLayers = NULL;
    pad = newWin->Create_pad();
    Init();
}

//
// Use this one to create a portal (derived from Pad_View)
//
Pad_View::Pad_View(Pad *p) :
Pad_Object(p)
{
    pad = p;
    win = p->view->win;
    layers = 0;
    visibleLayers = NULL;
    invisibleLayers = NULL;
    Init();
}

void
Pad_View::Init(void)
{
    _type = PAD_VIEW;
    viewFlags = PAD_NO_MASK;
    xview = 0.0;
    yview = 0.0;
    zoom = 1.0;
    renderTime = 0;
    Update_layers();
    Set_visiblelayers_default();
    dirtyPortals = FALSE;
    Compute_view_bounding_box();
}

//
// A Pad_View always looks onto its window
//
Pad_Win *
Pad_View::Get_lookon(void)
{
    return(win);
}

//
// Invert_view methods apply the inverse view's transformation to parameters.
// So, if the view has a magnification of 2, invoking Invert_view()
// to an item will make it half as big.
//
void 
Pad_View::Invert_view(Pad_Object *obj)
{
    obj->anchorpt.x = xview + (obj->anchorpt.x / zoom);
    obj->anchorpt.y = yview + (obj->anchorpt.y / zoom);
    obj->transform.Scale(1.0 / zoom);
    obj->Set_position_from_anchor();
}

void 
Pad_View::Invert_view(float &x, float &y, float &s)
{
    x = xview + (x / zoom);
    y = yview + (y / zoom);
    s /= zoom;
}

void 
Pad_View::Invert_view(float *bb)
{
    float s = 1.0;
    Invert_view(bb[XMIN], bb[YMIN], s);
    Invert_view(bb[XMAX], bb[YMAX], s);
}

void
Pad_View::Invert_view(Pad_BBox &bb)
{
    Invert_view(bb.Get());
}

//
// Apply_view methods apply the view's transformation to parameters.
// So, if the view has a magnification of 2, invoking Apply_view()
// to an item will make it twice as big.
//
void 
Pad_View::Apply_view(Pad_Object *obj)
{
    obj->anchorpt.x = (obj->anchorpt.x - xview) * zoom;
    obj->anchorpt.y = (obj->anchorpt.y - yview) * zoom;
    obj->transform.Scale(zoom);
    obj->Set_position_from_anchor();
}

void 
Pad_View::Apply_view(float &x, float &y, float &s)
{
    x = (x - xview) * zoom;
    y = (y - yview) * zoom;
    s *= zoom;
}

void 
Pad_View::Apply_view(float *bb)
{
    float s = 1.0;
    Apply_view(bb[XMIN], bb[YMIN], s);
    Apply_view(bb[XMAX], bb[YMAX], s);
}

//
// Set the layers this view sees and doesn't see.
// Syntax of layers is: "layer layer ... [-]layer ..."
// Only the layers specified are visible, unless the
// special layer "all" is specified, in which case, all
// layers are visible except layers preceeded with a '-'.
//
void 
Pad_View::Set_visiblelayers(char *layerstring)
{
    int i;
    char *token;
    Pad_Uid uid;
    Pad_String l;
    Pad_Bool all, neg;
    Pad_Layer *layer;

    if (!Get_lookon()) {
	return;
    }

    viewFlags |= VIEW_VISIBLELAYERS_SET;

    l = layerstring;		// Make a copy because we modify it while parsing it
				// Start by initializing layers to all off
    DOTIMES(i, layers) {
	visibleLayers[i] = FALSE;
	invisibleLayers[i] = FALSE;
    }

    all = FALSE;
    neg = FALSE;
    token = strtok(l.Get(), " ");
    while (token) {
				// This allows '-'s as either separate tokens,
				// or as the first character of another token.
	if (token[0] == '-') {
	    neg = TRUE;
	    token++;
	}
	
	if (token[0] != '\0') {
	    uid = Pad_GetUid(token);
	    if (uid == Pad_noneUid) {
				// No layers are visible
		DOTIMES(i, layers) {
		    visibleLayers[i] = FALSE;
		    invisibleLayers[i] = FALSE;
		}
		break;
	    }
	    layer = Get_lookon()->view->pad->Get_layer_from_name(uid);
	    if (!layer) {
				// Layer doesn't exist, so make it
		layer = Get_lookon()->view->pad->Create_layer(uid);
				// Creating a new layer can result in the
				// layer lists we are currently modifying to
				// be reallocated.  So, call this function
				// from scratch, and scrap the current effort.
		Set_visiblelayers(layerstring);
		return;
	    }
	    if (neg) {
		invisibleLayers[layer->id] = TRUE;
		visibleLayers[layer->id] = FALSE;
		neg = FALSE;
	    } else {
		if (uid == Pad_allUid) {
		    all = TRUE;
		} else {
		    visibleLayers[layer->id] = TRUE;
		    invisibleLayers[layer->id] = FALSE;
		}
	    }
	}
	token = strtok(NULL, " ");
    }

    if (all) {
	DOTIMES(i, layers) {
	    visibleLayers[i] = FALSE;
	}
	visibleLayers[0] = TRUE;
    }

				// Don't damage the view if it is just being created
    if (flags & PAD_MASK_CREATED) {
	Damage();
    }
}

void 
Pad_View::Set_visiblelayers_default(void)
{
    Set_visiblelayers(VIEW_DEFAULT_VISIBLELAYERS);
    viewFlags &= ~VIEW_VISIBLELAYERS_SET;
}

//
// Return layers this view sees and doesn't see in
// a format suitable for passing to Set_visiblelayers();
//
void
Pad_View::Get_visiblelayers(Pad_String &layerstring)
{
    int i;
    Pad_Bool first;
    Pad_Layer *layer;

    if (!Get_lookon()) {
	return;
    }

    layerstring = "";
    first = TRUE;
    DOTIMES(i, layers) {
	if (visibleLayers[i]) {
	    layer = Get_lookon()->view->pad->Get_layer_from_id(i);
	    if (first) {
		first = FALSE;
	    } else {
		layerstring += " ";
	    }
	    layerstring += layer->name;
	}
    }
    DOTIMES(i, layers) {
	if (invisibleLayers[i]) {
	    layer = Get_lookon()->view->pad->Get_layer_from_id(i);
	    if (first) {
		first = FALSE;
	    } else {
		layerstring += " ";
	    }
	    layerstring += "-";
	    layerstring += layer->name;
	}
    }
}

//
// Update the list of visible and invisible layers for this view.
// Check the current layers.  Some may have been added or deleted.
//
void
Pad_View::Update_layers(void)
{
    int i;
    int len;
    Pad_Bool *newVisibleLayers;
    Pad_Bool *newInvisibleLayers;
    Pad_Layer *layer;
    Pad_HashTableIterator hi;

    if (!Get_lookon()) {
	return;
    }

				// Determine max layer id
    len = 0;
    DOTABLE(hi, Get_lookon()->view->pad->layerNameTable, Pad_Layer, layer) {
	if (layer->id > len) {
	    len = layer->id;
	}
    }
    len++;			// Adjust for off-by-one

				// If layer arrays don't exist, then create them
    if (!visibleLayers) {
	layers = len;
	visibleLayers = new Pad_Bool[len];
	DOTIMES(i, len) {
	    visibleLayers[i] = FALSE;
	}
    }
    if (!invisibleLayers) {
	layers = len;
	invisibleLayers = new Pad_Bool[len];
	DOTIMES(i, len) {
	    invisibleLayers[i] = FALSE;
	}
    }

    if (len < layers) {
				// Some layers have been deleted
	newVisibleLayers = new Pad_Bool[len];
	DOTIMES(i, len) {
	    newVisibleLayers[i] = visibleLayers[i];
	}
	delete [] visibleLayers;
	visibleLayers = newVisibleLayers;

	newInvisibleLayers = new Pad_Bool[len];
	DOTIMES(i, len) {
	    newInvisibleLayers[i] = invisibleLayers[i];
	}
	delete [] invisibleLayers;
	invisibleLayers = newInvisibleLayers;

	layers = len;
    } else if (len > layers) {
				// Some layers have been added
	newVisibleLayers = new Pad_Bool[len];
	DOTIMES(i, layers) {
	    newVisibleLayers[i] = visibleLayers[i];
	}
	for (i=layers; i<len; i++) {
	    newVisibleLayers[i] = FALSE;
	}
	delete [] visibleLayers;
	visibleLayers = newVisibleLayers;

	newInvisibleLayers = new Pad_Bool[len];
	DOTIMES(i, layers) {
	    newInvisibleLayers[i] = invisibleLayers[i];
	}
	for (i=layers; i<len; i++) {
	    newInvisibleLayers[i] = FALSE;
	}
	delete [] invisibleLayers;
	invisibleLayers = newInvisibleLayers;

	layers = len;
    }
}

//
// Find all the portals visible (anywhere) looking onto this view
//
void 
Pad_View::Update_visibility(void)
{
    Pad_BBox bb;
    Pad_Portal *portal;
    Pad_Iterator pi;

    while ((portal = (Pad_Portal *)visiblePortals.Pop())) {
	portal->currentlyVisible = FALSE;
    }

				// Go through all the portals on this surface
    DOLIST(pi, win->viewPortals, Pad_Portal, portal) {
	portal->Get_global_bbox(bb);
	if (viewBBox.Overlaps(bb)) {
	    visiblePortals.Push_last(portal);
	    portal->currentlyVisible = TRUE;
	}
    }

    dirtyPortals = FALSE;
}

//
// Add <obj> to the list of sticky objects.
// Keep sticky object list in display-list order.
//
void
Pad_View::Add_sticky(Pad_Object *obj)
{
    obj->_Insert_forward(stickyObjects);
}

//
// Remove <obj> from the list of sticky objects.
//
void
Pad_View::Remove_sticky(Pad_Object *obj)
{
    stickyObjects.Remove(obj);
}

//
// Mark entire pad surface as changed, and register it
// for re-rendering.
//
void 
Pad_View::Damage(void)
{
    Pad_Portal *portal;
    Pad_Iterator pi;

    if (Pad_prc) {		// Damage can not occur within a render
	return;
    }

    if (win->biddingRestorer) {
        if (win->biddingRestorer->Is_full()) {
	    return;             // already marked whole pad as damaged
	}
	win->restorers.Remove((void*)win->biddingRestorer);
        delete win->biddingRestorer;
	win->biddingRestorer = NULL;
    }

    win->Cancel_restorers();

    win->biddingRestorer = new Pad_Restorer(win, TRUE);
    win->biddingRestorer->Schedule(RESTORE_WHEN_IDLE, NULL);

    DOLIST(pi, win->lookonPortals, Pad_Portal, portal) {
	portal->Damage();
    }
}

//
// Force all damaged areas to be re-rendered immediately.
// Refinements do not occur.
//
void
Pad_View::Update_display(void)
{
    Update_display(0, FALSE);
}

//
// Force all damaged areas to be re-rendered immediately.
// The dissolve speed can be specified (0 is a swap buffer,
// 3 is the slowest dissolve).
// All refinements also are rendered now.
//
void
Pad_View::Update_display(int dissolveSpeed, Pad_Bool withRefinements)
{
    Pad_Restorer *restorer;
    Pad_List *copyOfRestorers;
    int oldDissolveSpeed;

    oldDissolveSpeed = win->dissolveSpeed;

    win->dissolveSpeed = dissolveSpeed;

    copyOfRestorers = new Pad_List(win->restorers);

    while ((restorer = (Pad_Restorer *)copyOfRestorers->Pop())) {
	if (withRefinements || restorer->Is_idler()) {
	    restorer->Do_restore();
	}
    }

    win->dissolveSpeed = oldDissolveSpeed;
    delete copyOfRestorers;
}

//
// This is just here so portals can override it with their own render method.
//
// Return true to continue rendering other objects, or false if rendering
// was interrupted.
//
Pad_Bool
Pad_View::Render(void)
{
    cerr << "Pad_View::render: This should never get called" << endl;
    return(TRUE);
}

//
// Return true to continue rendering other objects, or false if rendering
// was interrupted.
//
Pad_Bool
Pad_View::Render(Pad_Restorer *restorer)
{
    Pad_Bool rc = TRUE;
    Pad_Bool refine;
    Pad_Restorer *refiner = NULL;
    Pad_Time time;
    long     startSec, startUsec;
    
				// Get the current time
    time.Update();
    startSec = time.Get_sec();
    startUsec = time.Get_usec();

    Pad_renderer = win->renderer;

				// Set up the render context
    Pad_prc = new Pad_RenderContext(win);
    Pad_prc->views.Push(this);
    win->activeRestorer = restorer;
    refiner = new Pad_Restorer(win, FALSE);
    win->refiningRestorer = refiner;

				// Set up the renderer
    Pad_renderer->Set_device(Pad_prc);

				// Initalize these slots here so rendering within portals 
				// gets included.
    pad->objectsRendered = 0;

    if (win->id)
      {
	  Pad_renderer->Init_transform_stack();

	  restorer->Initialize_clip_region();
    
				// Fill the background of the area being painted

	  if (win->tile1) {
	      // JM's experimental tiling facility:
	      // set the renderer's tile

	      float a;
	      if (zoom < win->tilemin) {
		  a = 0;
	      } else if (zoom > win->tilemax) {
		  a = 1;
	      } else {
		  a = (zoom - win->tilemin) / (win->tilemax - win->tilemin);
	      }
	      Pad_renderer->Set_tile(win->tile1, win->tile2, win->tiletmp,
				     a, win->tilex, win->tiley);
	  } else {
	      Pad_renderer->Set_color(win->background);
	  }
	  Pad_renderer->Draw_filled_box(0, 0, win->width, win->height);

	  if (win->tile1) {
	      Pad_renderer->Set_tile(NULL, NULL, NULL, 0, 0, 0);
	  }
    
				// Set the activeBBox to be this view's bounding box
	  Pad_prc->activeBBox = viewBBox;

				// Store current view for future transformations
	  Pad_renderer->Push_view(this);

	  pad->level = restorer->refinementLevel;

				// Call rendering methods
	  Pad_Callback *renderScript = pad->Get_renderscript();
	  if (renderScript) {
	      rc = pad->Fire_render_script(refine);
	  } else if (!pad->Render()) {
	      rc = FALSE;
	  }
	  Pad_renderer->Pop_view();	// Restore view stack

	  if (restorer->flush_all) {
				// Update the whole window
	      restorer->Unset_gc_clip(win->fgGC);
	      restorer->Unset_gc_clip(win->copyGC);
	  }
	  
	  Pad_renderer->Swap_buffers(pad->level == 0 ? win->dissolveSpeed
				 : win->refineDissolveSpeed);

				// Debug aid - show stipple over 
				// restorers' region
	  if (win->debugRegion && !restorer->Is_full()) {
	      Pad_renderer->Draw_clip_region(restorer);
	  }	

				// Turn off clipping
	  restorer->Finalize_clip_region();

				// Finished the render. Tidy up
	  if (restorer->Is_full()) {
	      win->highestRenderLevel = restorer->refinementLevel;
	  } else {
	      if (restorer->refinementLevel > win->highestRenderLevel) {
		  win->highestRenderLevel = restorer->refinementLevel;
	      }
	  }
      }
    
				// See if there is refinement needed
    if (refiner) {
	if (!rc || refiner->Is_empty()) {
				// Nothing needed refinement (or render interrupted)
	    delete refiner;
	} else {
				// Merge this refiner with an existing one if it is at the right
				// refinement level.
	    if (!Pad_prc->win->restorers.Is_empty()) {
		Pad_Iterator ri;
		Pad_Restorer *existing;
		DOLIST(ri, win->restorers, Pad_Restorer, existing) {
		    if (existing->Is_refiner() && (existing->refinementLevel == pad->level+1)) 
		      {
			  *existing += *refiner;
			  delete refiner;
			  refiner = NULL;
				// Since the existing refiner got appended with something current,
				// reschedule it to delay the refinement.
			  existing->Reschedule();
			  break;
		      }
		}
		
	    }
				// Otherwise schedule a new refinement
	    if (refiner) {	// something needs refinement		
		refiner->Schedule(RESTORE_REFINE, restorer);
	    }
	}
	refiner = NULL;
    }
    
    win->refiningRestorer = NULL;
    win->activeRestorer = NULL;     // This restorer has done its work
    Pad_prc->activeBBox.Set_zero();
				      // Reset render context
    if (Pad_prc) {
	delete Pad_prc;
	Pad_prc = NULL;
    }
    
				// Record total render time
    time.Update();
    renderTime = ((time.Get_sec() - startSec) * 1000) + ((time.Get_usec() - startUsec) / 1000);

    return(rc);
}

//
// Since Pad_View's aren't really objects, there is no
// notion of an "object" bounding box, thus Compute_bounding_box()
// doesn't do anything.
//
void 
Pad_View::Compute_bounding_box()
{
}

//
// This computes the bounding box visible within this view
//
void
Pad_View::Compute_view_bounding_box()
{
    viewBBox.Set(- 0.5 * win->width, - 0.5 * win->height,
		    0.5 * win->width,   0.5 * win->height);
    Invert_view(viewBBox);
    if (Type() == PAD_VIEW) {
	pad->globalBBox = viewBBox;
    }

    Pad_Object::Compute_bounding_box();
}

//
// Change the view so as to center to specification.  The thing to be centered may be
// either a single object, a list of objects, or a bounding box.  The view centering is
// animated over a period of <animationTime> milliseconds.  If <twostep> is TRUE,
// then the view will first be zoomed out so that the source and destination points
// are both visible - if appropriate.  <x> and <y> specify where on the view the bounding box
// is centered.  (0.5, 0.5) means centered.  (0, 0) means the lower left corner of the bbox
// is at the bottom left of the screen.  (1.0, 1.0) means the upper right corner of the bbox
// is at the top right of the screen.  <z> specifies how large the object should be.  
// 1.0 means it should fill the screen (in whatever dimension gets filled first).
// <view> specifies the view that should be centered within.  If it is a portal, then
// the view within that portal is updated.  If it is NULL, the top-level view is updated.
//

void
Pad_View::Center(Pad_Object *obj, int animationTime, Pad_Bool twostep,
		 float x, float y, float z, Pad_View *view)
{
    Pad_BBox bb;

    if (obj->Type() != PAD_PAD) {
        obj->Get_global_bbox(bb);
	Center(bb.Xmin(), bb.Ymin(), bb.Xmax(), bb.Ymax(), animationTime, twostep, x, y, z, view);
    }
}

void
Pad_View::Center(Pad_List &objs, int animationTime, Pad_Bool twostep,
		 float x, float y, float z, Pad_View *view)
{
    Pad_Iterator oi;
    Pad_BBox bb, tbb;
    Pad_Object *obj;

    obj = (Pad_Object *)objs.Pop();
    if (obj) {
        if (obj->Type() != PAD_PAD) {
	    obj->Get_global_bbox(bb);
	}
	DOLIST(oi, objs, Pad_Object, obj) {
	    if (obj->Type() != PAD_PAD) {
	        obj->Get_global_bbox(tbb);
		bb.Union(tbb);
	    }
	}

	Center(bb.Xmin(), bb.Ymin(), bb.Xmax(), bb.Ymax(), animationTime, twostep, x, y, z, view);
    }
}

void
Pad_View::Center(float x1, float y1, float x2, float y2, int animationTime, Pad_Bool twostep,
		 float x, float y, float z, Pad_View *view)
{
    float newx, newy, newz;
    float winWidth, winHeight;
    float bboxWidth, bboxHeight;
    Pad_BBox bb;

				// If no view is specified, then center objects within this view
    if (view == NULL) {
        view = this;
    }

    if (view->Type() == PAD_VIEW) {
	winWidth =  view->win->width;
	winHeight = view->win->height;
    } else {			// Else, it is a portal
	view->Get_bbox(bb);
	winWidth =  bb.Width();
	winHeight = bb.Height();
    }
    bboxWidth = x2 - x1;
    bboxHeight = y2 - y1;

    if ((bboxWidth > 0) && (bboxHeight > 0)) {
	newz = z * MIN(winWidth / bboxWidth, winHeight / bboxHeight);
	if (newz > 0) {
	    newx = winWidth * ((0.5 - x) / newz) + LERP(x, x1, x2);
	    newy = winHeight * ((0.5 - y) / newz) + LERP(y, y1, y2);
	
	    view->Animate_to(newx, newy, newz, animationTime, twostep);
	}
    }
}

//
// Animate to specified view in fixed time specified in 'animationTime'
// in milliseconds.  Use as many frames as possible to get there in time.
//
// Use slow-in, slow-out animation based on Pad_Siso_lerp().
//
// Seems easier than it is because:
//
//  * zoom is exponential.  so, if zoom is changed linearly, it will
//    appear to change faster when zoomed out, and slow down as you zoom in.
//    => fix this by doing an expontial lerp in z.
//  * Math is weird in zooming space.  Must convert to scale-space diagrams.
//    See Scale-Space Diagrams paper in CHI'95 (Furnas & Bederson).
//

//
// Helper function.
// Computes midpoint between current view and specified endpoint, such that
// the zoom will make start and endpoints visible.  Return true if using this
// midpoint is appropriate, and false if not (i.e., points too close).
//
static Pad_Bool
_Compute_midpoint(Pad_View *view, float x2, float y2, float z2, float &midx, float &midy, float &midz)
{
    Pad_Bool rc = FALSE;
    Pad_BBox bb;
    float x1, y1, z1;
    float winWidth, winHeight;
    float minWindowPixels;
    float dx, dy, dist;
    float totalPixels;

    x1 = view->xview;
    y1 = view->yview;
    z1 = view->zoom;

    dx = x2 - x1;
    dy = y2 - y1;
    dist = sqrt(dx*dx + dy*dy);

    totalPixels = dist * z1;
    if (view->Type() == PAD_VIEW) {
	winWidth =  view->win->width;
	winHeight = view->win->height;
    } else {
	view->Get_global_bbox(bb);
	winWidth =  bb.Width();
	winHeight = bb.Height();
    }
    minWindowPixels = MIN(winWidth, winHeight);

    if ((dist * MIN(z1, z2)) > (1.5 * minWindowPixels))
      {
	  midx = 0.5 * (x1 + x2);
	  midy = 0.5 * (y1 + y2);
	  midz = z1 * minWindowPixels / totalPixels;
	  rc = TRUE;
      }

    return(rc);
}

Pad_Bool
Pad_View::Animate_to(float newXview, float newYview, float newZoom, int animationTime, Pad_Bool twostep) 
{
    int  rc;
    int  lerpu;
    float ux, uy, v;
    float u1x, v1x, u2x, v2x;
    float u1y, v1y, u2y, v2y;
    float logV1, logV2;
    float x, y, z;
    float lerp;
    float straightLerp;
    float elapsedTime;
    long  startSec;
    long  startUsec;
    Pad_Time time;

    if (newZoom <= 0.0) {
	Pad_errorString = "View must have zoom > 0";
	return(FALSE);
    }
    
    if (animationTime > 0)
      {
				// Check if twostep is requested.  If so, determine if the animation
				// is far and get the midpoint.  Then first animate to the midpoint in
				// half the requested time, and then continue on to animate to the
				// requested viewpoint.
	  if (twostep) {
	      float midx, midy, midz;
		
	      rc = _Compute_midpoint(this, newXview, newYview, newZoom, midx, midy, midz);
	      if (rc) {
	          animationTime /= 2;
		  Animate_to(midx, midy, midz, animationTime, FALSE);
	      }
	  }
	
	  u1x = xview * zoom;
	  v1x = zoom;
	  u2x = newXview * newZoom;
	  v2x = newZoom;
	  
	  u1y = yview * zoom;
	  v1y = zoom;
	  u2y = newYview * newZoom;
	  v2y = newZoom;
	  
	  logV1 = log(v1x) / log(2.0);
	  logV2 = log(v2x) / log(2.0);
	  
				// Record current time
	  time.Update();
	  startSec = time.Get_sec();
	  startUsec = time.Get_usec();

	  lerp = 0.0;
	  
	  if (((v1x / v2x) < 1.1) &&
	      ((v1x / v2x) > 0.9)) {
	      lerpu = 1;
	  } else {
	      lerpu = 0;
	  }
	  
	  do
	    {
		// lerp v exponentially
		v = pow(2.0f, LERP(lerp, logV1, logV2));
		// Compute u as a function of v
		// unless v is constant, then lerp along u
		if (lerpu)
		  {
		      ux = LERP(lerp, u1x, u2x);
		      uy = LERP(lerp, u1y, u2y);
		  }
		else 
		  {
		      ux = u1x + (v - v1x) * ((u2x - u1x) / (v2x - v1x));
		      uy = u1y + (v - v1y) * ((u2y - u1y) / (v2y - v1y));
		  }
		
				// Convert (u,v) to (x,y)
		x = ux / v;
		y = uy / v;
		z = v;

				// Change view to new position
		Set_view(x, y, z, TRUE);

				// Calculate total elapsed time
		time.Update();
		elapsedTime = (1000000 * (time.Get_sec() - startSec) + (time.Get_usec() - startUsec)) / 1000;

				// Calculate next interpolation step
		straightLerp = elapsedTime / animationTime;
		lerp = Pad_Siso_lerp(straightLerp);
		
				// Check for interruption
		rc = Pad_renderer->Is_interrupted(win);
	    } while ((elapsedTime < animationTime) && !rc);
      }
      Set_view(newXview, newYview, newZoom, FALSE);

    return(TRUE);
}

//
// Change the view to the specified coordinates.
// Compute the new view's bounding box.
// Update each object's view list
// If it's a pan, do a smart pan.
//   (Shift over the image, render the missing strip, 
//    and trigger a repair.  The repair is needed because a shift
//    of the image is not guaranteed to be identical to a re-render
//    because of rounding).
// Else, render the whole world
// Don't allow zooming in or out too much.
//
Pad_Bool
Pad_View::Set_view(float xv, float yv, float zm, Pad_Bool render_now)
{
    int result;
    float prevXview = xview;
    float prevYview = yview;
    float prev_zoom = zoom;
    Pad_Iterator oi;
    Pad_Object *obj;
    Pad_Bool prevDamageEnabled;
    Pad_BBox prevBBox;

    if (zm <= 0.0) {
	Pad_errorString = "View can not have zoom <= 0.0";
	return(FALSE);
    }

				// Don't allow zooming in or out too far
    if ((win->maxZoom == 0.0) ||
	((zm < win->maxZoom) &&
	 (zm > (1.0 / win->maxZoom))))
      {
	  zoom = zm;
      }
    xview = xv;
    yview = yv;

    dirtyPortals = TRUE;	// Visible portal list now out-of-date

				// Execute viewScript call-backs
    Pad_Callback *viewScript;
    DOLIST(oi, viewScriptObjects, Pad_Object, obj) {
	prevDamageEnabled = obj->pad->view->win->damageEnabled;
	obj->pad->view->win->damageEnabled = FALSE;
	if ((viewScript = obj->Get_viewscript())) {
	    Pad_ObjectHandle *handle = new Pad_ObjectHandle(obj);
	    result = viewScript->Eval();
				// Be careful, this object could have been deleted by script
	    if (handle->Get_object() == NULL) {
	        delete handle;
		Pad_errorString = "Object referenced by view callback has been deleted";
		return(FALSE);
	    }
	    delete handle;

	    if (result == PAD_ERROR) {
		Pad_Background_error("Pad view callback");
	    }
	}
	obj->pad->view->win->damageEnabled = prevDamageEnabled;
    }

				// Update bounding boxes
    prevBBox = viewBBox;
    Compute_bounding_box();
    Compute_view_bounding_box();
    
    if (win->fastPan
	&& (win->doubleBuffer)
	&& (zoom == prev_zoom)
	&& (win->Is_mapped())
	&& (Type() != PAD_PORTAL)
	&& Pad_Is_overlapping(viewBBox.Get(), prevBBox.Get()))
      {
	  int dx, dy;
	  Pad_Iterator ri;
	  Pad_Restorer *restorer;
	  Pad_Restorer *repairer;
	  Pad_Object *obj;
	  
	  dx = (int)((xview - prevXview) * zoom);
	  dy = (int)((yview - prevYview) * zoom);
	  
	  xview = prevXview + (dx / zoom);
	  yview = prevYview + (dy / zoom);

				// Shift the image by copying the entire area
	  int srcx, srcy, dstx, dsty;
	  if (dx >= 0) {
	      srcx = dx;
	      dstx = 0;
	  } else {
	      srcx = 0;
	      dstx = -dx;
	  }
	  if (dy >= 0) {
	      srcy = 0;
	      dsty = dy;
	  } else {
	      srcy = - dy;
	      dsty = 0;
	  }
	  XCopyArea(win->dpy->display, win->dbl, win->dbl, win->copyGC, 
		    srcx, srcy,
		    win->width - ABS(dx), 
		    win->height - ABS(dy),
		    dstx, dsty);
	  
	  win->biddingRestorer = NULL; // Close any existing bidding
	  
				// Damage objects with view callbacks before pan
				// (They might modify themselves)
	  DOLIST(oi, viewScriptObjects, Pad_Object, obj) {
	      obj->Damage();
	  }

	  win->tilex -= dx;
	  win->tiley += dy;

				// Pan all the restorers
	  DOLIST(ri, win->restorers, Pad_Restorer, restorer) {
	      restorer->Translate(-dx, dy);
	  }
	  
				// Update position of sticky objects
	  DOLIST(oi, stickyObjects, Pad_Object, obj) {
	      obj->Transform_sticky();
	  }

				// Damage objects with view callbacks after pan
	  DOLIST(oi, viewScriptObjects, Pad_Object, obj) {
	      obj->Damage();
	  }
	  
	  if (win->biddingRestorer) {
	      restorer = win->biddingRestorer;
	  } else { 
	      restorer = new Pad_Restorer(win, FALSE);
	  }

				// Update horizontal and vertical strips
	  XRectangle rect;
	  if (dx != 0) {
	      if (dx > 0) {
		  rect.x = win->width - dx;
		  rect.width = dx;
	      } else {
		  rect.x = 0;
		  rect.width = -dx;
	      }
	      rect.y = 0; 
	      rect.height = win->height;
	      *restorer += rect;
	  }
	  if (dy != 0) {
	      if (dy < 0) {
		  rect.y = win->height + dy;
		  rect.height = - dy;
	      } else {
		  rect.y = 0;
		  rect.height = dy;
	      }
	      rect.x = 0; rect.width = win->width;
	      *restorer += rect;
	  }
				        // Force the restorer to run now 
					// and to update the whole window
	  restorer->flush_all = TRUE;	// makes sure whole window gets updated
	  restorer->Schedule(RESTORE_NOW, NULL);
	  
				        // We now need to schedule a refinement on the area
					// of the screen we copied. This is required because 
					// floating point rounding may be different at the
					// new pan coordinate.
	  repairer = NULL;
	  DOLIST(ri, win->restorers, Pad_Restorer, restorer) {
	      if (restorer->Is_repairer()) {
		  repairer = restorer;
	      }
	  }
	  if (!repairer) {
	      repairer = new Pad_Restorer(win, FALSE);
	  }
	  win->windowRestorer->Translate(-dx, dy);
	  *repairer += *win->windowRestorer;
	  win->windowRestorer->Translate(dx, -dy);

				// Repair delay starts from now.
	  repairer->Schedule(RESTORE_REPAIR, NULL); 
      } else {
				// Update position of sticky objects
	  DOLIST(oi, stickyObjects, Pad_Object, obj) {
	      obj->Transform_sticky();
	  }
	  
	  if (render_now) {
	      if (Type() == PAD_PORTAL) {
		  Damage();
		  Update_display();
	      } else {
				// Force main render immediately
		  Pad_Restorer *r;
		  
		  win->Cancel_restorers();
		  r = new Pad_Restorer(win, TRUE);
		  r->refinementLevel = win->defaultRenderLevel;
		  r->Schedule(RESTORE_NOW, NULL);
	      }
	  } else {
	      Damage();
	  }
      }

    Generate_event(Pad_ModifyNotify, NULL, "view");

    return(TRUE);
}

//
// Return the current view
//
void
Pad_View::Get_view(float &currentXview, float &currentYview, float &currentZoom)
{
    currentXview = xview;
    currentYview = yview;
    currentZoom = zoom;
}


//
// Zoom view around specified point by multiplicitive factor,
// and take <animationTime> milliseconds to do it (animating along the way).
//
void 
Pad_View::Zoom_around(float x, float y, float zmult, int animationTime)
{
    float xinc, yinc;
    float newXview, newYview, newZoom;
    Pad_Point old_dist, new_dist;
    
    newZoom = zoom * zmult;
    
    old_dist.x = zoom * (x - xview);
    old_dist.y = zoom * (y - yview);
    
    new_dist.x = zmult * old_dist.x;
    new_dist.y = zmult * old_dist.y;
    
    xinc = (new_dist.x - old_dist.x) / newZoom;
    yinc = (new_dist.y - old_dist.y) / newZoom;
    
    newXview = xview + xinc;
    newYview = yview + yinc;

    if (animationTime == 0) {
	Set_view(newXview, newYview, newZoom, FALSE);
    } else {
	Animate_to(newXview, newYview, newZoom, animationTime);
    }
}

void 
Pad_View::Print_debugging_info(void) 
{
    Pad_Iterator pi;
    Pad_Portal *portal;
    
    cout << endl;
    cout << "    Pad Debug Info" << endl;
    cout << "    --------------" << endl;
    cout << "Pad view at (" << xview << ", " << yview << ") zoom = " << zoom << endl;
    cout << "Objects: " << pad->Sizeof_tree() << endl;
    if (Pad_focus) {
	cout << "Edit object: " << Pad_focus << endl;
    }
    cout << "Portals looking at ";
    cout << win->Get_name() << ": ";
    DOLIST(pi, win->lookonPortals, Pad_Portal, portal) {
	cout << portal->id << " ";
    }
    cout << endl;
    cout << endl;
}

void 
Pad_View::Update_focus(Pad_Object *obj) 
{
    if (Pad_focus != obj) {
	if (Pad_focus) {
	    Pad_focus->Unset_focus();
	}
    
	Pad_focus = obj;
	if (Pad_focus) {
	    Pad_focus->Set_focus();
	}
    }
}
