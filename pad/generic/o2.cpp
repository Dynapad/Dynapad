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
#include "image.h"
#include "view.h"
#include "line.h"
#include "text.h"
#include "group.h"

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <errno.h>

#  include <unistd.h>

//
// Object has changed, so update it.  Note that this function should be used carefully.
// This function should be called when some internal state of an object has changed, and
// the system needs to be updated with the modified object.  In particular, the bounding box
// of the object must *not* be changed before this function is called.  Instead, some part
// of the object (like pen width) may have changed.  Calling this function will compute
// the new bounding box, and will make sure the screen is updated properly.  In particular,
// this function:
//
//    Damage area of original object
//    Compute the new bounding box
//    Keeping the same anchor point, compute the new transformation so anchor doesn't move
//    Damage area of updated object
//
void
Pad_Object::Update(void)
{
    Damage();
    Compute_bounding_box();
    Set_position_from_anchor();
    Damage();
}

//
// Object has changed, so update it.  However, in this case, we want to keep the same
// transformation, so the anchor is changed to reflect a new bounding box with the same
// transformation.
//    Damage area of original object
//    Compute the new bounding box
//    Keeping the same transformation point, compute the new anchor point
//    Damage area of updated object
//
void
Pad_Object::Update_and_compute_anchor(void)
{
    Damage();
    Compute_bounding_box();
    Set_anchor_from_position();
    Damage();
}

//
// Compute anchor from bounding box and transformation
// so that object won't move.
//
void
Pad_Object::Set_anchor_from_position(void)
{
    float x1, y1, x2, y2;
    float xctr, yctr;
    float bb[4];

    Get_bbox(bb);
    transform.Apply(bb);
    x1 = bb[XMIN];
    y1 = bb[YMIN];
    x2 = bb[XMAX];
    y2 = bb[YMAX];
    xctr = 0.5 * (x1 + x2);
    yctr = 0.5 * (y1 + y2);
    
    switch (anchor) 
      {
	case PAD_ANCHOR_N:
	  anchorpt.x = xctr;
	  anchorpt.y = y2;
	  break;
	case PAD_ANCHOR_NE:
	  anchorpt.x = x2;
	  anchorpt.y = y2;
	  break;
	case PAD_ANCHOR_E:
	  anchorpt.x = x2;
	  anchorpt.y = yctr;
	  break;
	case PAD_ANCHOR_SE:
	  anchorpt.x = x2;
	  anchorpt.y = y1;
	  break;
	case PAD_ANCHOR_S:
	  anchorpt.x = xctr;
	  anchorpt.y = y1;
	  break;
	case PAD_ANCHOR_SW:
	  anchorpt.x = x1;
	  anchorpt.y = y1;
	  break;
	case PAD_ANCHOR_W:
	  anchorpt.x = x1;
	  anchorpt.y = yctr;
	  break;
	case PAD_ANCHOR_NW:
	  anchorpt.x = x1;
	  anchorpt.y = y2;
	  break;
	case PAD_ANCHOR_CENTER:
	  anchorpt.x = xctr;
	  anchorpt.y = yctr;
	  break;
      }
			     // Update requested position of sticky objects
    if (Get_sticky() != PAD_STICKY_NONE) {
	Update_sticky();
    }
}

//
// Update the object's transformation from the object's anchor point, 
// anchor type (north, center, etc.), and bounding box.
//
void 
Pad_Object::Set_position_from_anchor(void)
{
    Pad_BBox bb;
    float x1, y1, x2, y2;
    float width, height;
    float newXoffset, newYoffset;
    float scale;

    Get_bbox(bb);
    scale = transform.Get_scale();
    x1 = scale * bb.Xmin();
    y1 = scale * bb.Ymin();
    x2 = scale * bb.Xmax();
    y2 = scale * bb.Ymax();
    width = x2 - x1;
    height = y2 - y1;
    
				// First, position object so the nw corner of its
				// bounding box is at the anchor point.  This is
				// necessary because the anchor point could be any place
				// relative to the bounding box.
    newXoffset = anchorpt.x - x1;
    newYoffset = anchorpt.y - y2;
    
				// Now, position bounding box as specified.
    switch (anchor) 
      {
	case PAD_ANCHOR_N:
	  newXoffset -= 0.5 * width;
	  break;
	case PAD_ANCHOR_NE:
	  newXoffset -= width;
	  break;
	case PAD_ANCHOR_E:
	  newXoffset -= width;
	  newYoffset += 0.5 * height;
	  break;
	case PAD_ANCHOR_SE:
	  newXoffset -= width;
	  newYoffset += height;
	  break;
	case PAD_ANCHOR_S:
	  newXoffset -= 0.5 * width;
	  newYoffset += height;
	  break;
	case PAD_ANCHOR_SW:
	  newYoffset += height;
	  break;
	case PAD_ANCHOR_W:
	  newYoffset += 0.5 * height;
	  break;
	case PAD_ANCHOR_NW:
	  break;
	case PAD_ANCHOR_CENTER:
	  newXoffset -= 0.5 * width;
	  newYoffset += 0.5 * height;
	  break;
      }
    transform.Set_offset(newXoffset, newYoffset);
    Update_global_bounding_box();
				// If item is a member of a group, then 
				// make sure group's bounding box is updated.
    if (group) {
	group->Compute_bounding_box();
    }
}

//
// Transform the specified local coordinate to global coordinates
// based on this object, as defined by the hierarchy of groups that
// this object is a member of.
//
void 
Pad_Object::Local_to_screen(float &x, float &y, float &s)
{
    Pad_Object *obj = this;

    do {
	obj->transform.Apply(x, y, s);
	obj = obj->group;
    } while (obj);
}

void 
Pad_Object::Local_to_screen(float *bb)
{
    float s = 1.0;
    Local_to_screen(bb[XMIN], bb[YMIN], s);
    Local_to_screen(bb[XMAX], bb[YMAX], s);
}

void 
Pad_Object::Local_to_screen(Pad_Point &pt)
{
    float s = 1.0;
    Local_to_screen(pt.x, pt.y, s);
}

void 
Pad_Object::Local_to_screen(float &s)
{
    Pad_Object *obj = this;

    do {
	obj->transform.Apply(s);
	obj = obj->group;
    } while (obj);
}

void 
Pad_Object::Local_to_screen(Pad_Event &event)
{
    Pad_Object *obj = this;

    do {
	obj->transform.Apply(&event);
	obj = obj->group;
    } while (obj);
}

//
// Transform the specified global coordinate to local coordinates
// based on this object, as defined by the hierarchy of groups that
// this object is a member of.
//
void 
Pad_Object::Screen_to_local(float &x, float &y, float &s)
{
    Pad_Object *obj = this;
    Pad_List objs;		// List of objs and groups to process starting at top group

				// First build list of things to transform by
    do {
	objs.Push(obj);
	obj = obj->group;
    } while (obj);

				// Then go through list top-down
    while ((obj = (Pad_Object *)objs.Pop())) {
	obj->transform.Invert(x, y, s);
    }
}

void 
Pad_Object::Screen_to_local(float *bb)
{
    float s = 1.0;
    Screen_to_local(bb[XMIN], bb[YMIN], s);
    Screen_to_local(bb[XMAX], bb[YMAX], s);
}

void 
Pad_Object::Screen_to_local(Pad_Point &pt)
{
    float s = 1.0;
    Screen_to_local(pt.x, pt.y, s);
}

void 
Pad_Object::Screen_to_local(float &s)
{
    Pad_Object *obj = this;
    Pad_List objs;		// List of objs and groups to process starting at top group

				// First build list of things to transform by
    do {
	objs.Push(obj);
	obj = obj->group;
    } while (obj);

				// Then go through list top-down
    while ((obj = (Pad_Object *)objs.Pop())) {
	obj->transform.Invert(s);
    }
}

void 
Pad_Object::Screen_to_local(Pad_Event &event)
{
    Pad_Object *obj = this;
    Pad_List objs;		// List of objs and groups to process starting at top group

				// First build list of things to transform by
    do {
	objs.Push(obj);
	obj = obj->group;
    } while (obj);

				// Then go through list top-down
    while ((obj = (Pad_Object *)objs.Pop())) {
	obj->transform.Invert(&event);
    }
}

//
// Insert <this> into <elements> so that <elements>
// is sorted in drawing order.  Don't insert <this>
// if it already is on the list.
//
void
Pad_Object::_Insert_forward(Pad_List &elements)
{
    Pad_Bool found;
    Pad_Object *obj;
    Pad_Iterator oi;

		// Very frequently, items are inserted
		// that have just been created, so first
		// check if this belongs at the end of the list.
		// If not, then insert normally.
    obj = (Pad_Object *)elements.Last();
    if (obj && (drawingOrder > obj->drawingOrder)) {
	elements.Push_last(this);
	return;
    }

    found = FALSE;
    DOLIST(oi, elements, Pad_Object, obj) {
	if (this == obj) {
				// Already on list.
	    return;
	}
	if (obj->drawingOrder > drawingOrder) {
	    elements.Insert_before(this, obj);
	    found = TRUE;
	    break;
	}
    }
    if (!found) {
	elements.Push_last(this);
    }
}

//
// Insert <this> into <elements> so that <elements>
// is sorted in reverse drawing order.  Don't insert <this>
// if it is already on the list.
//
void
Pad_Object::_Insert_reverse(Pad_List &elements)
{
    Pad_Bool found;
    Pad_Object *obj;
    Pad_Iterator oi;

    found = FALSE;
    DOLIST(oi, elements, Pad_Object, obj) {
	if (this == obj) {
	    return;		// Already on list - return without doing anything
	}
	if (obj->drawingOrder <= drawingOrder) {
	    elements.Insert_before(this, obj);
	    found = TRUE;
	    break;
	}
    }
    if (!found) {
	elements.Push_last(this);
    }
}

//
// Move this to the last position in the drawing order
// within the layer
//
Pad_Bool
Pad_Object::Raise(void) 
{
    Pad_Bool rc = TRUE;
    Pad_Object *l;
    Pad_Layer *layer;
				// Find element to raise in front of
    if (group) {
				// If group member, then only allow raises within group
	rc = group->Raise_member(this);
	return(rc);
    }

    layer = pad->Get_layer_from_id(layerId);
    if (layer->last == this) {
	return(rc);
    }

    Remove_from_drawing_order();

    if (layer->last) {
        l = layer->last;
    } else {
	l = pad->last;
    }

    Add_to_drawing_order(l);

    Damage();
    return(rc);
}

//
// Move this to just after <obj> in the drawing order
// within the layer
//
Pad_Bool
Pad_Object::Raise(Pad_Object *obj) 
{
    Pad_Bool rc = TRUE;
    Pad_Layer *layer;

    if (!obj) {
	rc = Raise();
	return(rc);
    }
				// Check if already done
    if ((prev == obj) || (this == obj)) {
	return(rc);
    }

				// If group member, then only allow raises within group
    if (group) {
	rc = group->Raise_member(this, obj);
	return(rc);
    }
    if (obj->group) {
				// If raising above group member, then raise above
				// containing group
	while (obj->group) {
	    obj = obj->group;
	}
    }
    
    layer = pad->Get_layer_from_id(layerId);
    if (layer->objects == 1) {
				// Only item on layer
	return(TRUE);
    }

    Remove_from_drawing_order();

    if (obj->drawingOrder > layer->last->drawingOrder) {
	obj = layer->last;
    }
    if (obj->drawingOrder < layer->first->drawingOrder) {
	obj = layer->first->prev;
    }
    Add_to_drawing_order(obj);

    Damage();
    return(rc);
}

//
// Move this to first position in drawing order
// within the layer
//
Pad_Bool
Pad_Object::Lower(void) 
{
    Pad_Bool rc = TRUE;
    Pad_Object *l;
    Pad_Layer *layer;

				// Find element to lower behind
    if (group) {
				// If group member, then only allow lowers within group
	rc = group->Lower_member(this);
	return(rc);
    }

    layer = pad->Get_layer_from_id(layerId);
    if (layer->first == this) {
				// Already at bottom of layer
	return(TRUE);
    }

    Remove_from_drawing_order();

    if (layer->first) {
	l = layer->first->prev;
    } else {
	l = NULL;
    }

    Add_to_drawing_order(l);

    Damage();
    return(rc);
}

//
// Move this to just before <obj> in drawing order
// within the layer
//
Pad_Bool
Pad_Object::Lower(Pad_Object *obj) 
{
    Pad_Bool rc = TRUE;
    Pad_Layer *layer;
    Pad_Object *l;

    if (!obj) {
	rc = Lower();
	return(rc);
    }
				// Check if already done
    if ((next == obj) || (this == obj)) {
	return(TRUE);
    }

    if (group) {
				// If group member, then only allow lowers within group
	rc = group->Lower_member(this, obj);
	return(rc);
    }
    if (obj->group) {
				// If lowering below group member, then lower below
				// containing group
	while (obj->group) {
	    obj = obj->group;
	}
    }

    l = obj->prev;
				// If trying to lower behind first object,
				// just lower to bottom.
    if (!l) {
	rc = Lower();
	return(rc);
    }

    layer = pad->Get_layer_from_id(layerId);
    if (layer->objects == 1) {
				// Only item on layer
	return(TRUE);
    }

    Remove_from_drawing_order();

    if (l->drawingOrder > layer->last->drawingOrder) {
	l = layer->last;
    }
    if (l->drawingOrder < layer->first->drawingOrder) {
	l = layer->first->prev;
    }
    Add_to_drawing_order(l);

    Damage();
    return(rc);
}

//
// Insert <this> into the drawing order just after <prev_obj>.
// If <prev_obj> is NULL, then this is the first item.
// This updates prev, next, neighbors' prev and next, and
// pad->first and pad->last.  And, compute drawingOrder.
// Also updates layer first and last pointers.
//
// Note: It is up to the caller to insert objects in
//       proper layer order.  
//
void
Pad_Object::Add_to_drawing_order(Pad_Object *prev_obj)
{
    Pad_Layer *layer;

				// Update four next and prev pointers
				// and pad->first and pad->last
    if (prev_obj == NULL) {
				// This is the first item
	if (pad->first == NULL) {
				// This is the only object.
	    prev = NULL;
	    next = NULL;
	    pad->last = this;
	} else {
				// This will be first object in the list.
	    prev = NULL;
	    next = pad->first;
	    next->prev = this;
	}
	pad->first = this;
    } else {
				// Else, this is not the first item
	next = prev_obj->next;
	prev = prev_obj;
	if (prev_obj->next) {
	    prev_obj->next->prev = this;
	} else {
	    pad->last = this;
	}
	prev_obj->next = this;
    }
				// Compute this object's drawing order
    if (prev_obj) {
	drawingOrder = prev_obj->Next_drawing_order();
    } else {
	if (next) {
	    drawingOrder = next->drawingOrder - 10;
	}
    }
				// Propogate drawing order to next items in list
    Propogate_drawing_order();

				// Update layer first and last pointers
    layer = pad->Get_layer_from_id(layerId);
    if (layer->first == NULL) {
				// First item on layer
	layer->first = this;
	layer->last = this;
    } else {
	if (layer->first == next) {
	    layer->first = this;
	}
	if (layer->last == prev) {
	    layer->last = this;
	}
    }
}

//
// Remove from drawing order This updates prev, next, 
// neighbors' prev and next, and pad->first, and pad->last.
// Layer first and last pointers are also updated.
//
void
Pad_Object::Remove_from_drawing_order(void)
{
    Pad_Layer *layer;

				// First update layer pointers
    layer = pad->Get_layer_from_id(layerId);
    if (layer->first == this) {
	if (layer->last == this) {
				// Only item in layer
	    layer->first = NULL;
	    layer->last = NULL;
	} else {
	    layer->first = next;
	}
    }
    if (layer->last == this) {
	layer->last = prev;
    }

				// Then update pad pointers
    if (pad->first == this) {
	pad->first = next;
    }
    if (pad->last == this) {
	pad->last = prev;
    }

				// Then update next/prev pointers
    if (prev) {
	prev->next = next;
    }
    if (next) {
	next->prev = prev;
    }

    next = NULL;
    prev = NULL;
}

//
// Return the next available drawing order number after
// this object's.  (Note: this may be overridden.)
//
int
Pad_Object::Next_drawing_order(void)
{
    int num;
    
    if (pad->last == this) {
	num = drawingOrder + 10;
    } else {
	num = drawingOrder + 1;
    }

    return(num);
}

//
// Make sure that every object further on in the display list
// has a greater drawing number than this.
//
void
Pad_Object::Propogate_drawing_order(void)
{
    Pad_Object *this_obj;
    Pad_Object *next_obj;

    this_obj = this;
    next_obj = pad->Next(this);
    while (next_obj && (next_obj->drawingOrder <= this_obj->drawingOrder)) {
	next_obj->drawingOrder = this_obj->drawingOrder + 1;
	this_obj = next_obj;
	next_obj = pad->Next(next_obj);
    }
}
