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
Groups are implemented by making Pad_Objects members of a Pad_Group object.  
Each group object has a transformation that hierarchically affects all of 
its members.  Note that the bounding boxes of members are not updated to 
reflect their transformed position.  Thus, bounding boxes of objects must 
be transformed by their groups.

Events go to group members, but they can't be found with the 'find' command.
*/

#include "defs.h"   // for win32
#include "object.h"
#include "group.h"
#include "pad.h"
#include "misc.h"
#include "win.h"
#include "global.h"
#include "renderer.h"
#include "menu.h"

#include <stdlib.h>

#define GROUP_DEFAULT_DIVISIBLE 1
#define GROUP_DEFAULT_MEMBERS   NULL

//////////////////////////////////////////////
//              Group definitions
//////////////////////////////////////////////  

Pad_Group::Pad_Group(Pad *pad) :
Pad_Component(pad)
{
    _type = PAD_GROUP;
    groupFlags = PAD_NO_MASK;

    // clear the bbox (Pad_Component sets it to a 10x10 min size which
    // is not suitable for groups and grids).
    transform.Set(0, 0, 1);
    Reshape(0, 0, 0, 0);   
    _componentFlags &= ~COMPONENT_RESHAPE_SET;    // Reset because Reshape sets it

    Set_divisible_default();
    Set_anchor(PAD_ANCHOR_CENTER);
}

Pad_Group::~Pad_Group()
{
    Pad_Object *obj;

    Generate_delete();

    while ((obj = (Pad_Object *)members.First())) {
	delete obj;
    }
}

//
// Gets called if there is an error configuring an object during creation.
// This gets called just before object is deleted, giving the object an
// opportunity to clean up if necessary.
//
void
Pad_Group::Create_obj_error(void)
{
    Make_empty(TRUE);		// Remove all group members and return them to the pad surface
}

Pad_Bool 
Pad_Group::Is_group(void)
{
    return(TRUE); // Most groups are groups
}

//
// Setters and getters for divisible
//
void
Pad_Group::Set_divisible(Pad_Bool new_divisible)
{
    groupFlags |= GROUP_DIVISIBLE_SET;
    divisible = new_divisible;
}

void
Pad_Group::Set_divisible_default(void)
{
    Set_divisible(GROUP_DEFAULT_DIVISIBLE);
    groupFlags &= ~GROUP_DIVISIBLE_SET;
}

Pad_Bool 
Pad_Group::Get_divisible(void)
{
    return(divisible); 
}

//
// Setters and getters for members
//
void
Pad_Group::Set_members(Pad_List *new_members)
{
    Pad_Object *obj;
    Pad_Iterator oi;
	  
    Make_empty(TRUE);
    if (new_members) {
	DOLIST(oi, *new_members, Pad_Object, obj) {
	    Add(obj, TRUE);
	}
    }

    groupFlags |= GROUP_MEMBERS_SET;
}

void
Pad_Group::Set_members_default(void)
{
    Set_members(GROUP_DEFAULT_MEMBERS);
    groupFlags &= ~GROUP_MEMBERS_SET;
}

void
Pad_Group::Get_members(Pad_List &list)
{
    Pad_Object *obj;
    Pad_Iterator oi;

    DOLIST(oi, members, Pad_Object, obj) {
	list.Push(obj);
    }
}

//
// Set the width of a group.
// Recursively sets the width of all the group members.
//
Pad_Bool
Pad_Group::Set_width(float width)
{
    float scale;
    float x, y, s;
    float x1, y1, x2, y2;
    float xctr, yctr;
    Pad_BBox bb;
    Pad_Object *obj;
    Pad_Iterator oi;

    Get_global_bbox(bb);
    x1 = bb.Xmin();
    y1 = bb.Ymin();
    x2 = bb.Xmax();
    y2 = bb.Ymax();
    xctr = bb.Xctr();
    yctr = bb.Yctr();
    scale = width / Get_width();
    DOLIST(oi, members, Pad_Object, obj) {
	obj->Get_abs_position(x, y, s);
	obj->Set_width(obj->Get_width() * scale);

	switch (anchor) {
	case PAD_ANCHOR_NE:
	case PAD_ANCHOR_E:
	case PAD_ANCHOR_SE:
	    x = x2 - scale * (x2 - x);
	    break;
	case PAD_ANCHOR_SW:
	case PAD_ANCHOR_W:
	case PAD_ANCHOR_NW:
	    x = x1 + scale * (x - x1);
	    break;
	case PAD_ANCHOR_N:
	case PAD_ANCHOR_S:
	case PAD_ANCHOR_CENTER:
	    x = xctr + scale * (x - xctr);
	    break;
	}
	obj->Set_abs_position(x, y, s);
    }

    return(TRUE);
}

//
// Set the height of a group.
// Recursively sets the height of all the group members.
//
Pad_Bool
Pad_Group::Set_height(float height)
{
    float scale;
    float x, y, s;
    float x1, y1, x2, y2;
    float xctr, yctr;
    Pad_BBox bb;
    Pad_Object *obj;
    Pad_Iterator oi;

    Get_global_bbox(bb);
    x1 = bb.Xmin();
    y1 = bb.Ymin();
    x2 = bb.Xmax();
    y2 = bb.Ymax();
    xctr = bb.Xctr();
    yctr = bb.Yctr();
    scale = height / Get_height();
    DOLIST(oi, members, Pad_Object, obj) {
	obj->Get_abs_position(x, y, s);
	obj->Set_height(obj->Get_height() * scale);

	switch (anchor) {
	case PAD_ANCHOR_S:
	case PAD_ANCHOR_SE:
	case PAD_ANCHOR_SW:
	    y = y1 + scale * (y - y1);
	    break;
	case PAD_ANCHOR_N:
	case PAD_ANCHOR_NE:
	case PAD_ANCHOR_NW:
	    y = y2 - scale * (y2 - y);
	    break;
	case PAD_ANCHOR_E:
	case PAD_ANCHOR_W:
	case PAD_ANCHOR_CENTER:
	    y = yctr + scale * (y - yctr);
	    break;
	}
	obj->Set_abs_position(x, y, s);
    }

    return(TRUE);
}

//
// Groups aren't clipped, but derived objects might be.
//
Pad_Bool
Pad_Group::Check_render_clipped(void)
{
    return(Pad_Component::Check_render_clipped());
}

//
// Remove all objects from this group (and return them to the pad surface).
//
void 
Pad_Group::Make_empty(Pad_Bool transform)
{
    Pad_Object *obj;

    while ((obj = (Pad_Object *)members.First())) {
	Remove(obj, transform);
    }
}

//
// True if group object should be picked if non of its members are.
//
Pad_Bool
Pad_Group::Pick_group(Pad_Event *)
{
    return(FALSE);
}

//
// Add <this> and all the members to the specified layer.
// 
void
Pad_Group::Add_to_layer(char *name)
{
    Pad_Object *obj;
    Pad_Iterator oi;
    Pad_List copyOfMembers;
    Pad_Layer *layer;
    Pad_Uid nameUid;

				// If already on the same layer, don't do anything
    layer = pad->Get_layer_from_id(layerId);
    nameUid = Pad_GetUid(name);
    if (layer->name == nameUid) {
	return;
    }

    copyOfMembers = members;

				// First remove all members from group
    DOLIST(oi, copyOfMembers, Pad_Object, obj) {
	Remove(obj, FALSE);
    }
				// Change group to new layer
    Pad_Component::Add_to_layer(name);

				// Then add members back (which will put them on the new layer)
    copyOfMembers.Reverse();
    DOLIST(oi, copyOfMembers, Pad_Object, obj) {
	Add(obj, FALSE);
    }
}

//
// Add obj to group (at end of display list)
// If <transform>, then transform object so it doesn't move.
// This method is providing for overloading by subclasses of Pad_Group
// in order to do sub-class specific functions for Add
//
//
Pad_Bool
Pad_Group::Add(Pad_Object *obj, Pad_Bool transform)
{
    Pad_Bool rc;

    rc = _Add(obj, transform);       // Add the object
    return(rc);
}

Pad_Bool
Pad_Group::_Add(Pad_Object *obj, Pad_Bool transform)
{
    Pad_Object *prev_obj, *g;
    Pad_Layer *layer;
    Pad_List groups;
    Pad_Iterator oi;

				// Make sure obj isn't a descendant of this.
    g = obj;
    while (g) {
	if (g == this) {
	    Pad_errorString.Printf("Can't add %d to %d because %d is a descendant of %d\n",
				   obj->id, id, obj->id, id);
	    return(FALSE);
	}
	g = g->group;
    }

    obj->Damage();		// Object might move, and thus must be damaged

				// If already member of another group, remove it from that one.
    if (obj->group) {
	obj->group->Remove(obj, transform);
    }
				// Make sure it is on the same layer as this group
    if (obj->layerId != layerId) {
	layer = pad->Get_layer_from_id(layerId);
	obj->Add_to_layer(layer->name);
    }

				// Remove from tree
    obj->Remove_from_drawing_order();

				// Add to end of group
				// Update display list, and drawing order
    obj->group = this;
    prev_obj = (Pad_Object *)members.First();
    members.Push(obj);		// Members in reverse display list order
    obj->next = NULL;
    obj->prev = prev_obj;
    if (prev_obj) {
	prev_obj->next = obj;
	obj->drawingOrder = prev_obj->Next_drawing_order();
    } else {
	obj->drawingOrder = drawingOrder + 1;
    }
    obj->Propogate_drawing_order();
    
    if (transform) {
		// Transform object so it doesn't move
	g = this;
	while (g) {
	    groups.Push(g);
	    g = g->group;
	}
	DOLIST(oi, groups, Pad_Object, g) {
	    g->transform.Invert(obj);
	}
    }

    Update_and_compute_anchor();

				// If group has fixed dimensions, then damage object because
				// it might have gotten clipped as a result of getting added
				// to this group.
    if ((optionFlags & PAD_WIDTH_SET) || (optionFlags & PAD_HEIGHT_SET)) {
	obj->Damage();
    }

                               // For menus, add their menuitems panel too
    if ((obj->Type() == PAD_MENU || obj->Type() == PAD_CHOICEMENU) &&
	this->Type() != PAD_MENUBAR) {
        Add(((Pad_Menu*)obj)->_itemsPanel, transform);
    }

    return(TRUE);
}

//
// Remove obj from group (and return it to the regular pad space).
// If <transform>, then transform object so it doesn't move.
// This method is providing for overloading by subclasses of Pad_Group
// in order to do sub-class specific functions for Remove
//
Pad_Bool
Pad_Group::Remove(Pad_Object *obj, Pad_Bool transform)
{
    Pad_Bool rc;

    rc = _Remove(obj,transform);
    return(rc);
}

Pad_Bool
Pad_Group::_Remove(Pad_Object *obj, Pad_Bool transform)
{
    Pad_Bool rc = TRUE;
    Pad_Object *g;
    Pad_Layer *layer;

    if (!members.Remove(obj)) {
	Pad_errorString.Printf("Can't remove %d from group id %d because it is not a member\n", obj->id, id);
	return(FALSE);
    }

    obj->Damage();		// Object might move, and thus must be damaged

    if (obj->next) {
	obj->next->prev = obj->prev;
    }
    if (obj->prev) {
	obj->prev->next = obj->next;
    }
    obj->group = NULL;
    obj->next = NULL;
    obj->prev = NULL;
				
				// Put object on layer which inserts it into the drawing order.
    if (obj->layerId == 0) {
	obj->Add_to_layer("main");
    } else {
	layer = pad->Get_layer_from_id(obj->layerId);
	obj->Add_to_layer(layer->name);
    }

				// Insert into display list just after group elements
				// Find top-most group to insert after.
    g = this;
    while (g->group) {
	g = g->group;
    }
    rc = obj->Raise(g);
    if (!rc) return(rc);
    
    if (transform) {
		// Transform object so it doesn't move
	g = this;
	while (g) {
	    g->transform.Apply(obj);
	    g = g->group;
	}
    }

    Update_and_compute_anchor();

				// If group has fixed dimensions, then damage object because
				// it might have been clipped as a result of being part of
				// this group.
    if ((optionFlags & PAD_WIDTH_SET) || (optionFlags & PAD_HEIGHT_SET)) {
	obj->Damage();
    }

    return(rc);
}

//
// Remove obj from group (and return it to the regular pad space).
//
Pad_Bool
Pad_Group::Delete(Pad_Object *obj)
{
    Pad_Bool rc = TRUE;

    if (!members.Remove(obj)) {
	Pad_errorString.Printf("Can't delete %d from group id %d because it is not a member\n", obj->id, id);
	return(FALSE);
    }

    obj->group = NULL;
    obj->Remove_from_drawing_order();
    Update_and_compute_anchor();

    return(rc);
}

//
// Move this to last position in drawing order.
//
Pad_Bool
Pad_Group::Raise_member(Pad_Object *obj) 
{
    Pad_Object *l;
				// Find element to raise in front of
    l = (Pad_Object *)members.First();
    if (l == obj) {		// Already at the top
	return(TRUE);
    }

    obj->Remove_from_drawing_order();
    members.Remove(obj);

    Add_to_drawing_order(obj, l);
    obj->_Insert_reverse(members);

    obj->Damage();

    return(TRUE);
}

//
// Move this to just after <obj> in drawing order.
//
Pad_Bool
Pad_Group::Raise_member(Pad_Object *obj, Pad_Object *ref) 
{
    Pad_Bool rc = TRUE;

    if (this != ref->group) {
	Pad_errorString = "Can't raise across group boundaries\n";
	return(FALSE);
    }
				// Check if already done
    if (obj->prev == ref) {
	return(rc);
    }
    
    obj->Remove_from_drawing_order();
    members.Remove(obj);

    Add_to_drawing_order(obj, ref);
    obj->_Insert_reverse(members);

    obj->Damage();

    return(rc);
}

//
// Move this to first position in drawing order.
//
Pad_Bool
Pad_Group::Lower_member(Pad_Object *obj) 
{
    members.Remove(obj);
    obj->Remove_from_drawing_order();

    Add_to_drawing_order(obj, NULL);
    obj->_Insert_reverse(members);

    Damage();
    return(TRUE);
}

//
// Move <obj> to just before <ref> in drawing order.
//
Pad_Bool
Pad_Group::Lower_member(Pad_Object *obj, Pad_Object *ref) 
{
    Pad_Bool rc = TRUE;

    if (this != ref->group) {
	Pad_errorString = "Can't lower across group boundaries\n";
	return(FALSE);
    }

    obj->Remove_from_drawing_order();
    members.Remove(obj);

    Add_to_drawing_order(obj, ref->prev);
    obj->_Insert_reverse(members);

    obj->Damage();
    return(rc);
}

//
// Return the next available drawing order number after
// this object's.  Since this object is a group, return
// a number after all it's children
//
int
Pad_Group::Next_drawing_order(void)
{
    int num;
    Pad_Object *child, *obj;
    
				// Children are in reverse display-list
				// order, so get the first one.
    child = (Pad_Object *)members.First();
    if (child) {
	obj = child;
    } else {
	obj = this;
    }
    if (pad->last == this) {
	num = obj->drawingOrder + 10;
    } else {
	num = obj->drawingOrder + 1;
    }

    return(num);
}

//
// Insert <obj> into the drawing order just after <prev_obj>.
// If <prev_obj> is NULL, then this is the first item.
// (i.e., update prev, next, neighbors' prev and next.
// And, compute drawing order.
//
// Note: Group members are never pointed to by the pad first/last
//       pointers, so those are not updated here.
//
// Note: Group members are always on the same layer as the group,
//       so there is no chance that the group first/last pointers
//       will be affected by this operation.  Thus, they are not updated.
//
void
Pad_Group::Add_to_drawing_order(Pad_Object *obj, Pad_Object *prev_obj)
{
    Pad_Object *first;

				// Update four next and prev pointers
    if (prev_obj == NULL) {
	obj->prev = NULL;
	first = (Pad_Object *)members.Last(); // Members in reverse display-list order.
	if (first == NULL) {
				// This is the only object.
	    obj->next = NULL;
	} else {
				// This will be first object in the list.
	    obj->next = first;
	    first->prev = obj;
	}
    } else {
	obj->next = prev_obj->next;
	obj->prev = prev_obj;
	if (prev_obj->next) {
	    prev_obj->next->prev = obj;
	}
	prev_obj->next = obj;
    }
				// Compute this object's drawing order
    if (prev_obj) {
	obj->drawingOrder = prev_obj->Next_drawing_order();
    } else {
	obj->drawingOrder = drawingOrder + 1;
    }
				// Propogate drawing order to next items in list
    obj->Propogate_drawing_order();
}

//
// Normally, when groups get the pointer, they don't
// care.  But some kinds of derived groups might.
//
void
Pad_Group::Pointer_in(Pad_Event *)
{
}

void
Pad_Group::Pointer_out(Pad_Event *)
{
}

//
// Return inset within group
// This is the area that items can get layed out within
//
Pad_Inset *
Pad_Group::Get_inset(void)
{
    return(&_inset);
}

//
// Render the group (by rendering all of its members).
// Since members are stored in reverse display-list order,
// Use the next pointer to traverse the list.
//
// Return true to continue rendering other objects, or false if rendering
// was interrupted.
//
Pad_Bool
Pad_Group::Render(void)
{
    Pad_Bool rc;
    Pad_Object *obj;

    obj = (Pad_Object *)members.Last();  	// Members in reverse display-list order
    rc = pad->Render_display_list(obj);

    return(rc);
}

//
// This gets called whenever the system is slow, and the object
// should render itself extra fast - even if that makes it ugly.
//
Pad_Bool
Pad_Group::Render_fast(void)
{
    float bb[4];

    Get_bbox(bb);
    Pad_renderer->Set_color(&Pad_Color::gray);
    Pad_renderer->Draw_filled_box(bb[XMIN], bb[YMIN], bb[XMAX],bb[YMAX]);

    return(TRUE);
}

//
// Set group's bbox to enclose members
//
void
Pad_Group::Compute_bounding_box(void) 
{
    Set_bounding_box_to_enclose_list(members);
				// Don't want item to move when members change,
				// so compute new anchor position so that item doesn't move.
    Set_anchor_from_position();

    Pad_Object::Compute_bounding_box();
}

//
// When the group's global bbox needs to be updated,
// so does all of its members
//
void
Pad_Group::Update_global_bounding_box(void)
{
    Pad_Object *obj;
    Pad_Iterator oi;

    Pad_Object::Update_global_bounding_box();

    DOLIST(oi, members, Pad_Object, obj) {
	obj->Update_global_bounding_box();
    }
}

//
// Determine if this group is rotatable by checking to
//  see if all of its members are rotatable
//

Pad_Bool
Pad_Group::Is_rotatable()
{
    Pad_Object *obj;
    Pad_Iterator oi;

    DOLIST(oi, members, Pad_Object, obj) {
	if ( !(obj->Is_rotatable()) ) {
	    Pad_errorString = "Group contains non-rotatable items";
	    return(FALSE);
	}
    }

    return(TRUE);
}

Pad_Bool
Pad_Group::Rotate(float dtheta)
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
Pad_Group::Rotate(float dtheta, Pad_Point &center)
{
    Pad_Object *obj;
    Pad_Iterator oi;
    float curAngle;

    curAngle = Get_angle();
    _Set_angle_data(curAngle + dtheta);
    optionFlags |= PAD_ANGLE_SET; // Set bit specifying that angle has been set

    if (Is_rotatable()) {
	DOLIST(oi, members, Pad_Object, obj) {
	    obj->Rotate(dtheta, center);
	}
	Compute_bounding_box();
    } else {
	return(FALSE);
    }

    return(TRUE);
}
