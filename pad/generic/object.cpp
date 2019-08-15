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
#include "win.h"
#include "restorer.h"
#include "events.h"
#include "portal.h"
#include "renderer.h"
#include "group.h"
#include "bind.h"
#include "alias.h"
#include "noisedata.h"
#include "misc.h"
#include "callback.h"
#include "tree.h"
#include "hashtab.h"
#include "trait.h"
#include "global.h"
#include "api.h"

#include <stdarg.h>
#include <stdlib.h>
#include <math.h>
#include <errno.h>
#  include <unistd.h>

#define OBJECT_DEFAULT_ALWAYSRENDER  0
#define OBJECT_DEFAULT_ANCHOR        PAD_ANCHOR_CENTER
#define OBJECT_DEFAULT_ANGLE         0
#define OBJECT_DEFAULT_CLIPPING      Pad_Object::CLIPPING_AUTO
#define OBJECT_DEFAULT_EVENTS        1
#define OBJECT_DEFAULT_FADERANGE     0.3
#define OBJECT_DEFAULT_INFO          NULL
#define OBJECT_DEFAULT_LAYER         "main"
#define OBJECT_DEFAULT_LOCK          0
#define OBJECT_DEFAULT_MAXSIZE       -1
#define OBJECT_DEFAULT_MINSIZE       0
#define OBJECT_DEFAULT_RENDERSCRIPT  NULL
#define OBJECT_DEFAULT_STICKY        PAD_STICKY_NONE
#define OBJECT_DEFAULT_TAGS          ""
#define OBJECT_DEFAULT_TRANSPARENCY  1.0
#define OBJECT_DEFAULT_VIEWSCRIPT    NULL
#define OBJECT_DEFAULT_TIMERSCRIPT   NULL
#define OBJECT_DEFAULT_TIMERRATE     0
#define OBJECT_DEFAULT_X             0.0
#define OBJECT_DEFAULT_Y             0.0
#define OBJECT_DEFAULT_Z             1.0

//////////////////////////////////////////////
//              Pad_Type Definitions
//////////////////////////////////////////////  

Pad_Type::Pad_Type()
{
    createScript = NULL;
}
		      
Pad_Type::~Pad_Type()
{
    if (createScript) {
	delete createScript;
	createScript = NULL;
    }
}
		      
//////////////////////////////////////////////
//              Pad_Option Definitions
//////////////////////////////////////////////  

Pad_Option::Pad_Option()
{
    optionScript = NULL;
    write = TRUE;		// Options should get written out by default
}
		      
Pad_Option::~Pad_Option()
{
    if (optionScript) {
	delete optionScript;
	optionScript = NULL;
    }
}
		      
//////////////////////////////////////////////
//              Pad_ZoomAction Definitions
//////////////////////////////////////////////  
		      
Pad_ZoomAction::Pad_ZoomAction() {
    growScript = NULL;
    shrinkScript = NULL;
    actionSize = 0.0;
    userActionSize = 0.0;
}

Pad_ZoomAction::~Pad_ZoomAction() {
    if (growScript) {
	delete growScript;
	growScript = NULL;
    }
    if (shrinkScript) {
	delete shrinkScript;
	shrinkScript = NULL;
    }
}

//////////////////////////////////////////////
// Pad_ObjectHandle definitions
// 
// These are a safe way to refer to an object
// that may get deleted.
//////////////////////////////////////////////  

Pad_ObjectHandle::Pad_ObjectHandle(Pad_Object *obj)
{
    if (obj) {
    	id =  obj->id;
	pad = obj->pad;
    } else {
    	id = 0;
	pad = NULL;
    }
}

Pad_Object *
Pad_ObjectHandle::Get_object(void)
{
    if (pad) {
        return(pad->Get_object_from_id(id));
    } else {
        return(NULL);
    }
}

//////////////////////////////////////////////
//              Pad_Object
//////////////////////////////////////////////  
		      
//
// Callback to delete an object
//
void 
Pad_Delete_callback(ClientData clientData)
{
    Pad_ObjectHandle *handle;
    Pad_Object *obj;
    
    handle = (Pad_ObjectHandle *)clientData;
    obj = handle->Get_object();
    delete handle;
    if (obj) {
	delete obj;
    }
}

//
// Main object destructor
//
Pad_Object::~Pad_Object()
{
    Pad_ZoomAction *zoomAction;
    Pad_ZoomActionSize *prevSize;
    static int deleting = 0;
    Pad_Win *win = NULL;

    Damage();
	
    deleting++;
    
    if (pad) {
	win = pad->view->win;
				// If member of a group, then remove self from group
	if (group) {
	    group->Delete(this);
	} else {
	    Remove_from_drawing_order();
	}
	
	if (_traitFlags & PAD_STICKY_TRAIT) {
	    pad->view->Remove_sticky(this);
	}
	
	Pad_DeleteAllBindings(win->bindingTable, this);
	Delete_all_tags();
	Remove_from_layer();
	
	Remove_all_aliases();
	
	pad->view->viewScriptObjects.Remove(this);
	
	Delete_id();
	    
	Pad_EventObjectDeleted(this);

	Pad_TreeNode *treenode = Get_treenode();
	if (treenode) {
	    Pad_TreeNode *treenode_ptr = treenode;   // Remember because it gets reset.
	    delete treenode;
	    Set_treenode(NULL);

				// Special case here.  If deleting treeroot, then clear
				// pad treeroot pointer.
	    if (treenode_ptr == pad->treeroot) {
		pad->treeroot = NULL;
	    }
	}
	Pad_TreeLayout *treelayout = Get_treelayout();
	if (treelayout) {
	    treelayout->Link_deleted();
	}
    }

				// Unset any handles using this object
    Pad_Handle *handle, *prevHandle;
    handle = _handle;
    while (handle) {
	handle->_obj = NULL;
	prevHandle = handle;
	handle = handle->_next;
	prevHandle->_next = NULL;
    }

    if (Has_focus()) {
	Pad_focus = NULL;
    }
    
    if (Pad_selection == this) {
	Pad_selection = NULL;
    }
    
    Pad_List *zoomActions = Get_zoomaction();
    if (zoomActions) {
	while ((zoomAction = (Pad_ZoomAction *)zoomActions->Pop())) {
	    delete zoomAction;
	}
    }

    Pad_List *prevSizes = Get_prevsizes();
    if (prevSizes) {
	while ((prevSize = (Pad_ZoomActionSize *)prevSizes->Pop())) {
	    delete prevSize;
	}
    }

    Pad_Iterator iter;
    Pad_Trait *trait;
    DOLIST(iter, _traitList, Pad_Trait, trait) {
       delete trait;
    }

    deleting--;
}

Pad_Object::Pad_Object()
{
    pad = NULL;
    flags = PAD_NO_MASK;
    Init();
    flags |= PAD_MASK_CREATED;
}

Pad_Object::Pad_Object(Pad *newPad)
{
    Pad_Layer *layer;

    pad = newPad;
    flags = PAD_NO_MASK;
    Init();
    Set_id();

				// Put object on layer which inserts it into the drawing order.
    if (layerId == 0) {
	Add_to_layer("main");
    } else {
	layer = pad->Get_layer_from_id(layerId);
	Add_to_layer(layer->name);
    }

    if (pad->idEventFirst) {
	flags |= PAD_MASK_ID_EVENT_FIRST;
    }
				// Generate a <Create> event unless manual <Create> event
				// generation has been requested.
    if ((newPad->padFlags & PADROOT_MANUAL_CREATE_EVENT) == 0) {
	Generate_event(Pad_CreateNotify, NULL);
    }
}

void
Pad_Object::Init(void)
{
    _type = PAD_OBJECT;
    optionFlags = PAD_NO_MASK;
    _traitFlags = PAD_NO_MASK;
    _handle = NULL;
    next = NULL;
    prev = NULL;
    group = NULL;
    Set_noisedata_default();
    drawingOrder = 0;
    layerId = 0;	// Don't set layer here.  It gets set later.
    Set_clipping_default();
    Set_renderscript_default();
    Set_viewscript_default();
    Set_zoomaction_default();
    id = 0;
    Set_info_default();
    Set_alwaysrender_default();
    Set_transparency_default();
    Set_minsize_default();
    Set_maxsize_default();
    Set_faderange_default();
    Set_events_default();
    Set_anchor_default();
    Set_lock_default();
    userdata = NULL;
    findable = TRUE; //default was FALSE; problems are possible?
    visible = FALSE;
    boundable = TRUE;
    viewable = FALSE;
}

//////////////////////////////////////////////
//              Pad_Object definitions
//////////////////////////////////////////////  

//
// Gets called with string args when object is created.  This routine must 
// process any arguments up to, but not including the options.  (argc, argv) 
// point to parameters on the creation line just after 'create <type>'.
// This returns the number of args processed.  If there is an error with the
// args, this should return -1, and set Pad_errorString to a descriptive error.
//
int
Pad_Object::Create_obj_args(int, char **)
{
    return(0);
}

//
// Gets called if there is an error configuring an object during creation.
// This gets called just before object is deleted, giving the object an
// opportunity to clean up if necessary.
//
void
Pad_Object::Create_obj_error(void)
{
}

//
// Base class for objects that can get initialized with a string
// (usually a filename).
//

int 
Pad_Object::Initialize(Pad_View &, char *) {
    return(1);
}

//
// Setter and getters for userType
//
Pad_String*
Pad_Object::Get_usertype(void)
{
    Pad_String *userType=NULL;
    _Get_trait(PAD_USERTYPE_TRAIT, userType);
    return userType;
}

Pad_Bool
Pad_Object::Set_usertype(Pad_String *type)
{
    return _Set_trait(PAD_USERTYPE_TRAIT, type);
}

//
// Setters and getters for treenode and treelayout
//

Pad_TreeNode*
Pad_Object::Get_treenode(void)
{
    Pad_TreeNode *treenode=NULL;
    Pad_TreeNodeTrait *trait = (Pad_TreeNodeTrait*)_Get_trait(PAD_TREENODE_TRAIT);
    if (trait) {
        trait->Get_value(treenode);
    }

    return treenode;
}

Pad_Bool
Pad_Object::Set_treenode(Pad_TreeNode *val)
{
    Pad_TreeNodeTrait *trait;
    Pad_Bool rc=TRUE;

    if (val) {
        trait = (Pad_TreeNodeTrait*)_Get_trait(PAD_TREENODE_TRAIT);
	if (trait) {
	    rc = trait->Set_value(val);
	} else {
	    trait = new Pad_TreeNodeTrait(PAD_TREENODE_TRAIT);
	    rc = trait->Set_value(val);
	    _traitList.Push(trait);
            _traitFlags |= PAD_TREENODE_TRAIT;
	}
    } else {
        rc = _Remove_trait(PAD_TREENODE_TRAIT);
    }

    return rc;
}

Pad_TreeLayout*
Pad_Object::Get_treelayout(void)
{
    Pad_TreeLayout *treelayout=NULL;
    Pad_TreeLayoutTrait *trait = (Pad_TreeLayoutTrait*)_Get_trait(PAD_TREELAYOUT_TRAIT);
    if (trait) {
        trait->Get_value(treelayout);
    }

    return treelayout;
}

Pad_Bool
Pad_Object::Set_treelayout(Pad_TreeLayout *val)
{
    Pad_TreeLayoutTrait *trait;
    Pad_Bool rc=TRUE;

    if (val) {
        trait = (Pad_TreeLayoutTrait*)_Get_trait(PAD_TREELAYOUT_TRAIT);
	if (trait) {
	    rc = trait->Set_value(val);
	} else {
	    trait = new Pad_TreeLayoutTrait(PAD_TREELAYOUT_TRAIT);
	    rc = trait->Set_value(val);
	    _traitList.Push(trait);
            _traitFlags |= PAD_TREELAYOUT_TRAIT;
	}
    } else {
        rc = _Remove_trait(PAD_TREELAYOUT_TRAIT);
    }

    return rc;
}

//
// Setter and getter methods for the angle option.
// The set method rotates the object from its current
// angle to the given angle.  And optional rotation center 
// point can be specified (default is the object anchor point).
//
Pad_Bool
Pad_Object::Set_angle(float degrees, Pad_Point *ctrpt)
{
    if (ctrpt == NULL) {
       Rotate(degrees - Get_angle());
    } else {
       Rotate(degrees - Get_angle(), *ctrpt);
    }

    return(TRUE);
}

void
Pad_Object::Set_angle_default(void)
{
    Set_angle(OBJECT_DEFAULT_ANGLE);
    optionFlags &= ~PAD_ANGLE_SET;
}

float
Pad_Object::Get_angle(void)
{
    float degrees=OBJECT_DEFAULT_ANGLE;
    Pad_Trait *trait = _Get_trait(PAD_ANGLE_TRAIT);
    if (trait) {
        float *value;
        trait->Get_value(value);
        degrees = *value;
    }

    return degrees;  // NOTE: this should be changed to degrees when hardcoded
                   // uses of angle are changed.
}

//
// Private method for setting the angle trait value.
// This method is needed since the setting of the angle
// value is done from the Rotate (and Write) methods and
// not directly from Set_angle().
//  
void
Pad_Object::_Set_angle_data(float degrees)
{
    Pad_Trait *trait = _Get_trait(PAD_ANGLE_TRAIT);

    if (degrees != OBJECT_DEFAULT_ANGLE) {
        // if trait is already created then just set its new value
        // otherwise, create it and insert it in the trait list.
        if (!trait) {
	    trait = new Pad_FloatTrait(PAD_ANGLE_TRAIT, degrees);
	    _traitList.Push(trait);
	    _traitFlags |= PAD_ANGLE_TRAIT;
	    optionFlags |= PAD_ANGLE_SET;
	} else {
	    trait->Set_value(&degrees);
	}
    } else {
       _Remove_trait(PAD_ANGLE_TRAIT);
       optionFlags &= ~PAD_ANGLE_SET;
    }
}

//
// Setters and Getters for anchor
//
Pad_Bool
Pad_Object::Set_anchor(Pad_Anchor new_anchor)
{
    optionFlags |= PAD_ANCHOR_SET;
    anchor = new_anchor;
    Damage();
    Set_position_from_anchor();
    Damage();

    return(TRUE);
}

void
Pad_Object::Set_anchor_default(void)
{
    Set_anchor(OBJECT_DEFAULT_ANCHOR);
    optionFlags &= ~PAD_ANCHOR_SET;
}

Pad_Anchor
Pad_Object::Get_anchor(void)
{
    return(anchor);
}

//
// Setters and Getters for zoom actions
//
void
Pad_Object::Modify_zoomaction(Pad_ZoomAction *newZoomAction)
{
    Pad_Bool found;
    Pad_Iterator zi;
    Pad_ZoomAction *zoomAction;
    Pad_List *zoomActions = Get_zoomaction();
    Pad_List *prevSizes;
    
    if (!zoomActions) {
        Pad_List emptyList;
        _Set_trait(PAD_ZOOMACTION_TRAIT, &emptyList);
        optionFlags |= PAD_ZOOMACTION_SET;
	zoomActions = Get_zoomaction();
	prevSizes = Get_prevsizes();
    }

    found = FALSE;
    DOLIST(zi, *zoomActions, Pad_ZoomAction, zoomAction) {
	if (zoomAction->userActionSize == newZoomAction->userActionSize) {
	    zoomActions->Remove(zoomAction);
	    delete zoomAction;
	    zoomActions->Push_last(newZoomAction);
	    found = TRUE;
	    break;
	}
    }

    if (!found) {
        zoomActions->Push_last(newZoomAction);
    }

    if (zoomActions->Is_empty()) {
        Set_zoomaction_default();
    } else {
        optionFlags |= PAD_ZOOMACTION_SET;
    }
}

void
Pad_Object::Set_zoomaction_default(void)
{
    Pad_ZoomAction *zoomAction=NULL;
    Pad_ZoomActionSize *prevSize;
    Pad_List *zoomActions = Get_zoomaction();

    if (zoomActions) {
	while ((zoomAction = (Pad_ZoomAction *)zoomActions->Pop())) {
	    delete zoomAction;
	}

	Pad_List *prevSizes = Get_prevsizes();
	if (prevSizes) {
	    while ((prevSize = (Pad_ZoomActionSize *)prevSizes->Pop())) {
	        delete prevSize;
	    }
	}
	    
	_Remove_trait(PAD_ZOOMACTION_TRAIT);
    }
    optionFlags &= ~PAD_ZOOMACTION_SET;
}

Pad_List *
Pad_Object::Get_zoomaction(void)
{
    Pad_List *zoomActions=NULL;
    Pad_ZoomActionTrait *trait = (Pad_ZoomActionTrait*)_Get_trait(PAD_ZOOMACTION_TRAIT);
    if (trait) {
        trait->Get_value(zoomActions);
    }
    return zoomActions;
}

Pad_List *
Pad_Object::Get_prevsizes(void)
{
    Pad_List *prevSizes=NULL;
    Pad_ZoomActionTrait *trait = (Pad_ZoomActionTrait*)_Get_trait(PAD_ZOOMACTION_TRAIT);
    if (trait) {
        trait->Get_prevsizes(prevSizes);
    }
    return prevSizes;
}

//
// Setters and Getters for the height of <this>
// (works in global coordinates)
//
Pad_Bool
Pad_Object::Set_height(float height)
{
    float scale;
    float yctr;
    Pad_BBox bb;

    Damage();
    Get_bbox(bb);

    scale = 1.0;
    Local_to_screen(scale);
    height /= scale;

    switch(anchor) {
      case PAD_ANCHOR_NW:
      case PAD_ANCHOR_N:
      case PAD_ANCHOR_NE:
	Set_bbox_ymin(bb.Ymax() - height);
	Set_bbox_ymax(bb.Ymax());
	break;
      case PAD_ANCHOR_W:
      case PAD_ANCHOR_CENTER:
      case PAD_ANCHOR_E:
	yctr = 0.5 * (bb.Ymin() + bb.Ymax());
	Set_bbox_ymin(yctr - 0.5 * height);
	Set_bbox_ymax(yctr + 0.5 * height);
	break;
      case PAD_ANCHOR_SW:
      case PAD_ANCHOR_S:
      case PAD_ANCHOR_SE:
	Set_bbox_ymin(bb.Ymin());
	Set_bbox_ymax(bb.Ymin() + height);
	break;
    }

    optionFlags |= PAD_HEIGHT_SET;
    Update();

    return(TRUE);
}

void
Pad_Object::Set_height_default(void)
{
    optionFlags &= ~PAD_HEIGHT_SET;

    Update();
}

float
Pad_Object::Get_height(void)
{
    float height;
    Pad_BBox bb;

    Get_global_bbox(bb);
    height = bb.Height();

    return(height);
}

//
// Setters and Getters for the width of <this>
// (works in global coordinates)
//
Pad_Bool
Pad_Object::Set_width(float width)
{
    float scale;
    float xctr;
    Pad_BBox bb;

    Damage();
    Get_bbox(bb);

    scale = 1.0;
    Local_to_screen(scale);
    width /= scale;

    switch(anchor) {
      case PAD_ANCHOR_NW:
      case PAD_ANCHOR_W:
      case PAD_ANCHOR_SW:
	Set_bbox_xmin(bb.Xmin());
	Set_bbox_xmax(bb.Xmin() + width);
	break;
      case PAD_ANCHOR_N:
      case PAD_ANCHOR_CENTER:
      case PAD_ANCHOR_S:
	xctr = 0.5 * (bb.Xmin() + bb.Xmax());
	Set_bbox_xmin(xctr - 0.5 * width);
	Set_bbox_xmax(xctr + 0.5 * width);
	break;
      case PAD_ANCHOR_NE:
      case PAD_ANCHOR_E:
      case PAD_ANCHOR_SE:
	Set_bbox_xmin(bb.Xmax() - width);
	Set_bbox_xmax(bb.Xmax());
	break;
    }

    optionFlags |= PAD_WIDTH_SET;
    Update();

    return(TRUE);
}

void
Pad_Object::Set_width_default(void)
{
    optionFlags &= ~PAD_WIDTH_SET;
    Update();
}

float
Pad_Object::Get_width(void)
{
    float width;
    Pad_BBox bb;

    Get_global_bbox(bb);
    width = bb.Width();

    return(width);
}

//
// Setters and Getters for always_render bit
// (works in global coordinates)
//
Pad_Bool
Pad_Object::Set_alwaysrender(Pad_Bool always_render)
{
    optionFlags |= PAD_ALWAYSRENDER_SET;
    if (always_render) {
	flags |= PAD_MASK_ALWAYS_RENDER;
    } else {
	flags &= ~PAD_MASK_ALWAYS_RENDER;
    }

    return(TRUE);
}

void
Pad_Object::Set_alwaysrender_default(void)
{
    Set_alwaysrender(OBJECT_DEFAULT_ALWAYSRENDER);
    optionFlags &= ~PAD_ALWAYSRENDER_SET;
}

Pad_Bool
Pad_Object::Get_alwaysrender(void)
{
    return((flags & PAD_MASK_ALWAYS_RENDER) ? TRUE : FALSE);
}

//
// Setters and Getters for clipping bits
//
void
Pad_Object::Set_clipping(Clipping clipping)
{
    switch (clipping) {
       case CLIPPING_AUTO:
       case CLIPPING_FALSE:
       case CLIPPING_TRUE:
	   // clear clipping bits first and then set them to new value
	   flags &= ~PAD_MASK_CLIPPING;
	   flags |= clipping;
	   break;
       default:
	   break;
    }

    if (clipping != OBJECT_DEFAULT_CLIPPING)
        optionFlags |= PAD_CLIPPING_SET;
    else {
        optionFlags &= ~PAD_CLIPPING_SET;
    }
}

void
Pad_Object::Set_clipping_default(void)
{
    Set_clipping(OBJECT_DEFAULT_CLIPPING);
}

Pad_Object::Clipping
Pad_Object::Get_clipping(void)
{
    Clipping val;
    switch (flags & PAD_MASK_CLIPPING) {
    case PAD_CLIPPING_FALSE:
      val = CLIPPING_FALSE;
      break;
    case PAD_CLIPPING_TRUE:
      val = CLIPPING_TRUE;
      break;
    default:
      val = CLIPPING_AUTO;
      break;
    }

    return val;
}

//
// Setters and Getters for lock bit
//
Pad_Bool
Pad_Object::Set_lock(Pad_Bool lock)
{
    optionFlags |= PAD_LOCK_SET;
    if (lock) {
	flags |= PAD_MASK_LOCK;
    } else {
	flags &= ~PAD_MASK_LOCK;
    }

    return(TRUE);
}

void
Pad_Object::Set_lock_default(void)
{
    Set_lock(OBJECT_DEFAULT_LOCK);
    optionFlags &= ~PAD_LOCK_SET;
}

Pad_Bool
Pad_Object::Get_lock(void)
{
    return((flags & PAD_MASK_LOCK) ? TRUE : FALSE);
}

//
// Setters and Getters for gets_events bit
//
Pad_Bool
Pad_Object::Set_events(Pad_Bool events)
{
    optionFlags |= PAD_EVENTS_SET;
    if (events) {
	flags |= PAD_MASK_EVENTS;
    } else {
	flags &= ~PAD_MASK_EVENTS;
    }

    return(TRUE);
}

void
Pad_Object::Set_events_default(void)
{
    Set_events(OBJECT_DEFAULT_EVENTS);
    optionFlags &= ~PAD_EVENTS_SET;
}

Pad_Bool
Pad_Object::Get_events(void)
{
    return((flags & PAD_MASK_EVENTS) ? TRUE : FALSE);
}

//
// Setters and Getters for sticky bit
//
Pad_Bool
Pad_Object::Set_sticky(int newSticky)
{
    Pad_Sticky *sticky=NULL;
    Pad_Trait *trait;
    
    if (newSticky != PAD_STICKY_NONE) {
	switch (newSticky) {
	  case PAD_STICKY_ALL:
	  case PAD_STICKY_X:
	  case PAD_STICKY_Y:
	  case PAD_STICKY_Z:
	  case PAD_STICKY_VIEW:
	    break;
	  default:
	    Pad_errorString = "Invalid sticky setting";
	    return(FALSE);
	}

        if ((trait = _Get_trait(PAD_STICKY_TRAIT))) {
	    trait->Get_value(sticky);
	}
	if (!sticky) {
	    Pad_Sticky tmpsticky;
	    trait = new Pad_StickyTrait(PAD_STICKY_TRAIT);
	    trait->Set_value(&tmpsticky);
	    _traitList.Push(trait);
	    _traitFlags |= PAD_STICKY_TRAIT;
	    optionFlags |= PAD_STICKY_SET;
	    trait->Get_value(sticky);
	}
	if (newSticky != sticky->type) {
	    sticky->type = newSticky;      // Set sticky type
	    Update_sticky();	           // Remember current position
	    pad->view->Add_sticky(this);   // Add to list of sticky objects
	}
    } else {
        _Remove_trait(PAD_STICKY_TRAIT);
	pad->view->Remove_sticky(this);
    }

    return(TRUE);
}

void
Pad_Object::Set_sticky_default(void)
{
    Set_sticky(OBJECT_DEFAULT_STICKY);
    optionFlags &= ~PAD_STICKY_SET;
}

int
Pad_Object::Get_sticky(void)
{
    int type;
    Pad_Sticky *sticky=NULL;
    Pad_Trait *trait=_Get_trait(PAD_STICKY_TRAIT);

    if (trait) {
        trait->Get_value(sticky);
    }

    if (sticky) {
	type = sticky->type;
    } else {
	type = PAD_STICKY_NONE;
    }

    return(type);
}

//
// Returns true if object is sticky or is member of a sticky group
//
Pad_Bool
Pad_Object::Is_sticky(void)
{
    if (Get_sticky() ||
	(group && group->Is_sticky())) {
	return(TRUE);
    } else {
	return(FALSE);
    }
}

//
// Transform sticky objects when the view has changed,
// so they appear in the correct place.  There are several
// kinds of sticky objects.
//   * Regular sticky objects
//     These are positioned as if the view was always "0 0 1".
//     That is, they do not move when the view changes.
//   * Sticky x
//     These do not zoom, and they do not pan in x, but pan y normally.
//   * Sticky y
//     These do not zoom, and they do not pan in y, but pan x normally.
//   * Sticky z
//     These do not zoom, but they pan normally.
//   * Sticky view
//     These do not zoom, and always stay within the view.
//
void
Pad_Object::Transform_sticky(void)
{
    int type;
    float x, y, s;
    float origX, origY;
    float dx, dy;
    Pad_BBox bb;
    Pad_BBox *vbb;
    Pad_View *view = pad->view;
    Pad_Object *obj;
    Pad_Sticky *sticky=NULL;
    Pad_Trait *trait;

    type = Get_sticky();
    if (type != PAD_STICKY_NONE) {
        trait = _Get_trait(PAD_STICKY_TRAIT);
	trait->Get_value(sticky);
	sticky->stickyTransform.Get(x, y, s);
    }
    
    switch (type) {
      case PAD_STICKY_NONE:
	break;
      case PAD_STICKY_ALL:
	view->Invert_view(x, y, s);
	Set_rel_position(x, y, s, FALSE);
	break;
      case PAD_STICKY_X:
	origY = y;
	view->Invert_view(x, y, s);
	Set_rel_position(x, origY, s, FALSE);
	break;
      case PAD_STICKY_Y:
	origX = x;
	view->Invert_view(x, y, s);
	Set_rel_position(origX, y, s, FALSE);
	break;
      case PAD_STICKY_Z:
	Set_rel_position(x, y, s / view->zoom, FALSE);
	break;
      case PAD_STICKY_VIEW:
	Set_rel_position(x, y, s / view->zoom, FALSE);   // First place this like a sticky z
	Get_global_bbox(bb);
	vbb = &view->viewBBox;
	dx = 0;
	dy = 0;
				                  
	if (bb.Xmin() < vbb->Xmin()) {            // Check if object is offscreen to the left
	    dx = vbb->Xmin() - bb.Xmin();
	} else if (bb.Xmax() > vbb->Xmax()) {     // Check if object is offscreen to the right
	    dx = vbb->Xmax() - bb.Xmax();
	}

	if (bb.Ymin() < vbb->Ymin()) {            // Check if object is offscreen to the bottom
	    dy = vbb->Ymin() - bb.Ymin();
	} else if (bb.Ymax() > vbb->Ymax()) {     // Check if object is offscreen to the top
	    dy = vbb->Ymax() - bb.Ymax();
	}

	if ((dx != 0) || (dy != 0)) {
				// If object is a member of a group, then scale down
				// correction by group's hierarchical transformations.
	    obj = this;
	    if (obj->group) {
		float ds = 1.0;
		do {
		    ds *= obj->group->transform.Get_scale();
		    obj = obj->group;
		} while (obj->group);
		dx /= ds;
		dy /= ds;
	    }
	    Set_rel_position(x + dx, y + dy, s / view->zoom, FALSE);
	    Update_sticky();	                  // Remember new position
	}
	break;
    }
}

//
// Updates desired sticky position.
// This must be called whenever a sticky object is moved
// so its desired position can be remembered.
//
void
Pad_Object::Update_sticky(void)
{
    int type;
    float x, y, s;
    float origX, origY;
    Pad_View *view = pad->view;
    Pad_Sticky *sticky=NULL;
    Pad_Trait *trait;

    Get_rel_position(x, y, s);

    type = Get_sticky();
    switch (type) {
      case PAD_STICKY_NONE:
	break;
      case PAD_STICKY_ALL:
	view->Apply_view(x, y, s);
	break;
      case PAD_STICKY_X:
	origY = y;
	view->Apply_view(x, y, s);
	y = origY;
	break;
      case PAD_STICKY_Y:
	origX = x;
	view->Apply_view(x, y, s);
	x = origX;
	break;
      case PAD_STICKY_Z:
	s *= view->zoom;
	break;
      case PAD_STICKY_VIEW:
	s *= view->zoom;
	break;
    }

    if (type != PAD_STICKY_NONE) {
        trait = _Get_trait(PAD_STICKY_TRAIT);
	trait->Get_value(sticky);
	sticky->stickyTransform.Set(x, y, s);
    }
}

//
// Setters and Getters for findable bit.
// If an object is findable, it will be returned
// by the find commands.  Else, it won't be
//
Pad_Bool
Pad_Object::Is_findable(void)
{
    if (flags & PAD_MASK_NOT_FINDABLE) {
	return(FALSE);
    } else {
	return(TRUE);
    }
}

void
Pad_Object::Set_findable(Pad_Bool findable)
{
    if (findable) {
	flags &= ~PAD_MASK_NOT_FINDABLE;
    } else {
	flags |= PAD_MASK_NOT_FINDABLE;
    }
}

//
// Setters and Getters for fadeRange
// Controls how quickly objects fade in and out (0.0 - 1.0)
//
Pad_Bool
Pad_Object::Set_faderange(float faderange)
{
    optionFlags |= PAD_FADERANGE_SET;
    fadeRange = faderange;
    Damage();

    return(TRUE);
}

void
Pad_Object::Set_faderange_default(void)
{
    Set_faderange(OBJECT_DEFAULT_FADERANGE);
    optionFlags &= ~PAD_FADERANGE_SET;
}

float
Pad_Object::Get_faderange(void)
{
    return(fadeRange);
}

//
// Getter for object transform
//
Pad_Transform *
Pad_Object::Get_transform(void)
{
    return(&transform);
}

//
// Set bounding box of object in local coordinates
//
void
Pad_Object::Set_bbox(float *bb)
{
    bbox.Set(bb);
}

void
Pad_Object::Set_bbox(Pad_BBox &bb)
{
    bbox.Set(bb);
}

void
Pad_Object::Set_bbox(float xmin, float ymin, float xmax, float ymax)
{
    bbox.Set(xmin, ymin, xmax, ymax);
}

void
Pad_Object::Set_bbox_xmin(float xmin)
{
    bbox.Set_xmin(xmin);
}

void
Pad_Object::Set_bbox_ymin(float ymin)
{
    bbox.Set_ymin(ymin);
}

void
Pad_Object::Set_bbox_xmax(float xmax)
{
    bbox.Set_xmax(xmax);
}

void
Pad_Object::Set_bbox_ymax(float ymax)
{
    bbox.Set_ymax(ymax);
}

//
// Return bounding box of object in local coordinates.
//
void
Pad_Object::Get_bbox(float bb[4])
{
    bbox.Get(bb);
}

void
Pad_Object::Get_bbox(Pad_BBox &bb)
{
    bb = bbox;
}

//
// Return bounding box of object in pad coordinates.
//
void
Pad_Object::Get_global_bbox(float bb[4])
{
    globalBBox.Get(bb);
}

void
Pad_Object::Get_global_bbox(Pad_BBox &bb)
{
    bb = globalBBox;
}

//
// Get total magnification of object (including group's transformations).
//
float
Pad_Object::Get_global_size(void)
{
    float x, y, s;

    x = 0.0;
    y = 0.0;
    s = 1.0;
    Local_to_screen(x, y, s);

    return(s);
}

//
// Make this object the focus object.
// Nothing special for most objects, but this may be overrided by sub-classes.
//
void
Pad_Object::Set_focus(void)
{
}

//
// This object no longer the focus object.
// Nothing special for most objects, but this may be overrided by sub-classes.
//
void
Pad_Object::Unset_focus(void)
{
}

//
// Return TRUE if this object has the focus
//
Pad_Bool
Pad_Object::Has_focus(void)
{
    return((Pad_focus == this) ? TRUE : FALSE);
}

//
// Objects know if the pointer is within the object.
// These get called on <Enter> and <Leave> events.
// The container (pad or group) that an object
// is in gets notified when a member gets or loses the pointer.
//
void
Pad_Object::Pointer_in(Pad_Event *padEvent)
{
    if (group) {
    	group->Pointer_in(padEvent);
    }
}

void
Pad_Object::Pointer_out(Pad_Event *padEvent)
{
    if (group) {
	group->Pointer_out(padEvent);
    }
}

//
// Set the current view
// Return TRUE if successful or FALSE otherwise
//
Pad_Bool 
Pad_Object::Set_view(float, float, float, Pad_Bool)
{
    Pad_errorString = "Pad_Object::Set_view should never be called";
    return FALSE;
}

//
// Set the pen color
// Return TRUE if successful or FALSE otherwise
//
Pad_Bool 
Pad_Object::Set_pen(int, int, int)
{
    Pad_errorString = "Pen color does not apply to Pad_Object base class";
    return FALSE;
}

void
Pad_Object::Get_pen_name(Pad_String &penname)
{
    Pad_errorString = "Pen color does not apply to Pad_Object base class";
}

Pad_Bool 
Pad_Object::Set_pen(const char *)
{
    Pad_errorString = "Pen color does not apply to Pad_Object base class";
    return FALSE;
}

//
// Set the fill color
// Return TRUE if successful or FALSE otherwise
//
Pad_Bool 
Pad_Object::Set_fill(int, int, int)
{
    Pad_errorString = "Fill color does not apply to Pad_Object base class";
    return FALSE;
}

Pad_Bool 
Pad_Object::Set_fill(const char *)
{
    Pad_errorString = "Fill color does not apply to Pad_Object base class";
    return FALSE;
}

void
Pad_Object::Get_fillname(Pad_String &penname)
{
    Pad_errorString = "Fill color does not apply to Pad_Object base class";
}

//
// Append the coordinates of an object.
// Return TRUE if successful or FALSE otherwise
//
Pad_Bool 
Pad_Object::Append_coords(Pad_PList &, Pad_Bool)
{
    Pad_errorString = "Pad_Object::Append_coords should never be called";
    return FALSE;
}

//
// Set the coordinates of an object.
// Return TRUE if successful or FALSE otherwise
//
Pad_Bool 
Pad_Object::Set_coords(Pad_PList &, Pad_Bool)
{
    Pad_errorString = "Pad_Object::Set_coords should never be called";
    return FALSE;
}

//
// Return the points of an object.  The points are returned
// in allocated memory and must be freed.
//
void
Pad_Object::Get_coords(Pad_PList &, Pad_Bool)
{
    Pad_errorString = "Pad_Object::Get_coords should never be called";
}

//
// Set the penwidth of an object.
//
Pad_Bool
Pad_Object::Set_penwidth(float, Pad_Bool)
{
    Pad_errorString = "Pad_Object::Set_penwidth should never be called";
    return(FALSE);
}

//
//  Does the line width scale or is it absolute pixels?
//
Pad_Bool
Pad_Object::Get_abslinestyle()
{
    Pad_errorString = "Pad_Object::Set_abslinestyle should never be called";
    return(FALSE);
}

float
Pad_Object::Get_penwidth(void)
{
    Pad_errorString = "Pad_Object::Get_penwidth should never be called";
    return 0;
}

//
// Setters and Getters of object layer
//
Pad_Bool
Pad_Object::Set_layer(char *new_layer)
{
    optionFlags |= PAD_LAYER_SET;
    if (pad) {
	Add_to_layer(new_layer);
    } else {
	layerId = 0;		// Not on a layer
    }
    Damage();

    return(TRUE);
}

void 
Pad_Object::Set_layer_default(void)
{
    Set_layer(OBJECT_DEFAULT_LAYER);
    optionFlags &= ~PAD_LAYER_SET;
}

char *
Pad_Object::Get_layer(void)
{
    Pad_Layer *layer;
    
    layer = pad->Get_layer_from_id(layerId);
    return(layer->name);
}

//
// Add <this> to the specified layer.
// If no such layer exists, make one and update all
// views with the new layer.  
// If no items exist on the layer, be sure to insert
// layer in correct order (based on pad->layers).
// Maintain the layer->first/last pointers.
// Maintain the # of objects on this layer.
//
void
Pad_Object::Add_to_layer(const char *name)
{
    int i, index;
    Pad_Object *l;
    Pad_Layer *layer, *prevLayer;

				// If group member, then don't allow changing layers
    if (group) {
	return;
    }

				// First remove from existing layer
    Remove_from_layer();

    layer = pad->Get_layer_from_name(name);
    if (layer) {
				// Layer already exists, use it
	layerId = layer->id;
    } else {
				// Layer doesn't exist, make a new one
	layer = pad->Create_layer(name);
	layerId = layer->id;
    }

    if (Type() != PAD_PAD) {
	layer->objects++;	// Increment # of objects on this layer

				// Insert item at end of layer
	if (layer->last) {
	    l = layer->last;
	} else {
				// Layer currently empty, so find last
				// item on earlier layer.
	    l = NULL;
	    index = pad->layers.Index(layer);
	    for (i=index-1; i>=0; i--) {
		prevLayer = (Pad_Layer *)pad->layers.Nth(i);
		if (prevLayer->last) {
		    l = prevLayer->last;
		    break;
		}
	    }
	}
	Add_to_drawing_order(l);
    }
}

//
// Remove <this> from its current layer.
// Maintain the layer->first/last pointers.
// Maintain the # of objects on this layer.
//
void
Pad_Object::Remove_from_layer(void)
{
    Pad_Layer *layer;

    if (layerId == 0) {		// Not on a layer
	return;
    }

    layer = pad->Get_layer_from_id(layerId);
    if (layer) {
	Remove_from_drawing_order();
	layerId = 0;
	layer->objects--;
	if (layer->objects == 0) {
				// Layer empty - Could delete layers if there
				// are no views looking at them.  However,
				// there isn't much cost associated with having
				// unused layers around, so don't bother deleting them.
	}
    } else {
	cerr << "INTERNAL ERROR: Trying to remove object from layer it isn't on" << endl;
    }
}

//
// Setters and Getters for the transparency of this object.
// 0.0 is transparent, 1.0 is fully opaque.
//
Pad_Bool
Pad_Object::Set_transparency(float trans)
{
    Pad_Trait *trait = _Get_trait(PAD_TRANSPARENCY_TRAIT);
    trans = MAX(0, trans);       // Clamp between 0 and 1
    trans = MIN(1, trans);
    unsigned char val = (unsigned char) (255*trans);
    if (trans != OBJECT_DEFAULT_TRANSPARENCY) {
      if (trait) {
	  trait->Set_value(&val);
      } else {
	  trait = new Pad_UCharTrait(PAD_TRANSPARENCY_TRAIT, val);
	  _traitList.Push(trait);
	  _traitFlags |= PAD_TRANSPARENCY_TRAIT;
	  optionFlags |= PAD_TRANSPARENCY_SET;
      }
    } else {
      _Remove_trait(PAD_TRANSPARENCY_TRAIT);
      optionFlags &= ~PAD_TRANSPARENCY_SET;
    }

    Damage();

    return(TRUE);
}

void 
Pad_Object::Set_transparency_default(void)
{
    Set_transparency(OBJECT_DEFAULT_TRANSPARENCY);
}

float 
Pad_Object::Get_transparency(void)
{
    Pad_Trait *trait = _Get_trait(PAD_TRANSPARENCY_TRAIT);
    float transparency = OBJECT_DEFAULT_TRANSPARENCY;
    if (trait) {
        unsigned char *val;
        trait->Get_value(val);
	transparency = (float)(*val / 255.0);
    }

    return transparency;
}

//
// Setters and Getters for the info field of each object
// (an arbitrary string).
//
Pad_Bool
Pad_Object::Set_info(char *new_info)
{
    if (new_info) {
	_Set_trait(PAD_INFO_TRAIT, new_info);
        optionFlags |= PAD_INFO_SET;
    } else {
        _Remove_trait(PAD_INFO_TRAIT);
	optionFlags &= ~PAD_INFO_SET;
    }

    return(TRUE);
}

void 
Pad_Object::Set_info_default(void)
{
    Set_info(OBJECT_DEFAULT_INFO);
    optionFlags &= ~PAD_INFO_SET;
}

char *
Pad_Object::Get_info(void)
{
    char *info=NULL;
    _Get_trait(PAD_INFO_TRAIT, info);
    if ( !info ) {
        info = "";
    }
    return info;
}

//
// Setters and Getters for viewscript
//
Pad_Bool
Pad_Object::Set_viewscript(Pad_Callback *new_viewscript)
{
    Pad_Callback *viewScript=NULL;
    Pad_Bool rc=TRUE;

    if (new_viewscript) {
        viewScript = Get_viewscript();
        if ( !viewScript) {
	    rc = _Set_trait(PAD_VIEWSCRIPT_TRAIT, new_viewscript);
	    optionFlags |= PAD_VIEWSCRIPT_SET;
	    pad->view->viewScriptObjects.Push_new_last(this);
	} else {
	    viewScript->Set(new_viewscript);
	}
	delete new_viewscript;

    } else {
        _Remove_trait(PAD_VIEWSCRIPT_TRAIT);
	if (pad && pad->view) {
	    pad->view->viewScriptObjects.Remove(this);
	}
	optionFlags &= ~PAD_VIEWSCRIPT_SET;
    }

    return rc;
}

void 
Pad_Object::Set_viewscript_default(void)
{
    Set_viewscript(OBJECT_DEFAULT_VIEWSCRIPT);
    optionFlags &= ~PAD_VIEWSCRIPT_SET;
}

Pad_Callback *
Pad_Object::Get_viewscript(void)
{
    Pad_Callback *viewScript=NULL;
    _Get_trait(PAD_VIEWSCRIPT_TRAIT, viewScript);
    return viewScript;
}

//
// Setters and Getters for renderscript
//
Pad_Bool
Pad_Object::Set_renderscript(Pad_Callback *newRenderScript)
{
    Pad_Callback *renderScript;
    Pad_Bool rc= TRUE;

    if (newRenderScript) {
        renderScript = Get_renderscript();
        if (!renderScript) {
	    rc = _Set_trait(PAD_RENDERSCRIPT_TRAIT, newRenderScript);
	    optionFlags |= PAD_RENDERSCRIPT_SET;
	} else {
	    renderScript->Set(newRenderScript);
	}
	delete newRenderScript;
    } else {
        _Remove_trait(PAD_RENDERSCRIPT_TRAIT);
	optionFlags &= ~PAD_RENDERSCRIPT_SET;
    }
    Damage();
    return(TRUE);
}

void 
Pad_Object::Set_renderscript_default(void)
{
    Set_renderscript(OBJECT_DEFAULT_RENDERSCRIPT);
    optionFlags &= ~PAD_RENDERSCRIPT_SET;
}

Pad_Callback *
Pad_Object::Get_renderscript(void)
{
    Pad_Callback *renderScript=NULL;
    _Get_trait(PAD_RENDERSCRIPT_TRAIT, renderScript);
    return renderScript;
}

//
// Set and get the min and max size fields.
// If the size ends in '%', then it specifies a percentage (0 - 100)
// of the minimum window dimension. Otherwise it is absolute in pixels.
// The get function returns a string suitable for passing into the
// set functions.
//
void 
Pad_Object::Set_minsize(char *minsize)
{
    int len;
    float s;

    len = strlen(minsize);
    if (len > 0) {
	s = atof(minsize);
	if (minsize[len - 1] == '%') {
	    Set_minsize_rel(s);
	} else {
	    Set_minsize_abs(UNITSTOVALUE(pad->view->win, s));
	}
    }
}

Pad_Bool
Pad_Object::Set_minsize_abs(float new_minsize)
{
    flags &= ~PAD_MASK_MINSIZE_RELATIVE;
    optionFlags |= PAD_MINSIZE_SET;
    minSize = new_minsize;
    Damage();

    return(TRUE);
}

Pad_Bool
Pad_Object::Set_minsize_rel(float new_minsize)
{
    flags |= PAD_MASK_MINSIZE_RELATIVE;
    optionFlags |= PAD_MINSIZE_SET;
    minSize = new_minsize;
    Damage();

    return(TRUE);
}

void 
Pad_Object::Set_minsize_default(void)
{
    Set_minsize_abs(OBJECT_DEFAULT_MINSIZE);
    optionFlags &= ~PAD_MINSIZE_SET;
}


void
Pad_Object::Get_minsize(Pad_String &result)
{
    if (flags & PAD_MASK_MINSIZE_RELATIVE) {
	result = minSize;
	result += "%";
    } else {
	result = VALUETOUNITS(pad->view->win, minSize);
    }
}

float
Pad_Object::Get_minsize()
{
    if (flags & PAD_MASK_MINSIZE_RELATIVE) {
	return minSize;
    } else {
	return VALUETOUNITS(pad->view->win, minSize);
    }
}

Pad_Bool
Pad_Object::Get_minsizep()
{
    if (flags & PAD_MASK_MINSIZE_RELATIVE) {
	return TRUE;
    } else {
	return FALSE;
    }
}

void 
Pad_Object::Set_maxsize(char *maxsize)
{
    int len;
    float s;

    len = strlen(maxsize);
    if (len > 0) {
	s = atof(maxsize);
	if (maxsize[len - 1] == '%') {
	    Set_maxsize_rel(s);
	} else {
	    Set_maxsize_abs(UNITSTOVALUE(pad->view->win, s));
	}
    }
}

Pad_Bool
Pad_Object::Set_maxsize_abs(float new_maxsize)
{
    flags &= ~PAD_MASK_MAXSIZE_RELATIVE;
    optionFlags |= PAD_MAXSIZE_SET;
    if (new_maxsize < 0) {
	maxSize = -1;
    } else {
	maxSize = new_maxsize;
    }
    Damage();

    return(TRUE);
}

Pad_Bool
Pad_Object::Set_maxsize_rel(float new_maxsize)
{
    flags |= PAD_MASK_MAXSIZE_RELATIVE;
    optionFlags |= PAD_MAXSIZE_SET;
    maxSize = new_maxsize;
    Damage();

    return(TRUE);
}

void 
Pad_Object::Set_maxsize_default(void)
{
    Set_maxsize_abs(OBJECT_DEFAULT_MAXSIZE);
    optionFlags &= ~PAD_MAXSIZE_SET;
}

void
Pad_Object::Get_maxsize(Pad_String &result)
{
    if (flags & PAD_MASK_MAXSIZE_RELATIVE) {
	result = maxSize;
	result += "%";
    } else {
	if (maxSize == -1) {
	    result = "-1";
	} else {
	    result = VALUETOUNITS(pad->view->win, maxSize);
	}
    }
}

float
Pad_Object::Get_maxsize()
{
    if (flags & PAD_MASK_MAXSIZE_RELATIVE) {
	return maxSize;
    } else {
	return VALUETOUNITS(pad->view->win, maxSize);
    }
}

Pad_Bool
Pad_Object::Get_maxsizep()
{
    if (flags & PAD_MASK_MAXSIZE_RELATIVE) {
	return TRUE;
    } else {
	return FALSE;
    }
}

Pad_Bool
Pad_Object::Get_name(Pad_String &) 
{
    return(FALSE);
}

Pad_Bool
Pad_Object::Set_noisedata(float pos, float freq, float amp, float steps)
{
    Pad_NoiseData *nd=NULL;
    Pad_Bool rc=TRUE;
    Pad_Trait *trait = _Get_trait(id);

    trait = _Get_trait(PAD_NOISEDATA_TRAIT);
    if (trait) {
        rc = trait->Get_value(nd);
    } else {
        trait = new Pad_NoiseTrait(PAD_NOISEDATA_TRAIT);
	_traitList.Push(trait);
	_traitFlags |= PAD_NOISEDATA_TRAIT;
        trait->Get_value(nd);
    }

    if (nd) {
        nd->pos = pos;
	nd->freq = freq;
	nd->amp = amp;
	nd->steps = steps;
	optionFlags |= PAD_NOISEDATA_SET;
    } else {
        rc = FALSE;
    }

    Update_and_compute_anchor();
    return rc;
}

void
Pad_Object::Set_noisedata_default(void)
{
    _Remove_trait(PAD_NOISEDATA_TRAIT);
    optionFlags &= ~PAD_NOISEDATA_SET;
}

Pad_NoiseData *
Pad_Object::Get_noisedata()
{
    Pad_NoiseData *nd=NULL;
    Pad_Trait *trait = _Get_trait(PAD_NOISEDATA_TRAIT);
    if (trait) {
        trait->Get_value(nd);
    }

    return nd;
}

//
// Return scale factor that will transform object so height of object
// fits within height of frame.
//
float
Pad_Object::Compute_scale_within_frame(float *)
{
    return(1.0);
}

//
// Setters and getters for absolute position of object
// (x, y) specifies the anchor point of the object
// and (s) specifies the scale of the object.
//
Pad_Bool
Pad_Object::Set_abs_position(float x, float y, float s, Pad_Bool useCoordFrames)
{
    Pad_Bool rc = TRUE;
    float zmult;
    float *frame;
    
    if (s <= 0.0) {
	Pad_errorString += "z value must be strictly greater than zero\n";
	rc = FALSE;
    } else {
	optionFlags |= PAD_PLACE_SET;
	Damage();
	if (group) {
	    group->Screen_to_local(x, y, s);
	}
	transform.Set_offset(x, y);
	anchorpt.x = x;
	anchorpt.y = y;
	if (Pad_coordFrames.Is_empty() || !useCoordFrames) {
	    transform.Set_scale(s);
	} else {
			// If within a coordinate frame, convert size relative to that frame
	    frame = (float *)Pad_coordFrames.First();
	    zmult = s * Compute_scale_within_frame(frame) / transform.Get_scale();
	    Scale(anchorpt.x, anchorpt.y, zmult);
	}
	Set_position_from_anchor();
        if (Get_sticky() != PAD_STICKY_NONE) {
          Update_sticky();
        }
	Damage();
    }

    return(rc);
}

void
Pad_Object::Set_abs_position_xy(float x, float y)
{
    Set_abs_position(x, y, Get_abs_position_z());
}

void
Pad_Object::Set_abs_position_x(float x)
{
    Set_abs_position(x, Get_abs_position_y(), Get_abs_position_z());
}

void
Pad_Object::Set_abs_position_y(float y)
{
    Set_abs_position(Get_abs_position_x(), y, Get_abs_position_z());
}

Pad_Bool
Pad_Object::Set_abs_position_z(float z)
{
    Pad_Bool rc;

    rc = Set_abs_position(Get_abs_position_x(), Get_abs_position_y(), z);

    return(rc);
}

//
// Setters and getters for relative place of object
// (x, y) specifies the anchor point of the object
// and (s) specifies the scale of the object.
//
Pad_Bool
Pad_Object::Set_rel_position(float x, float y, float s, Pad_Bool useCoordFrames)
{
    Pad_Bool rc = TRUE;
    float zmult;
    float *frame;
    
    if (s <= 0.0) {
	Pad_errorString += "z value must be strictly greater than zero\n";
	rc = FALSE;
    } else {
	optionFlags |= PAD_PLACE_SET;
	Damage();
	transform.Set_offset(x, y);
	anchorpt.x = x;
	anchorpt.y = y;
	if (Pad_coordFrames.Is_empty() || !useCoordFrames) {
	    transform.Set_scale(s);
	} else {
			// If within a coordinate frame, convert size relative to that frame
	    frame = (float *)Pad_coordFrames.First();
	    zmult = s * Compute_scale_within_frame(frame) / transform.Get_scale();
	    Scale(anchorpt.x, anchorpt.y, zmult);
	}
	Set_position_from_anchor();
	Damage();
    }

    return(rc);
}

void
Pad_Object::Set_rel_position_xy(float x, float y)
{
    Set_rel_position(x, y, transform.Get_scale());
}

void
Pad_Object::Set_rel_position_x(float x)
{
    Set_rel_position(x, anchorpt.y, transform.Get_scale());
}

void
Pad_Object::Set_rel_position_y(float y)
{
    Set_rel_position(anchorpt.x, y, transform.Get_scale());
}

Pad_Bool
Pad_Object::Set_rel_position_z(float z)
{
    Pad_Bool rc;

    rc = Set_rel_position(anchorpt.x, anchorpt.y, z);

    return(rc);
}

//
// Set the position of the object so that it is 
// centered in the current view, and scaled so it
// fills up 75% of the screen.
//
void
Pad_Object::Set_rel_position_center(void)
{
    Pad_BBox bb;
    float x, y, s;
    float dx, dy, ds;
    float width, height;
    float centerx, centery;
    float xoffset, yoffset;
    float maxObjDim, maxWindowDim;
    Pad_Win *win = pad->view->win;

    Damage();
    transform.Get_offset(xoffset, yoffset);
    dx = anchorpt.x - xoffset;
    dy = anchorpt.y - yoffset;
    Get_global_bbox(bb);
    width = bb.Width();
    height = bb.Height();
    maxObjDim = MAX(width, height);
    maxWindowDim = MAX(win->width, win->height);
    if (maxObjDim != 0) {
	ds = (0.75 * maxWindowDim) / (win->view->zoom * maxObjDim);
	transform.Scale(ds);
	dx *= ds;
	dy *= ds;
    }
    Update_global_bounding_box();
    Get_global_bbox(bb);
    centerx = 0.5 * (bb.Xmin() + bb.Xmax());
    centery = 0.5 * (bb.Ymin() + bb.Ymax());
    xoffset -= centerx - win->view->xview;
    yoffset -= centery - win->view->yview;
    
				// Keep relative offset from object's transformation.
    x = xoffset + dx;
    y = yoffset + dy;
    s = transform.Get_scale();

    Set_rel_position(x, y, s);
}

void
Pad_Object::Get_rel_position(float &x, float &y, float &s, Pad_Bool useCoordFrames)
{
    float *frame;
    float zmult;

    x = anchorpt.x;
    y = anchorpt.y;
    s = transform.Get_scale();

			// If within a coordinate frame, convert size relative to that frame
    if (!Pad_coordFrames.Is_empty() && useCoordFrames) {
	frame = (float *)Pad_coordFrames.First();
	zmult = Compute_scale_within_frame(frame);
	s /= zmult;
    }
}

float
Pad_Object::Get_rel_position_x(void)
{
    return(anchorpt.x);
}

float
Pad_Object::Get_rel_position_y(void)
{
    return(anchorpt.y);
}

float
Pad_Object::Get_rel_position_z(void)
{
    float x, y, z;

    Get_rel_position(x, y, z);
    return(z);
}

//
// Get absolute position
//
void
Pad_Object::Get_abs_position(float &x, float &y, float &s, Pad_Bool useCoordFrames)
{
    float *frame;
    float zmult;

    x = anchorpt.x;
    y = anchorpt.y;
    s = transform.Get_scale();

    if (group) {
	group->Local_to_screen(x, y, s);
    }

			// If within a coordinate frame, convert size relative to that frame
    if (!Pad_coordFrames.Is_empty() && useCoordFrames) {
	frame = (float *)Pad_coordFrames.First();
	zmult = Compute_scale_within_frame(frame);
	s /= zmult;
    }
}

float
Pad_Object::Get_abs_position_x(void)
{
    float x = anchorpt.x;
    float y = 0;
    float s = 1.0;

    if (group) {
	group->Local_to_screen(x, y, s);
    }

    return(x);
}

float
Pad_Object::Get_abs_position_y(void)
{
    float x = 0;
    float y = anchorpt.y;
    float s = 1.0;

    if (group) {
	group->Local_to_screen(x, y, s);
    }

    return(y);
}

float
Pad_Object::Get_abs_position_z(void)
{
    float x, y, z;

    Get_abs_position(x, y, z);
    return(z);
}

//
// Generate a delete event (only if this object not already deleted).
// This can get called on sub-type destructers (after the derived-type
// destructor has already been called).
//
int
Pad_Object::Generate_delete(void)
{
    int result = PAD_OK;

    if (!(flags & PAD_MASK_DELETED)) {
	flags |= PAD_MASK_DELETED;
	result = Generate_event(Pad_DeleteNotify, NULL);
    }

    return(result);
}

//
// Generate an event of <type>, ignoring the result.
//
int
Pad_Object::Generate_event(int type, Pad_List *portals)
{
    return(Generate_event(type, portals, NULL));
}

//
// Generate an event of <type> and return the return string
// of the event in <result>
//
int
Pad_Object::Generate_event(int type, Pad_String &result)
{
    int rc = PAD_OK;
    XEvent event;
    Pad_Event *padEvent;
    Pad_Alias *alias;
    Pad_List *aliases;
    Pad_Iterator oi;

				// Don't generate event if they are turned off
    if (pad->padFlags & PADROOT_EVENTSOFF) {
	return(PAD_OK);
    }
				// Don't generate an event if object's
				// <Create> event hasn't been fired yet.
    if ((type == Pad_CreateNotify) || (flags & PAD_MASK_CREATED)) {
	if (type == Pad_CreateNotify) {
	    flags |= PAD_MASK_CREATED;
	}

				// Damage all aliases of this object on modify and delete
	if ((type == Pad_ModifyNotify) || (type == Pad_DeleteNotify)) {
	    aliases = Get_aliases();
	    if (aliases) {
		DOLIST(oi, *aliases, Pad_Alias, alias) {
		    alias->Update();
		}
	    }
	}

	event.type = type;
	padEvent = new Pad_Event(pad->view->win, &event);
	padEvent->result = &result;
	padEvent->Do(this);
	rc = padEvent->resultCode;

	delete padEvent;
    }
    
    return(rc);
}

//
// Generate an event of <type> with the specified <info>.
//
int
Pad_Object::Generate_event(int type, Pad_List *portals, const char *info)
{
    int result;
    XEvent event;
    Pad_Event *padEvent;
    Pad_Alias *alias;
    Pad_List *aliases;
    Pad_Iterator oi;

    result = PAD_OK;
				// Don't generate event if they are turned off
    if (pad->padFlags & PADROOT_EVENTSOFF) {
	return(result);
    }
				// Don't generate an event if object's
				// <Create> event hasn't been fired yet.
    if ((type == Pad_CreateNotify) || (flags & PAD_MASK_CREATED)) {
	if (type == Pad_CreateNotify) {
	    flags |= PAD_MASK_CREATED;
	}

				// Damage all aliases of this object on modify and delete
	if ((type == Pad_ModifyNotify) || (type == Pad_DeleteNotify)) {
	    aliases = Get_aliases();
	    if (aliases) {
		DOLIST(oi, *aliases, Pad_Alias, alias) {
		    alias->Update();
		}
	    }
	}

	event.type = type;
	event.xmotion.x = 0;    // Some default reasonable value
	event.xmotion.y = 0;
	event.xany.window = pad->view->win->id;
	padEvent = new Pad_Event(pad->view->win, &event);
	if (info) {
	    padEvent->info = info;
	}
	if (portals) {
	    padEvent->portals = portals;
	}
	padEvent->Do(this);
	result = padEvent->resultCode;

	delete padEvent;
    }
    
    return(result);
}

//
// Set the order of event firing.
// If type = "specific", fire most specific events first (id, tags, all)
// If type = "general",  fire most general events first (all, tags, id)
// Return FALSE if invalid type specified.
//
Pad_Bool
Pad_Object::Set_event_order(char *type)
{
    Pad_Bool rc;

    if (!strcmp(type, "general")) {
	flags &= ~PAD_MASK_ID_EVENT_FIRST;
	rc = TRUE;
    } else if (!strcmp(type, "specific")) {
	flags |= PAD_MASK_ID_EVENT_FIRST;
	rc = TRUE;
    } else {
	rc = FALSE;
    }
    return(rc);
}

//
// Return a static string specifying the event order.
//
char *
Pad_Object::Get_event_order(void)
{
    if (flags & PAD_MASK_ID_EVENT_FIRST) {
	return "specific";
    } else {
	return "general";
    }
}

//
// Set the id of this object to a unique id greater than
// all previously allocated objects.
// Return FALSE if it was previously set.
//
Pad_Bool 
Pad_Object::Set_id(void)
{
    int rc;
    int dummy;
    
    rc = (id == 0) ? TRUE : FALSE;
    if (pad) {
	pad->idTable->Set((char *)pad->objid, this);
	id = pad->objid;
	pad->objid++;
    } else {
	cerr << "INTERNAL ERROR: Can't set id of object not on a pad, " << this << endl;
	rc = FALSE;
    }

    return(rc);
}

//
// Set the id of this object to <new_id>.
// Return FALSE if unable to (because it already being used).
//
Pad_Bool 
Pad_Object::Set_id(int new_id)
{
    int rc;
    int dummy;
    Pad_Object *obj;
    
    if (pad) {
	obj = pad->Get_object_from_id(new_id);
	if (obj) {
	    rc = FALSE;
	} else {
	    Delete_id();
	    pad->idTable->Set((char *)new_id, this);
	    id = new_id;
	    rc = TRUE;
	}
    } else {
	cerr << "INTERNAL ERROR: Can't set id of object not on a pad, " << this << endl;
	rc = FALSE;
    }

    return(rc);
}

//
// Delete this object's id.
//
Pad_Bool 
Pad_Object::Delete_id(void)
{
    Pad_Bool rc;
    Pad_Object *obj;
    
    if (pad) {
	obj = (Pad_Object *)pad->idTable->Get((char *)id);
	if (obj) {
	    pad->idTable->Remove((char *)id);
	    id = 0;
	    rc = TRUE;
	} else {
	    cerr << "INTERNAL ERROR: Can't delete id of object, " << this << endl;
	    rc = FALSE;
	}
    } else {
	cerr << "INTERNAL ERROR: Can't set id of object not on a pad, " << this << endl;
	rc = FALSE;
    }

    return(rc);
}

//
// Add <tag> to <this>, maintaining the hashtable of tags.
// The hash table has one entry for each tag, where the entry
// is the list of objects sharing that tag, in display-list order.
// An object may only have one copy of a tag.
//
void 
Pad_Object::Add_tag(const char *tag)
{
    int newentry;
    Pad_Uid uid;
    Pad_List *objs;

    uid = Pad_GetUid(tag);
    tags.Push_new_last(uid);
    if (pad) {
	objs = (Pad_List *)pad->tagTable->Get(&uid);
	if (!objs) {
	    objs = new Pad_List;
	    pad->tagTable->Set(uid, objs);
	}
	_Insert_forward(*objs);
	optionFlags |= PAD_TAGS_SET;
    } else {
	cerr << "INTERNAL ERROR: Can't add a tag to an object not on a pad";
	cerr << this << ", tag = " << tag << endl;
    }
}

//
// Delete <tag> from <this>, maintaining the hashtable of tags.
// Return TRUE if tag was a member of this object.
//
Pad_Bool
Pad_Object::Delete_tag(const char *tag)
{
    Pad_Bool rc;
    Pad_Uid uid;
    Pad_List *objs;

    uid = Pad_GetUid(tag);
    rc = tags.Remove(uid);
    if (pad) {
	if (rc) {
	    objs = (Pad_List *)pad->tagTable->Get(uid);
	    if (!objs) {
		//cerr << "INTERNAL ERROR: Tag not in tagTable, " << tag << endl;
	    } else {
		if (!objs->Remove(this)) {
		    //cerr << "INTERNAL ERROR: Object not in tagTable, " << this << endl;
		}
	    }
	}
	optionFlags |= PAD_TAGS_SET;
    } else {
	cerr << "INTERNAL ERROR: Can't delete a tag from an object not on a pad";
	cerr << this << ", tag = " << tag << endl;
    }

    return(rc);
}

//
// Delete all tags from this object
// Return TRUE if there were any tags to delete.
//
Pad_Bool
Pad_Object::Delete_all_tags(void)
{
    Pad_Bool rc;
    Pad_Uid tag;

    rc = FALSE;
    while ((tag = (Pad_Uid)tags.First())) {
	rc |= Delete_tag(tag);
    }

    return(rc);
}

//
// Add an alias to this object.
// Return bool specifying if action was successful.
//
Pad_Bool
Pad_Object::Add_alias(Pad_Object *obj)
{
    Pad_List *aliases = Get_aliases();
    if (!aliases) {
        Pad_List emptyList;
	_Set_trait(PAD_ALIASES_TRAIT, &emptyList);
	optionFlags |= PAD_ALIASES_SET;
	aliases = Get_aliases();
    }
    aliases->Push_last(obj);

    return(TRUE);
}

//
// Get the alias list.  Return NULL if none.
//
Pad_List*
Pad_Object::Get_aliases(void)
{
    Pad_List *aliases=NULL;
    _Get_trait(PAD_ALIASES_TRAIT, aliases);
    return aliases;
}


//
// Remove an alias from this object.
// Return bool specifying if action was successful.
//
Pad_Bool
Pad_Object::Remove_alias(Pad_Object *obj)
{
    Pad_Bool rc;
    Pad_List *aliases = Get_aliases();

    if (aliases) {
	rc = aliases->Remove(obj);
	if (aliases->Is_empty()) {
	    _Remove_trait(PAD_ALIASES_TRAIT);
	    optionFlags &= ~PAD_ALIASES_SET;
	}
    } else {
	rc = FALSE;
    }

    return(rc);
}

//
// Get all objects aliased to this object.
//
void
Pad_Object::Get_aliases(Pad_List &aliases)
{
    Pad_Alias *alias;
    Pad_List items;
    Pad_Iterator oi;

    aliases.Make_empty();
    pad->Find_with_type("alias", items, TRUE);
    DOLIST(oi, items, Pad_Alias, alias) {
	if (alias->reference == this) {
	    aliases.Push_last(alias);
	}
    }
}

//
// Remove all aliases from this object.
//
void
Pad_Object::Remove_all_aliases(void)
{
    Pad_Alias *alias;
    Pad_List *aliases;

    while ((aliases = Get_aliases())) {
	alias = (Pad_Alias *)aliases->First();
				// Note: Set_reference removes an item from this
				// object's alias list with Remove_alias(), and 
				// when it is empty, the aliases list is deleted.
	alias->Set_reference(NULL);
    }
}

//
// Returns true if object should be sent through the render_object() method.
// However, this does not check if the object's bounding box is within the view.
// The caller must do that.
// This means that the:
//   * Object is within size range
//   * Object is on a visible layer
//   * Object is not a portal being rendered within itself
//
// Note that this returns true even when the object's -transparency option is 0
// (so that zoomactions can occur on objects that are not rendered.)
// This flag must be checked to determine if the object should actually be rendered.
//
// If the object is too small to be rendered now, the object's bounding box is 
// added to the refiningRestorer.
//
Pad_Bool 
Pad_Object::Check_for_render(void)
{
    Pad_Bool render = FALSE;
    float globalWidth, globalHeight;
    Pad_View *view;

    view = (Pad_View *)Pad_prc->views.Last();
				// Don't render portals within themselves recursively
    if (this != view) {
	if (Check_render_layer(view)) {
				// If object within size limits
	    Compute_dimensions(globalWidth, globalHeight);
	    if (Check_render_size()) {
		render = TRUE;
	    }
	}
    }

    return(render);
}

//
// Return true if the object is visible within the main view OR
// in any of the visiblePortals in the current view.  
// This can be true even when an object isn't getting rendered 
// because region management can cause just a portion of the screen
// to be rendered leaving visible objects unrendered.
//
Pad_Bool 
Pad_Object::Is_visible(void)
{
    // check the main view first
    Pad_Bool vis = Is_visible(pad->view->viewBBox.Get(), pad->view);
    if (vis) {
        return TRUE;
    }

    // check the portals
    Pad_Iterator iter;
    Pad_Portal *portal;
    DOLIST(iter, pad->view->visiblePortals, Pad_Portal, portal) {
        vis = Is_visible(portal->viewBBox.Get(), portal);
	if (vis) {
	    return TRUE;
	}
    }

    return FALSE;
}

//
// Return true if object is visible in a given view (and bbox).
//
Pad_Bool 
Pad_Object::Is_visible(float viewBB[4], Pad_View *view)
{
    Pad_Bool rc;
    float globalWidth, globalHeight;
    float bb[4];
    Pad_Bool bbox_visible;
    Pad_List tmpList;

    // First check the view bbox
    Get_global_bbox(bb);
    bbox_visible = Pad_Is_overlapping(bb, viewBB);

    if (bbox_visible) {
	if (Get_transparency() == 0) {
	    rc = FALSE;
	} else {
        // Check the layer and size constraints
	// NOTE: have to call Compute_dimensions() so it sets the
	// pixelDim and percentDim for the view :(
	    float orig_pixelDim = pixelDim;
	    float orig_percentDim = percentDim;
	    tmpList.Push(view);
	    Compute_dimensions(tmpList, globalWidth, globalHeight);
	    
	    if (Check_render_layer(view) &&
		Check_render_size()) {
		rc = TRUE;
	    }
	    else {
		rc = FALSE;
	    }
	    
	    pixelDim = orig_pixelDim;
	    percentDim = orig_percentDim;
	}
    } else {
        rc = FALSE;
    }
    
    return(rc);
}

//
// Return true if object should be rendered based on size.
//
Pad_Bool
Pad_Object::Check_render_size()
{
    Pad_Bool rc;
    Pad_Bool min_ok, max_ok;

    if (pixelDim > 0) {
	if (flags & PAD_MASK_MINSIZE_RELATIVE) {
	    min_ok = (percentDim >= minSize);
	} else {
	    min_ok = (pixelDim >= minSize);
	}
	
	if (flags & PAD_MASK_MAXSIZE_RELATIVE) {
	    max_ok = (percentDim <= maxSize);
	} else {
	    max_ok = (maxSize == -1) || (pixelDim <= maxSize);
	}

	rc = min_ok && max_ok;
    } else {
	rc = FALSE;
    }

    return(rc);
}
	

//
// Return true if object should be rendered based on layers.
// If asked if visible about another top-level view, then can't
// answer that, so answer about visibility within current top-level view
//
Pad_Bool
Pad_Object::Check_render_layer(Pad_View *view)
{
    Pad_Bool rc = FALSE;

    if (view->visibleLayers[0]) {
	rc = TRUE;
				// If view sees all layers, make sure this
				// layer isn't on the list of layers not seen
	if (view->invisibleLayers[layerId]) {
	    rc = FALSE;
	}
    } else if (view->visibleLayers[layerId]) {
	rc = TRUE;
    }

    return(rc);
}

//
// Return true to continue rendering other objects, or false if rendering
// was interrupted.
//
// This is a stub that should never be called.
// All objects that are rendered are derived from this.
//
Pad_Bool
Pad_Object::Render(void)
{
    return(TRUE);
}

//
// These gets called whenever the system is slow, and the object
// should render itself extra fast - even if that makes it ugly.
//
Pad_Bool
Pad_Object::Render_medium(void)
{
    return(Render());
}

Pad_Bool
Pad_Object::Render_fast(void)
{
    return(Render());
}


//
// Return true if an object was successfully rotated and false if 
// nothing was rotated.
//
// These are stubs that are called only if there is an attempt to rotate
// an object for which rotation has not been implemented.
//
Pad_Bool
Pad_Object::Is_rotatable(void) 
{
    return(FALSE);
}

Pad_Bool
Pad_Object::Rotate(float)
{
    if (!Get_lock()) {
	Pad_errorString += "Objects of type ";
	Pad_errorString += Type_name();
	Pad_errorString += " not rotatable\n";
	return(FALSE);
    }
    return(TRUE);
}

Pad_Bool
Pad_Object::Rotate(float, Pad_Point &)
{
    if (!Get_lock()) {
	Pad_errorString += "Objects of type ";
	Pad_errorString += Type_name();
	Pad_errorString += " not rotatable\n";
	return(FALSE);
    }
    return(TRUE);
}

//
// Fire render script, if there is one.
// Return true to continue rendering, or false to interrupt
//
Pad_Bool
Pad_Object::Fire_render_script(Pad_Bool &refine)
{
    int result;
    Pad_Bool rc;
    Pad_Bool prevDamageEnabled;
    Pad_Callback *renderScript = Get_renderscript();

    if (!renderScript || !Pad_prc) {
	return(TRUE);
    }

    prevDamageEnabled = pad->view->win->damageEnabled;
    pad->view->win->damageEnabled = FALSE;
    result = renderScript->Eval();
    if (result == PAD_ERROR) {
	Pad_Background_error("Pad render callback");
    }
    // Used to set refine if interp->result == "refine"
    // But interp->result is no longer used, so default to FALSE ... for now
    refine = FALSE;
    pad->view->win->damageEnabled = prevDamageEnabled;
    rc = Pad_prc->result;

    return(rc);
}

//
// By default, objects only have one refinement level. Hence return false.
//
Pad_Bool 
Pad_Object::Continue_refinement(void)
{
	return(FALSE);       // Most objects don't need refinement
}

//
// Returns true if object derived from Pad_Group
//
Pad_Bool 
Pad_Object::Is_group(void)
{
    return(FALSE);		// Most object aren't groups
}

//
// Returns true if object derived from Pad_Container
//
Pad_Bool 
Pad_Object::Is_container(void)
{
    return(FALSE);		// Most object aren't containers
}

//
// Returns true if object derived from Pad_Text
//
Pad_Bool 
Pad_Object::Is_text(void)
{
    return(FALSE);		// Most object aren't text objects
}

//
// Returns text object associated with this object (if there is one)
//
Pad_Text *
Pad_Object::Get_text_obj(void)
{
    return(NULL);		// Most object aren't text objects
}

//
// True if object should be sensitive to events
//
Pad_Bool
Pad_Object::Gets_events(void)
{
    return((flags & PAD_MASK_EVENTS) ? TRUE : FALSE);
}

//
// Clip if object has a render script,
// or if width or height is set.
//
Pad_Bool 
Pad_Object::Check_render_clipped(void)
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
	   Pad_Callback *renderScript = Get_renderscript();
	   if (renderScript ||
	       (optionFlags & PAD_WIDTH_SET) ||
	       (optionFlags & PAD_HEIGHT_SET)) {
	       rc = TRUE;
	   } else {
	       rc = FALSE;
	   }
	   break;
    }
    
    return(rc);
}

//
// Given a list of Pad_Object's, this computes the bounding
// box of those objects
//
void 
Pad_Object::Set_bounding_box_to_enclose_list(Pad_List &list)
{
    Pad_BBox bb;
    Pad_BBox obbox;
    Pad_Object *f;
    Pad_Iterator oi;
    Pad_Object *o;
    int first=1;
    
    /*
    f = (Pad_Object *)list.First();
    
    if (f) {
	f->Get_bbox(bb);
	obbox.Set(bb);
	f->transform.Apply(obbox);
    } else {
	obbox.Set_zero();
    }
    */
    
    obbox.Set_zero();
    DOLIST(oi, list, Pad_Object, o) { 
	if(o->boundable) {
	    o->Get_bbox(bb);
	    o->transform.Apply(bb);
            if(first) {
              obbox.Set(bb);
            }
	    obbox.Union(bb);
	    first=0;
        }
    }

    if (!(optionFlags & PAD_WIDTH_SET)) {
	bbox.Set_x(obbox.Xmin(), obbox.Xmax());
    }
    if (!(optionFlags & PAD_HEIGHT_SET)) {
	bbox.Set_y(obbox.Ymin(), obbox.Ymax());
    }
}

//
// Scale the object around the point (x, y) by zmult
// If <transform> is TRUE, then scale amount independent of group membership
//
Pad_Bool
Pad_Object::Scale(float x, float y, float zmult, Pad_Bool transformp)
{
    float dx, dy;
    float xoffset, yoffset;
    float dxoffset, dyoffset;
    float anchor_dx, anchor_dy;
    float anchor_dxoffset, anchor_dyoffset;
    float s;
    
    if (Type() == PAD_PAD) {
	return(FALSE);
    }

    if (Get_lock()) {
	return(TRUE);		// Can't modify if locked
    }

    if (transformp) {
	if (group) {
	    group->Screen_to_local(x, y, s);
	}
    }

    Damage();
    
    transform.Get_offset(xoffset, yoffset);
    dxoffset = x - xoffset;
    dyoffset = y - yoffset;
    transform.Scale(zmult);
    dx = (dxoffset * zmult) - dxoffset;
    dy = (dyoffset * zmult) - dyoffset;
    transform.Set_offset(xoffset - dx, yoffset - dy);
    anchor_dxoffset = x - anchorpt.x;
    anchor_dyoffset = y - anchorpt.y;
    anchor_dx = (anchor_dxoffset * zmult) - anchor_dxoffset;
    anchor_dy = (anchor_dyoffset * zmult) - anchor_dyoffset;
    anchorpt.x -= anchor_dx;
    anchorpt.y -= anchor_dy;
			     // Update requested position of sticky objects
    if (Get_sticky()) {
	Update_sticky();
    }

				// If item is a member of a group, then 
				// make sure group's bounding box is updated.
    if (group) {
	group->Compute_bounding_box();
    }
    Update_global_bounding_box();
    Damage();
    Generate_event(Pad_ModifyNotify, NULL, "scale");

    return(TRUE);
}

//
// Slide (translate) the object by (dx, dy).
// If <transform> is TRUE, then slide amount independent of group membership
// (i.e., (dx, dy) is modified based on size of groups).
//
Pad_Bool
Pad_Object::Slide(float dx, float dy, Pad_Bool transformp)
{
    Pad_Object *g;

    if (Type() == PAD_PAD) {
	return(FALSE);
    }

    if (Get_lock()) {
	return(TRUE);		// Can't modify if locked
    }

    if (transformp) {
	g = group;
	while (g) {
	    dx /= g->transform.Get_scale();
	    dy /= g->transform.Get_scale();
	    g = g->group;
	}
    }

    Damage();
    transform.Set_offset(transform.Get_offset_x() + dx, transform.Get_offset_y() + dy);
    anchorpt.x += dx;
    anchorpt.y += dy;

			     // Update requested position of sticky objects
    if (Get_sticky()) {
	Update_sticky();
    }

				// If item is a member of a group, then 
				// make sure group's bounding box is updated.
    if (group) {
	group->Compute_bounding_box();
    }
    Update_global_bounding_box();
    Damage();
    Generate_event(Pad_ModifyNotify, NULL, "slide");

    return(TRUE);
}

//
// True if the parameter, pad_object, overlaps with this
//
Pad_Bool
Pad_Object::Overlaps(Pad_Object *obj)
{
    Pad_Bool rc;
    Pad_BBox bb1, bb2;

    if (pad) {
	Get_global_bbox(bb1);
	obj->Get_global_bbox(bb2);
	rc = bb1.Overlaps(bb2);
    } else {
	rc = TRUE;
    }

    return(rc);
}

//
// True if <obj> is completely enclosed by <this>
//
Pad_Bool
Pad_Object::Enclosed(Pad_Object *obj)
{
    Pad_Bool rc;
    Pad_BBox bb1, bb2;

    if (Type() == PAD_PAD) {
	rc = TRUE;
    } else {
	if (pad && obj->pad) {
	    Get_global_bbox(bb1);
	    obj->Get_global_bbox(bb2);
	    rc = bb1.Enclosed(bb2);
	} else {
	    rc = TRUE;
	}
    }

    return(rc);
}

//
// True if <obj> is completely enclosed by <this>, or the same as <this>
//
Pad_Bool
Pad_Object::Enclosed_or_same(Pad_Object *obj)
{
    Pad_Bool rc;
    Pad_BBox bb1, bb2;

    if (Type() == PAD_PAD) {
	rc = TRUE;
    } else {
	if (pad && obj->pad) {
	    Get_global_bbox(bb1);
	    obj->Get_global_bbox(bb2);
	    rc = bb1.Enclosed_or_same(bb2);
	} else {
	    rc = TRUE;
	}
    }

    return(rc);
}

//
// This must get called when a derived type finishes
// computing its bounding box.  It updates any containing
// group's bbox, and updates the global bbox.
//
void 
Pad_Object::Compute_bounding_box(void) 
{
    int type;

    type = Type();
    if ((type == PAD_PAD) || (type == PAD_VIEW)) {
	return;
    }
				// If item is a member of a group, then 
				// make sure group's bounding box is updated.
    if (group) {
	group->Compute_bounding_box();
    }

    Update_global_bounding_box();
}

//
// This updates the global bounding box given that the
// local bounding box is up to date.
//
void
Pad_Object::Update_global_bounding_box(void)
{
    globalBBox = bbox;
    Local_to_screen(globalBBox.Get());
}

//
// Return the object dimensions in pixels, and as a percentage of the view
// based on the current render.
//
float
Pad_Object::Compute_dimensions(float &globalWidth, float &globalHeight)
{
    float dim;

    dim = Compute_dimensions(Pad_prc->views, globalWidth, globalHeight);

    return(dim);
}

//
// Return the object dimensions in pixels, and as a percentage of the view
// based on the specified event.
//
float
Pad_Object::Compute_dimensions(Pad_Event *event, float &globalWidth, float &globalHeight)
{
    float dim;
    float mag;
    Pad_Portal *portal;
    Pad_View *view;
    Pad_List views;
    Pad_Iterator oi;

				// Event portal lists don't contain the top-level window,
				// so push it on to the list of views.
    views = event->portals;
    portal = (Pad_Portal *)views.First();
    if (portal) {
				// If event went through a portal, then use the top-level view
				// that the first portal was on.
	views.Push(portal->pad->view);
    } else {
	views.Push(pad->view);
    }

    mag = 1.0;
    DOLIST(oi, views, Pad_View, view) {
				// If the portal is a member of a group, then we must
				// apply the groups transformation (recursively).
	if (view->group) {
	    view->group->Local_to_screen(mag);
	}
	mag *= view->zoom;
	mag *= view->transform.Get_scale();
    }

    dim = _Compute_dimensions(views, mag, globalWidth, globalHeight);

    return(dim);
}

//
// Return the object dimensions in pixels, and as a percentage of the view
// within the specified list of views.
//
float
Pad_Object::Compute_dimensions(Pad_List &views, float &globalWidth, float &globalHeight)
{
    float mag;
    Pad_View *view;
    Pad_Iterator oi;
    
    mag = 1.0;
    DOLIST(oi, views, Pad_View, view) {
				// If the portal is a member of a group, then we must
				// apply the groups transformation (recursively).
	if (view->group) {
	    view->group->Local_to_screen(mag);
	}
	mag *= view->zoom;
	mag *= view->transform.Get_scale();
    }

    return (_Compute_dimensions(views, mag, globalWidth, globalHeight));
}

float
Pad_Object::_Compute_dimensions(Pad_List &views, float mag, float &globalWidth, float &globalHeight)
{
    Pad_BBox bb;
    float objWidth, objHeight;
    float viewWidth, viewHeight;
    float widthPercent, heightPercent;
    Pad_View *view;
    
    view = (Pad_View *)views.Last();
    Get_global_bbox(bb);
				// objWidth & objHeight are in global coordinates,
				// independent of view.
    objWidth = bb.Width();
    objHeight = bb.Height();
				// globalWidth & globalHeight are the actual
				// size of the object on the screen in pixels.
    globalWidth = mag * objWidth;
    globalHeight = mag * objHeight;
				// Force thin objects to be visible
    if ((globalHeight > 1) && (globalWidth < 1) && (globalWidth > 0)) {
	globalWidth = 1.0;
    }
    if ((globalWidth > 1) && (globalHeight < 1) && (globalHeight > 0)) {
	globalHeight = 1.0;
    }
    if ((globalWidth > 0) && (globalHeight > 0)) {
	pixelDim = MAX(globalWidth, globalHeight);

	viewWidth =  view->viewBBox.Width();
	viewHeight = view->viewBBox.Height();
	    
	percentDim = 0;
	if ((viewWidth > 0) && (viewHeight > 0)) {
	    widthPercent = 100 * objWidth / viewWidth;
	    heightPercent = 100 * objHeight / viewHeight;
	    percentDim = MAX(widthPercent, heightPercent);
	}
    } else {
	percentDim = 0;
	pixelDim = 0;
    }

    return(pixelDim);
}

//
// Decide if this object should be fading in or out.
// Return 1.0 if normal (i.e., opaque), 0.0 if completely transparent,
// or a number inbetween representing its opacity.
//
float
Pad_Object::Compute_fade(void)
{
    float fade_size;
    float dim;
    float trans;
    
    trans = 1.0;		// By default, fully opaque

				// Fade-out
    if (flags & PAD_MASK_MAXSIZE_RELATIVE) {
	dim = percentDim;
    } else {
	dim = pixelDim;
    }
    if (maxSize != -1) {
	fade_size = maxSize - (fadeRange * maxSize);
	if (dim > fade_size)
	  {
	      if (dim > maxSize) {
		  trans = 0.0;
	      } else {
		  trans = (maxSize - dim) / (maxSize - fade_size);
	      }
	  }
    }
				// Fade-in
    if (flags & PAD_MASK_MINSIZE_RELATIVE) {
	dim = percentDim;
    } else {
	dim = pixelDim;
    }
    if (minSize != 0) {
	fade_size = minSize + (fadeRange * minSize);
	if (dim < fade_size) {
	    if (dim < minSize) {
		trans = 0.0;
	    } else {
		trans = (dim - minSize) / (fade_size - minSize);
	    }
	}
    }

    return(trans);
}

int 
Pad_Object::Sizeof_tree(void) 
{
    int s;
    Pad_Iterator oi;
    Pad_Object *obj;

    if (Type() == PAD_PAD) {
	obj = pad->first;
	s = 0;
	while (obj) {
	    s += obj->Sizeof_tree();
	    obj = obj->next;
	}
    } else {
	s = 1;
    }
    
    if (Is_group()) {
	DOLIST(oi, ((Pad_Group *)this)->members, Pad_Object, obj) {
	    s += obj->Sizeof_tree();
	}
    }
    
    return(s);
}

//
// Determine if the event is within <halo> of <this>'s bounding box.
//
Pad_Bool
Pad_Object::Pick(Pad_Event *event, float halo)
{
    float nearDist;
    float dx1, dy1, dx2, dy2;
    float bb[4];
    Pad_Bool rc;

    Get_bbox(bb);

    nearDist = halo / event->mag;
    dx1 = (bb[XMIN] - nearDist) - event->pt.x;
    dy1 = (bb[YMIN] - nearDist) - event->pt.y;
    dx2 = event->pt.x - (bb[XMAX] + nearDist);
    dy2 = event->pt.y - (bb[YMAX] + nearDist);

    if ((dx1 < 0) && (dy1 < 0) && (dx2 < 0) && (dy2 < 0)) {
	rc = TRUE;
    } else {
	rc = FALSE;
    }

    return(rc);
}

Pad_Bool
Pad_Object::Get_text(Pad_String &) 
{
    Pad_errorString = "Text option doesn't apply to ";
    Pad_errorString += Type_name();
    return(FALSE);
}

Pad_Bool
Pad_Object::Set_text(const char *)
{
    Pad_errorString = "Text option doesn't apply to ";
    Pad_errorString += Type_name();
    return(FALSE);
}

Pad_Bool
Pad_Object::Set_text_default(void)
{
    Pad_errorString = "Text option doesn't apply to ";
    Pad_errorString += Type_name();
    return(FALSE);
}

Pad_Bool
Pad_Object::Contains_text(const char *) 
{
    return(FALSE);
}

class Pad_ObjView
{
  public:
    Pad_BBox bbox;		// Screen coordinates of bounding box of obj for this view
    Pad_View *view;		// View object visible within
};

//
// Object has been determined to be visible within the list of
// specified portals, so calculate where in the window it will
// appear, and store the result in the specified list of views
// as a Pad_ObjView element
//
static void 
add_damaged_view(Pad_Object *obj, Pad_BBox &obbox, Pad_List *portals, Pad_List *views)
{
    Pad_BBox bbox;
    Pad_Portal *portal, *topPortal;
    Pad_View *view;
    Pad_ObjView *objview;
    Pad_Iterator pi;

    if (portals->Is_empty()) {	// The portal or view object visible within
	view = obj->pad->view;
    } else {
	view = (Pad_View *)portals->Last();
    }
    if (!view->win->Is_mapped()) {
	return;
    }
    bbox = obbox;

    DOLIST(pi, *portals, Pad_Portal, portal) {
	portal->Pass_through(bbox.Get());
    }
				// Don't add bbox if clipped to nothing
    if ((bbox.Width() <= 0) || (bbox.Height() <= 0)) {
	return;
    }

    objview = new Pad_ObjView;
    objview->view = view;

    topPortal = (Pad_Portal *)portals->Last();

				// view may be a Portal or Pad_View, and need to find
				// the surface the view is on.  Then, compute bbox
				// on that view.
    view->pad->view->Apply_view(bbox.Get());

    objview->bbox = bbox;

    views->Push_last(objview);
}

static Pad_List portals;

//
// Find portals object is visible within recursively.
//
static void 
damage_portal(Pad_Object *obj, float mag_context, Pad_Portal *portal, Pad_List *views)
{
    float mag;
    Pad_BBox pbbox;
    Pad_Portal *p;
    Pad_Iterator pi;
				// If visiblePortals list is out-of-date, then update it
    if (portal->dirtyPortals) {
	portal->Update_visibility();
    }
				// Add view of object within recursive portals
    DOLIST(pi, portal->visiblePortals, Pad_Portal, p) {
	if (!portals.Member(p)) {
	    portals.Push(p);
	    mag = mag_context * (p->transform.Get_scale() * p->zoom);
	    obj->Get_global_bbox(pbbox);
				// Convert bbox to true screen coordinates
	    if ((obj->pad->view->win == p->lookon) &&
		(Pad_Is_overlapping(pbbox.Get(), p->viewBBox.Get()))) {
		add_damaged_view(obj, pbbox, &portals, views);
	    }
	    damage_portal(obj, mag_context, p, views);
	    portals.Pop();
	}
    }
}

//
// Find places an object is visible on the primary surface, first-level portals, 
// and other surfaces it might be visible on through portals.
//
static void 
damage_view(Pad_Object *obj, Pad_BBox &obbox, Pad_View *view, Pad_List *views)
{
    float mag;
    Pad_BBox pbbox;
    Pad_Portal *portal;
    Pad_Iterator pi;
    Pad_Win *win;
    static Pad_List surfaces;
    static Pad_List newSurfaces;
    static int recurse = 0;

    recurse++;

    surfaces.Push_new_last(view->win);

				// Add view of object within its Pad_View
    if (view == obj->pad->view) {
	if (Pad_Is_overlapping(obbox.Get(), obj->pad->view->viewBBox.Get())) {
	    add_damaged_view(obj, obbox, &portals, views);
	}
    }
				// If visiblePortals list is out-of-date, then update it
    if (view->dirtyPortals) {
	view->Update_visibility();
    }
				// Check for visibility through portals on this surface
    DOLIST(pi, view->visiblePortals, Pad_Portal, portal) {
	if (obj->Check_render_layer(portal)) {
	    portals.Push(portal);
				// We must get the bbox of this object within the context of the portals.
				// This info must be manually specified because damage can be called
				// anywhere (in the context of a render, event, or nothing), so we
				// must specify the context (for objects whose bbox changes with magnification).
	    mag = (portal->transform.Get_scale() * portal->zoom);
	    obj->Get_global_bbox(pbbox);
	    if ((obj->pad->view->win == portal->lookon) &&
		(Pad_Is_overlapping(pbbox.Get(), portal->viewBBox.Get()))) {
		add_damaged_view(obj, pbbox, &portals, views);
	    }
	    damage_portal(obj, mag, portal, views);
	    portals.Pop();
	}
    }
				// Check for visibility through portals on other surfaces
    DOLIST(pi, view->win->lookonPortals, Pad_Portal, portal) {
	if (portal->win->view->dirtyPortals) {
	    portal->win->view->Update_visibility();
	}
	if (portal->currentlyVisible) {
	    if (!surfaces.Member(portal->win)) {
		newSurfaces.Push_new_last(portal->win);
	    }
	}
    }

    while ((win = (Pad_Win *)newSurfaces.Pop())) {
	damage_view(obj, obbox, win->view, views);
    }

    recurse--;
    if (recurse == 0) {
	newSurfaces.Make_empty();
	surfaces.Make_empty();
    }
}

//
// Mark areas currently covered by an object as changed (i.e., needing restoration)
// This includes all the portals and surfaces it is visible within.
//
void 
Pad_Object::Damage(void)
{
    Pad_Win *win;
    Pad_ObjView *objview;
    Pad_Restorer *r;
    Pad_Portal *portal;
    Pad_Iterator oi;
    Pad_List newRestorers;
    Pad_List views;
    Pad_Alias *alias;
    Pad_BBox obbox;

				// Disallow damage during render
    if (Pad_prc && !pad->view->win->damageEnabled) {
	return;
    }
				// Can't damage if object not completely initialized
				// (Can legally happen during object creation.)
    if (!pad || !pad->view || !pad->view->win) {
        return;
    }

    if (Type() == PAD_PORTAL) {
	pad->view->dirtyPortals = TRUE;
	DOLIST(oi, pad->view->win->viewPortals, Pad_Portal, portal) {
	    portal->dirtyPortals = TRUE;
	}
	DOLIST(oi, pad->view->win->lookonPortals, Pad_Portal, portal) {
	    portal->dirtyPortals = TRUE;
	}
    }
    
				// Find the places this object is visible
    Get_global_bbox(obbox);
    damage_view(this, obbox, pad->view, &views);
				// For every place this object is visible,
				// add that region to a restorer 
				// (creating and scheduling a restorer if necessary)
    DOLIST(oi, views, Pad_ObjView, objview) {
	win = objview->view->win;
	if (!win->biddingRestorer) {
	    win->biddingRestorer = new Pad_Restorer(win, FALSE);
	    newRestorers.Push_last(win->biddingRestorer);
	}
	if (!win->biddingRestorer->Is_full()) {
	    win->biddingRestorer->Add_rect(objview->bbox.Xmin(), objview->bbox.Ymin(),
					   objview->bbox.Xmax(), objview->bbox.Ymax());
	}
    }
    
    DOLIST(oi, newRestorers, Pad_Restorer, r) {
	r->Schedule(RESTORE_WHEN_IDLE, NULL);
    }

				// Free up list of generated views
    while ((objview = (Pad_ObjView *)views.Pop())) {
	delete objview;
    }

				// Damage all aliases of this object as well
    Pad_List *aliases = Get_aliases();
    if (aliases) {
	DOLIST(oi, *aliases, Pad_Alias, alias) {
	    alias->Damage();
	}
    }
}
