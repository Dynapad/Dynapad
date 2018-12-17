/*
"(c) Copyright 1993-1995 Pad++ Consortium {University of New Mexico (UNM),
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

#include "defs.h"
#include "list.h"
#include "line.h"
#include "object.h"
#include "pad.h"
#include "tree.h"
#include "tree-layout.h"
#include "global.h"

#include <stdlib.h>
#include <stdio.h>

#define TREE_FOCUS_MAG   5      // Default dynamic range of focus size
#define TREE_FALLOFFRATE 0.5	// Default size decrease rate with distance from focus





////////////////////////////////////////////////////////////////////////
// 
//   Pad Tree Nodes
//
////////////////////////////////////////////////////////////////////////

//
// Root node layout
//
Pad_TreeNode::Pad_TreeNode(void)
{
   _focus  = 0.0;
   _focus_mag = TREE_FOCUS_MAG;
   _layout = NULL;
   _obj    = NULL;
   _parent = NULL;
   _scale   = 0.0;
}



Pad_TreeNode::Pad_TreeNode(Pad_Object *o)
{
   if (o) {
      _focus  = 0.0;
      _focus_mag = TREE_FOCUS_MAG;
      _layout = NULL;
      _obj    = o;
      _parent = NULL;
      _scale   = _obj->transform.Get_scale();

      _obj->Set_treenode(this);

   } else {
     printf("ERROR - null object passed to Pad_TreeNode\n");
   }
}



Pad_TreeNode::Pad_TreeNode(Pad_TreeNode *p)
{
   _focus  = 0.0;
   _focus_mag = TREE_FOCUS_MAG;
   _parent = p;
   if (p) _parent->Add_node(this);
   _layout = NULL;
   _obj    = NULL; 
   _scale   = 1.0;
}


Pad_TreeNode::Pad_TreeNode(Pad_TreeNode *p, Pad *pad)
{
   _focus  = 0.0;
   _focus_mag = TREE_FOCUS_MAG;
   _parent = p;
   _obj    = Create_dummy_object(pad); 
   _layout = new Pad_TreeLayout_Default(this); 
   _scale   = 1.0;
   if (p) _parent->Add_node(this);
}


Pad_TreeNode::Pad_TreeNode(Pad_Object *o, Pad_TreeNode *p)
{
   if (o && p) {
      _focus  = 0.0;
      _focus_mag = TREE_FOCUS_MAG;
      _obj    = o;
      _parent = p;
      _scale   = _obj->transform.Get_scale();

      _layout = new Pad_TreeLayout_Default(this); 
      _obj->Add_tag( "treenode");
      _obj->Set_treenode(this);
      if (p) _parent->Add_node(this);
   } else if (!o) {
      printf("ERROR - null object passed to Pad_TreeNode\n");
   } else if (!p) {
      printf("ERROR - null parent passed to Pad_TreeNode\n");
   }
}



Pad_TreeNode::Pad_TreeNode(Pad_Object *o, Pad_TreeNode *p, Pad_TreeLayout *l)
{
   if (o && p && l) {
      _focus  = 0.0;
      _focus_mag = TREE_FOCUS_MAG;
      _layout = l; 
      _obj    = o;
      _parent = p;
      _scale   = _obj->transform.Get_scale();

      _obj->Add_tag( "treenode");
      _obj->Set_treenode(this);
      if (p) _parent->Add_node(this);
   } else if (!o) {
      printf("ERROR - null object passed to Pad_TreeNode\n");
   } else if (!p) {
      printf("ERROR - null parent passed to Pad_TreeNode\n");
   } else if (!l) {
      printf("ERROR - null layout passed to Pad_TreeNode\n");
   }
}


Pad_TreeNode::~Pad_TreeNode()
{
    Pad_TreeNode *child;
    if (_layout) {
	delete _layout;
	_layout = NULL;
    }
				// If we are not the tree root, then promote children to be
				// children of our parent, and then remove ourselves
				// from parent's list of children.
    if (this != _obj->pad->treeroot) {
	while ((child = (Pad_TreeNode *)_children.First())) {
	    _parent->Add_node(child);     // Add_node automatically removes child from <this>
	    child->Lower(this);
	}
	_parent->Remove_node(this);
    } else {
				// Else, we are the tree root, so recursively delete
				// all tree nodes and links.
	while ((child = (Pad_TreeNode *)_children.First())) {
	    child->Delete_subtree(FALSE);
	}
    }
    _obj->Delete_tag("treenode");
    _obj->Set_treenode(NULL);
    _obj = NULL;
}


Pad_TreeNode *
Pad_TreeNode::Get_root(void)
{
   Pad_TreeNode *node=this;

   while (node->_parent->Get_parent() != NULL) node = node->_parent;
   return node;
}

//
//
//
Pad_Object * 
Pad_TreeNode::Create_dummy_object (Pad *pad)
{
   Pad_Line *l;
   Pad_Point pt;
   Pad_PList pts;

   l = new Pad_Line(pad);

   pt.Set(0, 0);
   pts.Push_last(&pt);

   l->Set_transparency(0);
   l->Set_events(FALSE);
   l->Set_penwidth(0.0);	// Necessary in order to keep bbox zeroed
   l->Set_coords(pts, FALSE);
   l->Add_tag("treenode");

   l->Set_treenode(this);

   return l;
}



//
// Deletes a tree node.
// If <with_obj> is true, then delete object associated with tree node as well.
//
Pad_Bool
Pad_TreeNode::Delete_node(Pad_Bool with_obj)
{
    Pad_TreeNode *child;

    if (_parent) {
	while ((child = (Pad_TreeNode *)_children.First())) {
	    _parent->Add_node(child);     // This automatically removes child from <this>
	    child->Lower(this);
	}
    } 

    if (with_obj && _obj) {
	delete _obj;
    } else {
	delete this;
    }

    return TRUE;
}



//
// Delete entire subtree of this node including this one.
// If <with_obj> is true, then delete objects associated with tree nodes as well.
//
Pad_Bool
Pad_TreeNode::Delete_subtree(Pad_Bool with_obj)
{
    Pad_TreeNode *child;

    while ((child = (Pad_TreeNode *)_children.Pop())) {
	child->Delete_subtree(with_obj);
    }

    if (with_obj && _obj) {
	delete _obj;
    } else {
	delete this;
    }

    return TRUE;
}



//
// Add <newchild> to become a child of <this> node.
// do not add if a cycle is created.
//
Pad_Bool 
Pad_TreeNode::Add_node_safe(Pad_TreeNode *newchild)
{
    Pad_TreeNode *node=this;

    if (!newchild) {
	return FALSE;
    } else {
	if (newchild == this) {
	    return FALSE;
	} else {

	    // check for the creation of cycle

	    node = this;
	    while (node->_parent != NULL){
		if( node->_parent == newchild){
		    Pad_errorString = "Can't add node because a cycle would be formed";
		    return FALSE;
		}
		node = node->Get_parent();
	    }

	    // everything seems okay
	    Add_node(newchild);
	    return TRUE;

	}
    }
}

//
// Add <child> to become a child of <this> node.
//
Pad_Bool 
Pad_TreeNode::Add_node(Pad_TreeNode *child)
{
    if (child) {
				// If child belongs to someone else, remove it.
       if (child->Get_parent()) {
	   child->Get_parent()->Remove_node(child);
       }
				// Construct node pointers
       child->_parent = this;
       _children.Push_last(child);
       child->Set_focus_mag(_focus_mag);

				// Deal with new child's link
       if (_obj && !(_obj->Get_transparency() > 0)) {
	   if (child->Get_layout()) {
	       child->Get_layout()->Adjust_link();
	       child->Get_layout()->Hide_link(FALSE);
	   }
       } else {
	   if (child->Get_layout()) child->Get_layout()->Hide_link(TRUE); 
       }

       return TRUE;
    } else {
       printf("ERROR - adding NULL child\n");
       return FALSE;
    }
}



//
// Add_node to this node
//
Pad_Bool 
Pad_TreeNode::Add_node(Pad_Object *object)
{
    if (object) {
       return Add_node(object->Get_treenode());
    } else {
       printf("ERROR - adding NULL child\n");
       return FALSE;
    }
}



//
//
//
Pad_TreeNode *
Pad_TreeNode::Get_parent(void)
{
   return _parent;
}


//
// Lower this to the bottom of the list
//
void
Pad_TreeNode::Lower(void)
{
    Pad_List *children;

    if (_parent) {
	children = _parent->Get_children();
	children->Remove(this);
	children->Push_last(this);
    }
}



//
// Lower this just before <ref>
//
void
Pad_TreeNode::Lower(Pad_TreeNode *ref)
{
    Pad_List *children;

    if (_parent) {
	children = _parent->Get_children();
	children->Remove(this);
	children->Insert_before(this, ref);
    }
}



//
//
//
void
Pad_TreeNode::Make_node_root(void)
{
    if (_parent) {
	_parent->Remove_node(this);
    }
}


//
// Raise this to the top of the list
//
void
Pad_TreeNode::Raise(void)
{
    Pad_List *children;

    if (_parent) {
	children = _parent->Get_children();
	children->Remove(this);
	children->Push(this);
    }
}



//
// Raise this just after <ref>
//
void
Pad_TreeNode::Raise(Pad_TreeNode *ref)
{
    Pad_List *children;

    if (_parent) {
	children = _parent->Get_children();
	children->Remove(this);
	children->Insert_after(this, ref);
    }
}



//
//  Remove_node
//
Pad_Bool
Pad_TreeNode::Remove_node(Pad_TreeNode *child)
{
    _children.Remove(child);
    child->_parent = NULL;

    return(TRUE);
}


//
// Reparent this subtree
//
Pad_Bool 
Pad_TreeNode::Reparent_subtree(Pad_TreeNode *newparent)
{
    if (_parent) {
       _parent->Remove_node(this);
       newparent->Add_node_safe(this);
       return TRUE;
    } else {
       // report error - tried to reparent the root
       return FALSE;
    }
}



//
// Reparent this node
//
Pad_Bool 
Pad_TreeNode::Reparent(Pad_TreeNode *newparent)
{
    Pad_TreeNode *child;

    if (_parent) {
       while ((child = (Pad_TreeNode *)_children.Pop())) {
         _parent->Add_node_safe(child);
       }
       _children.Make_empty();
       _parent->Remove_node(this);
       newparent->Add_node_safe(this);
       return TRUE;
    } else {
      // report error - tried to reparent the root node
      return FALSE;
    }
}


//
// set the focus val at a node
//
void
Pad_TreeNode::Set_focus(float f) 
{
    if ((f >= 0) && (f <= 1.0)) {
	_focus = f;
    } else {
	_focus = 1;
	printf("ERROR - incorrect value for _focus - %f\n", f);
    }
}

//
// Set the dynamic range of focus for the entire subtree
// starting at this node.
//
Pad_Bool
Pad_TreeNode::Set_focus_mag(float f) 
{
    Pad_Bool rc;
   Pad_Iterator oi;
   Pad_TreeNode *child;

    if (f > 0) {
	_focus_mag = f;
	DOLIST(oi, _children, Pad_TreeNode, child) {
	    child->Set_focus_mag(f);
	}
	rc = TRUE;
    } else {
	Pad_errorString = "Invalid focusmag.  Must be > 0";
	rc = FALSE;
    }

    return(rc);
}

//
// Set the scale of this tree node to be used when at
// a focus of 0.
//
Pad_Bool
Pad_TreeNode::Set_treenode_scale(float newScale)
{
    Pad_Bool rc;

    if (newScale > 0) {
	_scale = newScale;
	rc = TRUE;
    } else {
	Pad_errorString = "Invalid scale.  Must be > 0";
	rc = FALSE;
    }

    return(rc);
}

//
// return the list of children
//
Pad_List *
Pad_TreeNode::Get_children(void)
{
    return (&_children);
}



//
// call the layout object at a node, if present
//
void
Pad_TreeNode::Layout(void)
{
   if (_layout) {
   _layout->Layout();
   } else {
     printf("ERROR - no layout present at node\n");
   }
}



//
// call Layout_node on the layout object, if present
//
void
Pad_TreeNode::Layout_node(void)
{
   if (_layout) {
   _layout->Layout_node();
   } else {
     printf("ERROR - no layout present at node\n");
   }
}



void
Pad_TreeNode::Print(void)
{
   if (_obj) {
      printf("%d ", _obj->id);
   } else {
      printf("no object in node\n");
   }
}



void 
Pad_TreeNode::Print_subtree(void)
{
   Pad_Iterator oi;
   Pad_TreeNode *node;

   Print();
   printf(" - ");

   DOLIST(oi, _children, Pad_TreeNode, node)
   {
      if (node) {
        node->Print();
      }
   }
   printf("\n");

   DOLIST(oi, _children, Pad_TreeNode, node)
   {
      if (node) {
        node->Print_subtree();
      }
   }
   
}



void
Pad_TreeNode::Get_bbox(float *bbox)
{
   _obj->Get_bbox(bbox);
}


void
Pad_TreeNode::Get_global_bbox(float *bbox)
{
   _obj->Get_global_bbox(bbox);
}


void
Pad_TreeNode::Get_bbox_children(float *bbox)
{
   _layout->Get_bbox_children(bbox);
}



void
Pad_TreeNode::Get_bbox_subtree(float *bbox)
{
   _layout->Get_bbox_subtree(bbox);
}



Pad_TreeLayout *
Pad_TreeNode::Get_layout(void)
{
  return _layout;
}



Pad_Object *
Pad_TreeNode::Get_obj(void)
{
  return _obj;
}



Pad *
Pad_TreeNode::Get_pad(void)
{
  return _obj->pad;
}



void
Pad_TreeNode::Propogate_focus(float f, int levels)
{
    Propogate_focus(f, levels, TREE_FALLOFFRATE); 
}


void
Pad_TreeNode::Propogate_focus(float f, int levels, float falloff)
{
    _layout->Propogate_focus(f, levels, falloff); 
}


void 
Pad_TreeNode::Set_layout(Pad_TreeLayout *new_layout) 
{
    if (_layout) {
	delete _layout;
    }
    _layout = new_layout;
}


void
Pad_TreeNode::Set_obj(Pad_Object *o)
{
   if (_obj) {
      _obj->Set_treenode(NULL);
   }
   _obj = o;
}
