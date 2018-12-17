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

#ifndef PADNODE_H
#define PADNODE_H 1

#include "defs.h"
#include "list.h"

class Pad;
class Pad_Object;
class Pad_TreeRootObj;
class Pad_TreeLayout;



//////////////////////////////////////////////////////////////////////
// 
// Base class for Pad trees
//
//////////////////////////////////////////////////////////////////////

// NOTES:
//
// _zoom 
//   is a measure of the object's "tree zoom" - when an object is
//   attached to the tree, it is assumed to be the right size, and therefor
//   its tree zoom is a copy of the object zoom at the time the node
//   is created 
//

class Pad_TreeNode
{
  public:

     // tree node functionality
     // -----------------------

             Pad_Bool         Add_node( Pad_TreeNode *c );
             Pad_Bool         Add_node_safe( Pad_TreeNode *c );
             Pad_Bool         Add_node( Pad_Object  *o );
             Pad_Bool         Delete_node(Pad_Bool b);
             Pad_Bool         Delete_subtree(Pad_Bool b);
	     Pad_List       * Get_children(void);
             float            Get_focus(void) { return _focus; };
             float            Get_focus_mag(void) { return _focus_mag; };
	     Pad_TreeNode   * Get_parent( void );
	     Pad_TreeNode   * Get_root(void);
     virtual void             Print( void );
     virtual void             Print_subtree( void );
             Pad_Bool         Remove_node( Pad_TreeNode *c );
             Pad_Bool         Reparent( Pad_TreeNode *rp );
             Pad_Bool         Reparent_subtree( Pad_TreeNode *rp );


     // Pad++ specific stuff
     // --------------------

	     void             Get_bbox( float *bbox );	// BBox of the node only
	     void             Get_global_bbox( float *bbox );	// BBox of the node only
	     void             Get_bbox_children( float *bbox );	// BBox of the node only
	     void             Get_bbox_subtree( float *bbox );	// BBox of the node only
	     Pad_TreeLayout * Get_layout();
	     Pad_Object *     Get_obj();
	     Pad  *           Get_pad();
             float            Get_treenode_scale(void) { return _scale; };
     virtual void             Layout( void );
     virtual void             Layout_node( void );
             void             Lower(void);
             void             Lower(Pad_TreeNode *t);
     virtual void             Make_node_root( void );
             void             Raise(void);
             void             Raise(Pad_TreeNode *t);
             void             Propogate_focus(float f, int levels);
             void             Propogate_focus(float f, int levels, float falloff);
             void             Set_focus( float f );
             Pad_Bool         Set_focus_mag(float f);
	     void             Set_layout( Pad_TreeLayout *l );
             void             Set_obj( Pad_Object *o );
             Pad_Bool         Set_treenode_scale(float newScale);


     // construction/destruction 
     // ------------------------

                              Pad_TreeNode( void ); 
                              Pad_TreeNode( Pad_Object *o ); 
                              Pad_TreeNode( Pad_TreeNode *p ); 
                              Pad_TreeNode( Pad_TreeNode *p, Pad *pad ); 
                              Pad_TreeNode( Pad *pad ); 
                              Pad_TreeNode( Pad_Object *o, Pad_TreeNode *p ); 
                              Pad_TreeNode( Pad_Object *o, Pad_TreeNode *p, Pad_TreeLayout *l);
     virtual                 ~Pad_TreeNode();

  protected:
     virtual Pad_Object      *Create_dummy_object(Pad *pad);

     Pad_List                _children;
     float                   _focus;	// degree of focus at a node
     float                   _focus_mag;// Dynamic range of focus size (from a focus of 0 to 1)
     Pad_List               *_foci;     // list of foci on this tree
     Pad_TreeLayout         *_layout;	// layout object owned by this node
     Pad_Object             *_obj;	// pad object maintained by this node
     Pad_TreeNode           *_parent;
     float		     _scale;	// Scale of object when at a focus of 0

};

#endif
