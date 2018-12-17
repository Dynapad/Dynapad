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

#ifndef TREELAYOUT_H
#define TREELAYOUT_H 1

#include "defs.h"
#include "list.h"
#include "point.h"

class Pad_Point;
class Pad_TreeNode;
class Pad_TreeFocus;
class Pad_Line;
class Pad_Object;



//
// Base class for layout 
// will connect nodes per the treenode hierarchy
//
class Pad_TreeLayout
{
  public:
    virtual void           Adjust_link();
    virtual void           Calc_bboxes();
    virtual void           Connect_node();             // draw parent and children links 
    virtual void           Connect_children();         // draw parent and children links 
    virtual void           Get_bbox_children(float *f);
    virtual void           Get_bbox_subtree(float *f);
    virtual void           Get_bbox_final(float *f);
    virtual Pad_TreeFocus *Get_focus_obj(void) { return _focus_obj; };
    virtual int            Get_animation_speed(){ return _animation_speed; };
    virtual Pad_TreeNode  *Get_treenode() { return _treenode; };
    virtual Pad_Object    *Get_link(void);
    virtual Pad_Bool       Get_link_mode(void);
    virtual void           Hide_link(Pad_Bool b);
    virtual void           Layout();	                // go to top of hierarchy
    virtual void           Layout_main(void);
    virtual void           Layout_node();	        // start from this node
    virtual void           Link_deleted();              // Link was deleted by someone else
    virtual void           Propogate_focus(float f, int level, float falloff);
    virtual void           Reset_view_state();
    virtual Pad_Bool       Set_animation_speed(int t); 
    virtual void           Set_animate_view(Pad_Bool b) { _anim_view = b; };
    virtual void           Set_final_layout_state(float x, float y)=0;
    virtual void           Set_final_view_state(float x, float y, float z)=0; 
    virtual void           Set_anim_view(Pad_Bool b) { _anim_view = b; };
    virtual void           Set_bbox_children(float *f);
    virtual void           Set_bbox_subtree(float *f);
    virtual void           Set_link_coords(Pad_PList &pts);
    virtual void           Set_link_visible(Pad_Bool b); 
    virtual void           Set_dist(float x, float y);
    virtual void           Set_link_mode(Pad_Bool isfixed );

    virtual void           Set_focus_obj(Pad_TreeFocus *f) { _focus_obj = f; };
    virtual void           Set_treenode(Pad_TreeNode *node);

		           Pad_TreeLayout();
		           Pad_TreeLayout(Pad_TreeNode *n);
    virtual               ~Pad_TreeLayout();

  protected:
    Pad_Bool             _anim_view;		// animate the view during state change?
    float                _bbox_children[4];     // BBox of subtree not including self
    float                _bbox_subtree[4];      // BBox of subtree including self
    Pad_TreeFocus       *_focus_obj;
    int                  _link_id;              // Id of link object
    Pad_Bool             _penwidth_isfixed;     // if link is displayed at constant size;
    int                  _animation_speed;      // length of animation in milliseconds
    Pad_TreeNode        *_treenode;	        // owner of this layout
    Pad_Point           *_offset;
};



//
//
//
class Pad_TreeLayout_Default : public Pad_TreeLayout
{
  public:
    virtual void     Calc_bboxes();
    virtual void     Get_bbox_final(float *f);
    virtual void     Layout();
    virtual void     Layout_main(void); 
    virtual void     Layout_node();
    virtual void     Reset_view_state();
    virtual void     Set_dist(float x, float y);
    virtual void     Set_final_layout_state(float x, float y);
    virtual void     Set_final_view_state(float x, float y, float z); 

		     Pad_TreeLayout_Default(void);
		     Pad_TreeLayout_Default(Pad_TreeNode *t);
    virtual         ~Pad_TreeLayout_Default();

  protected:
    Pad_Point        _dest_v;      // destination vector 
    Pad_Point        _dest_init;   // destination v init
    float	     _dest_init_z; // initial zoom value;
    float            _dest_dz;     // destination zoom
    Pad_Point        _dest_end;    // destination point
    Pad_Point        _dist;        // amount of distortion in x, y
    float            _siso_val;    // slow in slow out value
    float            _z_ratio;     // factor for calcs, zfuture/zcurrent
    float            _view_x_init; // view animation information
    float            _view_y_init;
    float            _view_z_init;
    float            _view_x_final;
    float            _view_z_final;
    float            _view_y_final;


  private:
            void     Init(void);
	    void     Anim_view(float time);
	    void     Anim_tree(float time);
};

class Pad_TreeLayout;




//////////////////////////////////////////////////////////////////////
// 
// Base class for Pad_TreeFocus
// a class that manages the focus of a Dynamic tree
//
//////////////////////////////////////////////////////////////////////


class Pad_TreeFocus
{
  public:
     virtual Pad_Bool  Propogate_focus(float f, int level, float falloff);

                       Pad_TreeFocus(void); 
                       Pad_TreeFocus(Pad_TreeLayout *l); 
     virtual          ~Pad_TreeFocus();

  protected:
    static int       _master_visit_level;     // Master recursion id of all tree focii
    int              _visit_level;            // Equal to the recursion id
    Pad_TreeLayout  *_layout;

  private:
     virtual Pad_Bool  _Propogate_focus(float f, int level, int visit_level, float falloff);
};



#endif

