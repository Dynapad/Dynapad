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
#include "tree-layout.h"
#include "tree.h"
#include "list.h"
#include "line.h"
#include "object.h"
#include "pad.h"
#include "point.h"
#include "renderer.h"
#include "win.h"
#include "global.h"
#include "misc.h"

#include <math.h>
class Pad_TreeFocus;


//
// Helper function used by siso
//
static float 
bias(float b, float x) { 
    return pow(x, log10(b)/log10(0.5f));
}

//
// Do a slow-in-slow-out interpolation,
// mapping t_in to t_out on [0.0, 1.0].
// gain specifies how much distortion there is.
// gain of 0.0 means no distortion.
// gain of 0.8 is a reasonable value.
// t_in < 0.0 or > 1.0 maps linearly
//
static float 
siso(float gain, float t_in) {
    float t_out;

    if ((t_in < 0.0) || (t_in > 1.0)) {
	t_out = t_in;
    } else if (t_in < 0.5) {
	t_out = bias((1.0-gain), 2.0*t_in)/2.0;
    } else {
        t_out = 1.0 - (bias((1.0-gain),  (2.0 - 2.0*t_in))/2.0);
    }

    return(t_out);
}


//
//
//
Pad_TreeLayout::Pad_TreeLayout()
{
   Pad_Point pts[2];

   _anim_view = FALSE;

   _bbox_subtree[0] = 0.0;
   _bbox_subtree[1] = 0.0;
   _bbox_subtree[2] = 0.0;
   _bbox_subtree[3] = 0.0;

   _bbox_children[0] = 0.0;
   _bbox_children[1] = 0.0;
   _bbox_children[2] = 0.0;
   _bbox_children[3] = 0.0;

   pts[0].x = 0.0;
   pts[0].y = 0.0;
   pts[1].x = 0.0;
   pts[1].y = 0.0;

   _focus_obj = new Pad_TreeFocus(this);
   _link_id   = 0;
   _penwidth_isfixed   = FALSE;
   _animation_speed = 500;
   _treenode  = NULL;
   _offset    = new Pad_Point; 
   _offset->x = 0.0;
   _offset->y = 0.0;
}



Pad_TreeLayout::Pad_TreeLayout(Pad_TreeNode *t)
{
   Pad_PList pts;
   Pad_Point pt;
   Pad_Line *line;
   float new_penwidth;

   _bbox_subtree[0] = 0.0;
   _bbox_subtree[1] = 0.0;
   _bbox_subtree[2] = 0.0;
   _bbox_subtree[3] = 0.0;

   _bbox_children[0] = 0.0;
   _bbox_children[1] = 0.0;
   _bbox_children[2] = 0.0;
   _bbox_children[3] = 0.0;

   pt.Set(0, 0);
   pts.Push_last(&pt);
   pt.Set(0, 0);
   pts.Push_last(&pt);

   _focus_obj = new Pad_TreeFocus(this);

   if (t) {
   _treenode = t;
   } else {
     printf("ERROR - null pad object passed to Pad_TreeLayout\n");
   }

   if (_treenode->Get_pad()) {
       new_penwidth = t->Get_obj()->transform.Get_scale();
       line = new Pad_Line(_treenode->Get_pad());
       line->Set_transparency(1.0);
       line->Set_events(FALSE);
       line->Set_coords(pts, FALSE);
       line->Set_pen("black");
       line->Set_penwidth(new_penwidth);
       line->Set_capstyle("projecting");
       line->Add_tag("treelink");
       _link_id = line->id;
       _penwidth_isfixed   = FALSE;
   }

   _offset   = new Pad_Point; 
   _offset->x = 0.0;
   _offset->y = 0.0;
}



Pad_TreeLayout::~Pad_TreeLayout()
{
   if (_focus_obj) {
      delete _focus_obj;
      _focus_obj = NULL;
   }
   if (_offset) {
      delete _offset;
      _offset = NULL;
   }
   _treenode->Get_pad()->Delete_obj(_link_id);
   _link_id = 0;
}

//
// Return Pad_Object that this layout's link is represented by
// if one exists, or NULL if not.
//
Pad_Object *
Pad_TreeLayout::Get_link(void)
{
    Pad_Object *obj;

    obj = _treenode->Get_pad()->Get_object_from_id(_link_id);
	
    return(obj);
}

//
//
//
void
Pad_TreeLayout::Adjust_link()
{
    Pad_Object *obj;

    obj = Get_link();
    if (obj && _treenode->Get_parent() && _treenode->Get_parent()->Get_obj()) {
	obj->Lower(_treenode->Get_parent()->Get_obj());
    }
}

//
// Call this function if link is deleted by outsider
//
void
Pad_TreeLayout::Link_deleted(void)
{
    _link_id = 0;
}

//
// calculates the values for the _bbox_subtree and _bbox_node
// values at the node 
//
void
Pad_TreeLayout::Calc_bboxes(void) 
{
    Pad_Iterator pi;
    Pad_TreeNode *node;
    float bb_c[4] = { 1000000.0, 1000000.0, -1000000.0, -1000000.0 };
    float bb_n[4] = { 0.0, 0.0, 0.0, 0.0 };
    int i=0;

    if (_treenode->Get_children()->Is_empty()) {
	_treenode->Get_global_bbox(_bbox_children);
	_treenode->Get_global_bbox(_bbox_subtree);

    } else {
	DOLIST(pi, *(_treenode->Get_children()), Pad_TreeNode, node) {
	    node->Get_layout()->Calc_bboxes();
	    node->Get_layout()->Get_bbox_subtree(bb_n);
	    
	    if (bb_n[0] < bb_c[0]) bb_c[0] = bb_n[0]; 
	    if (bb_n[1] < bb_c[1]) bb_c[1] = bb_n[1]; 
	    if (bb_n[2] > bb_c[2]) bb_c[2] = bb_n[2]; 
	    if (bb_n[3] > bb_c[3]) bb_c[3] = bb_n[3]; 
	}
	
	for (i=0;i<4;i++) {
	    _bbox_children[i] = bb_c[i];
	}
	
	_treenode->Get_global_bbox(bb_n);
	
	if (bb_n[0] < bb_c[0]) bb_c[0] = bb_n[0]; 
	if (bb_n[1] < bb_c[1]) bb_c[1] = bb_n[1]; 
	if (bb_n[2] > bb_c[2]) bb_c[2] = bb_n[2]; 
	if (bb_n[3] > bb_c[3]) bb_c[3] = bb_n[3]; 
	
	for (i=0;i<4;i++) {
	    _bbox_subtree[i] = bb_c[i];
	}
    }
}



//
//
// Layout the tree
//
void
Pad_TreeLayout::Layout(void)
{
   Layout_main();
}



//
//
// Layout the tree
//
void
Pad_TreeLayout::Layout_node(void)
{
   //Layout_main(this);
   Layout_main();
}



//
//
// Layout the tree
//
void
Pad_TreeLayout::Layout_main(void)
{
   Pad_Iterator  pi;
   Pad_TreeNode *node;

   if (_treenode->Get_obj() && (_treenode->Get_obj()->Get_transparency() > 0)) Connect_children();

   DOLIST(pi, *(_treenode->Get_children()), Pad_TreeNode, node)
   {
      node->Get_layout()->Connect_children();
   }
}



void
Pad_TreeLayout::Set_bbox_children(float *bb)
{
   _bbox_children[0] = bb[0];
   _bbox_children[1] = bb[1];
   _bbox_children[2] = bb[2];
   _bbox_children[3] = bb[3];
}



void
Pad_TreeLayout::Set_bbox_subtree(float *bb)
{
   _bbox_subtree[0] = bb[0];
   _bbox_subtree[1] = bb[1];
   _bbox_subtree[2] = bb[2];
   _bbox_subtree[3] = bb[3];
}



void 
Pad_TreeLayout::Propogate_focus(float f, int level, float falloff)
{
    if (_focus_obj) {
	_focus_obj->Propogate_focus(f, level, falloff);
    } 
}



//
//
//
void
Pad_TreeLayout::Set_link_coords(Pad_PList &pts)
{
    Pad_Object *obj;

    obj = Get_link();
    if (obj && (obj->Get_transparency() > 0)) {
	obj->Set_coords(pts, FALSE);
    }
}



//
//
//
void
Pad_TreeLayout::Set_link_visible(Pad_Bool b) 
{
    Pad_Object *obj;

    obj = Get_link();
    if (obj) {
	if (b) {
	    obj->Set_transparency(1);
	} else {
	    obj->Set_transparency(0);
	}
    }
}


//
//
//
void
Pad_TreeLayout::Set_treenode(Pad_TreeNode *t) 
{
   _treenode = t;
}


//
//
//
void 
Pad_TreeLayout::Set_dist(float, float) {
}



//
// links the node to its parent and children 
//
void 
Pad_TreeLayout::Connect_node(void)
{
    Pad_PList     pts;
    Pad_Point     pt;
    float         bbox_n[4], bbox_p[4];
    Pad_TreeNode  *parent;

    parent = _treenode->Get_parent();
    if (parent) {
	if (parent->Get_obj()) {
	    _treenode->Get_global_bbox(bbox_n);
	    parent->Get_global_bbox(bbox_p);
	    
	    pt.Set(bbox_p[2], bbox_p[1] + 0.5*(bbox_p[3]-bbox_p[1]));
	    pts.Push_last(&pt);
	    pt.Set(bbox_n[0], bbox_n[1] + 0.5*(bbox_n[3]-bbox_n[1]));
	    pts.Push_last(&pt);
	    
	    Set_link_coords(pts);
	}
	if (_treenode->Get_obj() && (_treenode->Get_obj()->Get_transparency() > 0)) {
	    Connect_children();
	}
    }
}



//
//
//
void
Pad_TreeLayout::Connect_children(void)
{
    Pad_PList     pts;
    Pad_Point     pt;
    Pad_Iterator  pi;
    Pad_TreeNode *node;
    float         bbox_s[4];
    float         bbox_n[4];
    Pad_Object *  link;

    _treenode->Get_global_bbox(bbox_s);

    DOLIST(pi, *(_treenode->Get_children()), Pad_TreeNode, node)
    {
       node->Get_global_bbox(bbox_n);
       pts.Make_empty();
       pt.Set(bbox_s[2], bbox_s[1] + 0.5*(bbox_s[3]-bbox_s[1]));
       pts.Push_last(&pt);
       pt.Set(bbox_n[0], bbox_n[1] + 0.5*(bbox_n[3]-bbox_n[1]));
       pts.Push_last(&pt);

       node->Get_layout()->Set_link_coords(pts);

				// Update penwidth of link depending on scale of child
       link = node->Get_layout()->Get_link();
       if( node->Get_layout()->Get_link_mode() == TRUE )
	 link->Set_penwidth(0.0);
       else
	 link->Set_penwidth(node->Get_obj()->transform.Get_scale()/node->Get_treenode_scale());
    }
}



void
Pad_TreeLayout::Get_bbox_children(float *bb)
{
   bb[0] = _bbox_children[0];
   bb[1] = _bbox_children[1];
   bb[2] = _bbox_children[2];
   bb[3] = _bbox_children[3];
}


//
// return the future bbox of a node
//
void 
Pad_TreeLayout::Get_bbox_final(float *bb)
{
  bb[0] = 0;
  bb[1] = 0;
  bb[2] = 0;
  bb[3] = 0;
}




void
Pad_TreeLayout::Get_bbox_subtree(float *bb)
{
   bb[0] = _bbox_subtree[0];
   bb[1] = _bbox_subtree[1];
   bb[2] = _bbox_subtree[2];
   bb[3] = _bbox_subtree[3];
}

void
Pad_TreeLayout::Hide_link(Pad_Bool b) 
{
    Pad_Object *obj;

    obj = Get_link();
    if (obj) {
	if (b) {
	    obj->Set_transparency(1);
	} else {
	    obj->Set_events(FALSE);
	    obj->Set_transparency(0);
	}
    }
}


//
//
//
void
Pad_TreeLayout::Reset_view_state()
{
}

//
//
//
Pad_Bool
Pad_TreeLayout::Set_animation_speed(int t) 
{
    Pad_Iterator pi;
    Pad_TreeNode *node;

    if (t >= 0) {
	_animation_speed = t;
    } else {
	Pad_errorString = "Error: Tree animation speed must be > 0";
	return(FALSE);
    }

    DOLIST(pi, *(_treenode->Get_children()), Pad_TreeNode, node) {
	node->Get_layout()->Set_animation_speed(t);
    }
    
    return(TRUE);
}


void
Pad_TreeLayout::Set_link_mode(Pad_Bool isfixed) 
{
 Pad_Iterator pi;
 Pad_TreeNode *node;

  _penwidth_isfixed = isfixed;
    
  DOLIST(pi, *(_treenode->Get_children()), Pad_TreeNode, node) {
      node->Get_layout()->Set_link_mode(isfixed);
  }
}

Pad_Bool 
Pad_TreeLayout::Get_link_mode() 
{
  return _penwidth_isfixed;
}



/////////////////////////////////////////////////////////////
// 
// Pad_TreeLayoutDefault
// 
/////////////////////////////////////////////////////////////
void
Pad_TreeLayout_Default::Init()
{
   _anim_view    = FALSE;
   _dest_v.x     = 0.0;
   _dest_v.y     = 0.0;
   _dest_init.x  = 0.0;
   _dest_init.y  = 0.0;
   _dest_init_z  = 0.0;
   _dest_dz      = 0.0;
   _dest_end.x   = 0.0;
   _dest_end.y   = 0.0;
   _dist.x       = 10.0;
   _dist.y       = 10.0;
   _siso_val     = 0.8;
   _animation_speed = 500;
   _view_x_init  = 0.0;
   _view_y_init  = 0.0;
   _view_z_init  = 0.0;
   _view_x_final = 0.0;
   _view_y_final = 0.0;
   _view_z_final = 0.0;
   _z_ratio     = 0.0;
}



Pad_TreeLayout_Default::Pad_TreeLayout_Default(void):Pad_TreeLayout()
{
   Init();
}



Pad_TreeLayout_Default::Pad_TreeLayout_Default(Pad_TreeNode *t):Pad_TreeLayout(t)
{
   Init();
}


Pad_TreeLayout_Default::~Pad_TreeLayout_Default() {
}


void 
Pad_TreeLayout_Default::Get_bbox_final(float *bb)
{
  float tbb[4];
  float off[2];

  _treenode->Get_obj()->Get_global_bbox(tbb);

  off[0] = -(tbb[XMIN] + 0.5*(tbb[XMAX] - tbb[XMIN]));
  off[1] = -(tbb[YMIN] + 0.5*(tbb[YMAX] - tbb[YMIN]));

  // take midpoint of left side to origin
  bb[XMIN] = tbb[XMIN] + off[0]; 
  bb[YMIN] = tbb[YMIN] + off[1]; 
  bb[XMAX] = tbb[XMAX] + off[0]; 
  bb[YMAX] = tbb[YMAX] + off[1]; 

  // scale for the future
  bb[XMIN] *= _z_ratio; 
  bb[YMIN] *= _z_ratio;
  bb[XMAX] *= _z_ratio;
  bb[YMAX] *= _z_ratio;

  // place at the future location 
  bb[XMIN] = bb[XMIN] - off[0] + _dest_v.x;
  bb[YMIN] = bb[YMIN] - off[1] + _dest_v.y;
  bb[XMAX] = bb[XMAX] - off[0] + _dest_v.x;
  bb[YMAX] = bb[YMAX] - off[1] + _dest_v.y;
}



void
Pad_TreeLayout_Default::Layout(void)
{
    float x, y;
    float bb[4];

    _treenode->Get_obj()->Get_global_bbox(bb);
    x = bb[0];
    y = bb[1] + 0.5*(bb[3]-bb[1]);

    Calc_bboxes();
    Set_final_layout_state(x, y);
    Layout_main();
}



void
Pad_TreeLayout_Default::Layout_node(void)
{
    float x, y;
    float bb[4];

    _treenode->Get_global_bbox(bb);
    x = bb[0];
    y = 0.5*(bb[1]+bb[3]);

    Calc_bboxes();
    Set_final_layout_state(x, y);
    Layout_main(); 
}





void 
Pad_TreeLayout_Default::Layout_main()
{
    float t;
    float current_wait;
    float desired_wait;
    long startSec;
    long startUsec;
    Pad_Bool rc;
    Pad_View *view;
    Pad_Time time;

    if (_animation_speed > 0) {
				// Turn internal event generation off while performing animation
	_treenode->Get_pad()->Set_event_control(FALSE);

				// Record current time
	time.Update();
	startSec = time.Get_sec();
	startUsec = time.Get_usec();

	desired_wait = _animation_speed;
	t = 0.0;
	view = _treenode->Get_obj()->pad->view;
	
	do {
	    Anim_tree(t);
				// Animate view along with tree
	    if (_anim_view) {
		Anim_view(t);
	    }
	    view->Damage();            // Damage entire surface
	    view->Update_display();    // Force damage to get rendered
	    
				// Calculate total elapsed time
	    time.Update();
	    current_wait = (1000000 * (time.Get_sec() - startSec) + (time.Get_usec() - startUsec)) / 1000;
	      
				// Calculate next interpolation step
	    t = current_wait / desired_wait;
	    t = siso(_siso_val, t); // Convert interpolation to slow-in-slow-out
		
				// Check for interruption
	    rc = Pad_renderer->Is_interrupted(view->win);
	} while ((t < 1.0) && !rc);
    }
    Anim_tree(1.0);
    if (_anim_view) {
	Anim_view(1.0);
	_anim_view = FALSE;
    }
    Reset_view_state();
				// Turn internal event generation back on
    _treenode->Get_pad()->Set_event_control(TRUE);
}

//
// using time, varying between 0.0 and 1.0, produce the next
// set of positions for the tree
//
void
Pad_TreeLayout_Default::Anim_view(float time)
{
    int prev_fast_pan;
    Pad_View *view;

    float x = (_view_x_init + time*(_view_x_final));
    float y = (_view_y_init + time*(_view_y_final));
    float z = (_view_z_init + time*(_view_z_final));

				// HACK ALERT:  Fast panning seems to be somewhat broken
				// when panning and changing object positions at the
				// same time.  So, turn off fast panning temporarily,
				// and then restore its state.
    view = _treenode->Get_pad()->view;
    prev_fast_pan = view->win->fastPan;
    view->win->fastPan = FALSE;

    view->Set_view(x, y, z, FALSE);
    view->win->fastPan = prev_fast_pan;
}



//
// using time, varying between 0.0 and 1.0, produce the next
// set of positions for the tree 
// 
void
Pad_TreeLayout_Default::Anim_tree(float time)
{
   float x, y, z;
   Pad_Iterator pi;
   Pad_TreeNode *node;
   Pad_TreeLayout_Default *lyt;
   Pad_Object *obj;

   x       = _dest_init.x + time*(_dest_v.x);
   y       = _dest_init.y + time*(_dest_v.y);
   z       = _dest_init_z + time*(_dest_dz);
  
   obj = _treenode->Get_obj();
   obj->Set_rel_position(x, y, z);

   DOLIST(pi, *(_treenode->Get_children()), Pad_TreeNode, node)
   {
      lyt = (Pad_TreeLayout_Default *)(node->Get_layout());
      lyt->Anim_tree(time);
   }
  
   if (_treenode->Get_obj() && (_treenode->Get_obj()->Get_transparency() > 0)) {
       Connect_children();
   }
}


//
//
//
void
Pad_TreeLayout_Default::Reset_view_state () 
{
   _view_x_init  = 0;
   _view_y_init  = 0;
   _view_z_init  = 0;

   _view_x_final = 0;
   _view_y_final = 0;
   _view_z_final = 0;
}


//
//
//
void 
Pad_TreeLayout_Default::Set_final_view_state(float x, float y, float z)
{
   // get the current zoom

   _view_x_init  = _treenode->Get_pad()->view->xview;
   _view_y_init  = _treenode->Get_pad()->view->yview;
   _view_z_init  = _treenode->Get_pad()->view->zoom;
   _view_x_final = x - _view_x_init;
   _view_y_final = y - _view_y_init;
   _view_z_final = z - _view_z_init;

   _anim_view = TRUE;
}


//
//  Sets the animation vector information at each node
//
void 
Pad_TreeLayout_Default::Set_final_layout_state(float x, float y)
{
    Pad_Iterator pi;
    float new_x, new_y;
    Pad_TreeNode *node;
    Pad_TreeLayout_Default *lyt;
    float bb[4];
    float magnified_distx, magnified_disty;

    _treenode->Get_global_bbox(bb);
    magnified_distx = LERP(_treenode->Get_focus(), _dist.x, _dist.x * _treenode->Get_focus_mag());
    magnified_disty = LERP(_treenode->Get_focus(), _dist.y, _dist.y * _treenode->Get_focus_mag());

    // _z_ratio is for future size   z_future/z_initial

    if (_treenode->Get_obj() && (_treenode->Get_obj()->Get_transparency() > 0)) {
	new_x = x + _z_ratio*(bb[2] - bb[0]) + magnified_distx;
    } else {
	new_x = x + _z_ratio*(bb[2] - bb[0]);
    }
    new_y = y + 0.5*(_bbox_children[3]); 

    _dest_end.x  = x + _z_ratio*0.5*(bb[2] - bb[0]);
    _dest_end.y  = y;

    _dest_v.x += _dest_end.x - _dest_init.x;
    _dest_v.y += _dest_end.y - _dest_init.y;

    DOLIST(pi, *(_treenode->Get_children()), Pad_TreeNode, node) {
	lyt = (Pad_TreeLayout_Default *)(node->Get_layout());
	new_y -= 0.5*(lyt->_bbox_subtree[3]);
	lyt->Set_final_layout_state(new_x, new_y);
	new_y -= magnified_disty + 0.5*(lyt->_bbox_subtree[3]);
    }
}


void
Pad_TreeLayout_Default::Set_dist(float x, float y) 
{
    Pad_Iterator pi;
    Pad_TreeNode *node;

    _dist.x = x;
    _dist.y = y;
    
    DOLIST(pi, *(_treenode->Get_children()), Pad_TreeNode, node) {
	node->Get_layout()->Set_dist(x, y);
    }
}


//
// calculates the "zoomed" state of the bboxes for all the nodes
//
void
Pad_TreeLayout_Default::Calc_bboxes(void) 
{
    Pad_Iterator pi;
    Pad_TreeNode *node;
    float obj_zoom;
    float focus, focus_mag;
    float bb_c[4] = { 0.0, 0.0, 0.0, 0.0 };
    float bb_n[4] = { 0.0, 0.0, 0.0, 0.0 };
    int i=0;
    float magnified_distx, magnified_disty;
    float _x_current, _y_current, _z_current, z_future;

    focus     = _treenode->Get_focus();
    focus_mag = _treenode->Get_focus_mag();
    obj_zoom  = _treenode->Get_treenode_scale();


    magnified_distx = LERP( focus, _dist.x, _dist.x * focus_mag );
    magnified_disty = LERP( focus, _dist.y, _dist.y * focus_mag );

    _dest_init_z	= _treenode->Get_obj()->transform.Get_scale();

    z_future = LERP(focus, obj_zoom, focus_mag * obj_zoom);
    _dest_dz = z_future - _dest_init_z;
    _z_ratio = z_future / _dest_init_z;

    _treenode->Get_obj()->Get_rel_position(_x_current, _y_current, _z_current);

    if (_treenode->Get_children()->Is_empty()) {
     
	_treenode->Get_global_bbox(bb_c);

	_bbox_children[0] = 0.0; 
	_bbox_children[1] = 0.0; 
	_bbox_children[2] = _z_ratio*(bb_c[2] - bb_c[0]);
	_bbox_children[3] = _z_ratio*(bb_c[3] - bb_c[1]);

	for (i=0;i<4;i++) _bbox_subtree[i] = _bbox_children[i];

	_dest_init.x = _x_current;
	_dest_init.y = _y_current; 
	_dest_v.x = ( _x_current - 0.5*(bb_c[2]+bb_c[0]) ) * _z_ratio ;
	_dest_v.y = ( _y_current - 0.5*(bb_c[3]+bb_c[1]) ) * _z_ratio ; 

    } else {

	for (i=0;i<4;i++) {
	    _bbox_children[i] = 0.0; 
	    _bbox_subtree[i]  = 0.0;
	}

	DOLIST(pi, *(_treenode->Get_children()), Pad_TreeNode, node)
	    {
		node->Get_layout()->Calc_bboxes();
		node->Get_layout()->Get_bbox_subtree(bb_n);

		if (bb_n[2] > _bbox_children[2]) _bbox_children[2] = bb_n[2]; 
		_bbox_children[3] += bb_n[3];
	    }

	    _bbox_children[3] += (_treenode->Get_children()->Length() - 1) * magnified_disty;

	    _treenode->Get_global_bbox(bb_n);

	    _bbox_subtree[0] = 0.0;
	    _bbox_subtree[1] = 0.0;
	    _bbox_subtree[2] = _z_ratio*(bb_n[2] - bb_n[0]) + magnified_distx + _bbox_children[2];
	    _bbox_subtree[3] = _z_ratio*(bb_n[3] - bb_n[1]); 

	    if (_bbox_subtree[3] < _bbox_children[3]) _bbox_subtree[3] = _bbox_children[3]; 

	    _dest_init.x = _x_current;
	    _dest_init.y = _y_current; 
	    _dest_v.x = ( _x_current - 0.5*(bb_n[2]+bb_n[0]) ) * _z_ratio;
	    _dest_v.y = ( _y_current - 0.5*(bb_n[3]+bb_n[1]) ) * _z_ratio; 

    }
}




////////////////////////////////////////////////////////////////////////
// 
//   Pad_TreeFocus 
//
////////////////////////////////////////////////////////////////////////

int Pad_TreeFocus::_master_visit_level = 0;

Pad_TreeFocus::Pad_TreeFocus(void)
{
   _layout        = NULL;
   _visit_level  = 0;
}



Pad_TreeFocus::Pad_TreeFocus(Pad_TreeLayout *l)
{
   if (l) _layout = l;
   else {
     printf("ERROR - null treelayout passed to Pad_TreeFocus\n");
   }
   _visit_level = 0;
}



//
//
//
Pad_TreeFocus::~Pad_TreeFocus()
{
   if (_layout) {
     _layout = NULL;
   } 
}


//
// Propogate the specified focus through the tree starting at this node,
// and propogating <level> levels deep.  If <level> is -1, then propogate
// through entire tree.
//
Pad_Bool
Pad_TreeFocus::Propogate_focus(float new_focus, int level, float falloff) 
{
    Pad_Bool rc;

    _master_visit_level++;
    rc = _Propogate_focus(new_focus, level, _master_visit_level, falloff);

    return(rc);
}

//
// Helper function for Propogate_focus.
//
Pad_Bool
Pad_TreeFocus::_Propogate_focus(float new_focus, int level, int new_visit_level, float falloff) 
{
    Pad_Iterator  pi;
    Pad_TreeNode *node = NULL;
    Pad_TreeNode *parent;
    Pad_List     *children;
    Pad_TreeFocus *focus_node;

    if (new_visit_level != _visit_level) {
	_visit_level = new_visit_level;
	if (_layout) {
				// Self
	    _layout->Get_treenode()->Set_focus(new_focus);
	    if (level != 0) {   // Recurse until level = 0, or if level=-1, recurse entire tree
				// Focus decreases for each level
		new_focus *= falloff;
		parent = _layout->Get_treenode()->Get_parent();
		if (parent) {
				// Parent
		    focus_node = parent->Get_layout()->Get_focus_obj();
		    focus_node->_Propogate_focus(new_focus, level - 1, new_visit_level, falloff);
		}

				// Children
		children = _layout->Get_treenode()->Get_children();
		DOLIST(pi, *children, Pad_TreeNode, node) {
		    focus_node = node->Get_layout()->Get_focus_obj();
		    focus_node->_Propogate_focus(new_focus, level - 1, new_visit_level, falloff);
		}
	    }
	} else {
	    printf("ERROR - this focus object has no _layout\n");
	}
    }

    return TRUE;
}
