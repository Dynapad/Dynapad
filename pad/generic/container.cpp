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
#include "container.h"
#include "global.h"
#include "pad.h"
#include "view.h"
#include "renderer.h"
#include "win.h"
#include "bind.h"
#include "callback.h"
#include "display.h"
#include "menu.h"

#include <ctype.h>
#include <stdlib.h>

#define CONTAINER_DEFAULT_HILITEWIDTH 0                   // Focus border
#define CONTAINER_DEFAULT_WIDTH 200                       // Default width
#define CONTAINER_DEFAULT_HEIGHT 200                      // Default height

#define WINDOW_DEFAULT_RESIZEABLE 1                       // Window are resizeable by default

#define FRAME_BORDER_WIDTH 7                              // Border width of window
#define FRAME_TITLE_WIDTH  2                              // Border width of window title
#define FRAME_TITLE_HEIGHT 17                             // Height of title bar (includes FRAME_TITLE_WIDTH)
#define FRAME_MIN_WIDTH    (2 * (FRAME_BORDER_WIDTH + FRAME_TITLE_HEIGHT))
#define FRAME_MIN_HEIGHT   (2 * (FRAME_BORDER_WIDTH + FRAME_TITLE_HEIGHT))
#define FRAME_MENUBAR_HEIGHT 28


//////////////////////////////////////////////////////////////
//
// Pad_Container class
//
//////////////////////////////////////////////////////////////

Pad_Container::Pad_Container(Pad *pad) :
Pad_Group(pad)
{
    _type = PAD_CONTAINER;

    Set_clipping(CLIPPING_TRUE);
    Set_hilite_width(CONTAINER_DEFAULT_HILITEWIDTH);
    Set_anchor(PAD_ANCHOR_SW);

    Add_tag("Container");
}

Pad_Container::~Pad_Container()
{
    Damage();			// Must damage container before it gets deleted
				// to insure it the entire area gets repainted properly.
}

//
// Returns true if object derived from Pad_Container
//
Pad_Bool 
Pad_Container::Is_container(void)
{
    return(TRUE);
}

//
// Move component into container
//
Pad_Bool
Pad_Container::Add(Pad_Object *obj, Pad_Bool)
{
    float dx, dy;
    Pad_Bool rc;
    Pad_BBox bb;

    Get_bbox(bb);

    dx = bb.Xmin();
    dy = bb.Ymin();

    obj->Slide(dx, dy);
    rc = Pad_Group::Add(obj, FALSE);

    return(rc);
}

//
// Move component out of container
//
Pad_Bool
Pad_Container::Remove(Pad_Object *obj, Pad_Bool)
{
    float dx, dy;
    Pad_Bool rc;
    Pad_BBox bb;

    Get_bbox(bb);

    dx = - bb.Xmin();
    dy = - bb.Ymin();

    obj->Slide(dx, dy);
    rc = Pad_Group::Remove(obj, FALSE);

    return(rc);
}

//
// The minimum dimensions necessary for the window to look normal
//
void
Pad_Container::Minimum_size(Pad_Dimension &dimension)
{
    dimension.width = CONTAINER_DEFAULT_WIDTH;
    dimension.height = CONTAINER_DEFAULT_HEIGHT;
}

void
Pad_Container::Preferred_size(Pad_Dimension &dimension)
{
    Minimum_size(dimension);
}

//
// A container should use the component's bounding box
// mechanism - not groups.
//
void
Pad_Container::Compute_bounding_box(void)
{
    Pad_Component::Compute_bounding_box();
}

//
// Set the width of a container.
//
Pad_Bool
Pad_Container::Set_width(float width)
{
				// Bypass Pad_Group's method
    return(Pad_Component::Set_width(width));
}

//
// Set the height of a group.
// Recursively sets the height of all the group members.
//
Pad_Bool
Pad_Container::Set_height(float height)
{
				// Bypass Pad_Group's method
    return(Pad_Component::Set_height(height));
}

//
// Reshape, and keep members properly positioned
// Position is relative to parent's SW corner
//
void
Pad_Container::Reshape(float x, float y, float width, float height)
{
    float dx, dy;
    Pad_Object *obj;
    Pad_Iterator oi;

				// Set bit here because otherwise, sliding an
				// object could cause this objects bbox to
				// be recomputed, which could cause reshape
				// to get called recursively, ad infinitum...
    _componentFlags |= COMPONENT_RESHAPE_SET;

				// Keep members in the same relative position
				// to the container's *NW* corner.  (This is
				// necessary because Java expects the origin
				// to be in the NW.
    dx = x - _activeArea.x;
    dy = (y + height) - (_activeArea.y + _activeArea.height);

    DOLIST(oi, members, Pad_Object, obj) {
	obj->Slide(dx, dy);
    }

    Pad_Component::Reshape(x, y, width, height);
}

//
// Make containers get events if there is no member that gets it
//
Pad_Bool
Pad_Container::Pick_group(Pad_Event *)
{
    return(TRUE);
}

//
// Gets called with string args when object is created.  This routine must 
// process any arguments up to, but not including the options.  (argc, argv) 
// point to parameters on the creation line just after 'create <type>'.
// This returns the number of args processed.
//
int
Pad_Container::Create_obj_args(int argc, char **argv)
{
    int pts, coords;
    float x1, y1, x2, y2;
		
				// Process args to find how many coords are specified
    coords = 0;
    while ((coords < argc) &&
	   ((argv[coords][0] != '-') || (isdigit(argv[coords][1])) || (argv[coords][1] == '.'))) {
	    coords++;
    }
    pts = coords / 2;
    
    if ((coords != 0) && (coords != 4)) {
	Pad_errorString = "wrong # args: should be \"pathName create ";
	Pad_errorString += Type_name();
	Pad_errorString += " ?x1 y1 x2 y2? ?option value ...?\"";
	return(-1);
    }

    if (coords == 0) {
	return(0);
    }

    x1 = ATOXF(pad->view->win, argv[0]);
    y1 = ATOYF(pad->view->win, argv[1]);
    x2 = ATOXF(pad->view->win, argv[2]);
    y2 = ATOYF(pad->view->win, argv[3]);

    Reshape(x1, y1, x2 - x1, y2 - y1);

    Update_and_compute_anchor();

    return(coords);
}

//////////////////////////////////////////////////////////////
//
// Pad_Panel class
//
//////////////////////////////////////////////////////////////

Pad_Panel::Pad_Panel(Pad *pad) :
Pad_Container(pad)
{
    _type = PAD_PANEL;

    Delete_all_tags();
    Add_tag("Panel");
}

//
// Render a Panel
//
Pad_Bool
Pad_Panel::Render(void)
{
				// Render panel's background
    if (_fillColor.Is_set()) {
	float dx, dy;

				// Hack to make panel fill up clipping region
				// so components don't stick out.  This is a by-product
				// of the fact that the clipping region is slightly
				// bigger than the bounding box to ensure that items
				// don't clipped incorrectly due to rounding error.
	dx = _activeArea.width * 0.1;
	dy = _activeArea.height * 0.1;
	Pad_renderer->Set_color(_fillColor);
	Pad_renderer->Draw_filled_box(_activeArea.x - dx, _activeArea.y - dy, 
				      _activeArea.x + _activeArea.width + 2*dx, 
				      _activeArea.y + _activeArea.height + 2*dy);
    }

				// Render panel's components
    Pad_Group::Render();

    return(TRUE);
}

//
// If pointer moves to a panel, update window that panel is a member of.
//
void
Pad_Panel::Pointer_in(Pad_Event *padEvent)
{
    if (group) {
	group->Pointer_in(padEvent);
    }
}

void
Pad_Panel::Pointer_out(Pad_Event *padEvent)
{
    if (group) {
	group->Pointer_out(padEvent);
    }
}

//////////////////////////////////////////////////////////////
//
// Pad_Window class
//
//////////////////////////////////////////////////////////////

Pad_Window::Pad_Window(Pad *pad) :
Pad_Container(pad)
{
    _type = PAD_WINDOW;

    _windowFlags = PAD_NO_MASK;
    _moveCallback = NULL;
    _resizeCallback = NULL;
    _destroyCallback = NULL;
    _prevFocus = NULL;

    Set_resizeable_default();

    Delete_all_tags();
    Add_tag("Window");
}

Pad_Window::~Pad_Window()
{
    if (_destroyCallback) {
	_destroyCallback->Eval();
	delete _destroyCallback;
	_destroyCallback = NULL;
    }
    if (_moveCallback) {
	delete _moveCallback;
	_moveCallback = NULL;
    }
    if (_resizeCallback) {
	delete _resizeCallback;
	_resizeCallback = NULL;
    }
    if (_prevFocus) {
	delete _prevFocus;
	_prevFocus = NULL;
    }
}

//
// Render a Window
//
Pad_Bool
Pad_Window::Render(void)
{
    float bb[4];
				// Render window's background
    if (_fillColor.Is_set()) {
	Pad_renderer->Set_color(_fillColor);
	Pad_renderer->Draw_filled_box(_activeArea.x, _activeArea.y, 
				      _activeArea.x + _activeArea.width, 
				      _activeArea.y + _activeArea.height);
    }

				// Render window's components
				// Clip contents to window
    Get_bbox(bb);
    bb[XMIN] += _inset.left;
    bb[YMIN] += _inset.bottom;
    bb[XMAX] -= _inset.right;
    bb[YMAX] -= _inset.top;
    
    Pad_prc->win->activeRestorer->Push_clip(bb);
    Pad_Group::Render();
    Pad_prc->win->activeRestorer->Pop_clip();

    return(TRUE);
}

//
// When the frame is destroyed, it should invoke cb(this, cb_data)
//
void
Pad_Window::Set_destroy_callback(Pad_Callback *cb)
{
    if (_destroyCallback) {
	delete _destroyCallback;
    }
    _destroyCallback = cb;
}

//
// Determines whether this frame is resizable.
//
void
Pad_Window::Set_resizeable(Pad_Bool resizeable)
{
    if (resizeable) {
	_windowFlags |= WINDOW_RESIZEABLE;
    } else {
	_windowFlags &= ~WINDOW_RESIZEABLE;
    }
}

void
Pad_Window::Set_resizeable_default(void)
{
    Set_resizeable(WINDOW_DEFAULT_RESIZEABLE);
}

Pad_Bool
Pad_Window::Is_resizeable(void)
{
    return((_windowFlags & WINDOW_RESIZEABLE) ? TRUE : FALSE);
}

//
// When the window is resized it should invoke cb(this, cb_data)
//
void
Pad_Window::Set_resize_callback(Pad_Callback *cb)
{
    if (_resizeCallback) {
	delete _resizeCallback;
    }
    _resizeCallback = cb;
}

Pad_Callback *
Pad_Window::Get_resize_callback(void)
{
    return(_resizeCallback);
}

//
// When the window is moved it should invoke cb(this, cb_data)
//
void
Pad_Window::Set_move_callback(Pad_Callback *cb)
{
    if (_moveCallback) {
	delete _moveCallback;
    }
    _moveCallback = cb;
}

Pad_Callback *
Pad_Window::Get_move_callback(void)
{
    return(_moveCallback);
}

//
// Specify if pointer is within Window
// These get called whenever pointer moves in and out of window,
// *OR* in and out of any member of the window.
// Thus, we must check actual pointer coordinates.
//
void
Pad_Window::Pointer_in(Pad_Event *)
{
    Pad_Object *obj;

    _windowFlags |= WINDOW_HAS_POINTER;

    if (_prevFocus) {
				// If window previously had a focus, then make that
				// object get the new focus.
	obj = _prevFocus->Get_object();
	pad->view->Update_focus(obj);
	delete _prevFocus;
	_prevFocus = NULL;
    }

    Damage();
}

void
Pad_Window::Pointer_out(Pad_Event *padEvent)
{
    Pad_Object *f;
    Pad_BBox *bb;

    bb = Get_global_bbox();

				// If point still within window, then don't
				// do anything.
    if (bb->Contains(padEvent->pt)) {
	return;
    }

    _windowFlags &= ~WINDOW_HAS_POINTER;

				// If user is interacting with window, then
				// don't turn the focus off
    if (Is_user_interacting()) {
	return;
    }

				// If focus was in this window, then remember it
    f = Pad_focus;
    while (f) {
	if (f == this) {
	    _prevFocus = new Pad_ObjectHandle(Pad_focus);
	    pad->view->Update_focus(NULL);
	    break;
	}
	f = f->group;
    }

    Damage();
}

Pad_Bool
Pad_Window::Has_pointer(void)
{
    return ((_windowFlags & WINDOW_HAS_POINTER) ? TRUE : FALSE);
}

//////////////////////////////////////////////////////////////
//
// Pad_Frame class
//
//////////////////////////////////////////////////////////////

Pad_Frame::Pad_Frame(Pad *pad) :
Pad_Window(pad)
{
				// Initialize default event handlers for frames
  _Initialize_events(pad->view->win);

    _type = PAD_FRAME;

    _frameFlags = PAD_NO_MASK;
    _cursorType = Pad_Win::cursorDefault;
    _menuBar = NULL;

    _inset.Set(FRAME_BORDER_WIDTH, 
	       FRAME_BORDER_WIDTH, 
	       FRAME_BORDER_WIDTH, 
	       FRAME_BORDER_WIDTH + FRAME_TITLE_HEIGHT);

    int red, green, blue;
    _fillColor.Get(red, green, blue);

    _border.Set((int)LERP(0.7, 0, red),
		(int)LERP(0.7, 0, green),
		(int)LERP(0.7, 0, blue));

    _activeBorder.Set((int)LERP(1.0, 0, red),
		      (int)LERP(1.0, 0, green),
		      (int)LERP(0.6, 0, blue));
    Update();

    Delete_all_tags();
    Add_tag("Frame");
}

Pad_Frame::~Pad_Frame()
{
}

//
// Render a Frame
//
Pad_Bool
Pad_Frame::Render(void)
{
    float xOffset, yOffset;
    Pad_Point min, max;
    Pad_BBox bb, tbb;
    Pad_Bool hasborder = FALSE;

    _refineNeeded = FALSE;

    Pad_Window::Render();	// Render window

				// Render frame's window dressing
    if (Has_pointer() || Is_user_interacting()) {
	if (_activeBorder.Is_set()) {
	    Pad_renderer->Set_border(_activeBorder);
	    hasborder = TRUE;
	}
    } else {
	if (_border.Is_set()) {
	    Pad_renderer->Set_border(_border);
	    hasborder = TRUE;
	}
    }

				// Return if no border color
    if (!hasborder) {
	return(TRUE);
    }

    Get_bbox(bb);
				// Render outside border
    min.Set(bb.Xmin(), bb.Ymin());
    max.Set(bb.Xmax(), bb.Ymax());
    Pad_renderer->Draw_3d_rectangle(min, max, FRAME_BORDER_WIDTH, PAD_RELIEF_RIDGE);

				// Render border corner marks
    float d = FRAME_BORDER_WIDTH + FRAME_TITLE_HEIGHT;
				// Left side
				// NWW
    min.Set(bb.Xmin(), bb.Ymax() - d - 1);
    max.Set(bb.Xmin() + FRAME_BORDER_WIDTH, bb.Ymax() - d + 1);
    Pad_renderer->Draw_3d_horizontal_bevel(min, max, TRUE, TRUE, TRUE, PAD_RELIEF_GROOVE);
				// SWW
    min.Set(bb.Xmin(), bb.Ymin() + d - 1);
    max.Set(bb.Xmin() + FRAME_BORDER_WIDTH, bb.Ymin() + d + 1);
    Pad_renderer->Draw_3d_horizontal_bevel(min, max, TRUE, TRUE, TRUE, PAD_RELIEF_GROOVE);
				// Right side
				// NEE
    min.Set(bb.Xmax() - FRAME_BORDER_WIDTH, bb.Ymax() - d - 1);
    max.Set(bb.Xmax(), bb.Ymax() - d + 1);
    Pad_renderer->Draw_3d_horizontal_bevel(min, max, TRUE, TRUE, TRUE, PAD_RELIEF_GROOVE);
				// SEE
    min.Set(bb.Xmax() - FRAME_BORDER_WIDTH, bb.Ymin() + d - 1);
    max.Set(bb.Xmax(), bb.Ymin() + d + 1);
    Pad_renderer->Draw_3d_horizontal_bevel(min, max, TRUE, TRUE, TRUE, PAD_RELIEF_GROOVE);
				// Top side
				// NWN
    min.Set(bb.Xmin() + d - 1, bb.Ymax() - FRAME_BORDER_WIDTH);
    max.Set(bb.Xmin() + d + 1, bb.Ymax());
    Pad_renderer->Draw_3d_vertical_bevel(min, max, TRUE, PAD_RELIEF_GROOVE);
				// NEN
    min.Set(bb.Xmax() - d - 1, bb.Ymax() - FRAME_BORDER_WIDTH);
    max.Set(bb.Xmax() - d + 1, bb.Ymax());
    Pad_renderer->Draw_3d_vertical_bevel(min, max, TRUE, PAD_RELIEF_GROOVE);
				// Bottom side
				// SWS
    min.Set(bb.Xmin() + d - 1, bb.Ymin());
    max.Set(bb.Xmin() + d + 1, bb.Ymin() + FRAME_BORDER_WIDTH);
    Pad_renderer->Draw_3d_vertical_bevel(min, max, TRUE, PAD_RELIEF_GROOVE);
				// SES
    min.Set(bb.Xmax() - d - 1, bb.Ymin());
    max.Set(bb.Xmax() - d + 1, bb.Ymin() + FRAME_BORDER_WIDTH);
    Pad_renderer->Draw_3d_vertical_bevel(min, max, TRUE, PAD_RELIEF_GROOVE);

				// Render title bar
    min.Set(bb.Xmin() + FRAME_BORDER_WIDTH, bb.Ymax() - FRAME_BORDER_WIDTH - FRAME_TITLE_HEIGHT);
    max.Set(bb.Xmax() - FRAME_BORDER_WIDTH, bb.Ymax() - FRAME_BORDER_WIDTH);
    if (Is_titlebar_pressed()) {
	Pad_renderer->Draw_filled_3d_rectangle(min, max, FRAME_TITLE_WIDTH, PAD_RELIEF_SUNKEN);
    } else {
	Pad_renderer->Draw_filled_3d_rectangle(min, max, FRAME_TITLE_WIDTH, PAD_RELIEF_RAISED);
    }
				// Render title
    if (_title != "") {
	Pad_renderer->Set_font(_font);
	Pad_renderer->Set_font_height(FRAME_TITLE_HEIGHT - (2 * FRAME_TITLE_WIDTH) - 4);
	Pad_renderer->Set_color(&Pad_Color::black);
	Pad_renderer->Get_string_bbox(_title.Get(), &tbb);
	xOffset = 0.5 * (max.x - min.x - tbb.Width());
	yOffset = 0.0;
				// If label is too big, then clip it
	if (xOffset < 0) {
	    tbb.Set(min.x + FRAME_TITLE_WIDTH, min.y + FRAME_TITLE_WIDTH,
		    max.x - FRAME_TITLE_WIDTH, max.y - FRAME_TITLE_WIDTH);
	    Pad_prc->win->activeRestorer->Push_clip(tbb.Get());
	}
	if (Is_titlebar_pressed()) {
	    xOffset += FRAME_TITLE_WIDTH;
	    yOffset -= FRAME_TITLE_WIDTH;
	}
	if (Pad_renderer->Draw_string(_title.Get(), 2, 
				      min.x + xOffset + FRAME_TITLE_WIDTH + 2, 
				      min.y + yOffset + FRAME_TITLE_WIDTH + 2)) {
	    _refineNeeded = TRUE;
	}
	if (xOffset < 0) {
	    Pad_prc->win->activeRestorer->Pop_clip();
	}
    }

    return(TRUE);
}

//
// Set the frame's menubar.
// Add the menubar to the frame, and position it appropriately.
//
void
Pad_Frame::Set_menubar(Pad_MenuBar *mbar)
{
    _menuBar = mbar;
    if (_menuBar) {

        Add(_menuBar);
	
	// add menuitem panels to the frame too
	Pad_List menus;
	Pad_Iterator iter;
	Pad_MenuItem *m;

	_menuBar->Get_members(menus);
	DOLIST(iter, menus, Pad_MenuItem, m) {
	    if (m->Type() == PAD_MENU) {
	        Add(((Pad_Menu*)m)->_itemsPanel);
	    }
	}

        Position_menubar();
    }
}

//
// Return the frame's menubar
//
Pad_MenuBar*
Pad_Frame::Get_menubar(void)
{
    return _menuBar;
}

//
// Position frame's menubar below the titlebar, set its width to that of
// the frame, and its height to default value.
// 
void
Pad_Frame::Position_menubar(void)
{
    if (_menuBar) {
        Pad_BBox bb;
	Get_bbox(bb);
        _menuBar->Set_height(FRAME_MENUBAR_HEIGHT);
	_menuBar->Set_width(bb.Width() - FRAME_BORDER_WIDTH*2);
	_menuBar->Set_anchor(PAD_ANCHOR_SW);
	_menuBar->Set_rel_position(bb.Xmin() + FRAME_BORDER_WIDTH,
		   bb.Ymax() - FRAME_BORDER_WIDTH - FRAME_TITLE_HEIGHT 
			     - FRAME_MENUBAR_HEIGHT, 1.0, FALSE);
	_menuBar->Raise(NULL);
    }
}

//
// Returns true if object needs more refinement.
//
Pad_Bool 
Pad_Frame::Continue_refinement(void)
{
	return (_refineNeeded);
}

//
// Notice when pointer goes in and out of frame to set cursor properly
//
void
Pad_Frame::Pointer_in(Pad_Event *padEvent)
{
    if (!Is_user_interacting()) {
	pad->view->win->Set_cursor(Get_cursor());
	
	Pad_Window::Pointer_in(padEvent);
    }
}

void
Pad_Frame::Pointer_out(Pad_Event *padEvent)
{
    if (!Is_user_interacting()) {
	pad->view->win->Set_cursor(Pad_Win::cursorDefault);

	Pad_Window::Pointer_out(padEvent);
    }
}

//
// Don't let frames get too small
//
void
Pad_Frame::Reshape(float x, float y, float width, float height)
{
    if (width < FRAME_MIN_WIDTH) {
	width = FRAME_MIN_WIDTH;
    }
    if (height < FRAME_MIN_HEIGHT) {
	height = FRAME_MIN_HEIGHT;
    }

    Pad_Container::Reshape(x, y, width, height);
    Position_menubar();
}

//
// Set/get cursor for this frame to the specified cursor.
//
void
Pad_Frame::Set_cursor(int cursorType)
{
    _cursorType = cursorType;
}

int
Pad_Frame::Get_cursor(void)
{
    return(_cursorType);
}

//
// Sets the title of this frame to the specified title.
//
void
Pad_Frame::Set_title(char *title)
{
    _title = title;
}

//
// Interact with pressing of titlebar
//
void
Pad_Frame::Set_titlebar_press(Pad_Bool pressed)
{
    if (pressed) {
	_frameFlags |= FRAME_TITLEBAR_PRESSED;
    } else {
	_frameFlags &= ~FRAME_TITLEBAR_PRESSED;
    }
    Damage();
}

Pad_Bool
Pad_Frame::Is_titlebar_pressed(void)
{
    return ((_frameFlags & FRAME_TITLEBAR_PRESSED) ? TRUE : FALSE);
}

//
// Keep track of user interacting with frame
//
void
Pad_Frame::Set_user_interacting(Pad_Bool interacting)
{
    if (interacting) {
	_frameFlags |= FRAME_USER_INTERACTING;
    } else {
	_frameFlags &= ~FRAME_USER_INTERACTING;
    }
}

Pad_Bool
Pad_Frame::Is_user_interacting(void)
{
    return ((_frameFlags & FRAME_USER_INTERACTING) ? TRUE : FALSE);
}

//////////////////////////////////////////////////////////////
//
// Definitions of Frame default event handlers
//
//////////////////////////////////////////////////////////////

				// Variables to control zooming of frames
static float zoomSpeed = 1.05;
static float zoomAmt = 1;
static float zoomX=0, zoomY=0;     // Zooming center
static Pad_TimerToken zoomTimer = NULL;

//
// Call this once to define event handlers for all Frames
//
void
Pad_Frame::_Initialize_events(Pad_Win *win)
{
    Pad_Callback *callback;
    static Pad_List initializedWindows;

				// If already initialized for this window, then return.
				// Else, add to list of initialized windows and initialize.
    if (initializedWindows.Member(win)) {
        return;
    }
    initializedWindows.Push_last(win);

				// Motion over frame
    callback = new Pad_Callback(Motion, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid("Frame"), "<Motion>", callback);

				// Leave frame
    callback = new Pad_Callback(Leave, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid("Frame"), "<Leave>", callback);

				// Frame Press
    callback = new Pad_Callback(Press, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid("Frame"), "<Run-ButtonPress-1>", callback);

				// Frame Drag
    callback = new Pad_Callback(Drag, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid("Frame"), "<Run-B1-Motion>", callback);

				// Frame Release
    callback = new Pad_Callback(Release, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid("Frame"), "<Run-ButtonRelease-1>", callback);

				// Zoom in on mouse-2
    callback = new Pad_Callback(Zoom_in_press, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid("Frame"), "<ButtonPress-2>", callback);

    callback = new Pad_Callback(Zoom_in_drag, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid("Frame"), "<B2-Motion>", callback);

    callback = new Pad_Callback(Zoom_in_release, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid("Frame"), "<ButtonRelease-2>", callback);

				// Zoom in on mouse-3
    callback = new Pad_Callback(Zoom_out_press, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid("Frame"), "<ButtonPress-3>", callback);

    callback = new Pad_Callback(Zoom_out_drag, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid("Frame"), "<B3-Motion>", callback);

    callback = new Pad_Callback(Zoom_out_release, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid("Frame"), "<ButtonRelease-3>", callback);

}

// A FrameComp is the part of the frame the user is interacting with
static enum FrameComp {NONE, LEFT, RIGHT, BOTTOM, TOP, NW, NE, SW, SE, TITLE} frameComp;

// The point the frame was pressed on (in global coordinates)
static Pad_Point framePt;

// Current cursor of frame
static int frameCursorType = -1;

static FrameComp
Frame_get_component(Pad_Frame *frame, Pad_Event *padEvent)
{
    float d;
    Pad_BBox bb;
    Pad_Inset *inset;
    Pad_Point *pt;
    FrameComp comp;

    frame->Get_bbox(bb);
    inset = frame->Get_inset();
    pt = &padEvent->objPt;
    d = FRAME_BORDER_WIDTH + FRAME_TITLE_HEIGHT;

    if (frame->Is_resizeable()) {
	if ((pt->x - bb.Xmin()) < inset->left) {
	    if ((pt->y - bb.Ymin()) < d) {
		comp = SW;
	    } else if ((bb.Ymax() - pt->y) < d) {
		comp = NW;
	    } else {
		comp = LEFT;
	    }
	} else if ((bb.Xmax() - pt->x) < inset->right) {
	    if ((pt->y - bb.Ymin()) < d) {
		comp = SE;
	    } else if ((bb.Ymax() - pt->y) < d) {
		comp = NE;
	    } else {
		comp = RIGHT;
	    }
	} else if ((pt->y - bb.Ymin()) < inset->bottom) {
	    if ((pt->x - bb.Xmin()) < d) {
		comp = SW;
	    } else if ((bb.Xmax() - pt->x) < d) {
		comp = SE;
	    } else {
		comp = BOTTOM;
	    }
	} else if ((bb.Ymax() - pt->y) < (inset->top) - FRAME_TITLE_HEIGHT) {
	    if ((pt->x - bb.Xmin()) < d) {
		comp = NW;
	    } else if ((bb.Xmax() - pt->x) < d) {
		comp = NE;
	    } else {
		comp = TOP;
	    }
	} else if ((bb.Ymax() - pt->y) < inset->top) {
	    comp = TITLE;
	} else {
	    comp = NONE;
	}
    } else {
	comp = NONE;
    }

    return(comp);
}

//
// Move pointer over the frame
//
int
Pad_Frame::Motion(Pad_Object *obj, ClientData, Pad_Event *padEvent)
{
    int cursorType;
    Pad_Frame *frame;

    if (obj->Type() == PAD_FRAME) {
	frame = (Pad_Frame *)obj;

	frameComp = Frame_get_component(frame, padEvent);

	switch (frameComp) {
	  case NONE:
	    cursorType = frame->Get_cursor();
	    break;
	  case TITLE:
	    cursorType = Pad_Win::cursorTopLeftArrow;
	    break;
	  case LEFT:
	    cursorType = Pad_Win::cursorLeftSide;
	    break;
	  case RIGHT:
	    cursorType = Pad_Win::cursorRightSide;
	    break;
	  case BOTTOM:
	    cursorType = Pad_Win::cursorBottomSide;
	    break;
	  case TOP:
	    cursorType = Pad_Win::cursorTopSide;
	    break;
	  case SW:
	    cursorType = Pad_Win::cursorBottomLeftCorner;
	    break;
	  case NW:
	    cursorType = Pad_Win::cursorTopLeftCorner;
	    break;
	  case NE:
	    cursorType = Pad_Win::cursorTopRightCorner;
	    break;
	  case SE:
	    cursorType = Pad_Win::cursorBottomRightCorner;
	}
	if (cursorType != frameCursorType) {
	    frame->pad->view->win->Set_cursor(cursorType);
	    frameCursorType = cursorType;
	}
    }

    return(PAD_OK);
}

//
// Pointer leaves frame.
// Necessary to update cached current cursor
//
int
Pad_Frame::Leave(Pad_Object *obj, ClientData, Pad_Event *)
{
    if (obj->Type() == PAD_FRAME) {
	frameCursorType = -1;
    }

    return(PAD_OK);
}

// Initiate manipulating a frame
//
int
Pad_Frame::Press(Pad_Object *obj, ClientData, Pad_Event *padEvent)
{
    Pad_Frame *frame;

    if (obj->Type() == PAD_FRAME) {
	frame = (Pad_Frame *)obj;

	frame->Set_user_interacting(TRUE);   // Mark frame as being manipulated by user
	framePt = padEvent->objPt;
	frameComp = Frame_get_component(frame, padEvent);

	switch (frameComp) {
	  case NONE:
	    break;
	  case TITLE:
	    frame->Set_titlebar_press(TRUE);
				// Fall through to other cases
	  case LEFT:
	  case RIGHT:
	  case BOTTOM:
	  case TOP:
	  case SW:
	  case NW:
	  case NE:
	  case SE:
	    frame->Raise();
	    break;
	}
    }

    return(PAD_BREAK);
}

//
// Reposition or reshape a frame, depending on what point is being dragged
//
int
Pad_Frame::Drag(Pad_Object *obj, ClientData, Pad_Event *padEvent)
{
    Pad_Bool move, resize;
    float x, y, width, height;
    float prevX, prevY, prevWidth, prevHeight;
    float dx, dy;
    Pad_Frame *frame;
    Pad_Point *pt;
    Pad_BBox bb;
    Pad_Inset *inset;
    Pad_Callback *moveCallback;
    Pad_Callback *resizeCallback;
    Pad_ObjectHandle *handle;

    if (obj->Type() == PAD_FRAME) {
	frame = (Pad_Frame *)obj;
	frame->Get_bbox(bb);
	pt = &padEvent->objPt;
	x = bb.Xmin();
	y = bb.Ymin();
	width = bb.Width();
	height = bb.Height();

	prevX = x;
	prevY = y;
	prevWidth = width;
	prevHeight = height;

	dx = pt->x - framePt.x;
	dy = pt->y - framePt.y;

	moveCallback = frame->Get_move_callback();
	resizeCallback = frame->Get_resize_callback();
	
	move = FALSE;
	resize = FALSE;
	inset = frame->Get_inset();
	switch (frameComp) {
	  case NONE:
	    break;
	  case LEFT:
	    move = TRUE; resize = TRUE;
	    if (pt->x > (bb.Xmax() - FRAME_MIN_WIDTH)) {
		pt->x = bb.Xmax() - FRAME_MIN_WIDTH;
	    }
	    x = pt->x;
	    width -= dx;
	    break;
	  case RIGHT:
	    move = TRUE; resize = TRUE;
	    if (pt->x < (bb.Xmin() + FRAME_MIN_WIDTH)) {
		pt->x = bb.Xmin() + FRAME_MIN_WIDTH;
	    }
	    width += dx;
	    break;
	  case BOTTOM:
	    move = TRUE; resize = TRUE;
	    if (pt->y > (bb.Ymax() - FRAME_MIN_HEIGHT)) {
		pt->y = bb.Ymax() - FRAME_MIN_HEIGHT;
	    }
	    y = pt->y;
	    height -= dy;
	    break;
	  case TOP:
	    move = TRUE; resize = TRUE;
	    if (pt->y < (bb.Ymin() + FRAME_MIN_HEIGHT)) {
		pt->y = bb.Ymin() + FRAME_MIN_HEIGHT;
	    }
	    height += dy;
	    break;
	  case SW:
	    move = TRUE; resize = TRUE;
	    if (pt->x > (bb.Xmax() - FRAME_MIN_WIDTH)) {
		pt->x = bb.Xmax() - FRAME_MIN_WIDTH;
	    }
	    x = pt->x;
	    width -= dx;
	    if (pt->y > (bb.Ymax() - FRAME_MIN_HEIGHT)) {
		pt->y = bb.Ymax() - FRAME_MIN_HEIGHT;
	    }
	    y = pt->y;
	    height -= dy;
	    break;
	  case NW:
	    move = TRUE; resize = TRUE;
	    if (pt->x > (bb.Xmax() - FRAME_MIN_WIDTH)) {
		pt->x = bb.Xmax() - FRAME_MIN_WIDTH;
	    }
	    if (pt->y < (bb.Ymin() + FRAME_MIN_HEIGHT)) {
		pt->y = bb.Ymin() + FRAME_MIN_HEIGHT;
	    }
	    x = pt->x;
	    width -= dx;
	    height += dy;
	    break;
	  case NE:
	    move = TRUE; resize = TRUE;
	    if (pt->x < (bb.Xmin() + FRAME_MIN_WIDTH)) {
		pt->x = bb.Xmin() + FRAME_MIN_WIDTH;
	    }
	    if (pt->y < (bb.Ymin() + FRAME_MIN_HEIGHT)) {
		pt->y = bb.Ymin() + FRAME_MIN_HEIGHT;
	    }
	    width += dx;
	    height += dy;
	    break;
	  case SE:
	    move = TRUE; resize = TRUE;
	    width += dx;
	    if (pt->x < (bb.Xmin() + FRAME_MIN_WIDTH)) {
		pt->x = bb.Xmin() + FRAME_MIN_WIDTH;
	    }
	    if (pt->y > (bb.Ymax() - FRAME_MIN_HEIGHT)) {
		pt->y = bb.Ymax() - FRAME_MIN_HEIGHT;
	    }
	    y = pt->y;
	    height -= dy;
	    break;
	  case TITLE:
	    move = TRUE;
	    x += dx;
	    y += dy;
	    break;
	}

				// No hack too crude!
				// The following in a major hack to get around a 
				// problem that stems from the fact that pad has a coordinate
				// system with y increasing instead of decreasing as it does
				// with most coordinate systems.  When reshaping a window
				// vertically but not horizontally, java has a simple
				// optimization which causes a problem (to be described shortly).
				// To get around this, when resizing a window vertically only,
				// we first resize it horizontally, and then cancel it by
				// resizing it to the desired shape.
				// 
				// The java problem comes with "North" layout of components.
				// When lengthening a window, components on the north
				// side of the window don't move relative to a NW coord system.
				// The java Component.reshape method doesn't do anything if
				// the shape doesn't change, so things break because the
				// components shape *does* change relative to pad's
				// SW coord system
	handle = new Pad_ObjectHandle(frame);
	if ((height != prevHeight) && (width == prevWidth)) {
	    width += 10;
	    frame->Reshape(x, y, width, height);
	    if (move && moveCallback) {
		moveCallback->Eval(frame, NULL);
				// Be careful, this object could be deleted in the callback
		if (handle->Get_object() == NULL) {
		    delete handle;
		    return(PAD_BREAK);
		}
	    }
	    if (resize && resizeCallback) {
		resizeCallback->Eval(frame, NULL);
				// Be careful, this object could be deleted in the callback
		if (handle->Get_object() == NULL) {
		    delete handle;
		    return(PAD_BREAK);
		}
	    }
	    width -= 10;
	}
				// Call requested callbacks
	frame->Reshape(x, y, width, height);
	if (move && moveCallback) {
	    moveCallback->Eval(frame, NULL);
				// Be careful, this object could be deleted in the callback
	    if (handle->Get_object() == NULL) {
	        delete handle;
		return(PAD_BREAK);
	    }
	}
	if (resize && resizeCallback) {
	    resizeCallback->Eval(frame, NULL);
				// Be careful, this object could be deleted in the callback
	    if (handle->Get_object() == NULL) {
	        delete handle;
		return(PAD_BREAK);
	    }
	}

	delete handle;
	framePt = padEvent->objPt;
    }

    return(PAD_BREAK);
}

//
// Let go of frame
//
int
Pad_Frame::Release(Pad_Object *obj, ClientData, Pad_Event *padEvent)
{
    Pad_Frame *frame;

    if (obj->Type() == PAD_FRAME) {
	frame = (Pad_Frame *)obj;

	switch (frameComp) {
	  case NONE:
	    break;
	  case LEFT:
	    break;
	  case RIGHT:
	    break;
	  case BOTTOM:
	    break;
	  case TOP:
	    break;
	  case NW:
	  case SW:
	  case NE:
	  case SE:
	    break;
	  case TITLE:
	    frame->Set_titlebar_press(FALSE);
	    break;
	}

	frame->Set_user_interacting(FALSE);   // Mark frame as no longer being manipulated by user

				// Necessary because frame doesn't become inactive
				// if the pointer leaves the frame while the user
				// is interacting with it.
	if (!frame->Has_pointer()) {
	    frame->Pointer_out(padEvent);
	}
    }

    return(PAD_BREAK);
}

//
// Timer callback to zoom frames
//
static void
Frame_zoom_callback(void *clientData) 
{
    Pad_Frame *frame = (Pad_Frame *)clientData;
    Pad_Callback *moveCallback;
    Pad_ObjectHandle *handle;

    frame->Scale(zoomX, zoomY, zoomAmt);
    moveCallback = frame->Get_move_callback();
    if (moveCallback) {
        handle = new Pad_ObjectHandle(frame);
	moveCallback->Eval(frame, NULL);
				// Be careful, this object could be deleted in the callback
	if (handle->Get_object() == NULL) {
	    delete handle;
	    return;
	}
	delete handle;
    }

    zoomTimer = Pad_CreateTimerHandler(10, Frame_zoom_callback, frame);
}

//
// Event handlers for scaling up a frame
//
int
Pad_Frame::Zoom_in_press(Pad_Object *obj, ClientData, Pad_Event *padEvent)
{
    Pad_Frame *frame;

    if (obj->Type() == PAD_FRAME) {
	frame = (Pad_Frame *)obj;

	frame->Set_user_interacting(TRUE);   // Mark frame as being manipulated by user
	frameComp = Frame_get_component(frame, padEvent);

	if (frameComp == TITLE) {
	    frame->Set_titlebar_press(TRUE);
	    frame->Raise();
	    zoomAmt = zoomSpeed;
	    zoomX = padEvent->pt.x;
	    zoomY = padEvent->pt.y;
	    Frame_zoom_callback(frame);
	}
    }

    return(PAD_BREAK);
}

int
Pad_Frame::Zoom_in_drag(Pad_Object *obj, ClientData, Pad_Event *padEvent)
{
    if (obj->Type() == PAD_FRAME) {
	if (frameComp == TITLE) {
	    zoomX = padEvent->pt.x;
	    zoomY = padEvent->pt.y;
	}
    }

    return(PAD_BREAK);
}

int
Pad_Frame::Zoom_in_release(Pad_Object *obj, ClientData, Pad_Event *)
{
    Pad_Frame *frame;

    if (obj->Type() == PAD_FRAME) {
	frame = (Pad_Frame *)obj;

	if (frameComp == TITLE) {
	    Pad_DeleteTimerHandler(zoomTimer);
	    frame->Set_titlebar_press(FALSE);
	}
	frame->Set_user_interacting(FALSE);   // Mark frame as no longer being manipulated by user
    }

    return(PAD_BREAK);
}

//
// Event handlers for scaling down a frame
//
int
Pad_Frame::Zoom_out_press(Pad_Object *obj, ClientData, Pad_Event *padEvent)
{
    Pad_Frame *frame;

    if (obj->Type() == PAD_FRAME) {
	frame = (Pad_Frame *)obj;

	frame->Set_user_interacting(TRUE);   // Mark frame as being manipulated by user
	frameComp = Frame_get_component(frame, padEvent);
	
	if (frameComp == TITLE) {
	    frame->Set_titlebar_press(TRUE);
	    frame->Raise();
	    zoomAmt = 1.0 / zoomSpeed;
	    zoomX = padEvent->pt.x;
	    zoomY = padEvent->pt.y;
	    Frame_zoom_callback(frame);
	}
    }

    return(PAD_BREAK);
}

int
Pad_Frame::Zoom_out_drag(Pad_Object *obj, ClientData, Pad_Event *padEvent)
{
    if (obj->Type() == PAD_FRAME) {
	if (frameComp == TITLE) {
	    zoomX = padEvent->pt.x;
	    zoomY = padEvent->pt.y;
	}
    }

    return(PAD_BREAK);
}

int
Pad_Frame::Zoom_out_release(Pad_Object *obj, ClientData, Pad_Event *)
{
    Pad_Frame *frame;

    if (obj->Type() == PAD_FRAME) {
	frame = (Pad_Frame *)obj;

	if (frameComp == TITLE) {
	    Pad_DeleteTimerHandler(zoomTimer);
	    frame->Set_titlebar_press(FALSE);
	}
	frame->Set_user_interacting(FALSE);   // Mark frame as no longer being manipulated by user
    }

    return(PAD_BREAK);
}
