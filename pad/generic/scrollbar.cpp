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
#include "scrollbar.h"
#include "global.h"
#include "pad.h"
#include "view.h"
#include "win.h"
#include "renderer.h"
#include "bind.h"
#include "display.h"
#include "global.h"

#include <stdlib.h>
#include <string.h>

#define SCROLLBAR_DEFAULT_LENGTH      200
#define SCROLLBAR_DEFAULT_THICKNESS   20
#define SCROLLBAR_DEFAULT_ORIENTATION 0         // Horizontal
#define SCROLLBAR_DEFAULT_FROM        0
#define SCROLLBAR_DEFAULT_TO          100
#define SCROLLBAR_DEFAULT_PAGESIZE    5
#define SCROLLBAR_DEFAULT_LINESIZE    1
#define SCROLLBAR_BORDER_WIDTH        2
#define SCROLLBAR_ARROW_ASPECT        0.95       // Length / Thickness
#define SCROLLBAR_MIN_THUMBLENGTH     3
#define SCROLLBAR_MIN_THUMBSPACE      1          // Minimum space available for thumb to move within

#define COMPONENT_TIMER_START         500        // Milliseconds until auto-action components start
#define COMPONENT_TIMER_REPEAT        50         // Milliseconds until auto-action components repeat

//////////////////////////////////////////////////////////////
//
// Pad_Scrollbar class
//
//////////////////////////////////////////////////////////////

Pad_Scrollbar::Pad_Scrollbar(Pad *pad) :
Pad_Component(pad)
{
				// Initialize default event handlers for buttons
    _Initialize_events(pad->view->win);

    _type = PAD_SCROLLBAR;
    _scrollbarFlags = PAD_NO_MASK;
    _command = NULL;
    _lineUpCommand = NULL;
    _lineDownCommand = NULL;
    _pageUpCommand = NULL;
    _pageDownCommand = NULL;
    _positionCommand = NULL;
    _value = 0;
				// Set things here because default functions
				// do error checking on _from and _to assuming
				// they are already set.
    _from = SCROLLBAR_DEFAULT_FROM;
    _to = SCROLLBAR_DEFAULT_TO;
    _lineSize = SCROLLBAR_DEFAULT_LINESIZE;
    _pageSize = SCROLLBAR_DEFAULT_PAGESIZE;
    Set_orientation_default();
    Set_fill_default();
    Set_active_component(NONE);
    Update();

    _Update();			// Update scrollbar shape

    Add_tag("Scrollbar");
}

Pad_Scrollbar::~Pad_Scrollbar()
{
    if (_command) {
	delete _command;
	_command = NULL;
    }
    if (_lineUpCommand) {
	delete _lineUpCommand;
	_lineUpCommand = NULL;
    }
    if (_lineDownCommand) {
	delete _lineDownCommand;
	_lineDownCommand = NULL;
    }
    if (_pageUpCommand) {
	delete _pageUpCommand;
	_pageUpCommand = NULL;
    }
    if (_pageDownCommand) {
	delete _pageDownCommand;
	_pageDownCommand = NULL;
    }
    if (_positionCommand) {
	delete _positionCommand;
	_positionCommand = NULL;
    }
}

//
// Render a Scrollbar
//
Pad_Bool
Pad_Scrollbar::Render(void)
{
    int relief;
    int orientation;
    float bw = SCROLLBAR_BORDER_WIDTH;
    Pad_Point min, max;

    if (_bgBorder.Is_set()) {
	Pad_renderer->Set_border(_bgBorder);
	min.Set(_activeArea.x, _activeArea.y);
	max.Set(_activeArea.x + _activeArea.width, _activeArea.y + _activeArea.height);
				// Background
	Pad_renderer->Draw_filled_3d_rectangle(min, max, bw, PAD_RELIEF_SUNKEN);
    }

    if (_border.Is_set()) {
	orientation = Get_orientation();
	Pad_renderer->Set_border(_border);
				// From arrow
	relief = (_activeComponent == FROM_ARROW) ? PAD_RELIEF_SUNKEN : PAD_RELIEF_RAISED;
	Pad_renderer->Draw_filled_3d_polygon(_fromArrow, bw, relief);

				// To arrow
	relief = (_activeComponent == TO_ARROW) ? PAD_RELIEF_SUNKEN : PAD_RELIEF_RAISED;
	Pad_renderer->Draw_filled_3d_polygon(_toArrow, bw, relief);

				// Thumb
	Pad_renderer->Draw_filled_3d_rectangle(_thumbMin, _thumbMax, bw, PAD_RELIEF_RAISED);
    }

    Pad_Component::Render();

    return(TRUE);
}

//
// Compute the dimensions of various components of the scrollbar
//
void
Pad_Scrollbar::_Compute_dimensions(float &thickness, float &length, float &arrowThickness, float &arrowLength,
				   float &thumbLength, float &troughLength)
{
    int orientation;
    float availableArrowThickness;
    float bw = SCROLLBAR_BORDER_WIDTH;

    orientation = Get_orientation();
    if (orientation == HORIZONTAL) {
	thickness = _activeArea.height - (2 * bw);
	length = _activeArea.width - (2 * bw);
    } else {
	thickness = _activeArea.width - (2 * bw);
	length = _activeArea.height - (2 * bw);
    }

    availableArrowThickness = 0.5 * (length - SCROLLBAR_MIN_THUMBLENGTH - (2 * SCROLLBAR_MIN_THUMBSPACE)) * 
	SCROLLBAR_ARROW_ASPECT;
    
    if (availableArrowThickness < thickness) {
	arrowThickness = availableArrowThickness;
	arrowLength = arrowThickness / SCROLLBAR_ARROW_ASPECT;
    } else {
	arrowThickness = thickness;
	arrowLength = arrowThickness / SCROLLBAR_ARROW_ASPECT;
    }
    troughLength = length - 2 * arrowLength;
    if (_to == _from) {
	thumbLength = troughLength;
    } else {
	thumbLength = troughLength * _pageSize / (_to - _from);
    }
    if (thumbLength < SCROLLBAR_MIN_THUMBLENGTH) {
	thumbLength = SCROLLBAR_MIN_THUMBLENGTH;
    }
}

//
// Call this when any properties of the scrollbar change
// This recomputes the visual description of the scrollbar.
// We compute and cache the visual description of the scrollbar
// so we don't have to recompute it every render, but only
// when the scrollbar changes.
//
void
Pad_Scrollbar::_Update(void)
{
    Pad_Point point;
    int orientation;
    float thickness, length;
    float arrowLength, arrowThickness;
    float thumbLength, thumbCenter;
    float troughLength;
    float xoffset, yoffset;

    xoffset = _activeArea.x + SCROLLBAR_BORDER_WIDTH;
    yoffset = _activeArea.y + SCROLLBAR_BORDER_WIDTH;
    orientation = Get_orientation();
    _Compute_dimensions(thickness, length, arrowThickness, arrowLength, thumbLength, troughLength);

				// From arrow
    _fromArrow.Make_empty();
    if (orientation == HORIZONTAL) {
	point.Set(xoffset, yoffset + (0.5 * thickness));
    } else {
	point.Set(xoffset + (0.5 * thickness), yoffset);
    }
    _fromArrow.Push_last(&point);
    if (orientation == HORIZONTAL) {
	point.Set(xoffset + arrowLength, yoffset + (0.5 * thickness) - (0.5 * arrowThickness));
    } else {
	point.Set(xoffset + (0.5 * thickness) + (0.5 * arrowThickness), yoffset + arrowLength);
    }
    _fromArrow.Push_last(&point);
    if (orientation == HORIZONTAL) {
	point.Set(xoffset + arrowLength, yoffset + (0.5 * thickness) + (0.5 * arrowThickness));
    } else {
	point.Set(xoffset + (0.5 * thickness) - (0.5 * arrowThickness), yoffset + arrowLength);
    }
    _fromArrow.Push_last(&point);

				// To arrow
    _toArrow.Make_empty();
    if (orientation == HORIZONTAL) {
	point.Set(xoffset + length, yoffset + (0.5 * thickness));
    } else {
	point.Set(xoffset + (0.5 * thickness), yoffset + length);
    }
    _toArrow.Push_last(&point);
    if (orientation == HORIZONTAL) {
	point.Set(xoffset + length - arrowLength, yoffset + (0.5 * thickness) + (0.5 * arrowThickness));
    } else {
	point.Set(xoffset + (0.5 * thickness) - (0.5 * arrowThickness), yoffset + length - arrowLength);
    }
    _toArrow.Push_last(&point);
    if (orientation == HORIZONTAL) {
	point.Set(xoffset + length - arrowLength, yoffset + (0.5 * thickness) - (0.5 * arrowThickness));
    } else {
	point.Set(xoffset + (0.5 * thickness) + (0.5 * arrowThickness), yoffset + length - arrowLength);
    }
    _toArrow.Push_last(&point);

				// Thumb
    if (_to == _from) {
	thumbCenter = arrowLength + (0.5 * thumbLength) + (troughLength - thumbLength);
    } else {
	thumbCenter = arrowLength + (0.5 * thumbLength) + ((_value - _from) / (_to - _from)) * 
	    (troughLength - thumbLength);
    }
    if (orientation == HORIZONTAL) {
	_thumbMin.Set(xoffset + thumbCenter - (0.5 * thumbLength), yoffset);
    } else {
	_thumbMin.Set(xoffset, yoffset + thumbCenter - (0.5 * thumbLength));
    }
    if (orientation == HORIZONTAL) {
	_thumbMax.Set(xoffset + thumbCenter + (0.5 * thumbLength), yoffset + thickness);
    } else {
	_thumbMax.Set(xoffset + thickness, yoffset + thumbCenter + (0.5 * thumbLength));
    }

    Damage();
}

// 
// Return the component under the specified point
// (in local coordinates)
//
Pad_Scrollbar::Component
Pad_Scrollbar::Pick_component(Pad_Point &pickPt)
{
    Pad_BBox bb;
    Component component;
    Pad_Point pt;
    float thickness, length;
    float arrowLength, arrowThickness;
    float thumbLength;
    float troughLength;

    Get_bbox(bb);
    pt.Set(pickPt.x - bb.Xmin(),
	   pickPt.y - bb.Ymin());

    _Compute_dimensions(thickness, length, arrowThickness, arrowLength, thumbLength, troughLength);

    if (bb.Contains(pickPt)) {
	if (pickPt.In_polygon(_fromArrow)) {
	    component = FROM_ARROW;
	} else if (pickPt.In_polygon(_toArrow)) {
	    component = TO_ARROW;
	} else if ((pickPt.x >= _thumbMin.x) && (pickPt.y >= _thumbMin.y) &&
		   (pickPt.x <= _thumbMax.x) && (pickPt.y <= _thumbMax.y)) {
	    component = THUMB;
	} else {
	    if (Get_orientation() == HORIZONTAL) {
		if ((pt.x >= arrowLength) && (pt.x <= (arrowLength + troughLength))) {
		    if (pickPt.x <= _thumbMin.x) {
			component = TROUGH_FROM;
		    } else {
			component = TROUGH_TO;
		    }
		} else {
		    component = NONE;
		}
	    } else {
		if ((pt.y >= arrowLength) && (pt.y <= (arrowLength + troughLength))) {
		    if (pickPt.y <= _thumbMin.y) {
			component = TROUGH_FROM;
		    } else {
			component = TROUGH_TO;
		    }
		} else {
		    component = NONE;
		}
	    }
	}
    } else {
	component = NONE;
    }

    return(component);
}

///////////////////////////////////////////////////////////////////
// Interaction functions
// These get called when the user interacts with the scrollbar
///////////////////////////////////////////////////////////////////

static void
_Eval_callback(Pad_Scrollbar *scrollbar, Pad_Callback *callback, Pad_Event *event)
{
    Pad_String str;
    Pad_Callback *newCallback = nullptr;

				// First fire specific callback
    if (callback) {
	//str = callback->Get();
	if (str == "<C++ Event Handler>") {
	    callback->Eval(scrollbar, event);
	} else {
	    str += " ";
	    str += scrollbar->Get_value();
	    //newCallback = new Pad_Callback(str.Get(), callback->Language());
	    newCallback->Eval(scrollbar, event);
	    delete newCallback;
	}
    }

				// Then, if there is a command callback, fire that too.
    callback = scrollbar->Get_command_callback();
    if (callback) {
	//str = callback->Get();
	if (str == "<C++ Event Handler>") {
	    callback->Eval(scrollbar, event);
	} else {
	    str += " ";
	    str += scrollbar->Get_value();
	    //newCallback = new Pad_Callback(str.Get(), callback->Language());
	    newCallback->Eval(scrollbar, event);
	    delete newCallback;
	}
    }
}

//
// Move up one line
//
void
Pad_Scrollbar::Line_up(Pad_Event *event)
{
    Pad_Callback *callback;

    Set_value(Get_value() + Get_line_size());

    callback = Get_lineup_callback();
    _Eval_callback(this, callback, event);
}

//
// Move down one line
//
void
Pad_Scrollbar::Line_down(Pad_Event *event)
{
    Pad_Callback *callback;

    Set_value(Get_value() - Get_line_size());

    callback = Get_linedown_callback();
    _Eval_callback(this, callback, event);
}

//
// Move up one page
//
void
Pad_Scrollbar::Page_up(Pad_Event *event)
{
    Pad_Callback *callback;

    Set_value(Get_value() + Get_page_size());

    callback = Get_pageup_callback();
    _Eval_callback(this, callback, event);
}

//
// Move down one page
//
void
Pad_Scrollbar::Page_down(Pad_Event *event)
{
    Pad_Callback *callback;

    Set_value(Get_value() - Get_page_size());

    callback = Get_pagedown_callback();
    _Eval_callback(this, callback, event);
}

//
// Move to the position specified by <event>,
// so that on drags, the thumb follows the pointer.
//
void
Pad_Scrollbar::Position(Pad_Event *event, Pad_Bool drag)
{
    Pad_Callback *callback;

    Set_value(event->objPt, drag);

    callback = Get_position_callback();
    _Eval_callback(this, callback, event);
}

//
// Return the minimum dimensions necessary for the component to look normal
//
void
Pad_Scrollbar::Minimum_size(Pad_Dimension &dimension)
{
    if (Get_orientation() == HORIZONTAL) {
	dimension.width = SCROLLBAR_DEFAULT_LENGTH;
	dimension.height = SCROLLBAR_DEFAULT_THICKNESS;
    } else {
	dimension.height = SCROLLBAR_DEFAULT_LENGTH;
	dimension.width = SCROLLBAR_DEFAULT_THICKNESS;
    }
}

//
// Return the preferred (relaxed) dimension of the component 
//
void
Pad_Scrollbar::Preferred_size(Pad_Dimension &dimension)
{
    Minimum_size(dimension);
}

//
// Set the dimensions of the scrollbar
//
void
Pad_Scrollbar::Reshape(float x, float y, float width, float height)
{
    Pad_Component::Reshape(x, y, width, height);
    _Update();
}

Pad_Bool
Pad_Scrollbar::Set_height(float height)
{
    Pad_Component::Set_height(height);
    _Update();

    return(TRUE);
}

Pad_Bool
Pad_Scrollbar::Set_width(float width)
{
    Pad_Component::Set_width(width);
    _Update();

    return(TRUE);
}

//
// Gets called when scrollbar background is changed to 
// change the border color appropriately.
//
void
Pad_Scrollbar::Fill_changed(void)
{
    intptr_t red, green, blue;

    Pad_Component::Fill_changed();

    _fillColor.Get(red, green, blue);
    _border.Set(red, green, blue);

    red   = (intptr_t)(LERP(0.85, 0, red));
    green = (intptr_t)(LERP(0.85, 0, green));
    blue  = (intptr_t)(LERP(0.85, 0, blue));
    _bgBorder.Set(red, green, blue);
}


//
// Setter and getter for orientation.
// can be HORIZONTAL or VERTICAL
//
void
Pad_Scrollbar::Set_orientation(int orientation)
{
    if (orientation == HORIZONTAL) {
	_scrollbarFlags |= SCROLLBAR_HORIZONTAL;
    } else {
	_scrollbarFlags &= ~SCROLLBAR_HORIZONTAL;
    }
    _scrollbarFlags |= SCROLLBAR_ORIENTATION_SET;

    _Update();
}

void
Pad_Scrollbar::Set_orientation_default(void)
{
    Set_orientation(SCROLLBAR_DEFAULT_ORIENTATION);
    _scrollbarFlags &= ~SCROLLBAR_ORIENTATION_SET;
}

int
Pad_Scrollbar::Get_orientation(void)
{
    return ((_scrollbarFlags & SCROLLBAR_HORIZONTAL) ? HORIZONTAL : VERTICAL);
}

//
// Setters and getter for 'value'
//

//
// Set the scrollbars value so that the thumb is centered
// under the specified point (in local coordinates).
// If <drag> is FALSE, then this is the press, and
// the offset needs to be remembered
//
void
Pad_Scrollbar::Set_value(Pad_Point &pt, Pad_Bool drag)
{
    float value, position;
    float thickness, length;
    float arrowLength, arrowThickness;
    float thumbLength;
    float troughLength;
    float troughPosition;
    float thumbMid;
    float bwh = SCROLLBAR_BORDER_WIDTH + _hiliteWidth;
    Pad_BBox bb;
    static float dx, dy;

				// This was a press, so just remember the 
				// point and return
    if (drag == FALSE) {
	if (Get_orientation() == HORIZONTAL) {
	    thumbMid = 0.5 * (_thumbMin.x + _thumbMax.x);
	    dx = thumbMid - pt.x;
	} else {
	    thumbMid = 0.5 * (_thumbMin.y + _thumbMax.y);
	    dy = thumbMid - pt.y;
	}
	return;
    }
    
				// Else, this was a drag.
    Get_bbox(bb);
    _Compute_dimensions(thickness, length, arrowThickness, arrowLength, thumbLength, troughLength);

    if (Get_orientation() == HORIZONTAL) {
	position = pt.x + dx - bb.Xmin();
    } else {
	position = pt.y + dy - bb.Ymin();
    }
    troughPosition = position - (arrowLength + bwh);
    if (troughLength == thumbLength) {
	value = _from;
    } else {
	value = _from + (_to - _from) * (troughPosition - (0.5 * thumbLength)) / (troughLength - thumbLength);
    }

    Set_value(value);
}

void
Pad_Scrollbar::Set_value(float value)
{
    if (value < _from) {
	_value = _from;
    } else if (value > _to) {
	_value = _to;
    } else {
	_value = value;
    }

    _Update();
}

void
Pad_Scrollbar::Set_value_default(void)
{
    Set_value(Get_from());
}

float
Pad_Scrollbar::Get_value(void)
{
    return (_value);
}

//
// Setter and getter for 'pageSize'
//
void
Pad_Scrollbar::Set_page_size(float pageSize)
{
    if (pageSize < 0) {
	_pageSize = 0;
    } else if (pageSize > (_to - _from)) {
	_pageSize  = _to - _from;
    } else {
	_pageSize = pageSize;
    }
    _scrollbarFlags |= SCROLLBAR_PAGESIZE_SET;

    _Update();
}

void
Pad_Scrollbar::Set_page_size_default(void)
{
    Set_page_size(SCROLLBAR_DEFAULT_PAGESIZE);
    _scrollbarFlags &= ~SCROLLBAR_PAGESIZE_SET;
}

float
Pad_Scrollbar::Get_page_size(void)
{
    return (_pageSize);
}

//
// Setter and getter for 'lineSize'
//
void
Pad_Scrollbar::Set_line_size(float lineSize)
{
    if (lineSize < 0) {
	_lineSize = 0;
    } else if (lineSize > (_to - _from)) {
	_lineSize  = _to - _from;
    } else {
	_lineSize = lineSize;
    }
    _scrollbarFlags |= SCROLLBAR_LINESIZE_SET;

    _Update();
}

void
Pad_Scrollbar::Set_line_size_default(void)
{
    Set_line_size(SCROLLBAR_DEFAULT_LINESIZE);
    _scrollbarFlags &= ~SCROLLBAR_LINESIZE_SET;
}

float
Pad_Scrollbar::Get_line_size(void)
{
    return (_lineSize);
}

//
// Setter and getter for _activeComponent
//
void
Pad_Scrollbar::Set_active_component(Component component)
{
    _activeComponent = component;
    Damage();
}

Pad_Scrollbar::Component
Pad_Scrollbar::Get_active_component(void)
{
    return(_activeComponent);
}

//
// Setter and getter for 'from'
//
void
Pad_Scrollbar::Set_from(float from)
{
    _from = from;
    Set_value(Get_value());     // Keep value in bounds
    _scrollbarFlags |= SCROLLBAR_FROM_SET;
    
    _Update();
}

void
Pad_Scrollbar::Set_from_default(void)
{
    Set_from(SCROLLBAR_DEFAULT_FROM);
    _scrollbarFlags &= ~SCROLLBAR_FROM_SET;
}

float
Pad_Scrollbar::Get_from(void)
{
    return (_from);
}

//
// Setter and getter for 'to'
//
void
Pad_Scrollbar::Set_to(float to)
{
    _to = to;
    Set_value(Get_value());     // Keep value in bounds
    _scrollbarFlags |= SCROLLBAR_TO_SET;

    _Update();
}

void
Pad_Scrollbar::Set_to_default(void)
{
    Set_to(SCROLLBAR_DEFAULT_TO);
    _scrollbarFlags &= ~SCROLLBAR_TO_SET;
}

float
Pad_Scrollbar::Get_to(void)
{
    return (_to);
}

//
// Store specified callback with scrollbar.
// This will get called when the user interacts with the scrollbar
//
void
Pad_Scrollbar::Set_command_callback(Pad_Callback *command)
{
    _scrollbarFlags |= SCROLLBAR_COMMAND_SET;
    if (_command) {
	delete _command;
    }
    _command = command;
}

void
Pad_Scrollbar::Set_command_callback_default(void)
{
    Set_command_callback(NULL);
    _scrollbarFlags &= ~SCROLLBAR_COMMAND_SET;
}

Pad_Callback *
Pad_Scrollbar::Get_command_callback(void)
{
    return(_command);
}

//
// Store specified callback with scrollbar.
// This will get called when the line-up widget is invoked.
//
void
Pad_Scrollbar::Set_lineup_callback(Pad_Callback *command)
{
    if (_lineUpCommand) {
	delete _lineUpCommand;
    }
    _lineUpCommand = command;
}

Pad_Callback *
Pad_Scrollbar::Get_lineup_callback(void)
{
    return(_lineUpCommand);
}

//
// Store specified callback with scrollbar.
// This will get called when the line-down widget is invoked.
//
void
Pad_Scrollbar::Set_linedown_callback(Pad_Callback *command)
{
    if (_lineDownCommand) {
	delete _lineDownCommand;
    }
    _lineDownCommand = command;
}

Pad_Callback *
Pad_Scrollbar::Get_linedown_callback(void)
{
    return(_lineDownCommand);
}

//
// Store specified callback with scrollbar.
// This will get called when the page-up widget is invoked.
//
void
Pad_Scrollbar::Set_pageup_callback(Pad_Callback *command)
{
    if (_pageUpCommand) {
	delete _pageUpCommand;
    }
    _pageUpCommand = command;
}

Pad_Callback *
Pad_Scrollbar::Get_pageup_callback(void)
{
    return(_pageUpCommand);
}

//
// Store specified callback with scrollbar.
// This will get called when the page-down widget is invoked.
//
void
Pad_Scrollbar::Set_pagedown_callback(Pad_Callback *command)
{
    if (_pageDownCommand) {
	delete _pageDownCommand;
    }
    _pageDownCommand = command;
}

Pad_Callback *
Pad_Scrollbar::Get_pagedown_callback(void)
{
    return(_pageDownCommand);
}

//
// Store specified callback with scrollbar.
// This will get called when the thumb widget is moved.
//
void
Pad_Scrollbar::Set_position_callback(Pad_Callback *command)
{
    if (_positionCommand) {
	delete _positionCommand;
    }
    _positionCommand = command;
}

Pad_Callback *
Pad_Scrollbar::Get_position_callback(void)
{
    return(_positionCommand);
}

//////////////////////////////////////////////////////////////
//
// Definitions of Scrollbar default event handlers
//
//////////////////////////////////////////////////////////////

//
// Call this once to define event handlers for all Scrollbars
//
void
Pad_Scrollbar::_Initialize_events(Pad_Win *win)
{
    Pad_Callback *callback;
    static Pad_List initializedWindows;

				// If already initialized for this window, then return.
				// Else, add to list of initialized windows and initialize.
    if (initializedWindows.Member(win)) {
        return;
    }
    initializedWindows.Push_last(win);

				// Scrollbar Press
    callback = new Pad_Callback(Press, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid("Scrollbar"), "<Run-ButtonPress-1>", callback);

				// Scrollbar Drag
    callback = new Pad_Callback(Drag, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid("Scrollbar"), "<Run-B1-Motion>", callback);

				// Scrollbar Release
    callback = new Pad_Callback(Release, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid("Scrollbar"), "<Run-ButtonRelease-1>", callback);
}

static Pad_Point scrollbarPt;                     // Used to keep cursor offset from thumb as originally pressed
static Pad_TimerToken scrollbarTimer = NULL;	  // Timer for auto-scrolling

				// Forward reference static functions
static void Scrollbar_timer_callback(void *clientData);

//
// Event handlers to interact with the scrollbar
//
int
Pad_Scrollbar::Press(Pad_Object *obj, ClientData, Pad_Event *padEvent)
{
    Pad_Scrollbar *scrollbar;
    Pad_Scrollbar::Component activeComponent;

    if (obj->Type() == PAD_SCROLLBAR) {
	scrollbar = (Pad_Scrollbar *)obj;
	if (scrollbar->Is_enabled()) {
	    activeComponent = scrollbar->Pick_component(padEvent->objPt);
	    scrollbar->Set_active_component(activeComponent);

	    switch (activeComponent) {
	      case Pad_Scrollbar::FROM_ARROW:
		scrollbar->Line_down(padEvent);
		break;
	      case Pad_Scrollbar::TO_ARROW:
		scrollbar->Line_up(padEvent);
		break;
	      case Pad_Scrollbar::THUMB:
		scrollbar->Position(padEvent, FALSE);
		break;
	      case Pad_Scrollbar::TROUGH_FROM:
		scrollbar->Page_down(padEvent);
		break;
	      case Pad_Scrollbar::TROUGH_TO:
		scrollbar->Page_up(padEvent);
		break;
	      case Pad_Scrollbar::NONE:
	        break;
	    }
	    scrollbarTimer = Pad_CreateTimerHandler(COMPONENT_TIMER_START, Scrollbar_timer_callback, scrollbar);
	}
    }

    return(PAD_BREAK);
}

int
Pad_Scrollbar::Drag(Pad_Object *obj, ClientData, Pad_Event *padEvent)
{
    Pad_Scrollbar *scrollbar;

    if (obj->Type() == PAD_SCROLLBAR) {
	scrollbar = (Pad_Scrollbar *)obj;
	if (scrollbar->Is_enabled()) {
	    switch (scrollbar->Get_active_component()) {
	      case Pad_Scrollbar::THUMB:
		scrollbar->Position(padEvent, TRUE);
		break;
	      case Pad_Scrollbar::TROUGH_TO:
	      case Pad_Scrollbar::TROUGH_FROM:
	      case Pad_Scrollbar::TO_ARROW:
	      case Pad_Scrollbar::FROM_ARROW:
	      case Pad_Scrollbar::NONE:
		break;
	    }
	}
    }

    return(PAD_BREAK);
}

int
Pad_Scrollbar::Release(Pad_Object *obj, ClientData, Pad_Event *)
{
    Pad_Scrollbar *scrollbar;

    if (obj->Type() == PAD_SCROLLBAR) {
	scrollbar = (Pad_Scrollbar *)obj;
	scrollbar->Set_active_component(Pad_Scrollbar::NONE);
	if (scrollbar->Is_enabled()) {
	}
    }

    if (scrollbarTimer) {
	Pad_DeleteTimerHandler(scrollbarTimer);
	scrollbarTimer = NULL;
    }

    return(PAD_BREAK);
}

//
// Timer callback for auto scrolling
//
static void
Scrollbar_timer_callback(void *clientData) 
{
    float value, lineSize, pageSize;
    Pad_Scrollbar *scrollbar;

    scrollbar = (Pad_Scrollbar *)clientData;
    if (scrollbar->Is_enabled()) {
	value = scrollbar->Get_value();
	pageSize = scrollbar->Get_page_size();
	lineSize = scrollbar->Get_line_size();

	switch (scrollbar->Get_active_component()) {
	  case Pad_Scrollbar::FROM_ARROW:
	    scrollbar->Line_down(NULL);
	    break;
	  case Pad_Scrollbar::TO_ARROW:
	    scrollbar->Line_up(NULL);
	    break;
	  case Pad_Scrollbar::TROUGH_FROM:
	    scrollbar->Page_down(NULL);
	    break;
	  case Pad_Scrollbar::TROUGH_TO:
	    scrollbar->Page_up(NULL);
	    break;
	  case Pad_Scrollbar::THUMB:
	  case Pad_Scrollbar::NONE:
	    break;
	}
    }

    scrollbarTimer = Pad_CreateTimerHandler(COMPONENT_TIMER_REPEAT, Scrollbar_timer_callback, scrollbar);
}
