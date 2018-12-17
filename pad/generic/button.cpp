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
#include "button.h"
#include "global.h"
#include "pad.h"
#include "view.h"
#include "win.h"
#include "renderer.h"
#include "bind.h"
#include "display.h"

#include <string.h>

#define LABEL_DEFAULT_TEXT      ""                    // Default label text
#define LABEL_DEFAULT_OFFSET    2                     // Offset that text is drawn from edge of label
#define LABEL_DEFAULT_ALIGNMENT COMPONENT_ALIGN_LEFT  // Alignment of text within label
#define LABEL_DEFAULT_HILITEWIDTH 0                   // Focus border

#define BUTTON_DEFAULT_OFFSET   6                     // Offset that text is drawn from edge of button
#define BUTTON_DEFAULT_RELIEF   PAD_RELIEF_RAISED      // Default 3D border relief
#define CHECKBOX_SIZE           10                    // Size of checkbox indicator
#define CHECKBOX_OFFSET          2                    // Offset of checkbox indicator

Pad_Bool Pad_Checkbox::_dragedOut = FALSE;

//////////////////////////////////////////////////////////////
//
// Pad_Label class
//
//////////////////////////////////////////////////////////////

Pad_Label::Pad_Label(Pad *pad) :
Pad_Component(pad)
{
    _type = PAD_LABEL;
    _labelFlags = PAD_NO_MASK;
    _offset = LABEL_DEFAULT_OFFSET;
    _alignment = LABEL_DEFAULT_ALIGNMENT;
    _labelOffsetX = 0;
    _labelOffsetY = 0;

    Set_hilite_width(LABEL_DEFAULT_HILITEWIDTH);
    Set_label_default();
    Update();

    Add_tag("Label");
}

Pad_Label::~Pad_Label()
{
}

//
// Render a label
//
Pad_Bool
Pad_Label::Render(void)
{
    float bb[4];

    _refineNeeded = FALSE;       // Set to true if refinement is needed

    if (_fillColor.Is_set()) {
	Pad_renderer->Set_color(_fillColor);
	Pad_renderer->Draw_filled_box(_activeArea.x, _activeArea.y, 
				      _activeArea.x + _activeArea.width, _activeArea.y + _activeArea.height);
    }

    if (_penColor.Is_set() && _font.Is_set()) {
				// Compute clipping region so text stays within label
	bb[XMIN] = _activeArea.x + (_offset * 0.5);
	bb[YMIN] = _activeArea.y + (_offset * 0.5);
	bb[XMAX] = _activeArea.x + _activeArea.width - (_offset * 0.5);
	bb[YMAX] = _activeArea.y + _activeArea.height - (_offset * 0.5);
	Pad_prc->win->activeRestorer->Push_clip(bb);

	Pad_renderer->Set_color(_penColor);
	Pad_renderer->Set_font(_font);
	float ypos = _activeArea.y + 
	             (_activeArea.height - Pad_renderer->Get_font_height())/2.0;

	if (Pad_renderer->Draw_string(_label.Get(), 2, 
				  _activeArea.x + _offset + _labelOffsetX,
				//_activeArea.y + _offset + _labelOffsetY)) {
				      ypos)) {
	    _refineNeeded = TRUE;
	}

	Pad_prc->win->activeRestorer->Pop_clip();
    }

    Pad_Component::Render();

    return(TRUE);
}

//
// Returns true if object needs more refinement.
//
Pad_Bool 
Pad_Label::Continue_refinement(void)
{
	return (_refineNeeded);
}

//
// Set bounding box of label to specification,
// Update label alignment to new dimensions
//
void
Pad_Label::Reshape(float x, float y, float width, float height)
{
    Pad_Component::Reshape(x, y, width, height);
    _Align();
    _Set_clipping();		// If label smaller then width, then turn clipping on
}

//
// Return the minimum dimensions necessary for the component to look normal
//
void
Pad_Label::Minimum_size(Pad_Dimension &dimension)
{
    Pad_BBox bb;
    Pad_Renderer *renderer = pad->view->win->renderer;

    if (_font.Is_set()) {
	renderer->Set_font(_font);
	renderer->Get_string_bbox(_label.Get(), &bb);
	dimension.width = bb.Width() + (2 * _offset);
	dimension.height = renderer->Get_font_height() + (2 * _offset);
    } else {
	Pad_Component::Minimum_size(dimension);
    }
}

//
// Return the preferred (relaxed) dimension of the component 
//
void
Pad_Label::Preferred_size(Pad_Dimension &dimension)
{
    Minimum_size(dimension);
}

void
Pad_Label::Set_alignment(int alignment)
{
    _alignment = alignment;
    _Align();
}

//
// Set and get label
//
void
Pad_Label::Set_label(char *newLabel)
{
    _labelFlags |= LABEL_TEXT_SET;
    _label = newLabel;
    _Align();
    _Set_clipping();		// If label smaller then width, then turn clipping on
    Damage();
}

void
Pad_Label::Set_label_default(void)
{
    Set_label(LABEL_DEFAULT_TEXT);
    _labelFlags &= ~LABEL_TEXT_SET;
}

char *
Pad_Label::Get_label(void)
{
    return(_label.Get());
}

//
// Update label to align itself with widget dimensions
// using the current alignment
//
void
Pad_Label::_Align(void)
{
    Pad_Dimension dimension;

    Preferred_size(dimension);

    switch (_alignment) {
      case COMPONENT_ALIGN_LEFT:
	_labelOffsetX = 0;
	break;
      case COMPONENT_ALIGN_CENTER:
	_labelOffsetX = 0.5 * (_activeArea.width - dimension.width);
	break;
      case COMPONENT_ALIGN_RIGHT:
	_labelOffsetX = _activeArea.width - dimension.width;
	break;
    }

    _labelOffsetY = 0.5 * (_activeArea.height - dimension.height);
}

//
// If label smaller then width, then turn clipping on
//
void
Pad_Label::_Set_clipping(void)
{
    Pad_Dimension dimension;

    Preferred_size(dimension);
    if ((dimension.width + (2 * _offset)) > _activeArea.width) {
	Set_clipping(CLIPPING_TRUE);
    } else {
	Set_clipping(CLIPPING_FALSE);
    }
}

//////////////////////////////////////////////////////////////
//
// Pad_Button class
//
//////////////////////////////////////////////////////////////

Pad_Button::Pad_Button(Pad *pad) :
Pad_Label(pad)
{
				// Initialize default event handlers for buttons
    _Initialize_events(pad->view->win);

    _type = PAD_BUTTON;
    _offset = BUTTON_DEFAULT_OFFSET;
    _buttonFlags = PAD_NO_MASK;
    _command = NULL;
    _relief = BUTTON_DEFAULT_RELIEF;
    Set_hilite_width(COMPONENT_DEFAULT_HILITEWIDTH);   // Necessary to override label's default

    Set_fill_default();
    Update();

    Add_tag("Button");
    Delete_tag("Label");
}

Pad_Button::~Pad_Button()
{
    if (_command) {
	delete _command;
	_command = NULL;
    }
}

//
// Render a button, depending on its state 
// (enabled/disabled, normal/active/armed)
//
Pad_Bool
Pad_Button::Render(void)
{
    float bb[4];
    float xoffset, yoffset;
    Pad_Point min, max;

				// First draw background
    min.Set(_activeArea.x, _activeArea.y);
    max.Set(_activeArea.x + _activeArea.width, _activeArea.y + _activeArea.height);

    _refineNeeded = FALSE;

    if (Is_armmed()) {
	if (_activeBorder.Is_set()) {
	    Pad_renderer->Set_border(_activeBorder);
	    Pad_renderer->Draw_filled_3d_rectangle(min, max, (_offset * 0.5), PAD_RELIEF_FLAT);
	}
    } else {
	if (_border.Is_set()) {
	    Pad_renderer->Set_border(_border);
	    Pad_renderer->Draw_filled_3d_rectangle(min, max, (_offset * 0.5), PAD_RELIEF_FLAT);
	}
    }

				// Then draw label
    if (_penColor.Is_set() && _font.Is_set()) {
        if (Is_enabled()) {
	    Pad_renderer->Set_color(_penColor);
	} else {
	    Pad_renderer->Set_color(&Pad_Color::gray);
	}
	Pad_renderer->Set_font(_font);
	xoffset = _offset + _labelOffsetX;
	yoffset = _offset + _labelOffsetY;
	if (Is_armmed()) {
	    xoffset += (_offset * 0.25);
	    yoffset -= (_offset * 0.25);
	}
	
				// Compute clipping region so text stays within button
	bb[XMIN] = min.x + (_offset * 0.5);
	bb[YMIN] = min.y + (_offset * 0.5);
	bb[XMAX] = max.x - (_offset * 0.5);
	bb[YMAX] = max.y - (_offset * 0.5);
	
	Pad_prc->win->activeRestorer->Push_clip(bb);
	if (Pad_renderer->Draw_string(_label.Get(), 2, 
				      _activeArea.x + xoffset, 
				      _activeArea.y + yoffset)) {
	    _refineNeeded = TRUE;
	}

	Pad_prc->win->activeRestorer->Pop_clip();
    }

				// Then draw border
    if (Is_armmed()) {
	if (_activeBorder.Is_set()) {
	    Pad_renderer->Set_border(_activeBorder);
	    Pad_renderer->Draw_3d_rectangle(min, max, (_offset * 0.5), _relief);
	}
    } else {
	if (_border.Is_set()) {
	    Pad_renderer->Set_border(_border);
	    Pad_renderer->Draw_3d_rectangle(min, max, (_offset * 0.5), _relief);
	}
    }

				// Finally, give component opportunity to add finishing touches
    Pad_Component::Render();

    return(TRUE);
}

//
// Gets called when button fill is changed to 
// change the border color appropriately.
//
void
Pad_Button::Fill_changed(void)
{
    int red, green, blue;

    Pad_Component::Fill_changed();

    _border.Set(_fillColor);

    _fillColor.Get(red, green, blue);
    red   = (int)(LERP(0.9, 0, red));
    green = (int)(LERP(0.9, 0, green));
    blue  = (int)(LERP(0.9, 0, blue));
    _activeBorder.Set(red, green, blue);
}


//
// Store specified callback with button.
// This will get called when button is invoked
//
void
Pad_Button::Set_callback(Pad_Callback *command)
{
    _buttonFlags |= BUTTON_COMMAND_SET;
    if (_command) {
	delete _command;
    }
    _command = command;
}

void
Pad_Button::Set_callback_default(void)
{
    Set_callback(NULL);
   _buttonFlags &= ~BUTTON_COMMAND_SET;
}

Pad_Callback *
Pad_Button::Get_callback(void)
{
    return(_command);
}

//
// Arm button (so it looks depressed);
//
void
Pad_Button::Arm(void)
{
    _buttonFlags |= BUTTON_ARMMED;
    Set_relief(PAD_RELIEF_SUNKEN);
    Damage();
}

//
// Disarm button (so it looks normal);
//
void
Pad_Button::Disarm(void)
{
    _buttonFlags &= ~BUTTON_ARMMED;
    Set_relief(PAD_RELIEF_RAISED);
    Damage();
}

//
// Check if button is armmed
//
Pad_Bool
Pad_Button::Is_armmed(void)
{
    return ((_buttonFlags & BUTTON_ARMMED) ? TRUE : FALSE);
}

//
// Set and get 3D border relief
//
void
Pad_Button::Set_relief(int relief)
{
    _buttonFlags |= BUTTON_RELIEF_SET;
    _relief = relief;
    Damage();
}

void
Pad_Button::Set_relief_default(void)
{
    Set_relief(BUTTON_DEFAULT_RELIEF);
    _buttonFlags &= ~BUTTON_RELIEF_SET;
}

int
Pad_Button::Get_relief(void)
{
    return(_relief);
}

//////////////////////////////////////////////////////////////
//
// Definitions of Button default event handlers
//
//////////////////////////////////////////////////////////////

//
// Call this once to define event handlers for all Buttons
//
void
Pad_Button::_Initialize_events(Pad_Win *win)
{
    Pad_Callback *callback;
    static Pad_List initializedWindows;

				// If already initialized for this window, then return.
				// Else, add to list of initialized windows and initialize.
    if (initializedWindows.Member(win)) {
        return;
    }
    initializedWindows.Push_last(win);

				// Button Press
    callback = new Pad_Callback(Press, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid("Button"), "<Run-ButtonPress-1>", callback);

				// Button Drag
    callback = new Pad_Callback(Drag, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid("Button"), "<Run-B1-Motion>", callback);

				// Button Release
    callback = new Pad_Callback(Release, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid("Button"), "<Run-ButtonRelease-1>", callback);
}

//
// Arm button
//
int
Pad_Button::Press(Pad_Object *obj, ClientData, Pad_Event *)
{
    Pad_Button *button;

    if (obj->Type() == PAD_BUTTON ||
	obj->Type() == PAD_CHECKBOX) {
	button = (Pad_Button *)obj;
	if (button->Is_enabled()) {
	    button->Request_focus();
	    button->Arm();
	}
    }

    return(PAD_BREAK);
}

//
// Determine if pointer is still over button.
// If so, make sure button is still armmed.
// Else, Disarm button.
//
int
Pad_Button::Drag(Pad_Object *obj, ClientData, Pad_Event *padEvent)
{
    Pad_Button *button;

    if (obj->Type() == PAD_BUTTON ||
	obj->Type() == PAD_CHECKBOX) {
	button = (Pad_Button *)obj;
	if (button->Is_enabled()) {
	    if (button->Get_active_area()->Contains(padEvent->objPt)) {
				// Pointer within button
		if (!button->Is_armmed()) {
		    button->Arm();
		}
	    } else {
				// Pointer outside of button
		if (button->Is_armmed()) {
		    button->Disarm();
		}
	    }
	}
    }

    return(PAD_BREAK);
}

int
Pad_Button::Release(Pad_Object *obj, ClientData, Pad_Event *padEvent)
{
    Pad_Button *button;
    Pad_Callback *command;
    Pad_ObjectHandle *handle;

    if (obj->Type() == PAD_BUTTON ||
	obj->Type() == PAD_CHECKBOX) {
	button = (Pad_Button *)obj;
	if (button->Is_enabled()) {
	    if (button->Is_armmed()) {
		command = button->Get_callback();
		handle = new Pad_ObjectHandle(button);
		if (command) {
		    command->Eval(button, padEvent);
				// Be careful, this object could be deleted in the callback
		    if (handle->Get_object() == NULL) {
		        delete handle;
			return(PAD_BREAK);
		    }
		}
		button->Disarm();
		delete handle;
	    }
	}
    }

    return(PAD_BREAK);
}

//////////////////////////////////////////////////////////////
//
// Pad_Checkbox class
//
//////////////////////////////////////////////////////////////

Pad_Checkbox::Pad_Checkbox(Pad *pad) :
Pad_Button(pad)
{
				// Initialize default event handlers for checkboxes
    _Initialize_events(pad->view->win);

    _type = PAD_CHECKBOX;
    _inCheckboxGroup = FALSE;
    Set_alignment(COMPONENT_ALIGN_CENTER);
    Update();

    Add_tag("Checkbox");
    Delete_tag("Button");
}

void
Pad_Checkbox::Minimum_size(Pad_Dimension &dimension)
{
    Pad_Button::Minimum_size(dimension);
    dimension.width += CHECKBOX_SIZE + CHECKBOX_OFFSET;
}

//
// Gets called when checkbox fill is changed to 
// change the border color appropriately.
//
void
Pad_Checkbox::Fill_changed(void)
{
    Pad_Component::Fill_changed();

				// For the moment, these two
				// borders are the same, but we are
				// using two for possible future 
				// expansion.
    _border.Set(_fillColor);
    _activeBorder.Set(_fillColor);
}

//
// Render a checkbox, depending on its state
// (enabled/disabled, normal/active/armed), and
// checkbox group member.
//

Pad_Bool
Pad_Checkbox::Render(void)
{
    float bb[4];
    float xoffset, yoffset;
    Pad_Point min, max;
    Pad_PList ptList;

    _refineNeeded = FALSE;

				// First draw background
    if (_fillColor.Is_set()) {
        Pad_renderer->Set_color(_fillColor);
	Pad_renderer->Draw_filled_box(_activeArea.x, _activeArea.y,
				      _activeArea.x + _activeArea.width, _activeArea.y + _activeArea.height);
    }

                                // Then draw the checkbox
                                // Draw a rectangle if not in a checkbox group
                                // Draw a radio button otherwise
    if (!Get_checkboxGroup()) {
        min.Set(_activeArea.x+CHECKBOX_OFFSET, 
		_activeArea.y + (_activeArea.height/2.0 - CHECKBOX_SIZE/2.0));
	max.Set(_activeArea.x + CHECKBOX_OFFSET+CHECKBOX_SIZE, 
		_activeArea.y + (_activeArea.height/2.0 + CHECKBOX_SIZE/2.0));
    } else {
        Pad_Point pt;
	pt.Set(_activeArea.x+CHECKBOX_OFFSET, 
	       _activeArea.y + (_activeArea.height/2.0));
	ptList.Push_last(&pt);
	pt.Set(_activeArea.x+CHECKBOX_OFFSET+CHECKBOX_SIZE/2.0, 
	       _activeArea.y + (_activeArea.height/2.0)+CHECKBOX_SIZE/2.0); 
	ptList.Push_last(&pt);
	pt.Set(_activeArea.x+CHECKBOX_OFFSET+CHECKBOX_SIZE, 
	       _activeArea.y + (_activeArea.height/2.0)); 
	ptList.Push_last(&pt);
	pt.Set(_activeArea.x+CHECKBOX_OFFSET+CHECKBOX_SIZE/2.0, 
	       _activeArea.y + (_activeArea.height/2.0)-CHECKBOX_SIZE/2.0); 
	ptList.Push_last(&pt);
    }

    if (Is_armmed()) {
        if (_activeBorder.Is_set()) {
	    Pad_renderer->Set_border(_activeBorder);
	    if (!Get_checkboxGroup()) {
	        Pad_renderer->Draw_filled_3d_rectangle(min, max, 1, PAD_RELIEF_SUNKEN);
	    } else {
	        Pad_renderer->Draw_filled_3d_polygon(ptList, 1, PAD_RELIEF_SUNKEN);
	    }
	}
    } else {
        if (_border.Is_set()) {
	    Pad_renderer->Set_border(_border);
	    if (!Get_checkboxGroup()) {
	        Pad_renderer->Draw_filled_3d_rectangle(min, max, 1, PAD_RELIEF_RAISED);
	    } else {
	        Pad_renderer->Draw_filled_3d_polygon(ptList, 1, PAD_RELIEF_RAISED);
	    }
	}
    }

				// Then draw label
    min.Set(_activeArea.x+CHECKBOX_OFFSET+CHECKBOX_SIZE, _activeArea.y);
    max.Set(_activeArea.x+CHECKBOX_OFFSET+CHECKBOX_SIZE + 
	    _activeArea.width, _activeArea.y + _activeArea.height);
    
    if (_penColor.Is_set() && _font.Is_set()) {
	Pad_renderer->Set_color(_penColor);
	Pad_renderer->Set_font(_font);
	xoffset = _offset + _labelOffsetX;
	yoffset = _offset + _labelOffsetY;
	
				// Compute clipping region so text stays within button
	bb[XMIN] = min.x + (_offset * 0.5);
	bb[YMIN] = min.y + (_offset * 0.5);
	bb[XMAX] = max.x - (_offset * 0.5);
	bb[YMAX] = max.y - (_offset * 0.5);
	
	Pad_prc->win->activeRestorer->Push_clip(bb);
	if (Pad_renderer->Draw_string(_label.Get(), 2, 
				      _activeArea.x+CHECKBOX_SIZE + xoffset, 
				      _activeArea.y + yoffset)) {
	    _refineNeeded = TRUE;
	}

	Pad_prc->win->activeRestorer->Pop_clip();
    }

 				// Finally, give component opportunity to add finishing touches
    Pad_Component::Render();

    return(TRUE);
}       
//////////////////////////////////////////////////////////////
//
// Definitions of Checkbox default event handlers
//
//////////////////////////////////////////////////////////////

//
// Call this once to define event handlers for all Checkboxes
//
void
Pad_Checkbox::_Initialize_events(Pad_Win *win)
{
    Pad_Callback *callback;
    static Pad_List initializedWindows;

				// If already initialized for this window, then return.
				// Else, add to list of initialized windows and initialize.
    if (initializedWindows.Member(win)) {
        return;
    }
    initializedWindows.Push_last(win);

				// Button Press
    callback = new Pad_Callback(Press, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid("Checkbox"), "<Run-ButtonPress-1>", callback);

				// Button Drag
    callback = new Pad_Callback(Drag, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid("Checkbox"), "<Run-B1-Motion>", callback);

				// Button Release
    callback = new Pad_Callback(Release, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid("Checkbox"), "<Run-ButtonRelease-1>", callback);
}
    
//
// Toggle checkbox state
//
int
Pad_Checkbox::Press(Pad_Object *obj, ClientData, Pad_Event *)
{
    Pad_Checkbox *checkbox;

    if (obj->Type() == PAD_CHECKBOX) {
	checkbox = (Pad_Checkbox *)obj;
	_dragedOut = FALSE;
	if (checkbox->Is_enabled()) {
	    checkbox->Request_focus();
	    if (!checkbox->Is_armmed()) {
	        checkbox->Arm();
	    } else {
	        checkbox->Disarm();
	    }
	}
    }

    return(PAD_BREAK);
}

//
// Determine if pointer is still over checkbox.
// If so, make sure checkbox state is toggled.
// Else, toggle state and make note of it.
//
int
Pad_Checkbox::Drag(Pad_Object *obj, ClientData, Pad_Event *padEvent)
{
    Pad_Checkbox *checkbox;

    if (obj->Type() == PAD_CHECKBOX) {
	checkbox = (Pad_Checkbox *)obj;
	if (checkbox->Is_enabled()) {
	    if (checkbox->Get_active_area()->Contains(padEvent->objPt)) {
				// Pointer within button
	                        // Normally don't need to do anything (since
	                        // toggle was done at button press, but
	                        // if mouse was draged out then have to
	                        // redo the toggle.
	        if (_dragedOut) {
		    _dragedOut = FALSE;
		    if (!checkbox->Is_armmed()) {
		        checkbox->Arm();
		    } else {
		        checkbox->Disarm();
		    }
		}
	    } else {		// Pointer outside of button
	        if (!_dragedOut) {
		    _dragedOut = TRUE;
		    if (checkbox->Is_armmed()) {
		        checkbox->Disarm();
		    } else {
		        checkbox->Arm();
		    }
		}
	    }
	}
    }

    return(PAD_BREAK);
}

int
Pad_Checkbox::Release(Pad_Object *obj, ClientData, Pad_Event *padEvent)
{
    Pad_Checkbox *checkbox;
    Pad_Callback *command;
    Pad_ObjectHandle *handle;

    if (obj->Type() == PAD_CHECKBOX) {
	checkbox = (Pad_Checkbox *)obj;
	if (checkbox->Is_enabled()) {
	    command = checkbox->Get_callback();
	    handle = new Pad_ObjectHandle(checkbox);
	    if (command) {
	        command->Eval(checkbox, padEvent);
				// Be careful, this object could be deleted in the callback
		if (handle->Get_object() == NULL) {
		    delete handle;
		    return(PAD_BREAK);
		}
	    }
	    delete handle;
	}
    }

    return(PAD_BREAK);
}

//
// Set the checkbox state to set or unset (normal or active).
//
void
Pad_Checkbox::Set_state(Pad_Bool state)
{
    if (state) {
        if (!Is_armmed()) {
	    Arm();
	}
    } else {
        Disarm();
    }
}

//
// Return checkbox state.
//
Pad_Bool
Pad_Checkbox::Get_state(void)
{
    return Is_armmed();
}

//
// Make/Remove checbox as a member of a group.
// This will only effect checkbox rendering.
//
void
Pad_Checkbox::Set_checkboxGroup(Pad_Bool inGroup)
{
    if (inGroup != Get_checkboxGroup()) {
        _inCheckboxGroup = inGroup;
	Damage();
    }
}

//
// Return true if checkbox is member of a checkbox group.
// False otherwise.
//
Pad_Bool
Pad_Checkbox::Get_checkboxGroup(void)
{
    return _inCheckboxGroup;
}

//
// Wrappers around Pad_Button arm methods.
// 

void
Pad_Checkbox::Arm(void)
{
    Pad_Button::Arm();
}

void 
Pad_Checkbox::Disarm(void)
{
    Pad_Button::Disarm();
}

Pad_Bool
Pad_Checkbox::Is_armmed(void)
{
    return Pad_Button::Is_armmed();
}
