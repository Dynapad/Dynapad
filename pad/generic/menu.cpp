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

#include <string.h>
#include <stdlib.h>
#include "defs.h"
#include "button.h"
#include "global.h"
#include "pad.h"
#include "view.h"
#include "win.h"
#include "renderer.h"
#include "bind.h"
#include "display.h"
#include "menu.h"
#include "line.h"


Pad_List Pad_MenuItem::initializedWindows;
Pad_List Pad_CheckboxMenuItem::initializedWindows;
Pad_Bool Pad_CheckboxMenuItem::_dragedOut = FALSE;
Pad_List Pad_Menu::initializedWindows;
Pad_List Pad_ChoiceMenu::initializedWindows;

#define CHECKBOX_SIZE           10                    // Size of checkbox indicator
#define CHECKBOX_OFFSET          2                    // Offset of checkbox indicator


//////////////////////////////////////////////////////////////
//
// Pad_MenuItem class
//
//////////////////////////////////////////////////////////////

Pad_MenuItem::Pad_MenuItem(Pad *pad) :
  Pad_Button(pad)
{
    _Initialize_events(pad->view->win,"MenuItem");
    _type = PAD_MENUITEM;
    _hilite = FALSE;
    _menu = NULL;
    _menuBar = NULL;
    _seperator = FALSE;
    Set_alignment(COMPONENT_ALIGN_LEFT);
    Update();

    Delete_tag("Button");
    Add_tag("MenuItem");
}

//
// Set the menuitem label.  A "-" label indicates the item is really just
// a seperator.
//
void
Pad_MenuItem::Set_label(char *label)
{
    Pad_Button::Set_label(label);
    if (strcmp(label,"-")) {
        _seperator = FALSE;
    } else {
        _seperator = TRUE;
	Set_height(5);
	Set_events(FALSE);
    }

    Update();
}

//
// Set the menu the item belongs to.
// Item is removed from its existing menu.
//
void
Pad_MenuItem::Set_menu(Pad_Menu *menu) 
{
    if ( _menu ) {
        _menu->Remove(this);
    } else if (_menuBar) {
        _menuBar->Remove(this);
    }

    _menu = menu;
}

Pad_Menu*
Pad_MenuItem::Get_menu(void)
{
    return _menu;
}

//
// Set the menubar item belongs to.
// Item is removed from its exsiting menu or menubar.
//
void
Pad_MenuItem::Set_MenuBar(Pad_MenuBar *menuBar) 
{
    if ( _menu ) {
        _menu->Remove(this);
    } else if (_menuBar) {
        _menuBar->Remove(this);
    }

    _menuBar = menuBar;
    //_menuBar->Add(this);  hacking, assume this is done already...
}

Pad_MenuBar*
Pad_MenuItem::Get_MenuBar(void)
{
    return _menuBar;
}

//
// Render a menuitem depending on its hilite state and seperator.
//
Pad_Bool
Pad_MenuItem::Render(void)
{
    Pad_Bool rc;
    Pad_ColorRef enabledColor;

    if (!_seperator) {
                             // If item is disabled then use a grey pen color
        if (!Is_enabled()) {
	    enabledColor.Set(_penColor);
	    _penColor.Set(&Pad_Color::gray);
	}
	                    // If item is not selected or to be hilited then 
	                    // redner it as a label.  Render button otherwise.
        if (!_hilite) {
	    rc = Pad_Label::Render();
	} else {
	    rc = Pad_Button::Render();
	}

	                     // Restore the pen color
	if (!Is_enabled()) {
	    _penColor.Set(enabledColor);
	}

    } else {                // Item is a seperator, hack a beveled line
        Pad_BBox bb;
	Pad_Point min, max;
	Get_bbox(bb);
	min.Set(bb.Xmin(), bb.Ymin());
	if (_menu) {
	  max.Set(bb.Xmin() + _menu->_itemsPanel->bbox.Width(), bb.Ymin() + 2);
	} else {
	  max.Set(bb.Xmin() + Get_width(), bb.Ymin() + 2);
	}
	Pad_renderer->Set_border(_activeBorder);	    
	Pad_renderer->Draw_3d_horizontal_bevel(min, max, TRUE, TRUE, TRUE,
					       PAD_RELIEF_GROOVE);	
    }

    return(rc);
}       

//
// Wrappers around Pad_Button arm methods.
// 
void
Pad_MenuItem::Arm(void)
{
    Pad_Button::Arm();
}

void 
Pad_MenuItem::Disarm(void)
{
    Pad_Button::Disarm();
}

Pad_Bool
Pad_MenuItem::Is_armmed(void)
{
    return Pad_Button::Is_armmed();
}

//////////////////////////////////////////////////////////////
//
// Definitions of MenuItem default event handlers
//
//////////////////////////////////////////////////////////////

//
// Call this once to define event handlers for all MenuItemes
//
void
Pad_MenuItem::_Initialize_events(Pad_Win *win, char *type)
{
    Pad_Callback *callback;

				// If already initialized for this window, then return.
				// Else, add to list of initialized windows and initialize.
    if (initializedWindows.Member(win)) {
        return;
    }
    initializedWindows.Push_last(win);

				// Mouse Etner
    callback = new Pad_Callback(Enter, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid(type), "<Run-Enter>", callback);

				// Mouse Leave
    callback = new Pad_Callback(Leave, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid(type), "<Run-Leave>", callback);

				// Button Press
    callback = new Pad_Callback(Press, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid(type), "<Run-ButtonPress-1>", callback);

				// Button Drag
    callback = new Pad_Callback(Drag, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid(type), "<Run-B1-Motion>", callback);

				// Button Release
    callback = new Pad_Callback(Release, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid(type), "<Run-ButtonRelease-1>", callback);
}

//
// Hilite menuitem when mouse enters it.
// If a submenu then display its items.
//
int
Pad_MenuItem::Enter(Pad_Object *obj, ClientData, Pad_Event *)
{
    Pad_MenuItem *item;

    if (obj->Type() == PAD_MENUITEM || obj->Type() == PAD_CHECKBOXMENUITEM ||
	obj->Type() == PAD_MENU || obj->Type() == PAD_CHOICEMENU) {
        item = (Pad_MenuItem*)obj;
	if (item->Is_enabled()) {
	    item->_hilite = TRUE;
	    item->Damage();
	    if (item->Type() == PAD_MENU && item->Get_menu()) {
	        ((Pad_Menu*)item)->Display_items();
	    }
	}
    }

    return(PAD_BREAK);
}

//
// Don't hilite menuitem when mouse leaves it.
// For submenus hide the items.
//
int
Pad_MenuItem::Leave(Pad_Object *obj, ClientData, Pad_Event *padEvent)
{
    Pad_MenuItem *item;

    if (obj->Type() == PAD_MENUITEM ||  obj->Type() == PAD_CHECKBOXMENUITEM ||
	obj->Type() == PAD_MENU || obj->Type() == PAD_CHOICEMENU) {
        item = (Pad_MenuItem*)obj;
	if (item->Is_enabled()) {
	    item->_hilite = FALSE;
	    if (item->Type() == PAD_MENU && item->Get_menu()) {
	        if (!((Pad_Menu*)item)->Find_item(padEvent->pt)) {
		          // a submenu, if cursor is not on an option then
		          // hide its menuitems.
		    ((Pad_Menu*)item)->Hide_items();	        
		}
	    }
	    item->Damage();
	}
    }

    return(PAD_BREAK);
}

//
// Arm menuitem on button press.
//
int
Pad_MenuItem::Press(Pad_Object *obj, ClientData data, Pad_Event *ev)
{
    Pad_MenuItem *item;

    if (obj->Type() == PAD_MENUITEM) {
	item = (Pad_MenuItem*)obj;
	if (item->Is_enabled()) {
	    item->Request_focus();
	    item->Arm();
	}
    } else if (obj->Type() == PAD_CHECKBOXMENUITEM) {
        return ((Pad_CheckboxMenuItem*)obj)->Press(obj, data, ev);
    }

    return(PAD_BREAK);
}

//
// Arm/disarm menuitem as the cursor enters/leaves it.
//
int
Pad_MenuItem::Drag(Pad_Object *obj, ClientData, Pad_Event *padEvent)
{
    Pad_MenuItem *item;

    if (obj->Type() == PAD_MENUITEM) {
	item = (Pad_MenuItem *)obj;
	if (item->Is_enabled()) {
	    if (item->Get_active_area()->Contains(padEvent->objPt)) {
				// Pointer within menuitem, arm it.
		if (!item->Is_armmed()) {
		    item->_hilite = TRUE;
		    item->Arm();
		}
	    } else {
				// Pointer outside menuitem, disarm.
		if (item->Is_armmed()) {
		    item->_hilite = FALSE;
		    item->Disarm();
		}
	    }
	}
    }

    return(PAD_BREAK);
}

//
// Activate menuitem command on mouse release.
// Pull up all menus and submenus.
//
int
Pad_MenuItem::Release(Pad_Object *obj, ClientData data, Pad_Event *padEvent)
{
    Pad_MenuItem *item;
    Pad_Callback *command;
    Pad_ObjectHandle *handle;

    if (obj->Type() == PAD_MENUITEM) {
	item = (Pad_MenuItem *)obj;
	if (item->Is_enabled()) {
	    if (item->Is_armmed()) {
	                         // Get callback and execute it
		command = item->Get_callback();
		handle = new Pad_ObjectHandle(item);
		if (command) {
		    command->Eval(item, padEvent);
		    if (handle->Get_object() == NULL) {
		        delete handle;
			return(PAD_BREAK);
		    }
		}

		                 // disram, and pull up menu (if there's one)
		item->Disarm();
		item->_hilite = FALSE;
		Pad_Menu *menu;
		                // pull up all submenus too
		while ((menu = item->Get_menu())) {
		    menu->_hilite = FALSE;
		    menu->Disarm();
		    menu->Hide_items();
		    if (menu->Get_MenuBar()) {
		        menu->Get_MenuBar()->Set_Current(NULL);
		    }		    

		                 // Activate callbacks for choicemennus
		    if (menu->Type() == PAD_CHOICEMENU) {
		        ((Pad_ChoiceMenu*)menu)->Set_Selected(item);
			command = menu->Get_callback();
			Pad_ObjectHandle *h = new Pad_ObjectHandle(menu);
			if (command) {
			    command->Eval(menu, padEvent);
			    if (handle->Get_object() == NULL) {
			        delete h;
				return(PAD_BREAK);
			    }
			    delete h;			
			}
		    }

		    item = menu;
		}
		
		delete handle;
	    }
	}
    } else if (obj->Type() == PAD_CHECKBOXMENUITEM) {
        return ((Pad_CheckboxMenuItem*)obj)->Release(obj, data, padEvent);
    }
 

    return(PAD_BREAK);
}
//////////////////////////////////////////////////////////////
//
// Pad_CheckboxMenuItem class
//
//////////////////////////////////////////////////////////////

Pad_CheckboxMenuItem::Pad_CheckboxMenuItem(Pad *pad) :
Pad_MenuItem(pad)
{
				// Initialize default event handlers
    _Initialize_events(pad->view->win,"CheckboxMenuItem");

    _type = PAD_CHECKBOXMENUITEM;

    Add_tag("CheckboxMenuItem");
    Delete_tag("MenuItem");
}

void
Pad_CheckboxMenuItem::Minimum_size(Pad_Dimension &dimension)
{
    Pad_MenuItem::Minimum_size(dimension);
    dimension.width += CHECKBOX_SIZE + CHECKBOX_OFFSET;
}

//
// Change the border color appropriately when fill is changed.
//
void
Pad_CheckboxMenuItem::Fill_changed(void)
{
    Pad_MenuItem::Fill_changed();

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
Pad_CheckboxMenuItem::Render(void)
{
    float bb[4];
    float xoffset, yoffset;
    Pad_Point min, max;
    Pad_PList ptList;

    _refineNeeded = FALSE;

				// First draw background
    if (_fillColor.Is_set()) {
      Pad_renderer->Set_color(_fillColor);
      if (!_hilite) {
	Pad_renderer->Draw_filled_box(_activeArea.x, _activeArea.y,
				      _activeArea.x + _activeArea.width,
				      _activeArea.y + _activeArea.height);
      } else {
	if (_border.Is_set()) {
	  min.Set(_activeArea.x, _activeArea.y);
	  max.Set(_activeArea.x + _activeArea.width, _activeArea.y + _activeArea.height);
	  Pad_renderer->Set_border(_border);
	  Pad_renderer->Draw_filled_3d_rectangle(min, max, _offset*0.5, PAD_RELIEF_RAISED);
	}
      }
    }

                                // Then draw the checkbox
    min.Set(_activeArea.x+CHECKBOX_OFFSET, 
	    _activeArea.y + (_activeArea.height/2.0 - CHECKBOX_SIZE/2.0));
    max.Set(_activeArea.x + CHECKBOX_OFFSET+CHECKBOX_SIZE, 
	    _activeArea.y + (_activeArea.height/2.0 + CHECKBOX_SIZE/2.0));

    if (Is_armmed()) {
        if (_activeBorder.Is_set()) {
	    Pad_renderer->Set_border(_activeBorder);
	    Pad_renderer->Draw_filled_3d_rectangle(min, max, 1, PAD_RELIEF_SUNKEN);
	}
    } else {
        if (_border.Is_set()) {
	    Pad_renderer->Set_border(_border);
	    Pad_renderer->Draw_filled_3d_rectangle(min, max, 1, PAD_RELIEF_RAISED);
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

    return(TRUE);
}       

//
// Call this once to define event handlers for all Checkboxes
//
void
Pad_CheckboxMenuItem::_Initialize_events(Pad_Win *win, char *type)
{
    Pad_Callback *callback;

    if (initializedWindows.Member(win)) {
        return;
    }
    initializedWindows.Push_last(win);

				// Mouse Etner
    callback = new Pad_Callback(Enter, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid(type), "<Run-Enter>", callback);

				// Mouse Leave
    callback = new Pad_Callback(Leave, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid(type), "<Run-Leave>", callback);
    
				// Button Press
    callback = new Pad_Callback(Press, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid(type), "<Run-ButtonPress-1>", callback);

				// Button Drag
    callback = new Pad_Callback(Drag, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid(type), "<Run-B1-Motion>", callback);

				// Button Release
    callback = new Pad_Callback(Release, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid(type), "<Run-ButtonRelease-1>", callback);
}
    
//
// Toggle checkbox state
//
int
Pad_CheckboxMenuItem::Press(Pad_Object *obj, ClientData, Pad_Event *)
{
    Pad_CheckboxMenuItem *checkbox;

    if (obj->Type() == PAD_CHECKBOXMENUITEM) {
	checkbox = (Pad_CheckboxMenuItem *)obj;
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
Pad_CheckboxMenuItem::Drag(Pad_Object *obj, ClientData, Pad_Event *padEvent)
{
    Pad_CheckboxMenuItem *checkbox;

    if (obj->Type() == PAD_CHECKBOXMENUITEM) {
	checkbox = (Pad_CheckboxMenuItem *)obj;
	if (checkbox->Is_enabled()) {
	    if (checkbox->Get_active_area()->Contains(padEvent->objPt)) {
				// Pointer within button
	                        // Normally don't need to do anything (since
	                        // toggle was done at button press, but
	                        // if mouse was draged out then have to
	                        // redo the toggle.
	        checkbox->_hilite = TRUE;
	        if (_dragedOut) {
		    _dragedOut = FALSE;
		    if (!checkbox->Is_armmed()) {
		        checkbox->Arm();
		    } else {
		        checkbox->Disarm();
		    }
		}
	    } else {		// Pointer outside of button
	        checkbox->_hilite = FALSE;
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
Pad_CheckboxMenuItem::Release(Pad_Object *obj, ClientData, Pad_Event *padEvent)
{
    Pad_CheckboxMenuItem *checkbox;
    Pad_Callback *command;
    Pad_ObjectHandle *handle;

    if (obj->Type() == PAD_CHECKBOXMENUITEM) {
	checkbox = (Pad_CheckboxMenuItem *)obj;
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
	    
	    checkbox->_hilite = FALSE;
	    checkbox->Damage();  // force render

	    Pad_Menu *menu;
	    Pad_MenuItem *item = checkbox;

		                // pull up all submenus too
	    while ((menu = item->Get_menu())) {
	      menu->_hilite = FALSE;
	      menu->Disarm();
	      menu->Hide_items();
	      if (menu->Get_MenuBar()) {
		menu->Get_MenuBar()->Set_Current(NULL);
	      }
	                   // Activate callbacks for choicemennus
	      if (menu->Type() == PAD_CHOICEMENU) {
		((Pad_ChoiceMenu*)menu)->Set_Selected(item);
		command = menu->Get_callback();
		Pad_ObjectHandle *h = new Pad_ObjectHandle(menu);
		if (command) {
		  command->Eval(menu, padEvent);
		  if (handle->Get_object() == NULL) {
		    delete h;
		    return(PAD_BREAK);
		  }
		  delete h;			
		}
	      }

	      item = menu;
	    }
	}
    }

    return(PAD_BREAK);
}

//
// Set the checkbox state to set or unset (normal or active).
//
void
Pad_CheckboxMenuItem::Set_state(Pad_Bool state)
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
Pad_CheckboxMenuItem::Get_state(void)
{
    return Is_armmed();
}

//////////////////////////////////////////////////////////////
//
// Pad_Menu class
//
//////////////////////////////////////////////////////////////

Pad_Menu::Pad_Menu(Pad *pad) :
  Pad_MenuItem(pad)
{
    _Initialize_events(pad->view->win,"Menu");
    _type = PAD_MENU;
    Set_alignment(COMPONENT_ALIGN_CENTER);

    // setup panel for menuitems
    _itemsPanel = new Pad_MenuItemsPanel(pad);
    _itemsPanel->Set_menu(this);

    Update();

    Delete_tag("MenuItem");
    Add_tag("Menu");
}

Pad_Menu::~Pad_Menu()
{
    // delete the items panel
    if (_itemsPanel) {
        _itemsPanel->Set_menu(NULL);  // so it won't try to delete this again
	delete _itemsPanel;
    }
}

Pad_List&
Pad_Menu::Get_items(void)
{
    return _itemsPanel->members;
}

Pad_Bool
Pad_Menu::Render(void)
{
    Pad_Bool rc;

                                  // Render the menu button itself as menuitem
    rc = Pad_MenuItem::Render();
    
                                  // Render an indicator for submenus
    if ( _menu ) {
        Pad_PList ptList;
        Pad_Point pt;
	pt.Set(_activeArea.x + _activeArea.width - 5,
	       _activeArea.y + (_activeArea.height/2.0));
	ptList.Push_last(&pt);
	pt.Set(pt.x - 5, pt.y - 5);
	ptList.Push_last(&pt);
	pt.Set(pt.x, pt.y + 10);
	ptList.Push_last(&pt);
	       
	Pad_renderer->Set_color(_fillColor);
	if (_hilite) {
	    Pad_renderer->Set_border(_border);
	    Pad_renderer->Draw_filled_3d_polygon(ptList, 1, PAD_RELIEF_RAISED);
	} else {
	    Pad_renderer->Set_border(_activeBorder);	  
	    Pad_renderer->Draw_filled_3d_polygon(ptList, 1, PAD_RELIEF_SUNKEN);
	}
    }

    return rc;
}

//
// Add a meunitem to the menu.  Item is added as the last option.
// 
void
Pad_Menu::Add(Pad_MenuItem *item)
{
    Pad_List &items = Get_items();

                               // No-op if item is already in the menu
    if (items.Member(item)) {
        return;
    }

                              // Scale and fill item to that of the menu
    item->Set_abs_position_z(Get_abs_position_z());
    int r, g, b;
    _fillColor.Get(r, g, b);
    item->Set_fill(r, g, b);

    float itemWidth = item->Get_width();

                             // If a submenu, add space for its indicator
    if (item->Type() == PAD_MENU) {
      itemWidth += 20;
      item-> Set_alignment(COMPONENT_ALIGN_LEFT);
      item->Set_width(itemWidth);
    }
                             // Adjust width of existing items if new item 
                             // is longer than them
    float maxWidth = _itemsPanel->Get_width();
    Pad_Iterator iter;
    Pad_MenuItem *curItem;

    if (itemWidth > maxWidth) {
        DOLIST(iter, items, Pad_MenuItem, curItem) {     
	    curItem->Set_width(itemWidth);
	}
    } else {
        item->Set_width(maxWidth);
    }
        
                            // Align new item below the current items
    if (items.Length() > 0) {
        Pad_List list;
	list.Push(item);
	Pad_Point targetPt(0,0), pt(0, 1);
	pad->Layout_position(_itemsPanel, targetPt, list, pt, 0);
    }

                           // Now add it to the items group
    _itemsPanel->Add(item, TRUE);
    item->Set_menu(this);
    
}

//
// Add a seperator menuitem to the menu.  Create a menuitem and set its
// label to "-".
//
void
Pad_Menu::Add_Seperator(void)
{
    Pad_MenuItem *item = new Pad_MenuItem(pad);
    item->Set_label("-");
    Add(item);
}

//
// Remove item from the menu.
//
void
Pad_Menu::Remove(Pad_MenuItem *item)
{
    _itemsPanel->Remove(item);
}

void
Pad_Menu::Remove(int index)
{
    Pad_List &items = Get_items();
    Remove((Pad_MenuItem*) items.Nth(index));
}

void
Pad_Menu::_Initialize_events(Pad_Win *win, char *type)
{
    Pad_Callback *callback;

    if (initializedWindows.Member(win)) {
        return;
    }
    initializedWindows.Push_last(win);

				// Mouse Etner
    callback = new Pad_Callback(Enter, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid(type), "<Run-Enter>", callback);

				// Mouse Leave
    callback = new Pad_Callback(Leave, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid(type), "<Run-Leave>", callback);

				// Button Press
    callback = new Pad_Callback(Press, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid(type), "<Run-ButtonPress-1>", callback);

				// Button Drag
    callback = new Pad_Callback(Drag, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid(type), "<Run-B1-Motion>", callback);

				// Button Release
    callback = new Pad_Callback(Release, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid(type), "<Run-ButtonRelease-1>", callback);
}

//
// On button press, display the mnu options
//
int
Pad_Menu::Press(Pad_Object *obj, ClientData, Pad_Event *)
{
    Pad_Menu *menu;

    if (obj->Type() == PAD_MENU ||
	obj->Type() == PAD_CHOICEMENU) {
	menu = (Pad_Menu*)obj;
	if (menu->Is_enabled()) {
	    menu->Request_focus();
	    menu->Arm();
	    menu->Display_items();
	    if (menu->Get_MenuBar()) {
	        menu->Get_MenuBar()->Set_Current(menu);
	    }
	}
    }

    return(PAD_BREAK);
}

//
// On pointer drag, keep track of which menuitem should be hilited.
//
int
Pad_Menu::Drag(Pad_Object *obj, ClientData data, Pad_Event *padEvent)
{
    Pad_Menu *menu;

    if (obj->Type() == PAD_MENU ||
	obj->Type() == PAD_CHOICEMENU) {
	menu = (Pad_Menu *)obj;
	                    // Make sure we are working with the current menu
	if (menu->Get_MenuBar()) {
	    if (menu != menu->Get_MenuBar()->Get_Current()) {
	        menu = menu->Get_MenuBar()->Get_Current();
	    }
	}	

	if (menu->Is_enabled()) {
	                           // Get the event point in menu coords
	    Pad_Point objPt = padEvent->pt;
	    menu->Screen_to_local(objPt);

	    if (menu->Get_active_area()->Contains(objPt)) {
				// Pointer within button
		if (!menu->Is_armmed()) {
		    menu->_hilite = TRUE;
		    menu->Arm();
		    menu->Display_items();
		}

	    } else {
				// Pointer outside of menu
	        Pad_MenuItem *item = menu->Find_item(padEvent->pt);
		if (item) {
		                // Pointer on a menuitem, hilite it only.
		    item->Enter(item, data, padEvent);
		    item->Get_menu()->Hilite_item(item, data, padEvent);

		} else {
		                // Pointer outside menuitems, check other menus
		     Pad_MenuBar *menuBar = menu->Get_MenuBar();
		     if (menuBar) {
		         Pad_Menu* curMenu = menuBar->Find_item(padEvent->pt);
			 if (curMenu) {
			     menu->Leave(menu, data, padEvent);
			     menu->Disarm();
			     curMenu->_hilite = TRUE;
			     curMenu->Press(curMenu, data, padEvent);
			 }
		     }
		}
	    }
	}
    }

    return(PAD_BREAK);
}

//
// On pointer release, activate selected menuitem.
//
int
Pad_Menu::Release(Pad_Object *obj, ClientData data, Pad_Event *padEvent)
{
    Pad_Menu *menu;
    Pad_ObjectHandle *handle;

    if (obj->Type() == PAD_MENU ||
	obj->Type() == PAD_CHOICEMENU) {
	menu = (Pad_Menu*)obj;
	if (menu->Get_MenuBar()) {
	    menu = menu->Get_MenuBar()->Get_Current();
	}
	if (menu->Is_enabled()) {
	    if (menu->Is_armmed()) {
	        Pad_Point objPt = padEvent->pt;
		menu->Screen_to_local(objPt);
	        if (menu->Get_active_area()->Contains(objPt)) {
		    // Pointer within menu, disarm it (but leave options up)
		    menu->Disarm();

		} else {
		    // If pointer released on a menu item, activate it
		    handle = new Pad_ObjectHandle(menu);
		    Pad_MenuItem *item = menu->Find_item(padEvent->pt);
		    if (item) {
		        item->Press(item, data, padEvent);
		        item->Release(item, data, padEvent);
		    }
		    
		    if (handle->Get_object() == NULL) {
		        delete handle;
			return PAD_BREAK;
		    }

		    menu->Disarm();

		    if ( !item || item->Type() != PAD_MENU ) {
		        menu->Hide_items();
			if (menu->Get_MenuBar()) {
			    menu->Get_MenuBar()->Set_Current(NULL);
			}
		    }
		}
	    }
	}
    }

    return(PAD_BREAK);
}

//
// Return menuitem which contains given point (check submenu items too).
// 
Pad_MenuItem*
Pad_Menu::Find_item(Pad_Point &padPt)
{
    Pad_MenuItem *item, *rc=NULL;
    Pad_Iterator iter;
    Pad_Point objPt, grpPt;
    Pad_List &items = Get_items();

    // Get event point in menu's coords
    grpPt = padPt;
    _itemsPanel->Screen_to_local(grpPt);
    
    DOLIST(iter, items, Pad_MenuItem, item) {
        objPt = grpPt;
        item->transform.Invert(objPt);
        if (item->Get_active_area()->Contains(objPt)) {
	    rc = item;
	    break;
	
	} else if (item->Type() == PAD_MENU) {
	    Pad_Menu *subMenu = (Pad_Menu*) item;
	    if ((rc = subMenu->Find_item(padPt))) {
	        break;
	    }
	}
    }

    return rc;
}

//
// Only hilite a given menu item.
//
void 
Pad_Menu::Hilite_item(Pad_MenuItem *curItem, ClientData data, Pad_Event *padEvent)
{
    Pad_MenuItem *item;
    Pad_Iterator iter;
    Pad_List &items = Get_items();

    DOLIST(iter, items, Pad_MenuItem, item) {
	item->Leave(item, data, padEvent);
    }

    curItem->Enter(curItem, data, padEvent);
}

//
// Position and display menuitems panel.
//
void
Pad_Menu::Display_items(void)
{
    Pad_Point targetPt(0,0), pt(0, 1);
    Pad_List list;

    // scale menuitems to that of the menu
    _itemsPanel->Set_abs_position_z(Get_abs_position_z());
   
    // position menuitems relative to the menu
    list.Push(_itemsPanel);
    if ( _menu ) {
      targetPt.Set(1,1);
    }

    pad->Layout_position(this, targetPt, list, pt, 0);    
    
    // make menuitems panel visible and receive events
    _itemsPanel->Set_transparency(1.0);
    _itemsPanel->Set_events(TRUE);
    _itemsPanel->Raise();

    int r, g, b;
    _fillColor.Get(r, g, b);
    _itemsPanel->Set_fill(r, g, b);

    // have menuitems receive events
    Pad_MenuItem *item;
    Pad_Iterator iter;    
    Pad_List &items = Get_items();
    ClientData dummyData=0;

    DOLIST(iter, items, Pad_MenuItem, item) {
	item->Set_fill(r, g, b);	
        item->Set_events((item->_seperator) ? FALSE : TRUE);
	item->Leave(item, dummyData, NULL);
    }

}

//
// Make menuitems panel invisible.
//
void
Pad_Menu::Hide_items(void)
{
    // make all items invisible and not receive evetns
    _itemsPanel->Set_transparency(0.0);
    _itemsPanel->Set_events(FALSE);

    // make submenus invisible too
    Pad_MenuItem *item;
    Pad_Iterator iter;    
    Pad_List &items = Get_items();

    DOLIST(iter, items, Pad_MenuItem, item) {
        item->Set_events(FALSE);
	if (item->Type() == PAD_MENU) {
	    ((Pad_Menu*)item)->Hide_items();
      }
    }
}

//////////////////////////////////////////////////////////////
//
// Pad_ChoiceMenu class
//
//////////////////////////////////////////////////////////////

Pad_ChoiceMenu::Pad_ChoiceMenu(Pad *pad)
  : Pad_Menu(pad)
{
    _Initialize_events(pad->view->win,"ChoiceMenu");
    _type = PAD_CHOICEMENU;
    _selectedItem = NULL;
    Set_alignment(COMPONENT_ALIGN_LEFT);    
    
    Delete_tag("Menu");
    Add_tag("ChoiceMenu");
}

//
// Create a menuitem and add it to the choice menu.
//
void
Pad_ChoiceMenu::Add(char *item, int /*index*/)
{
    Pad_MenuItem *mitem = new Pad_MenuItem(pad);
    mitem->Set_label(item);

    Add(mitem);  // HACK: ignore index for now, add as the last item
}

void
Pad_ChoiceMenu::Add(Pad_MenuItem *item)
{
    if (_selectedItem == NULL) {
        Set_Selected(item);
    }

    Pad_Menu::Add(item);

    if ((item->Get_width() + 10) > Get_width()) {
        Set_width(item->Get_width() + 10);
    }
}

void
Pad_ChoiceMenu::Set_Selected(int pos)
{
    Pad_List &items = Get_items();
    Set_Selected((Pad_MenuItem*) items.Nth(pos));
}

void
Pad_ChoiceMenu::Set_Selected(Pad_MenuItem *item)
{
    if (item) {
        Set_label(item->Get_label());
    } else {
        Set_label("");
    }
    
    _selectedItem = item;
    Update();
}

int
Pad_ChoiceMenu::Get_Selected(void)
{
    Pad_List &items = Get_items();
    int index = items.Index(_selectedItem);
    return index;
}

void
Pad_ChoiceMenu::Display_items(void)
{
    Pad_Menu::Display_items();
    if (_selectedItem) {
         ClientData dummyData=0;
        _selectedItem->Enter(_selectedItem,  dummyData, NULL);
    }
}
    
void
Pad_ChoiceMenu::Hilite_item(Pad_MenuItem *curItem, ClientData data, Pad_Event *padEvent)
{
    Pad_Menu::Hilite_item(curItem, data, padEvent);
    if (_selectedItem) {
        _selectedItem->Enter(_selectedItem, data, padEvent);
    }
}

Pad_Bool
Pad_ChoiceMenu::Render(void)
{
    Pad_Bool rc;
    Pad_Point min, max;

    rc = Pad_Button::Render();

                       // Render a checkbox next to menu title
    min.Set(_activeArea.x + _activeArea.width - 12,
	    _activeArea.y + (_activeArea.height/2.0 - 2.5));
    max.Set(_activeArea.x + _activeArea.width - 2,
	    _activeArea.y + (_activeArea.height/2.0 + 2.5));

    Pad_renderer->Set_color(_fillColor);

    if (Is_armmed()) {
        if (_activeBorder.Is_set()) {
	    Pad_renderer->Set_border(_activeBorder);
	    Pad_renderer->Draw_filled_3d_rectangle(min, max, 1, PAD_RELIEF_SUNKEN);
	}
    } else {
        if (_border.Is_set()) {
	    Pad_renderer->Set_border(_border);
	    Pad_renderer->Draw_filled_3d_rectangle(min, max, 1, PAD_RELIEF_RAISED);
	}
    }
    
    return(rc);
}

void
Pad_ChoiceMenu::_Initialize_events(Pad_Win *win, char *type)
{
    Pad_Callback *callback;

    if (initializedWindows.Member(win)) {
        return;
    }
    initializedWindows.Push_last(win);

				// Mouse Etner
    callback = new Pad_Callback(Enter, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid(type), "<Run-Enter>", callback);

				// Mouse Leave
    callback = new Pad_Callback(Leave, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid(type), "<Run-Leave>", callback);

				// Button Press
    callback = new Pad_Callback(Press, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid(type), "<Run-ButtonPress-1>", callback);

				// Button Drag
    callback = new Pad_Callback(Drag, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid(type), "<Run-B1-Motion>", callback);

				// Button Release
    callback = new Pad_Callback(Release, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid(type), "<Run-ButtonRelease-1>", callback);
}

//////////////////////////////////////////////////////////////
//
// Pad_MenuBar class
//
//////////////////////////////////////////////////////////////

Pad_MenuBar::Pad_MenuBar(Pad *pad)
  : Pad_Panel(pad)
{
    _type = PAD_MENUBAR;
    _curMenu = NULL;

    Delete_tag("Panel");
    Add_tag("MenuBar");
}

Pad_MenuBar::~Pad_MenuBar()
{
}

void
Pad_MenuBar::Add(Pad_MenuItem *menu)
{
    Add(menu, FALSE);
    //menu->Set_MenuBar(this);

    // Add the menuitem panel to the manubar's frame (if menubar belongs to one).
    if (group && menu->Type() == PAD_MENU) {
        group->Add(((Pad_Menu*)menu)->_itemsPanel);
    }
}

void
Pad_MenuBar::Remove(Pad_MenuItem *menu)
{
    Pad_Panel::Remove(menu, FALSE);
}

void
Pad_MenuBar::Remove(int menuIndex)
{
    Remove((Pad_Menu*) members.Nth(menuIndex));
}

void
Pad_MenuBar::Set_HelpMenu(Pad_Menu *menu)
{
    Add(menu);  // Hack it for now
}

Pad_Menu*
Pad_MenuBar::Get_HelpMenu(void)
{
    return (Pad_Menu*) members.Last();  // hacking for now
}

//
// Move component into menubar
//
Pad_Bool
Pad_MenuBar::Add(Pad_Object *obj, Pad_Bool)
{
    Pad_Bool rc = Pad_Panel::Add(obj, FALSE);
    ((Pad_MenuItem*)obj)->Set_MenuBar(this);  // HACK: just cast it for now
    return(rc);
}

//
// Reshape menubar and reposition its menus.
//
void
Pad_MenuBar::Reshape(float x, float y, float width, float height)
{
    Pad_Container::Reshape(x, y, width, height);
    Position_menus();
}

//
// Override container's Set_width() & height methods so members
// are not effected.
//
Pad_Bool
Pad_MenuBar::Set_width(float w)
{
    return Pad_Component::Set_width(w);
}

Pad_Bool
Pad_MenuBar::Set_height(float h)
{
    return Pad_Component::Set_height(h);
}

//
// Position menubar's menus by spreading horizontally.
//
void
Pad_MenuBar::Position_menus(void)
{
    Pad_Menu *menu;
    Pad_Iterator iter;    

    float xoffset, yoffset;
    xoffset = 10;
    yoffset = 0;

    DOLIST(iter, members, Pad_Menu, menu) {
        menu->Reshape(xoffset, yoffset+1, menu->Get_width(), Get_height()-2);
	menu->Hide_items();
	xoffset += menu->Get_width() + 20;
    }
  
}

//
// Find menubar's menu which caontains given point.
//
Pad_Menu*
Pad_MenuBar::Find_item(Pad_Point &padPt)
{
    Pad_Menu *item, *rc=NULL;
    Pad_Iterator iter;
    Pad_Point objPt, grpPt;

    // Get event point in menubar coords
    grpPt = padPt;
    Screen_to_local(grpPt);
   
    DOLIST(iter, members, Pad_Menu, item) {
        objPt = grpPt;
        item->transform.Invert(objPt);
        if (item->Get_active_area()->Contains(objPt)) {
	    rc = item;
	    break;
	
	}
    }

    return rc;
}

Pad_Menu*
Pad_MenuBar::Get_Current(void)
{
    return _curMenu;
}

void
Pad_MenuBar::Set_Current(Pad_Menu *menu)
{
    if (_curMenu) {
        _curMenu->Hide_items();
    }
    
    _curMenu = menu;
}

//
// Menubar rendering is like a panel except we want a 3d filled rectangle
// for its background.
//
Pad_Bool
Pad_MenuBar::Render(void)
{
    Pad_Point min, max;
    float dx, dy;
    dx = _activeArea.width * 0.1;
    dy = _activeArea.height * 0.1;
    dx = dy = 0;
   
    min.Set(_activeArea.x - dx, _activeArea.y - dy);
    max.Set(_activeArea.x + _activeArea.width + 2*dx,
	    _activeArea.y + _activeArea.height + 2*dy);
    Pad_renderer->Set_color(_fillColor);
    Pad_BorderRef border;
    border.Set(_fillColor);
    Pad_renderer->Set_border(border);
    Pad_renderer->Draw_filled_3d_rectangle(min, max, 2, PAD_RELIEF_RAISED);
    
    Pad_Bool rc = Pad_Group::Render();

    return rc;
}

//////////////////////////////////////////////////////////////
//
// Pad_MenuItemsPanel class
//
//////////////////////////////////////////////////////////////

Pad_MenuItemsPanel::Pad_MenuItemsPanel(Pad *pad)
  : Pad_Panel(pad)
{
}

Pad_MenuItemsPanel::~Pad_MenuItemsPanel()
{
    // delete the menu for this
    if (_menu) {
        _menu->_itemsPanel = NULL;  // so it won't try to delete this again
	delete _menu;
    }
}

//
// Override panel Add() method so we get a group like Add (panel is
// reshaped).
//
Pad_Bool
Pad_MenuItemsPanel::Add(Pad_Object *obj, Pad_Bool transform)
{
    return Pad_Group::Add(obj, transform);
}

void
Pad_MenuItemsPanel::Compute_bounding_box(void)
{
    Pad_Group::Compute_bounding_box();
    _activeArea.Set(bbox.Xmin(), bbox.Ymin(), bbox.Width(), bbox.Height());
}

Pad_Bool
Pad_MenuItemsPanel::Set_width(float w)
{
    return Pad_Component::Set_width(w);
}

Pad_Bool
Pad_MenuItemsPanel::Set_height(float h)
{
    return Pad_Component::Set_height(h);
}

void
Pad_MenuItemsPanel::Set_menu(Pad_Menu *m)
{
    _menu = m;
}

Pad_Menu*
Pad_MenuItemsPanel::Get_menu(void)
{
    return _menu;
}

Pad_Bool
Pad_MenuItemsPanel::Render(void)
{
    Pad_Point min, max;
    float dx, dy;
    dx = _activeArea.width * 0.1;
    dy = _activeArea.height * 0.1;
    dx = dy = 0;
    min.Set(_activeArea.x - dx, _activeArea.y - dy);
    max.Set(_activeArea.x + _activeArea.width + 2*dx, 
	    _activeArea.y + _activeArea.height + 2*dy);
    Pad_renderer->Set_color(_fillColor);
    Pad_BorderRef border;
    border.Set(_fillColor);
    Pad_renderer->Set_border(border);
    Pad_renderer->Draw_filled_3d_rectangle(min, max, 2, PAD_RELIEF_RAISED);
        
    return Pad_Group::Render();
}
