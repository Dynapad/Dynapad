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

#ifndef MENU_H
#define MENU_H 1

#include "defs.h"
#include "component.h"
#include "pad-string.h"
#include "callback.h"
#include "button.h"
#include "container.h"

class Pad_Menu;
class Pad_MenuBar;
class Pad_MenuItemsPanel;

//
// A Pad_MenuItem is like a button but with different rendering
// and callbacks.  It can belong to a menu, or a menubar.
//

class Pad_MenuItem : public Pad_Button {
public:

    Pad_MenuItem(Pad *pad);

                              // Methods for item's menu or menubar
    void         Set_menu(Pad_Menu*);
    Pad_Menu*    Get_menu(void);
    void         Set_MenuBar(Pad_MenuBar*);
    Pad_MenuBar* Get_MenuBar(void);

                              // Override button label and render methods
    virtual void     Set_label(char*);
    virtual Pad_Bool Render(void);

                              // Callbacks for mouse events
    static Pad_EventCallbackProc(Enter);
    static Pad_EventCallbackProc(Leave);
    static Pad_EventCallbackProc(Press);
    static Pad_EventCallbackProc(Release);
    static Pad_EventCallbackProc(Drag);  

    PAD_TYPE("menuitem");

                               // Hide button arm methods because menuitem 
			       // has its own state semantics.
    void Arm(void);
    void Disarm(void);
    Pad_Bool Is_armmed(void);

protected:
    Pad_Menu    *_menu;         // items's menu
    Pad_MenuBar *_menuBar;      // items's menubar

    static void     _Initialize_events(Pad_Win *win, const char *type);
    static Pad_List initializedWindows;

public:
    Pad_Bool _hilite;        // true when item should be hilitied
    Pad_Bool _seperator;     // true if item is a seperator in a menu
};


//
// A Pad_CheckboxMenuItem is a menuitem with a checkbox indicating
// its state (active/normal).
//
class Pad_CheckboxMenuItem : public Pad_MenuItem {
public:
    Pad_CheckboxMenuItem(Pad *pad);

                        // Mehtods for checkbox state
    virtual Pad_Bool Get_state(void);
    virtual void     Set_state(Pad_Bool);

                        // Override menuitem methods for special look and size
    virtual void     Minimum_size(Pad_Dimension &dimension);
    virtual Pad_Bool Render(void);
    virtual void     Fill_changed(void);
    
    PAD_TYPE("checkboxmenuitem");

			 // Mouse event handlers
    static Pad_EventCallbackProc(Press);
    static Pad_EventCallbackProc(Drag);
    static Pad_EventCallbackProc(Release);

private:
    static Pad_Bool _dragedOut;   // for dealing with mouse drags after button press
    static Pad_List initializedWindows;
    static void     _Initialize_events(Pad_Win *win, const char *);
};


//
// A Pad_Menu is a menuitem with a panel of menuitems,
// and bindings for displaying and selecting those menuitems.
//
class Pad_Menu : public Pad_MenuItem {
public:

    Pad_Menu(Pad *pad);
    virtual ~Pad_Menu(void);
  
                               // Methods for adding items and seperators
    virtual void Add(Pad_MenuItem *);
    void         Add_Seperator(void);
    virtual void Remove(int);
    virtual void Remove(Pad_MenuItem*);

                              // Override meothds for dealing with special
                              // options, rednering and writing.
    virtual Pad_Bool Render(void);

                              // Mouse event callbacks
    static Pad_EventCallbackProc(Press);
    static Pad_EventCallbackProc(Release);
    static Pad_EventCallbackProc(Drag);  

    PAD_TYPE("menu");

public:
                             // Methods for locating & displaying menuitems
    Pad_MenuItem* Find_item(Pad_Point &);
    virtual void  Hilite_item(Pad_MenuItem*, ClientData, Pad_Event*);
    virtual void  Display_items(void);
    virtual void  Hide_items(void);
    
protected:
    Pad_List&        Get_items(void);
    static void     _Initialize_events(Pad_Win *win, const char *);
    static Pad_List initializedWindows;

public:
    
    Pad_MenuItemsPanel *_itemsPanel;  // items panel
};


//
// A Pad_MenuItemsPanel is a private class used for managing items in menus.
//
class Pad_MenuItemsPanel : public Pad_Panel {
public:
  Pad_MenuItemsPanel(Pad*);
  ~Pad_MenuItemsPanel();

                         // Methods for dealing with panel's menu
  void      Set_menu(Pad_Menu*);
  Pad_Menu* Get_menu(void);

                         // Override panel methods for bbox and rendering, etc.
  void     Compute_bounding_box(void);  
  Pad_Bool Add(Pad_Object *obj, Pad_Bool transform=FALSE);
  Pad_Bool Render(void);
  Pad_Bool Set_height(float height);
  Pad_Bool Set_width(float width);

private:
  Pad_Menu *_menu;      // panel's menu (should really be an object ref)
};


//
// A Pad_ChoiceMenu is a Pad_Menu with the menu button restricted to
// one of the menuitems.
//

class Pad_ChoiceMenu : public Pad_Menu {
public:
    Pad_ChoiceMenu(Pad *);

                          // Methods for dealing with the selected item
    void     Set_Selected(int);
    void     Set_Selected(Pad_MenuItem*);
    int      Get_Selected(void);

                          // Methods for adding menuitems
    void     Add(char *item, int index);
    void     Add(Pad_MenuItem*);

    Pad_Bool Render(void);

    PAD_TYPE("choicemenu");

protected:    
    void     Display_items(void);
    void     Hilite_item(Pad_MenuItem *curItem, ClientData, Pad_Event *);
    Pad_MenuItem *_selectedItem;
    static void _Initialize_events(Pad_Win *win, const char *);
    static Pad_List initializedWindows;
};

//
// A Pad_MenuBar is a collection of menus.
//
class Pad_MenuBar : public Pad_Panel {
public:

    Pad_MenuBar(Pad*);
    ~Pad_MenuBar();

    void Add(Pad_MenuItem*);
    void Remove(Pad_MenuItem*);
    void Remove(int);

    void      Set_HelpMenu(Pad_Menu*);
    Pad_Menu* Get_HelpMenu(void);

    PAD_TYPE("menubar");

    Pad_Bool Add(Pad_Object*, Pad_Bool);
    void     Reshape(float x, float y, float width, float height);
    Pad_Bool Render(void);
    Pad_Bool Set_height(float height);
    Pad_Bool Set_width(float width);

    void      Position_menus(void);
    Pad_Menu* Find_item(Pad_Point &);
    Pad_Menu* Get_Current(void);
    void      Set_Current(Pad_Menu*);

public:
    Pad_Menu *_curMenu;
};

#endif
