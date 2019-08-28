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

#ifndef CONTAINER_H
#define CONTAINER_H 1

#include "defs.h"
#include "group.h"
#include "callback.h"

class Pad_Event;
class Pad_MenuBar;

//
// A Pad_Container is the base type for containment
// It has a fixed size - i.e., it does not shrink-wrap around its
// components like a Pad_Group, and components are added relative
// to its lower-left corner.
// This is not intended to be instantiated directly.
//
class Pad_Container : public Pad_Group {
  public:
    Pad_Container(Pad *pad);
    ~Pad_Container();

				// Methods for moving components in and out of container
    virtual Pad_Bool Add(Pad_Object *obj, Pad_Bool transform=FALSE);
    virtual Pad_Bool Remove(Pad_Object *obj, Pad_Bool transform=FALSE);
				// The minimum dimensions necessary for the window to look normal
    virtual void     Minimum_size(Pad_Dimension &dimension);
    virtual void     Preferred_size(Pad_Dimension &dimension);
				// Override group's bounding box mechanism
    virtual void     Compute_bounding_box(void);
				// Reshape, and keep members properly positioned
    virtual void     Reshape(float x, float y, float width, float height);

    virtual int      Create_obj_args(int argc, char **argv);   // Called when an item is created with string args
    virtual Pad_Bool Is_container(void);    // True if a container (or derived from one)

				// Get and set options
    virtual Pad_Bool Set_width(float width);
    virtual Pad_Bool Set_height(float height);

				// Make containers get event if there is no member within that gets it
    virtual Pad_Bool     Pick_group(Pad_Event *event);

    PAD_TYPE("container");
};

//
// A Pad_Panel is a container that is intended to be instantiated.
//
class Pad_Panel : public Pad_Container {
  public:
    Pad_Panel(Pad *pad);

				// Handle pointer moving in and out of panels
    virtual void     Pointer_in(Pad_Event *padEvent);
    virtual void     Pointer_out(Pad_Event *padEvent);
    virtual Pad_Bool Render(void);

    PAD_TYPE("panel");
};

//
// A Pad_Window is a container that acts like a top-level window
// within pad. It has no window dressing
//

				         // Bits for _windowFlags
#define WINDOW_RESIZEABLE   (1<<0)       // Set if window is resizable
#define WINDOW_HAS_POINTER  (1<<1)       // Set if window is active (has pointer)

class Pad_Window : public Pad_Container {
  public:
    Pad_Window(Pad *pad);
    ~Pad_Window();

    virtual Pad_Bool Render(void);

    virtual Pad_Bool Has_pointer(void);
    virtual void     Pointer_in(Pad_Event *padEvent);
    virtual void     Pointer_out(Pad_Event *padEvent);
				// When the frame is resized by a user it should invoke this callback
            void     Set_resize_callback(Pad_Callback *cb);
            Pad_Callback *Get_resize_callback(void);
				// When the frame is moved by a user it should invoke this callback
            void     Set_move_callback(Pad_Callback *cb);
            Pad_Callback *Get_move_callback(void);
				// When the frame is destroyed, it should invoke this callback
            void     Set_destroy_callback(Pad_Callback *cb);
				// Determines whether this frame is resizable.
            void     Set_resizeable(Pad_Bool resizeable);
            void     Set_resizeable_default(void);
            Pad_Bool Is_resizeable(void);

  private:
    unsigned char  _windowFlags;         // OR'd combination of flags
    Pad_Callback * _resizeCallback;      // This gets called whenever the frame is resized
    Pad_Callback * _moveCallback;        // This gets called whenever the frame is moved
    Pad_Callback * _destroyCallback;     // This gets called when the frame is destroyed
    Pad_ObjectHandle *_prevFocus;        // Focus obj within window when window is out of focus

    PAD_TYPE("window");
};

//
// A Pad_Frame is a window that has window dressing with default event handlers
// for manipulating it.
//
				           // Bits for _frameFlags
#define FRAME_TITLEBAR_PRESSED   (1<<0)    // Set if title bar is pressed
#define FRAME_USER_INTERACTING   (1<<1)    // Set if frame is currently being manipulated by user

class Pad_Frame : public Pad_Window {
  public:
    Pad_Frame(Pad *pad);
    virtual ~Pad_Frame();

    virtual Pad_Bool   Render(void);         // Render frame and its contents
    virtual void       Reshape(float x, float y, float width, float height);
    virtual Pad_Bool   Continue_refinement(void);

				// Notice when pointer goes in and out of frame
    virtual void       Pointer_in(Pad_Event *padEvent);
    virtual void       Pointer_out(Pad_Event *padEvent);
				// Sets the title of this frame to the specified title.
            void       Set_title(const char *title);

				// Keep track of user pressing of titlebar
            void       Set_titlebar_press(Pad_Bool pressed);
            Pad_Bool   Is_titlebar_pressed(void);

				// Keep track of user interacting with frame
            void       Set_user_interacting(Pad_Bool interacting);
            Pad_Bool   Is_user_interacting(void);

				// Sets/gets the cursor image to be one of the predefined cursors.
            void       Set_cursor(int cursorType);
            int        Get_cursor(void);

	    void         Set_menubar(Pad_MenuBar*);
	    Pad_MenuBar* Get_menubar(void);
    virtual void     Position_menubar(void);

				// Event handlers
    static Pad_EventCallbackProc(Motion);
    static Pad_EventCallbackProc(Leave);
    static Pad_EventCallbackProc(Press);
    static Pad_EventCallbackProc(Drag);
    static Pad_EventCallbackProc(Release);
    static Pad_EventCallbackProc(Zoom_in_press);
    static Pad_EventCallbackProc(Zoom_in_drag);
    static Pad_EventCallbackProc(Zoom_in_release);
    static Pad_EventCallbackProc(Zoom_out_press);
    static Pad_EventCallbackProc(Zoom_out_drag);
    static Pad_EventCallbackProc(Zoom_out_release);

				// Sets the icon image to display when this frame is iconized.
    //    void Set_icon_image(Image im);
				// Sets the menubar of this frame to the specified container.
    //    void Set_menu_bar(MenuBar mb);
    PAD_TYPE("frame");

  protected:
    int            _cursorType;          // Base Cursor for this frame
    unsigned char  _frameFlags;          // OR'd combination of flags
    Pad_BorderRef  _border;              // 3D Bevel
    Pad_BorderRef  _activeBorder;        // 3D Bevel when active
    Pad_String     _title;	         // Title across top of frame
    Pad_Bool       _refineNeeded;        // Records if label needs refinement
    Pad_MenuBar    *_menuBar;

  private:
    static void     _Initialize_events(Pad_Win *win);
};

#endif
