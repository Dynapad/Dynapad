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

//
// These test app still needs:
//   * Timers to work for proper rendering
//

#include "defs.h"
#include "win.h"
#include "pad.h"
#include "view.h"
#include "display.h"
#include "callback.h"
#include "bind.h"
#include "types.h"
#include "line.h"
#include "events.h"
#include "button.h"
#include "renderer.h"
#include "container.h"

#include <stdlib.h>
#include <iostream.h>

static Pad_Win * Init(void);
static void      Init_pad_events(Tcl_Interp *interp, Pad_Win *win);
static void      Main_loop(Pad_Win *win);

int
main(int, char **)
{
    Pad_Win *win;

    win = Init();
    Main_loop(win);

    return(0);
}

static Pad_Win *
Init(void)
{
    Display *display;
    Screen *screen;
    Visual *visual;
    Colormap colormap;
    int depth;
    int id;
    Tcl_Interp *interp;
    Pad_Win *win;

				// Open X connection, and create an X window
    if ((display = XOpenDisplay(NULL)) == NULL) {
	cerr << "Error connecting to X server" << endl;
	exit(1);
    }

    screen = DefaultScreenOfDisplay(display);
    visual = DefaultVisualOfScreen(screen);
    colormap = DefaultColormapOfScreen(screen);
    depth = DefaultDepthOfScreen(screen);

    id = XCreateWindow(display, RootWindowOfScreen(screen),
		       WIN_DEFAULT_X, WIN_DEFAULT_Y, WIN_DEFAULT_WIDTH, WIN_DEFAULT_HEIGHT,
		       0, depth, InputOutput, visual, 0, NULL);
    XMapWindow(display, id);

				// Initialize X events
    XSelectInput(display, id, 
		 StructureNotifyMask |
		 ExposureMask | 
		 EnterWindowMask | 
		 LeaveWindowMask | 
		 PointerMotionMask | 
		 ButtonPressMask | 
		 ButtonReleaseMask | 
		 KeyPressMask | 
		 KeyReleaseMask
		 );

				// Create Tcl interpreter
    interp = Tcl_CreateInterp();

				// Create Pad++ window out of X window
    win = new Pad_Win(interp, display, screen, visual, colormap, depth, id);

				// Create some Pad++ event handlers
    Init_pad_events(interp, win);

    return(win);
}

//
// Main loop.
// This handles X events, and passes them on to Pad++ appropriately
//
static void
Main_loop(Pad_Win *win)
{
    Pad_Bool done = FALSE;
    XEvent event;

    while (!done) {
	XNextEvent(win->dpy->display, &event);
	switch (event.type) {
	  case Expose:
	    win->Expose_win();
	    break;
	  case ConfigureNotify:
	    win->Configure(event.xconfigure.width, event.xconfigure.height);
	    break;
	  case DestroyNotify:
	    delete win;
	    done = TRUE;
	    break;
	  case MapNotify:
	    win->Map();
	    break;
	  case UnmapNotify:
	    win->Unmap();
	    break;
	  case EnterNotify:
	  case LeaveNotify:
	  case ButtonPress:
	  case ButtonRelease:
	  case MotionNotify:
	  case KeyPress:
	  case KeyRelease:
	    {  
		int prevFastPan;

		Pad_Bind_proc(win, &event);
				// Hack to make window get rendered immediately since timers are 
				// not working yet.
		prevFastPan = win->fastPan;
		win->fastPan = 0;
		win->view->Set_view(win->view->xview, win->view->yview, win->view->zoom, TRUE);
		win->fastPan = prevFastPan;
		break;
	    }
	}
    }

    XCloseDisplay(win->dpy->display);
}

//
// Create some Pad++ event handlers
//
static Pad_Bool
Button_command(Pad_Object *, ClientData clientData, Pad_Event *padEvent)
{
    Pad_Rectangle *rect = (Pad_Rectangle *)clientData;

    rect->Slide(10.0 / padEvent->win->view->zoom, 0, TRUE);

    return(TRUE);
}

static Pad_Bool
Create_component_func(Pad_Object *, ClientData clientData, Pad_Event *padEvent)
{
    Pad_Win *win = (Pad_Win *)clientData;
    Pad_Label *label;
    Pad_Button *button;
    static Pad_Container *container;
    static Pad_Frame *frame;
    static Pad_Panel *panel;
    Pad_Point pts[2];
    static int thing = 0;
    static int counter = 1;
    Pad_String l;
    Pad_Dimension dimension;
    Pad_Callback *command;
    float zoom = win->view->zoom;

    if (thing == 0) {
	static int containerType = 0;
	if (containerType == 0) {
	    frame = (Pad_Frame *)win->view->pad->Create_object(PAD_FRAME);
	    frame->Reshape(0, 0, 200, 200);
	    frame->Scale(0, 0, 1 / zoom);
	    frame->Slide(padEvent->pt.x, padEvent->pt.y);
	    container = frame;
	} else {
	    panel = (Pad_Panel *)win->view->pad->Create_object(PAD_PANEL);
	    panel->Reshape(75, 75, 100, 100);
	    //	    panel->Reshape(0, 0, 100, 100);
	    panel->Scale(0, 0, 1 / zoom);
	    //	    panel->Slide(75, 75);
	    panel->Set_fill("#8080ff");
	    frame->Add(panel);
	    container = panel;
	}

	label = (Pad_Label *)win->view->pad->Create_object(PAD_LABEL);
	l.Printf("Label %d", counter++);
	label->Set_label(l.Get());
	label->Preferred_size(dimension);
	label->Reshape(0, 0, dimension.width, dimension.height);
	label->Slide(-20, 10);
	container->Add(label);

	button = (Pad_Button *)win->view->pad->Create_object(PAD_BUTTON);
	l.Printf("Button %d", counter++);
	button->Set_label(l.Get());
	button->Preferred_size(dimension);
	button->Reshape(0, 0, dimension.width, dimension.height);
	button->Slide(50, 50);
	command = new Pad_Callback(Button_command, label);
	button->Set_callback(command);
	container->Add(button);

	containerType = (containerType + 1) % 2;
    } else {
	button = (Pad_Button *)win->view->pad->Create_object(PAD_BUTTON);
	l.Printf("Button %d", counter++);
	button->Set_label(l.Get());
	button->Preferred_size(dimension);
	button->Reshape(padEvent->pt.x, padEvent->pt.y, dimension.width, dimension.height);
	button->Scale(padEvent->pt.x, padEvent->pt.y, 1 / zoom);
	command = new Pad_Callback(Button_command, container);
	button->Set_callback(command);
    }
    
    thing = (thing + 1) % 2;

    return(TRUE);
}

//
// Event handler to quit on any keypress
//
static Pad_Bool
Key_func(ClientData)
{
    cout << "Exiting" << endl;
    exit(0);

    return(TRUE);
}

static void
Init_pad_events(Tcl_Interp *interp, Pad_Win *win)
{
    Pad_Callback *callback;

    win->interruptible = 0;	// Make animations not get interrupted

    callback = new Pad_Callback(Create_component_func, win);
    Pad_CreateBinding(interp, win->bindingTable, win->view->pad, "<ButtonPress-1>", callback);

    callback = new Pad_Callback(Key_func, win);
    Pad_CreateBinding(interp, win->bindingTable, Pad_GetUid("all"), "<KeyPress>", callback);
}
