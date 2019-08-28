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
#include "misc.h"
#include "tkwin.h"
#include "callback.h"
#include "init.h"
#include "global.h"
#include "display.h"
#include "renderer.h"
#include "effect.h"
#include "api.h"
#include "imagedata.h"
#include <string.h>
#include <stdlib.h>
#include <X11/X.h>
#include "hashtab.h"
#include <unistd.h>
#include "imagemagick.h"
#include <X11/extensions/XInput.h>

//
// Forward reference static procedures
//
// [unused]: static void Destroy_window(char *clientData);
static int  New_tcl_pad(ClientData clientData, int argc, char **argv);
static void Event_proc(ClientData clientData, XEvent *eventPtr);
// [unused]: static void Initialize_unit_factors(Pad_TkWin *tkwin);

int         motion_type = -1;
int         button_press_type = -1;
int         button_release_type = -1;
int         key_press_type = -1;
int         key_release_type = -1;
int         proximity_in_type = -1;
int         proximity_out_type = -1;

////////////////////////////////////////////////////////////////////////////
//
// Top-level procedures to interact directly with Tk
//
////////////////////////////////////////////////////////////////////////////

//
// Create a new pad widget within a top-level Tk window.
// <padName> must specify a valid Tk path to a new window.
// The pad widget must still be managed by a Tk Geometry Manager
// (such as pack or grid before it will appear).
//
Pad_Bool
Pad_Create_win(char *padName)
{
    int rc;
    char *args[2];

    Pad_InitIM();
    
    args[0] = (char *)"pad";
    args[1] = padName;
    rc = New_tcl_pad((ClientData)NULL, 2, args);

    return((rc == PAD_OK) ? TRUE : FALSE);
}

static
void
proc(ClientData clientdata, int mask)
{
    Pad_Win *win;
    Display *display;
    int n;
    XEvent event;
    static int m = 0;
    // [unused]: int o = 0;
    XEvent nextevent;

    win = (Pad_Win *)clientdata;
    display = win->dpy->display;
    XSync(display, False);
    n = QLength(display);
    while (n) {

        //cerr << "proc " << n << " " << m << " " << o++ << endl;
	XNextEvent(display, &event);

	// X Input Extension event types are dynamically defined.
	// Map them to static values to simplify use.
	if (event.type == motion_type)
	  event.type = MotionType;
	else if (event.type == button_press_type)
	  event.type = ButtonPressType;
	else if (event.type == button_release_type)
	  event.type = ButtonReleaseType;
	else if (event.type == proximity_in_type)
	  event.type = ProximityInType;
	else if (event.type == proximity_out_type)
	  event.type = ProximityOutType;

	switch (event.type) {
	  case Expose:
	  case ConfigureNotify:
	  case DestroyNotify:
	  case MapNotify:
	  case UnmapNotify:
	    Event_proc(win, &event);
	    break;
	  case MotionNotify:
	    while (1) {
	      if (QLength(display) == 0)
	        break;
	      if (XCheckTypedEvent(display, MotionNotify, &nextevent) == False)
	        break;
	      memcpy(&event, &nextevent, sizeof(XPointerMovedEvent));
	    }
	  case MotionType:
	    while (1) {
	      if (QLength(display) == 0)
	        break;
	      if (XCheckTypedEvent(display, motion_type, &nextevent) == False)
	        break;
	      nextevent.type = MotionType;
	      memcpy(&event, &nextevent, sizeof(XDeviceMotionEvent));
	    }
	  case EnterNotify:
	  case LeaveNotify:
	  case ButtonPress:
	  case ButtonRelease:
	  case KeyPress:
	  case KeyRelease:
	  case ButtonPressType:
	  case ButtonReleaseType:
          case ProximityInType:
          case ProximityOutType:
            Pad_Bind_proc(win, &event);
	    break;
	  case MappingNotify:
	    XRefreshKeyboardMapping(&event.xmapping);
	    break;
	}
        n = QLength(display);
    }
    m++;
}

XDeviceInfo *xinputdevices;
int num_xinputdevices;

static Pad_Win *
CreateWindow(Pad_Win *parent, char *name)
{
    Display *display;
    Screen *screen;
    Visual *visual;
    Colormap colormap;
    int depth;
    int id;
    Pad_Win *win;

				// Open X connection, and create an X window
#ifdef CYGWIN
    if ((display = XOpenDisplay("127.0.0.1:0")) == NULL) {
#else
    if ((display = XOpenDisplay(NULL)) == NULL) {
#endif
	cerr << "Error connecting to X server" << endl;
	exit(1);
    }

    screen = DefaultScreenOfDisplay(display);
    visual = DefaultVisualOfScreen(screen);
    colormap = DefaultColormapOfScreen(screen);
    depth = DefaultDepthOfScreen(screen);

    XEventClass event_list[20];
    int         number = 0;
    {
      XDeviceInfo *info;
      XDevice     *device;
      int         i, j;

      xinputdevices = XListInputDevices(display, &num_xinputdevices);
      for (i = 0; i < num_xinputdevices; i++) {
        info = &xinputdevices[i];
	if (info->use == IsXExtensionDevice) {
	  device = XOpenDevice(display, info->id);
	  if (!device) {
            fprintf(stderr, "unable to open device %d\n", 0);
            exit(1);
          }
          if (device->num_classes <= 0) {
            fprintf(stderr, "num_classes %d <= 0\n", device->num_classes);
            exit(1);
          }
	  for (j = 0; j < info->num_classes; j++) {
	    switch (device->classes[j].input_class) {
	      case ValuatorClass:
	        DeviceMotionNotify(device, motion_type, event_list[number]);
		number++;
		ProximityIn(device, proximity_in_type, event_list[number]);
		number++;
		ProximityOut(device, proximity_out_type, event_list[number]);
		number++;
		break;
	      case ButtonClass:
	        DeviceButtonPress(device, button_press_type, event_list[number]);
		number++;
		DeviceButtonRelease(device, button_release_type, event_list[number]);
		number++;
	        break;
	    }
	  }
	}
      }
    }

    id = XCreateWindow(display, RootWindowOfScreen(screen),
		       WIN_DEFAULT_X, WIN_DEFAULT_Y, WIN_DEFAULT_WIDTH, WIN_DEFAULT_HEIGHT,
		       0, depth, InputOutput, visual, 0, NULL);

    {
        char *windowname = (char *)"Dynapad";
	char *iconname = (char *)"dynapad";
	XTextProperty xwindowName, xiconName;
	XClassHint *xclasshint;
	/* without XSizeHints some window managers don't provide a border
	 * that can be clicked on for moving the window
	 */
	XSizeHints *xsizehints;

	XStringListToTextProperty(&windowname, 1, &xwindowName);
	XStringListToTextProperty(&iconname, 1, &xiconName);
	xclasshint = XAllocClassHint();
	xclasshint->res_name = (char *)"Dynapad";
	xclasshint->res_class = (char *)"Dynapad";
	xsizehints = XAllocSizeHints();
	xsizehints->flags = PPosition | PSize;
	XSetWMProperties(display, id, &xwindowName, &xiconName, 0, 0,
	  xsizehints, NULL, xclasshint);
    }
    XMapWindow(display, id);

				// Initialize X events
    if (number > 0)
      if (XSelectExtensionEvent(display, id, event_list, number)) {
        fprintf(stderr, "error selecting extended events\n");
        exit(1);
      }
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


				// Create Pad++ window out of X window
    win = new Pad_Win(display, screen, visual, colormap, depth, name, id);

    return(win);
}

//
// Called when Pad is executed - creation of an Pad.  This does several things.
//   * If no pathName is given, a unique name is created.
//   * Creates a new window
//   * Creates an 'Pad' data structure
//   * Creates a Pad_View and attaches it to this Pad
//   * Creates an event handler for this window
//   * Creates a command that handles this object
//   * Configures this Pad for the given arguments
//
static int
New_tcl_pad(ClientData clientData, int argc, char **argv)
{
    char *name;
    Pad_String newName;
    Pad_Win *main = (Pad_Win *)clientData;
    Pad_HashTableIterator hi;
    // unused Pad_Language *language;
    Display *display;
    int fd;
    Pad_Win *win;

    static int padId = 0;	// Counter for generating unique pad names

    //
    // Create the window.
    //
    if (argc > 1) {
	name = argv[1];
    }
    else {
	newName.Printf(".pad%d", padId++);
	name = newName.Get();
    }
    win = CreateWindow(main, name);
    if (win == NULL) {
	return(PAD_ERROR);
    }

    display = win->dpy->display;
    fd = ConnectionNumber(display);
    Pad_CreateFileHandler(fd, display, PAD_READABLE, proc, (void *)win);
    XSync(display, FALSE);

    win->Make_window_restorer();

    return PAD_OK;
}

////////////////////////////////////////////////////////////////////////////
//
// Tk configuration of pad widget
//
////////////////////////////////////////////////////////////////////////////

// True when window needs to be reconfigured
// [unused]: static Pad_Bool configurationNeeded = FALSE;

// True when window needs to be redrawn
// [unused]: static Pad_Bool damageNeeded = FALSE;

//		
// This gets called to handle Pad window configuration events
//
static void 
Event_proc(ClientData clientData, XEvent *eventPtr)
{
    Pad_Win *win = (Pad_Win *)clientData;

    switch (eventPtr->type)
      {
	case Expose:
	  if (eventPtr->xexpose.count == 0) {
	      win->Expose_win();
	  }
	  break;
        case ConfigureNotify:
	  win->Configure(eventPtr->xconfigure.width, eventPtr->xconfigure.height);
	  break;
	case DestroyNotify:
	  //EventuallyFree((ClientData)win, Destroy_window);
	  break;
	case MapNotify:
	  win->Map();
	  break;
	case UnmapNotify:
	  win->Unmap();
	  break;
      }
}

// [unused]:
//
// Call when Pad_TkWin is destroyed
//
// static void 
// Destroy_window(char *clientData)
// {
//     Pad_TkWin *tkwin = (Pad_TkWin *)clientData;
    
//     delete tkwin;
// }

//
// Set up conversion factors between input and world units
//

// [unused]:
// static void 
// Initialize_unit_factors(Pad_TkWin *tkwin)
// {
//     int widthmm, heightmm;
//     float ppmx, ppmy;		// pixels/mm

// 				// If user doesn't set screen size, then use X Server values.
//     if (tkwin->dpy->widthmmofscreen == 0) {
// 	widthmm = WidthMMOfScreen(tkwin->dpy->screen);
//     } else {
// 	widthmm = tkwin->dpy->widthmmofscreen;
//     }
//     if (tkwin->dpy->heightmmofscreen == 0) {
// 	heightmm = HeightMMOfScreen(tkwin->dpy->screen);
//     } else {
// 	heightmm = tkwin->dpy->heightmmofscreen;
//     }

//     ppmx = (float)WidthOfScreen(tkwin->dpy->screen) / widthmm;
//     ppmy = (float)HeightOfScreen(tkwin->dpy->screen) / heightmm;
//     switch (tkwin->units) {
//       case MM:
// 	tkwin->xfac = ppmx;
// 	tkwin->yfac = ppmy;
// 	break;
//       case POINTS:
// 	tkwin->xfac = ppmx * 25.4 / 72.0;
// 	tkwin->yfac = ppmy * 25.4 / 72.0;
// 	break;
//       case INCHES:
// 	tkwin->xfac = ppmx * 25.4;
// 	tkwin->yfac = ppmy * 25.4;
// 	break;
//       case PIXELS:
// 	tkwin->xfac = 1.0;
// 	tkwin->yfac = 1.0;
// 	break;
//     }
// }

//
// Tk Pad widget configuration stuff here
//
#define DEF_PAD_CURSOR           ((char *)NULL)
#define DEF_PAD_DEBUG_EVENT      ""
#define DEF_PAD_DEBUG_GEN        "0"
#define DEF_PAD_DEBUG_OUT        ""
#define DEF_PAD_DISSOLVE_SPEED   "2"
#define DEF_PAD_GAMMA            "1.0"      // Default gamma
#define DEF_PAD_HEIGHTMM         ""         // Default height of screen in mm
#define DEF_PAD_REFINEMENTDELAY  "1000"
#define DEF_PAD_WIDTHMM          ""         // Default width of screen in mm

