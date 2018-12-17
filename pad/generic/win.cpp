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


#include "pad.h"
#include "view.h"
#include "win.h"
#include "global.h"
#include "display.h"
#include "bind.h"
#include "portal.h"
#include "effect.h"
#include "renderer.h"
#include "init.h"


#  include "../unix/shm.h"

#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <signal.h>

#include <sys/types.h>

#  include <unistd.h>
#ifdef CYGWIN
#  include <cygwin/ipc.h>
#  include <cygwin/shm.h>
#else
#  include <sys/ipc.h>
#  include <sys/shm.h>
#endif
#  include <X11/Xlib.h>
#  include <X11/cursorfont.h>
#  include <X11/extensions/XShm.h>

				// Forward reference static procedures
static void Signal_handler_interrupt(SIGNAL_ARGS);
static void Signal_handler_error(SIGNAL_ARGS);

////////////////////////////////////////////////////////////////////////////
//
// Pad_RenderContext stuff
//
////////////////////////////////////////////////////////////////////////////

Pad_RenderContext::Pad_RenderContext(Pad_Win *newWin)
{
    win = newWin;
    dpy = newWin->dpy;
    result = 0;
    slow = FALSE;

}

Pad_RenderContext::~Pad_RenderContext()
{
    objects.Make_empty();
}

////////////////////////////////////////////////////////////////////////////
//
// Pad_Win stuff
//
////////////////////////////////////////////////////////////////////////////

const int Pad_Win::cursorDefault = 0;           // Default cursor
const int Pad_Win::cursorCrosshair = 1;
const int Pad_Win::cursorText = 2;
const int Pad_Win::cursorWait = 3;
const int Pad_Win::cursorBottomLeftCorner = 4;  // Resize corners
const int Pad_Win::cursorBottomRightCorner = 5;
const int Pad_Win::cursorTopLeftCorner = 6;
const int Pad_Win::cursorTopRightCorner = 7;
const int Pad_Win::cursorTopSide = 8;           // Resize edges
const int Pad_Win::cursorBottomSide = 9;
const int Pad_Win::cursorLeftSide = 10;
const int Pad_Win::cursorRightSide = 11;
const int Pad_Win::cursorHand = 12;
const int Pad_Win::cursorTopLeftArrow = 13;
const int Pad_Win::cursorBoxSpiral = 14;

//
// Constructor
//
Pad_Win::Pad_Win(Display *display, Screen *screen, 
		 Visual *visual, Colormap colormap, int depth, char *name, int newID)
{
    Pad_MasterInit();            // Initialize pad globals

    flags = PAD_NO_MASK;
    id = newID;
    dpy = Pad_Get_display(display, screen, visual, colormap, depth);
    Pad_renderer = new Pad_XRenderer(dpy, this);
    renderer = Pad_renderer;
    {
        XWindowAttributes xgwa;  /* return param for XGetWindowAttributes */
        XGetWindowAttributes(display, id, &xgwa);
        width = xgwa.width;
        height = xgwa.height;
    }
    doubleBuffer = WIN_DEFAULT_DOUBLEBUFFER;
    Set_sync(WIN_DEFAULT_SYNC);
    windowRestorer = NULL;
    lastX = 0;
    lastY = 0;
    lastWidth = 0;
    lastHeight = 0;
    xfac = 1.0;
    yfac = 1.0;
    units = PIXELS;
    background.Set(WIN_DEFAULT_FILL);
    cursor = None;
    globalEventCallback = NULL; // fired for all events that go through to Tcl

    /*
     * Invoke the script specified on the command line, if any.
     */


    // JM's experimental tiling facility
    tile1 = NULL;
    tile2 = NULL;
    tiletmp = NULL;
    tilex = 0;
    tiley = 0;

    fgGC = None;
    copyGC = None;
    dbl = None;
    effect = NULL;
    dissolveSpeed = 0;
    refineDissolveSpeed = 3;
    sharedMemory = FALSE;
    ximage = None;
    ninner = 0;
    nouter = 0;
    debugBB = WIN_DEFAULT_DEBUGBB;
    debugRegion = WIN_DEFAULT_DEBUGREGION;
    debugEvent = NULL;
    debugOut = NULL;
    fastPan = WIN_DEFAULT_FASTPAN;
    interruptible = WIN_DEFAULT_INTERRUPTIBLE;
    maxZoom = WIN_DEFAULT_MAXZOOM;
    defaultRenderLevel = WIN_DEFAULT_RENDERLEVEL;
    highestRenderLevel = 0;
    desiredFrameRate = WIN_DEFAULT_FRAMERATE;
    refinementDelay = WIN_DEFAULT_REFINEMENTDELAY;
    bindingTable = Pad_CreateBindingTable();
    fontCacheSize = dpy->Get_fontcache_size() * 100;
    closeEnough = WIN_DEFAULT_CLOSEENOUGH;
    smallObjSize = WIN_DEFAULT_SMALLSIZE;
    mediumObjSize = WIN_DEFAULT_MEDIUMSIZE;
    biddingRestorer = NULL;
    activeRestorer = NULL;
    refiningRestorer = NULL;
    damageEnabled = TRUE;
    _name = Pad_GetUid(name);

    view = new Pad_View(this);	// Make a new top-level view associated with this window

    _Add_pad(view->pad);

    signal(SIGINT,  Signal_handler_interrupt);
    signal(SIGSEGV, Signal_handler_error);
    signal(SIGTERM, Signal_handler_error);
    signal(SIGBUS,  Signal_handler_error);
}

//
// Destructor
//
Pad_Win::~Pad_Win()
{
    Pad_Iterator pi;
    Pad_Object *obj;
    Pad_Portal *portal;
    Pad_List copyLookonPortals;

    _Remove_pad(view->pad);

				// Delete all objects on the pad,
    while ((obj = view->pad->First())) {
	delete obj;
    }

    Cancel_restorers();
				// If a portal is looking at this surface,
				// Nullify the portal's lookon when this gets
				// deleted, and then damage it.
				// Make copy of the list since Set_lookon can modify it
    copyLookonPortals = lookonPortals;
    DOLIST(pi, copyLookonPortals, Pad_Portal, portal) {
	portal->Set_lookon(NULL);
	portal->Damage();
    }

    if (fgGC != None) {
	XFreeGC(dpy->display, fgGC);
	fgGC = None;
    }
    if (copyGC != None) {
	XFreeGC(dpy->display, copyGC);
	copyGC = None;
    }
    if ((dbl != None) && (doubleBuffer)) {
	XFreePixmap(dpy->display, dbl);
	dbl = None;
      }

    if (effect) {
	delete effect;
	effect = NULL;
    }

    Free_shared_ximage();

    if (windowRestorer) {
	delete windowRestorer;
	windowRestorer = NULL;
    }
				
    delete view->pad;    // Delete the pad
    delete view;         // Delete the view
    view = NULL;

    if (!shapePoints.Is_empty()) {
	Pad_Iterator li;
	Pad_Point *p;
	DOLIST(li, shapePoints, Pad_Point, p) {
	    delete p;	
	}
    }
    
				// Delete any restorers
    Cancel_restorers();
    if (biddingRestorer) {
	delete biddingRestorer;
	biddingRestorer = NULL;
    }
    if (refiningRestorer) {
	delete refiningRestorer;
	refiningRestorer = NULL;
    }

    if (renderer) {
	delete renderer;
	renderer = NULL;
	Pad_renderer = (Pad_Renderer *)Pad_renderers.First();
    }
    if (bindingTable) {
	Pad_DeleteBindingTable(bindingTable);
	bindingTable = NULL;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * Pad_Win::Create_pad
 *
 *      This method creates an application specific Pad.
 *      Applications with their own Pad objects (derived classes of Pad
 *      which perhaps create new objects, etc) need to modify this to
 *      create the appropriate object.
 *
 * Results:
 *      Returns a pointer to a dynamically allocated Pad.
 *
 *----------------------------------------------------------------------
 */

Pad *
Pad_Win::Create_pad(void)
{
    Pad *pad = new Pad(view);
    return pad;
}

//
// Set the cursor to a predefined cursor type
// This is a one-time action.  It does not
// make a permanent association, but rather sets
// it until it gets set again.
//
void
Pad_Win::Set_cursor(int cursorType)
{
    static int currentCursorType = -1;
    int xCursorType;
    Cursor cursor;

    if (cursorType != currentCursorType) {
	currentCursorType = cursorType;

	switch (cursorType) {
	  case cursorDefault:
	    xCursorType = XC_left_ptr;
	    break;
	  case cursorTopLeftArrow:
	    xCursorType = XC_top_left_arrow;
	    break;
	  case cursorCrosshair:
	    xCursorType = XC_crosshair;
	    break;
	  case cursorHand:
	    xCursorType = XC_hand2;
	    break;
	  case cursorText:
	    xCursorType = XC_xterm;
	    break;
	  case cursorWait:
	    xCursorType = XC_watch;
	    break;
	  case cursorTopSide:
	    xCursorType = XC_top_side;
	    break;
	  case cursorLeftSide:
	    xCursorType = XC_left_side;
	    break;
	  case cursorRightSide:
	    xCursorType = XC_right_side;
	    break;
	  case cursorBottomSide:
	    xCursorType = XC_bottom_side;
	    break;
	  case cursorBottomLeftCorner:
	    xCursorType = XC_bottom_left_corner;
	    break;
	  case cursorTopLeftCorner:
	    xCursorType = XC_top_left_corner;
	    break;
	  case cursorTopRightCorner:
	    xCursorType = XC_top_right_corner;
	    break;
	  case cursorBottomRightCorner:
	    xCursorType = XC_bottom_right_corner;
	    break;
	  case cursorBoxSpiral:
	    xCursorType = XC_box_spiral;
	    break;
	  default:
	    // Invalid cursor - do nothing
	    xCursorType = -1;
	    break;
	}
	if (xCursorType != -1) {
	    cursor = XCreateFontCursor(dpy->display, xCursorType);
	    XDefineCursor(dpy->display, id, cursor);
	    XSync(dpy->display, False);
	}
    }
}

//
// Set/Get window name
//
void
Pad_Win::Set_name(char *name)
{
    _name = Pad_GetUid(name);
}

char *
Pad_Win::Get_name(void)
{
    return(_name);
}

//
// Set/Get X synchronization name
//
void
Pad_Win::Set_sync(Pad_Bool newSync)
{
    _sync = newSync;
    XSynchronize(dpy->display, _sync);
}

Pad_Bool
Pad_Win::Is_synced(void)
{
    return(_sync);
}

//
// Return TRUE if window is mapped
//
Pad_Bool
Pad_Win::Is_mapped(void)
{
    return((flags & WIN_MAPPED) ? TRUE : FALSE);
}

//
// Window has appeared on screen
//
void
Pad_Win::Map(void)
{
    char buf[200];
    XGCValues gcValues;

    flags |= WIN_MAPPED;
    
    if (fgGC == None) {
	gcValues.function = GXcopy;
	gcValues.graphics_exposures = False;
	fgGC = XCreateGC(dpy->display, dpy->rootDrawable, GCFunction|GCGraphicsExposures, &gcValues);
    }
    if (copyGC == None) {
	gcValues.function = GXcopy;
	gcValues.graphics_exposures = False;
	copyGC = XCreateGC(dpy->display, dpy->rootDrawable,
			   GCFunction|GCGraphicsExposures, &gcValues);
    }

				// Allocated buffers associated with window dimensions
    Configure(width, height);
}

//
// Window has been removed from screen
//
void
Pad_Win::Unmap(void)
{
    flags &= ~WIN_MAPPED;
}

//
// Warp the Xpointer x pixels in the x direction and y pixels in y, relative
//   to the current position of the pointer
//
void
Pad_Win::Warp(int x,int y)
{
    XWarpPointer (dpy->display, 
                  None,         // Setting the source and dest to None means
                  None,         // that the warping will be relative to the
                  0,            // current position of the pointer
                  0,
                  0,
                  0,
                  x,
                  y);
}


//
// Window's configuration has changed
//
void
Pad_Win::Configure(int newWidth, int newHeight)
{
    Pad_String buffer;

    if (!Is_mapped()) {
	return;
    }

    extern void Pad_Reshape(Pad_Win *, Pad_Bool);      // See win.C
    Pad_Reshape(this, FALSE);

    width = newWidth;
    height = newHeight;
    view->Set_bbox(0, 0, width, height);
    Alloc_shared_ximage();
    Make_window_restorer();

				// Update drawable and effect
				// First free up, then allocate based
				// on buffering scheme.
    if ((dbl != None) && (dbl != id)) {
	XFreePixmap(dpy->display, dbl);
    }
    if (effect) {
	delete effect;
    }
    if (doubleBuffer) {
	dbl = XCreatePixmap(dpy->display, id, width, height, dpy->depth);
	effect = new Pad_Effect(dpy->display, id, width, height, dpy->depth);
    } else {
	dbl = id;
	effect = NULL;
    }

    view->Compute_view_bounding_box();

    view->Damage();
}

//
// Window exposed - redraw
//
void
Pad_Win::Expose_win()
{
    if (Is_mapped()) {
	if (doubleBuffer) {
	    Pad_RenderContext *prev_Pad_prc;
	    prev_Pad_prc = Pad_prc;
	    Pad_prc = new Pad_RenderContext(this);
	    Pad_renderer->Set_device(Pad_prc);
	    Pad_renderer->Swap_buffers(0);
	    delete Pad_prc;
	    Pad_prc = prev_Pad_prc;
	} else {
	    view->Damage();
	}
    }
}

//
// Make a restorer the size of this window
//
void
Pad_Win::Make_window_restorer(void)
{
    XRectangle rect;

    if (windowRestorer) {
	delete windowRestorer;
    }
    windowRestorer = new Pad_Restorer(this, FALSE);

    rect.x = 0;
    rect.y = 0;
    rect.width = width;
    rect.height = height;
    *windowRestorer += rect;
}

void
Pad_Win::Cancel_restorers(void)
{
    Pad_Restorer *r;

    while ((r = (Pad_Restorer *)restorers.Pop())) {
	delete r;
    }
}

#define SHM_UNKNOWN 0      /* not yet known */
#define SHM_OK 1           /* known to work */
#define SHM_BAD 2          /* known not to work */

//
// Allocate shared X memory for this Pad_Win.
// Make it the same size as the current window size.  All images
// will be rendered into this shared memory, and then immediately flushed.
//
void
Pad_Win::Alloc_shared_ximage(void)
{
    static Pad_Bool shm_status = SHM_UNKNOWN;
    XImage *newXimage = NULL;
    Window  xwin    = dpy->rootDrawable;


    if (Pad_sharedMemory == TRUE) {
#ifdef XSHM
	if (shm_status != SHM_BAD) {
				// Free any existing shared memory image
	    Free_shared_ximage();
	
				// Try to get a new shared memory image
	    newXimage = ShmCreateImage(dpy->display, width, height, dpy->depth, 0);
	}
    
	if (newXimage && shm_status == SHM_UNKNOWN) {
				// The X server is a big liar! Here we actually attempt a ShmPut to
				// see if the X server produces an error - if it does we give up
				// with shared memory.
	
	    Pixmap test_pixmap = XCreatePixmap(dpy->display, xwin, 1, 1, dpy->depth);
	    GC     test_GC     = XCreateGC(dpy->display, xwin, 0, NULL);
	    
	    if (ShmPutImage(dpy->display, test_pixmap, test_GC, newXimage, 0, 0, 0, 0, 1, 1,
			    SHM_PUT_TEST)) {
		shm_status = SHM_OK;
	    } else {
		shm_status = SHM_BAD;
		ShmFreeImage(dpy->display, newXimage);
		newXimage = NULL;
	    }
	    XFreeGC(dpy->display, test_GC);
	    XFreePixmap(dpy->display, test_pixmap);
	}
    }
    
    if (newXimage) {
	sharedMemory = TRUE;
#endif
    } else {
				// No shared memory, so create a regular old ximage.
	sharedMemory = FALSE;
	newXimage = XCreateImage(dpy->display, dpy->visual,
			      dpy->depth, ZPixmap, 0, NULL, width, height,
			      BitmapPad(dpy->display), 0);
	newXimage->data = (char *)malloc(newXimage->bytes_per_line * newXimage->height);
    }
    ximage = newXimage;
}
//
// Free_shared_ximage
//
//   Free shared X memory associated with this Pad_Win (if there is any).
//
void
Pad_Win::Free_shared_ximage(void)
{
    if (ximage) {

	if (sharedMemory) {
#ifdef XSHM
	    ShmFreeImage(dpy->display, ximage);
	    ximage = NULL;
	    sharedMemory = FALSE;
#endif
	} else {
	    XDestroyImage(ximage);
	    ximage = NULL;
	}
    }
}

//
// Get called when there is a signal.
// Make sure we clean up shared memory.
//
static void
Signal_handler_interrupt(SIGNAL_ARGS)
{
    Pad *pad;
    Pad_Name padName;
    Pad_List pads;
    Pad_Iterator oi;

    Pad_Win::Get_pads(pads);

    DOLISTI(oi, pads, Pad_Name, padName) {
	pad = Pad_Win::Get_pad_from_name(padName);
	pad->view->win->Free_shared_ximage();
    }

    exit(0);
}

static void
Signal_handler_error(SIGNAL_ARGS)
{
    Pad *pad;
    Pad_Name padName;
    Pad_List pads;
    Pad_Iterator oi;

    cerr << endl;
    cerr << "Dynapad Internal Error" << endl;
    cerr << "Please send email to dynapad@hci.ucsd.edu with a complete description as possible" << endl;
    cerr << "of what you were doing when this error ocurred.  Please also include:" << endl;
    cerr << "   The version of Dynapad you are running" << endl;
    cerr << "   The machine you are running on" << endl;
    cerr << "   The operating system and version you are running on" << endl;
    cerr << endl;

    Pad_Win::Get_pads(pads);

    DOLISTI(oi, pads, Pad_Name, padName) {
	pad = Pad_Win::Get_pad_from_name(padName);
	pad->view->win->Free_shared_ximage();
    }

    exit(0);
}


class _pad_id_node
{
public:
    _pad_id_node() {
	pad = NULL;
	padName = NULL;
	next = NULL;
    }

    _pad_id_node(Pad *newPad) {
	pad = newPad;
	padName = Pad_GetUid(newPad->view->win->Get_name());
	next = NULL;
    }

    Pad   *        pad;
    Pad_Name       padName;
    _pad_id_node * next;
};
    
_pad_id_node * Pad_Win::_padHead = NULL;        // Pointer to head of pad id list.


//
// Add a new pad to the list of pads.
//
void
Pad_Win::_Add_pad(Pad *pad) {
    _pad_id_node *node = _padHead;

    node = new _pad_id_node(pad);
    node->next = _padHead;
    _padHead = node;
}

//
// Search the list for the specified pad id.
// Move the found id to the front of the list, so
// the next time it will be found immediately.
//
Pad *
Pad_Win::_Get_pad(Pad_Name padName) 
{
    _pad_id_node *node = _padHead;

				// If first node, then just return it
    if (node) {
	if (node->padName == padName) {
	    return(node->pad);
	}
	node = node->next;
	while (node) {
	    if (node->padName == padName) {
		Pad *tmpPad;
		Pad_Uid tmpName;
		    
				// Bring the pad we just found to the front
				// of the list.
		tmpPad = _padHead->pad;
		tmpName = _padHead->padName;
		_padHead->pad = node->pad;
		_padHead->padName = node->padName;
		node->pad = tmpPad;
		node->padName = tmpName;
		
		return(_padHead->pad);
	    } else {
		node = node->next;
	    }
	}
    }

    return(NULL);
}

//
// Remove the specified pad widget pointer from the list of pads.
//
void
Pad_Win::_Remove_pad(Pad *pad) 
{
    _pad_id_node *prev, *node;

    prev = NULL;
    node = _padHead;
    while (node) {
	if (node->pad == pad) {
	    if (prev) {
		prev->next = node->next;
	    } else {
		_padHead = node->next;
	    }
	    delete node;
	    break;
	} else {
	    prev = node;
	    node = node->next;
	}
    }
}

//
// Return pointer to Pad given Pad_Name
//
Pad *  
Pad_Win::Get_pad_from_name(Pad_Name padName)
{
    return(_Get_pad(padName));
}

//
// Filling in list of Pad_Name's
//
void
Pad_Win::Get_pads(Pad_List &padNames)
{
    _pad_id_node *node = _padHead;

    while (node) {
	padNames.Push_last(node->padName);
	node = node->next;
    }
}

//////////////////////////////////////////////////////////////////
//
// This should probably all be member functions...
//
//////////////////////////////////////////////////////////////////

//
// Assigns a shape to a Pad's toplevel window
//
void
Pad_Reshape(Pad_Win *win, Pad_Bool force)
{
    extern void Pad_ReshapeWindow(Display *dpy, Window winid, 
				  int width, int height,
				  Pad_List &list, int ninner, int nouter);

    if (!win->id) {
	return;
    }

    if (force || (!win->shapePoints.Is_empty() 
		  && win->ninner && win->nouter)) {

	Window windowid = win->Get_toplevel_window_id();
	if (windowid != None) 
	  Pad_ReshapeWindow(win->dpy->display, windowid,
			    win->width, win->height, win->shapePoints,
			    win->ninner, win->nouter);
    }
}

Window
Pad_Win::Get_toplevel_window_id()
{
    return None;
}

