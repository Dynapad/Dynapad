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

#ifndef WIN_H
#define WIN_H 1

#include "defs.h"
#include "bbox.h"
#include "list.h"
#include "pad-string.h"
#include "color.h"
#include "callback.h"

//
// Bitmask of flags in Pad_Win::flags
//
#define WIN_LEFT_GRABBED_ITEM	0x01
#define WIN_REPICK_IN_PROGRESS	0x02
#define WIN_MAPPED              0x04

				// Forward reference classes
class Pad;
class Pad_Win;
class Pad_Display;
class Pad_Effect;
class Pad_Renderer;
class Pad_View;
class Pad_Restorer;
class Pad_ImageData;
class Pad_Handle;
class _pad_id_node;

//
// A render context describes info about this particular render.
// It exists just for the duration of the render.
//
class Pad_RenderContext
{
  public:
    Pad_Win *    win;		// Window render ocurrs in
    Pad_Display *dpy;		// Display window is in
    Pad_BBox     activeBBox;	// BBox of render
    Pad_List     objects;	// Current object being rendered (possibly within others)
    Pad_List     views;		// List of Pad_View's being rendered in.
    int          result;	// Result of current render
    Pad_Bool     slow;          // System is running slow, so render fast if possible

    

    Pad_RenderContext(Pad_Win *win);
    ~Pad_RenderContext();
};

typedef Pad_Uid Pad_Name;             // Token which specifies a Pad++ widget
				     // These tokens have a special indexing mechanism which makes
				     // them *very* fast to look up, and is memory-safe.


//
// A Pad_Win is an abstract class that defines the interface for creating
// and interacting with a top-level window that contains a pad widget.
// It may be extended by windows for particular systems, such as Tk or Java.
//
class Pad_Win
{
  public:
    static Pad *  Get_pad_from_name(Pad_Name padName);  // Return pointer to Pad given Pad_Name
				                        // Warning: Pad_Name is a Pad_Uid, not a char *.
    static void   Get_pads(Pad_List &padNames);         // Filling in list of Pad_Name's


    int           flags;	// Bitmask
    Pad_Display * dpy;
    unsigned int  id;           // X Window ID
    Pad_Renderer *renderer;	// Renderer for this window
    int           width;	// Width of window in pixels
    int           height;	// Height of window in pixels
    int           lastX;
    int           lastY;
    unsigned int  lastWidth;
    unsigned int  lastHeight;
    float         xfac;		// Conversion between input and world units
    float         yfac;
    int           units;	// Measurement unit
    Pad_ColorRef  background;

    // JM's experimental tiling facility
    Pad_ImageData *tile1;
    Pad_ImageData *tile2;
    long          *tiletmp;
    int           tilex, tiley;
    float         tilemin, tilemax;

    Cursor        cursor;
    GC            fgGC;
    GC            copyGC;
    Pad_View *    view;		        // Pad_View looked at inside this window
    Drawable      dbl;		        // Output drawable (pixmap or window)
    Pad_Bool      sharedMemory;	        // True if shared memory is supported
    XImage *      ximage;	        // Shared ximage memory
    Pad_Effect *  effect;	        // For special effects such as dissolves
    int           dissolveSpeed;        // Speed for dissolve - used by Pad_Update.
    int           refineDissolveSpeed;  // Speed for refinement dissolves
    Pad_List      shapePoints;	        // Top-level window shape
    int           ninner;               // Num. of inner/outer points for window shape
    int           nouter;

    Pad_Bool      debugBB;		// True to show bounding boxes
    Pad_Bool      debugRegion;	        // True to show region management debugging
    char *        debugEvent;		// "" if no event debugging
				        // "all" if event debugging on all items
				        // tagOrId to enable event debugging only on items specified by tagOrId
    char *        debugOut;		// "" if event debugging directed to stdout
				        // Name of Tcl variable to direct event debugging to that variable
    int           defaultRenderLevel;   // Render level when not refining
    int           highestRenderLevel;   // Highest render level currently performed on screen
    int           refinementDelay;	// Delay before refinement starts
    Pad_Bool      doubleBuffer;		// Double buffering flag
    int           fastPan;		// Panning uses copy areas with back buffer
    int           interruptible;	// True for rendering to be interruptible by X events
    int           fontCacheSize;        // 0 for no font cache, 1 or more for font cache 
    double        closeEnough;          // How close one must be to an object to pick it (in pixels)
    double        maxZoom;              // Maximum zoom (in and out)
    int           smallObjSize;         // Size below which an object is considered small (in pixels)
    int           mediumObjSize;        // Size below which an object is considered medium (in pixels)

    int           desiredFrameRate;

    Pad_List      restorers;            // List of currently working restorers.
    Pad_Restorer *biddingRestorer;      // Objects register changes with this restorer.
    Pad_Restorer *activeRestorer;       // Set during render to the restorer that is working.
    Pad_Restorer *refiningRestorer;     // Set during render. Objects needing refinement 
                                        // add their bbox's to this

				        // A restorer containing rect (0, 0, win->width, 
				        // win->height). Never scheduled for render.
    Pad_Restorer *windowRestorer; 
    Pad_Bool      damageEnabled;	// Flag determining whether damage can occur

    Pad_List      lookonPortals;	// List of portals not in this view looking at this view.
    Pad_List      viewPortals;	        // List of all portals in this view.

		        // The following slots are all for event bindings

    BindingTable *bindingTable;       // Binding table for object bindings

			// Define built-in cursors that pad knows about.
    static const int cursorDefault;
    static const int cursorTopLeftArrow;
    static const int cursorCrosshair;     // Misc.
    static const int cursorHand;
    static const int cursorText;
    static const int cursorWait;
    static const int cursorTopSide;       // Resize edges
    static const int cursorLeftSide;
    static const int cursorRightSide;
    static const int cursorBottomSide;
    static const int cursorBottomLeftCorner;  // Resize corners
    static const int cursorTopLeftCorner;
    static const int cursorTopRightCorner;
    static const int cursorBottomRightCorner;
    static const int cursorBoxSpiral;

    Pad_Callback *globalEventCallback;  // All events to objects are routed through here

            void      Alloc_shared_ximage(void);  // Allocate shared ximage
            void      Cancel_restorers(void);     // Cancel all active restorers
    virtual void      Configure(int newWidth, int newHeight);  // Window's configuration has changed
    virtual Pad *     Create_pad(void);           // Factory function to create new pads
    virtual void      Expose_win();               // Window exposed - redraw
            void      Free_shared_ximage(void);   // Free shared ximage
            const char * Get_name(void);   	  // Get window name
    virtual Window    Get_toplevel_window_id();   // Returns XID of toplevel window frame 

            Pad_Bool  Is_mapped(void);            // Return TRUE if window is mapped
            Pad_Bool  Is_synced(void);            // Return TRUE if window is synced
            void      Make_window_restorer(void); // Make a restorer the size of this window
    virtual void      Map(void);                  // Window has appeared on screen
            void      Set_cursor(int padCursor);  // Set the cursor to a predefined cursor type
            void      Set_name(const char *name);	  // Return window name
            void      Set_sync(Pad_Bool newSync); // Set window synchronization
    virtual void      Unmap(void);                // Window has been removed from screen
            void      Warp(int x,int y);          // Warp the Xpointer by x and y

				// Constructors
    Pad_Win(Display *display, Screen *screen, 
	    Visual *visual, Colormap colormap, int depth, char *name, int id=0);
    virtual ~Pad_Win();

  private:
    friend class Pad_WinHandle;

    Pad_Uid        _name;	// Name of window
    Pad_Bool      _sync;	// X Synchronization
    Pad_Handle *  _handle;	// Pointer to first handle to this window, or NULL if none

    static _pad_id_node * _padHead;        // Pointer to head of pad id list.
    static void      _Add_pad(Pad *pad);
    static Pad  *    _Get_pad(Pad_Name padName);
    static void      _Remove_pad(Pad *pad);
};

#endif

// Defaults for pad window
#define WIN_DEFAULT_X             500
#define WIN_DEFAULT_Y             100
#define WIN_DEFAULT_WIDTH         400
#define WIN_DEFAULT_HEIGHT        400
#define WIN_DEFAULT_DOUBLEBUFFER  TRUE
#define WIN_DEFAULT_DEBUGBB       FALSE
#define WIN_DEFAULT_DEBUGREGION   FALSE
#define WIN_DEFAULT_RENDERLEVEL   0
#define WIN_DEFAULT_FRAMERATE     10
#define WIN_DEFAULT_SYNC          FALSE
#define WIN_DEFAULT_FILL          "white"
#define WIN_DEFAULT_MAXZOOM       100000000
#define WIN_DEFAULT_FASTPAN       TRUE
#define WIN_DEFAULT_INTERRUPTIBLE TRUE
#define WIN_DEFAULT_CLOSEENOUGH   3
#define WIN_DEFAULT_SMALLSIZE     10
#define WIN_DEFAULT_MEDIUMSIZE    100
#define WIN_DEFAULT_REFINEMENTDELAY 1000
