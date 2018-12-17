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

/*

    Note: When looking for a handy drawable to use with 
    XCreateGC or XCreatePixmap (or other Xlib calls), 
    PLEASE DON'T USE DefaultRootWindow or DefaultRootWindowOfScreen. 
    Also please avoid using DefaultDepthOfScreen and similar macros. 

    Reason: When Pad++ is using a non-default visual, these macros return
    values which are incompatible with what Pad++ needs. When Pad++
    tries to use the resulting GC/Pixmap, X will complain about a BadMatch.

    Alternative: If you need a drawable to use with XCreateGC or 
    XCreatePixmap, instead use the Pad_Display's rootDrawable field. 
    If you need to find the depth of a Pad++ window, use the Pad_Display's 
    depth field. These fields are guaranteed to be compatible with what 
    Pad++ needs.

    JM
*/

#ifndef DISPLAY_H
#define DISPLAY_H 1

#include "list.h"
#include "pad-string.h"
extern "C" {
#  include <X11/Xlib.h>
}

typedef struct Pad_3DBorder_ *Pad_3DBorder;

class Pad_View;
class Pad_ImageData;
class Pad_ColorCube;
class Pad_FontData;
class Pad_FontCache;
class Pad_FontCacheList;
class Pad_Win;
struct Pad_StressedCmap;

#define STIP_SIZE 8		 // Stipple size
#define STIP_MASK (STIP_SIZE-1)

typedef struct {
    char pattern[STIP_SIZE][STIP_SIZE];
} Pad_Stipple;

// Stipple number 8 is 50% gray
#define GRAY_50_STIPPLE 8

// An array of stipple patterns used for dithering 
extern Pad_Stipple pad_stipples[17];

// Method for getting a Pad_display
extern Pad_Display *Pad_Get_display(Display *display, Screen *screen, Visual *visual, Colormap colormap, int depth);


class Pad_Display
{
  public:
    enum {COLORCUBE_LEVELS=6};

    Display *  display;
    Screen *   screen;
    Colormap   colormap;
    Visual *   visual;
    Pad_String name;		// Name of display (with screen identifier removed)
    Pad_StressedCmap *stressPtr;        // First in list of colormaps that have filled up,
				        // so we have to pick an approximate color.


    Pixmap   ditherStipples[17]; // Stipple patterns (1 bit bitmaps)
    int      depth;		// Screen depth in pixels
    int      width;		// Screen width in pixels
    int      height;		// Screen height in pixels
    int      widthmmofscreen;	// Width of screen in mm (0 to use Server measurements)
    int      heightmmofscreen;	// Height of screen in mm (0 to use Server measurements)
    Pad_Bool truecolor;         // True if it is a true-color display
    Drawable rootDrawable;      // Either a root window, or a pixmap (for non-default depths/visuals)

    ~Pad_Display();
    Pad_Display(Display *dpy, Screen *screen, Visual *vis, Colormap cmap, int depth);

    // Color Cube

    Pad_ColorCube *Get_colorcube(); // Gets current colormap (creates if necessary)

    float Get_gamma();              // Gamma level of colorcube
    void Set_gamma(float v);    

    // Colors
    XColor *Alloc_color(int r, int g, int b);
    void    Free_color(XColor *color);

    // Borders
    Pad_3DBorder Alloc_border(int r, int g, int b);
    void Free_border(Pad_3DBorder border);

    // Fonts
    void *Get_font_data(char *name, int style);
    
    // Font Cache
    Pad_FontCache *Get_fontcache(int font_id, int font_size);
    int Get_fontcache_size();
    void Set_fontcache_size(int s);

    // Misc

    void Add_image_data(Pad_ImageData *imageData);
    void Remove_image_data(Pad_ImageData *imageData);
    void Setup_dither();
    int  Screen_number();

    // Grab a portion of the screen
    Pad_ImageData *Grab(Window id, int x, int y, int w, int h);
    Pad_ImageData *Grab(Window id, int xdim, int ydim, int x, int y, int w, int h);

    // Information used primarily by bind.C:

#ifndef LU_IGNORE
#  define LU_IGNORE 0
#endif
#ifndef LU_CAPS
#  define LU_CAPS 1
#endif
#ifndef LU_SHIFT
#  define LU_SHIFT 2
#endif

    Pad_Bool bindInfoStale;     // True means the variables in this
				// part of the structure are potentially
				// incorrect and should be recomputed.
    unsigned int modeModMask;	// Has one bit set to indicate the modifier
				// corresponding to "mode shift".  If no
				// such modifier, than this is zero.
    unsigned int metaModMask;	// Has one bit set to indicate the modifier
				// corresponding to the "Meta" key.  If no
				// such modifier, then this is zero.
    unsigned int altModMask;	// Has one bit set to indicate the modifier
				// corresponding to the "Meta" key.  If no
				// such modifier, then this is zero.
    int lockUsage;		// Indicates how to interpret lock modifier.
    int numModKeyCodes;		// Number of entries in modKeyCodes array
				// below.
    KeyCode *modKeyCodes;	// Pointer to an array giving keycodes for
				// all of the keys that have modifiers
				// associated with them.  Allocated, but
				// may be NULL.
  private:
    float gamma;
    Pad_List imageDatas;	// Used to notify when color cube changes
    Pad_ColorCube *colorcube;
    Pad_FontCacheList *fontcachelist;

    void Alloc_colorcube(Pad_Bool notify_images);

};

#endif

