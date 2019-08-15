/* 
 * Modified from tkColor.c --
 *
 *	This file maintains a database of color values for the Tk
 *	toolkit, in order to avoid round-trips to the server to
 *	map color names to pixel values.
 *
 * Copyright (c) 1990-1994 The Regents of the University of California.
 * Copyright (c) 1994-1995 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * SCCS: @(#) tkColor.c 1.40 96/03/28 09:12:20
 */

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

#include <math.h>  // supress problem on Windows NT VC++


#include "defs.h"
#include "tkcolor.h"
#include "display.h"
#include "global.h"
#include "hashtab.h"

#include <string.h>
#include <stdlib.h>

extern "C" {
#  include <X11/Xutil.h>  // for unix
}


/*
 * A two-level data structure is used to manage the color database.
 * The top level consists of one entry for each color name that is
 * currently active, and the bottom level contains one entry for each
 * pixel value that is still in use.  The distinction between
 * levels is necessary because the same pixel may have several
 * different names.  There are two hash tables, one used to index into
 * each of the data structures.  The name hash table is used when
 * allocating colors, and the pixel hash table is used when freeing
 * colors.
 */

/*
 * Hash table for name -> TkColor mapping, and key structure used to
 * index into that table:
 */

static Pad_HashTable *nameTable;
typedef struct {
    Pad_Uid name;		/* Name of desired color. */
    Colormap colormap;		/* Colormap from which color will be
				 * allocated. */
    Display *display;		/* Display for colormap. */
} NameKey;

/*
 * Hash table for value -> TkColor mapping, and key structure used to
 * index into that table:
 */

static Pad_HashTable *valueTable;
typedef struct {
    int red, green, blue;	/* Values for desired color. */
    Colormap colormap;		/* Colormap from which color will be
				 * allocated. */
    Display *display;		/* Display for colormap. */
} ValueKey;

/*
 * One of the following data structures is used to keep track of
 * each color that this module has allocated from the X display
 * server.  These entries are indexed by two hash tables defined
 * below:  nameTable and valueTable.
 */

#define COLOR_MAGIC ((unsigned int) 0x46140277)

typedef struct TkColor {
    XColor color;		/* Information about this color. */
    unsigned int magic;		/* Used for quick integrity check on this
				 * structure.   Must always have the
				 * value COLOR_MAGIC. */
    GC gc;			/* Simple gc with this color as foreground
				 * color and all other fields defaulted.
				 * May be None. */
    Screen *screen;		/* Screen where this color is valid.  Used
				 * to delete it, and to find its display. */
    Colormap colormap;		/* Colormap from which this entry was
				 * allocated. */
    Visual *visual;             /* Visual associated with colormap. */
    int refCount;		/* Number of uses of this structure. */
    Pad_HashTable *tablePtr;	/* Hash table that indexes this structure
				 * (needed when deleting structure). */
    ValueKey key;		/* Hash table key for this
				 * structure. (for use in deleting entry). */
} TkColor;

static int initialized = 0;	/* 0 means static structures haven't been
				 * initialized yet. */


/*
 * Forward declarations for procedures defined in this file:
 */

static void		ColorInit(void);
static void		DeleteStressedCmap(Pad_Display *dpy,
			    Colormap colormap);
static void		FindClosestColor(Pad_Display *dpy,
			    XColor *desiredColorPtr, XColor *actualColorPtr);

/*
 *----------------------------------------------------------------------
 *
 * Pad_AllocXColor --
 *
 *	Given a desired set of red-green-blue intensities for a color,
 *	locate a pixel value to use to draw that color in a given
 *	window.
 *
 * Results:
 *	The return value is a pointer to an XColor structure that
 *	indicates the closest red, blue, and green intensities available
 *	to those specified in colorPtr, and also specifies a pixel
 *	value to use to draw in that color.
 *
 * Side effects:
 *	The color is added to an internal database with a reference count.
 *	For each call to this procedure, there should eventually be a call
 *	to Pad_FreeColor, so that the database is cleaned up when colors
 *	aren't in use anymore.
 *
 *----------------------------------------------------------------------
 */

XColor *
Pad_AllocXColor(Pad_Display *dpy, XColor *colorPtr)
{
    ValueKey valueKey;
    // [unused]: int newc;
    TkColor *tkColPtr;
    Display *display = dpy->display;

    if (!initialized) {
	ColorInit();
    }

    /*
     * First, check to see if there's already a mapping for this color
     * name.
     */

    valueKey.red = colorPtr->red;
    valueKey.green = colorPtr->green;
    valueKey.blue = colorPtr->blue;
    valueKey.colormap = dpy->colormap;
    valueKey.display = display;
    if ((tkColPtr = (TkColor *)valueTable->Get((void *)&valueKey))) {
	tkColPtr->refCount++;
	return &tkColPtr->color;
    }

    /*
     * The name isn't currently known.  Find a pixel value for this
     * color and add a new structure to valueTable.
     */

    tkColPtr = new TkColor;
    tkColPtr->color.red = valueKey.red;
    tkColPtr->color.green = valueKey.green;
    tkColPtr->color.blue = valueKey.blue;
    if (XAllocColor(display, valueKey.colormap, &tkColPtr->color) != 0) {
	DeleteStressedCmap(dpy, valueKey.colormap);
    } else {
	FindClosestColor(dpy, &tkColPtr->color, &tkColPtr->color);
    }
    tkColPtr->magic = COLOR_MAGIC;
    tkColPtr->gc = None;
    tkColPtr->screen = dpy->screen;
    tkColPtr->colormap = valueKey.colormap;
    tkColPtr->visual  = dpy->visual;
    tkColPtr->refCount = 1;
    tkColPtr->tablePtr = valueTable;
    memcpy((void *)&tkColPtr->key, (void *)&valueKey, sizeof(ValueKey));
    valueTable->Set((void *)&valueKey, (void *)tkColPtr);
    return &tkColPtr->color;
}

/*
 *----------------------------------------------------------------------
 *
 * Pad_FreeXColor --
 *
 *	This procedure is called to release a color allocated by
 *	Tk_GetColor.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The reference count associated with colorPtr is deleted, and
 *	the color is released to X if there are no remaining uses
 *	for it.
 *
 *----------------------------------------------------------------------
 */

void
Pad_FreeXColor(Pad_Display *dpy, XColor *colorPtr)
{
    TkColor *tkColPtr = (TkColor *) colorPtr;
    Visual *visual;
    Screen *screen = tkColPtr->screen;

    /*
     * Do a quick sanity check to make sure this color was really
     * allocated by Tk_GetColor.
     */

    if (tkColPtr->magic != COLOR_MAGIC) {
	cerr << ("Pad_FreeColor called with bogus color") << endl;
	exit(1);
    }

    tkColPtr->refCount--;
    if (tkColPtr->refCount == 0) {

	/*
	 * Careful!  Don't free black or white, since this will
	 * make some servers very unhappy.  Also, there is a bug in
	 * some servers (such Sun's X11/NeWS server) where reference
	 * counting is performed incorrectly, so that if a color is
	 * allocated twice in different places and then freed twice,
	 * the second free generates an error (this bug existed as of
	 * 10/1/92).  To get around this problem, ignore errors that
	 * occur during the free operation.
	 */

	visual = tkColPtr->visual;
	if ((visual->c_class != StaticGray) && (visual->c_class != StaticColor)
		&& (tkColPtr->color.pixel != BlackPixelOfScreen(screen))
		&& (tkColPtr->color.pixel != WhitePixelOfScreen(screen))) {
            int (*handler)(Display *, XErrorEvent *);

	    handler = XSetErrorHandler(NULL);

	    XFreeColors(DisplayOfScreen(screen), tkColPtr->colormap,
		    &tkColPtr->color.pixel, 1, 0L);

	    XSetErrorHandler(handler);
	}
	if (tkColPtr->gc != None) {
	    XFreeGC(DisplayOfScreen(screen), tkColPtr->gc);
	}
	DeleteStressedCmap(dpy, tkColPtr->colormap);
	tkColPtr->tablePtr->Remove((void *)&tkColPtr->key);
	tkColPtr->magic = 0;
	delete tkColPtr;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * ColorInit --
 *
 *	Initialize the structure used for color management.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Read the code.
 *
 *----------------------------------------------------------------------
 */

static void
ColorInit()
{
    initialized = 1;
    nameTable = new Pad_HashTable(sizeof(NameKey)/sizeof(int));
    valueTable = new Pad_HashTable(sizeof(ValueKey)/sizeof(int));
}

/*
 *----------------------------------------------------------------------
 *
 * FindClosestColor --
 *
 *	When Tk can't allocate a color because a colormap has filled
 *	up, this procedure is called to find and allocate the closest
 *	available color in the colormap.
 *
 * Results:
 *	There is no return value, but *actualColorPtr is filled in
 *	with information about the closest available color in tkwin's
 *	colormap.  This color has been allocated via X, so it must
 *	be released by the caller when the caller is done with it.
 *
 * Side effects:
 *	A color is allocated.
 *
 *----------------------------------------------------------------------
 */

static void
FindClosestColor(Pad_Display *dpy, XColor *desiredColorPtr, XColor *actualColorPtr)
{
    Pad_StressedCmap *stressPtr;
    float tmp, distance, closestDistance;
    int i, closest, numFound;
    XColor *colorPtr;
    Colormap colormap = dpy->colormap;
    XVisualInfo templatec, *visInfoPtr;

    /*
     * Find the Pad_StressedCmap structure for this colormap, or create
     * a new one if needed.
     */

    for (stressPtr = dpy->stressPtr; ; stressPtr = stressPtr->nextPtr) {
	if (stressPtr == NULL) {
	    stressPtr = new Pad_StressedCmap;
	    stressPtr->colormap = colormap;
	    templatec.visualid = XVisualIDFromVisual(dpy->visual);
	    visInfoPtr = XGetVisualInfo(dpy->display,
		    VisualIDMask, &templatec, &numFound);
	    if (numFound < 1) {
		cerr << ("FindClosestColor called with bogus color") << endl;
		exit(1);
	    }
	    stressPtr->numColors = visInfoPtr->colormap_size;
	    XFree((char *) visInfoPtr);
	    stressPtr->colorPtr = new XColor[stressPtr->numColors];
	    for (i = 0; i  < stressPtr->numColors; i++) {
		stressPtr->colorPtr[i].pixel = (unsigned long) i;
	    }

	    XQueryColors(dpy->display, colormap, stressPtr->colorPtr,
			 stressPtr->numColors);

	    stressPtr->nextPtr = dpy->stressPtr;
	    dpy->stressPtr = stressPtr;
	    break;
	}
	if (stressPtr->colormap == colormap) {
	    break;
	}
    }

    /*
     * Find the color that best approximates the desired one, then
     * try to allocate that color.  If that fails, it must mean that
     * the color was read-write (so we can't use it, since it's owner
     * might change it) or else it was already freed.  Try again,
     * over and over again, until something succeeds.
     */

    while (1)  {
	if (stressPtr->numColors == 0) {
	    cerr << ("FindClosestColor ran out of color") << endl;
	    exit(1);
	}
	closestDistance = 1e30;
	closest = 0;
	for (colorPtr = stressPtr->colorPtr, i = 0; i < stressPtr->numColors;
		colorPtr++, i++) {
	    /*
	     * Use Euclidean distance in RGB space, weighted by Y (of YIQ)
	     * as the objective function;  this accounts for differences
	     * in the color sensitivity of the eye.
	     */
    
	    tmp = .30*(((int) desiredColorPtr->red) - (int) colorPtr->red);
	    distance = tmp*tmp;
	    tmp = .61*(((int) desiredColorPtr->green) - (int) colorPtr->green);
	    distance += tmp*tmp;
	    tmp = .11*(((int) desiredColorPtr->blue) - (int) colorPtr->blue);
	    distance += tmp*tmp;
	    if (distance < closestDistance) {
		closest = i;
		closestDistance = distance;
	    }
	}

	if (XAllocColor(dpy->display, colormap,
		&stressPtr->colorPtr[closest]) != 0) {
	    *actualColorPtr = stressPtr->colorPtr[closest];
	    return;
	}

	/*
	 * Couldn't allocate the color.  Remove it from the table and
	 * go back to look for the next best color.
	 */

	stressPtr->colorPtr[closest] =
		stressPtr->colorPtr[stressPtr->numColors-1];
	stressPtr->numColors -= 1;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * Pad_CmapStressed --
 *
 *	Check to see whether a given colormap is known to be out
 *	of entries.
 *
 * Results:
 *	1 is returned if "colormap" is stressed (i.e. it has run out
 *	of entries recently), 0 otherwise.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

int
Pad_CmapStressed(Pad_Display *dpy, Colormap colormap)
{
    Pad_StressedCmap *stressPtr;

    for (stressPtr = dpy->stressPtr;
	    stressPtr != NULL; stressPtr = stressPtr->nextPtr) {
	if (stressPtr->colormap == colormap) {
	    return 1;
	}
    }
    return 0;
}

/*
 *----------------------------------------------------------------------
 *
 * DeleteStressedCmap --
 *
 *	This procedure releases the information cached for "colormap"
 *	so that it will be refetched from the X server the next time
 *	it is needed.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The Pad_StressedCmap structure for colormap is deleted;  the
 *	colormap is no longer considered to be "stressed".
 *
 * Note:
 *	This procedure is invoked whenever a color in a colormap is
 *	freed, and whenever a color allocation in a colormap succeeds.
 *	This guarantees that Pad_StressedCmap structures are always
 *	deleted before the corresponding Colormap is freed.
 *
 *----------------------------------------------------------------------
 */

static void
DeleteStressedCmap(Pad_Display *dpy, Colormap colormap)
{
    Pad_StressedCmap *prevPtr, *stressPtr;
    
    for (prevPtr = NULL, stressPtr = dpy->stressPtr; stressPtr != NULL;
	    prevPtr = stressPtr, stressPtr = stressPtr->nextPtr) {
	if (stressPtr->colormap == colormap) {
	    if (prevPtr == NULL) {
		dpy->stressPtr = stressPtr->nextPtr;
	    } else {
		prevPtr->nextPtr = stressPtr->nextPtr;
	    }
	    delete [] stressPtr->colorPtr;
	    delete stressPtr;
	    return;
	}
    }
}

