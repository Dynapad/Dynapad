/* 
 * Modified from tkGC.c --
 *
 *	This file maintains a database of read-only graphics contexts 
 *	for the Tk toolkit, in order to allow GC's to be shared.
 *
 * Copyright (c) 1990-1994 The Regents of the University of California.
 * Copyright (c) 1994 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * SCCS: @(#) tkGC.c 1.18 96/02/15 18:53:32
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

#include "defs.h"
#include "gc.h"
#include "display.h"

#include <stdlib.h>
#include <string.h>

/*
 * Hash table to map from a GC's values to a PadGC structure describing
 * a GC with those values (used by Pad_GetGC).
 */

static Pad_HashTable *valueTable;
typedef struct {
    XGCValues values;		/* Desired values for GC. */
    Display *display;		/* Display for which GC is valid. */
    int screenNum;		/* screen number of display */
    int depth;			/* and depth for which GC is valid. */
} ValueKey;

/*
 * Hash table for <display + GC> -> PadGC mapping. This table is used by
 * Pad_FreeGC.
 */

static Pad_HashTable *idTable;
typedef struct {
    Display *display;		/* Display for which GC was allocated. */
    GC gc;			/* X's identifier for GC. */
} IdKey;

/*
 * One of the following data structures exists for each GC that is
 * currently active.  The structure is indexed with two hash tables,
 * one based on the values in the graphics context and the other
 * based on the display and GC identifier.
 */

typedef struct {
    GC gc;			/* Graphics context. */
    Display *display;		/* Display to which gc belongs. */
    int refCount;		/* Number of active uses of gc. */
    ValueKey key;		/* Hash table key in valueTable
				 * (needed when deleting
				 * this structure). */
} PadGC;

static int initialized = 0;	/* 0 means static structures haven't been
				 * initialized yet. */

/*
 * Forward declarations for procedures defined in this file:
 */

static void		GCInit (void);

/*
 *----------------------------------------------------------------------
 *
 * Pad_GetGC --
 *
 *	Given a desired set of values for a graphics context, find
 *	a read-only graphics context with the desired values.
 *
 * Results:
 *	The return value is the X identifer for the desired graphics
 *	context.  The caller should never modify this GC, and should
 *	call Pad_FreeGC when the GC is no longer needed.
 *
 * Side effects:
 *	The GC is added to an internal database with a reference count.
 *	For each call to this procedure, there should eventually be a call
 *	to Pad_FreeGC, so that the database can be cleaned up when GC's
 *	aren't needed anymore.
 *
 *----------------------------------------------------------------------
 */

GC
Pad_GetGC(Pad_Display *dpy, unsigned long valueMask, XGCValues *valuePtr)
{
    ValueKey valueKey;
    IdKey idKey;
    PadGC *gcPtr;
    int newc;

    if (!initialized) {
	GCInit();
    }

    /*
     * Must zero valueKey at start to clear out pad bytes that may be
     * part of structure on some systems.
     */

    memset((void *) &valueKey, 0, sizeof(valueKey));

    /*
     * First, check to see if there's already a GC that will work
     * for this request (exact matches only, sorry).
     */

    if (valueMask & GCFunction) {
	valueKey.values.function = valuePtr->function;
    } else {
	valueKey.values.function = GXcopy;
    }
    if (valueMask & GCPlaneMask) {
	valueKey.values.plane_mask = valuePtr->plane_mask;
    } else {
	valueKey.values.plane_mask = (unsigned) ~0;
    }
    if (valueMask & GCForeground) {
	valueKey.values.foreground = valuePtr->foreground;
    } else {
	valueKey.values.foreground = 0;
    }
    if (valueMask & GCBackground) {
	valueKey.values.background = valuePtr->background;
    } else {
	valueKey.values.background = 1;
    }
    if (valueMask & GCLineWidth) {
	valueKey.values.line_width = valuePtr->line_width;
    } else {
	valueKey.values.line_width = 0;
    }
    if (valueMask & GCLineStyle) {
	valueKey.values.line_style = valuePtr->line_style;
    } else {
	valueKey.values.line_style = LineSolid;
    }
    if (valueMask & GCCapStyle) {
	valueKey.values.cap_style = valuePtr->cap_style;
    } else {
	valueKey.values.cap_style = CapButt;
    }
    if (valueMask & GCJoinStyle) {
	valueKey.values.join_style = valuePtr->join_style;
    } else {
	valueKey.values.join_style = JoinMiter;
    }
    if (valueMask & GCFillStyle) {
	valueKey.values.fill_style = valuePtr->fill_style;
    } else {
	valueKey.values.fill_style = FillSolid;
    }
    if (valueMask & GCFillRule) {
	valueKey.values.fill_rule = valuePtr->fill_rule;
    } else {
	valueKey.values.fill_rule = EvenOddRule;
    }
    if (valueMask & GCArcMode) {
	valueKey.values.arc_mode = valuePtr->arc_mode;
    } else {
	valueKey.values.arc_mode = ArcPieSlice;
    }
    if (valueMask & GCTile) {
	valueKey.values.tile = valuePtr->tile;
    } else {
	valueKey.values.tile = None;
    }
    if (valueMask & GCStipple) {
	valueKey.values.stipple = valuePtr->stipple;
    } else {
	valueKey.values.stipple = None;
    }
    if (valueMask & GCTileStipXOrigin) {
	valueKey.values.ts_x_origin = valuePtr->ts_x_origin;
    } else {
	valueKey.values.ts_x_origin = 0;
    }
    if (valueMask & GCTileStipYOrigin) {
	valueKey.values.ts_y_origin = valuePtr->ts_y_origin;
    } else {
	valueKey.values.ts_y_origin = 0;
    }
    if (valueMask & GCFont) {
	valueKey.values.font = valuePtr->font;
    } else {
	valueKey.values.font = None;
    }
    if (valueMask & GCSubwindowMode) {
	valueKey.values.subwindow_mode = valuePtr->subwindow_mode;
    } else {
	valueKey.values.subwindow_mode = ClipByChildren;
    }
    if (valueMask & GCGraphicsExposures) {
	valueKey.values.graphics_exposures = valuePtr->graphics_exposures;
    } else {
	valueKey.values.graphics_exposures = True;
    }
    if (valueMask & GCClipXOrigin) {
	valueKey.values.clip_x_origin = valuePtr->clip_x_origin;
    } else {
	valueKey.values.clip_x_origin = 0;
    }
    if (valueMask & GCClipYOrigin) {
	valueKey.values.clip_y_origin = valuePtr->clip_y_origin;
    } else {
	valueKey.values.clip_y_origin = 0;
    }
    if (valueMask & GCClipMask) {
	valueKey.values.clip_mask = valuePtr->clip_mask;
    } else {
	valueKey.values.clip_mask = None;
    }
    if (valueMask & GCDashOffset) {
	valueKey.values.dash_offset = valuePtr->dash_offset;
    } else {
	valueKey.values.dash_offset = 0;
    }
    if (valueMask & GCDashList) {
	valueKey.values.dashes = valuePtr->dashes;
    } else {
	valueKey.values.dashes = 4;
    }
    valueKey.display = dpy->display;
    valueKey.screenNum = dpy->Screen_number();
    valueKey.depth = dpy->depth;
    if (gcPtr = (PadGC *)valueTable->Get((void *)&valueKey)) {
	gcPtr->refCount++;
	return gcPtr->gc;
    }

    /*
     * No GC is currently available for this set of values.  Allocate a
     * new GC and add a new structure to the database.
     */

    gcPtr = new PadGC;

    gcPtr->gc = XCreateGC(valueKey.display, dpy->rootDrawable, 
			  valueMask, &valueKey.values);
    gcPtr->display = valueKey.display;
    gcPtr->refCount = 1;
    memcpy((void *)&gcPtr->key, (void *)&valueKey, sizeof(ValueKey));
    idKey.display = valueKey.display;
    idKey.gc = gcPtr->gc;
    if (idTable->Get((void *)&idKey)) {
	cerr << "GC already registered in Pad_GetGC" << endl;
	exit(1);
    }
    valueTable->Set((void *)&valueKey, (void *)gcPtr);
    idTable->Set((void *)&idKey, (void *)gcPtr);

    return gcPtr->gc;
}

/*
 *----------------------------------------------------------------------
 *
 * Pad_FreeGC --
 *
 *	This procedure is called to release a graphics context allocated by
 *	Pad_GetGC.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The reference count associated with gc is decremented, and
 *	gc is officially deallocated if no-one is using it anymore.
 *
 *----------------------------------------------------------------------
 */

void
Pad_FreeGC(Pad_Display *dpy, GC gc)
{
    IdKey idKey;
    PadGC *gcPtr;

    if (!initialized) {
	cerr << "Pad_FreeGC called before Pad_GetGC" << endl;
	exit(1);
    }

    idKey.display = dpy->display;
    idKey.gc = gc;
    if (!(gcPtr = (PadGC *)idTable->Get((void *)&idKey))) {
	cerr << "Pad_FreeGC received unknown gc argument" << endl;
	exit(1);
    }
    gcPtr->refCount--;
    if (gcPtr->refCount == 0) {
	XFreeGC(gcPtr->display, gcPtr->gc);
	valueTable->Remove((void *)&gcPtr->key);
	idTable->Remove((void *)&idKey);
	delete gcPtr;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * GCInit --
 *
 *	Initialize the structures used for GC management.
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
GCInit()
{
    initialized = 1;
    valueTable = new Pad_HashTable(sizeof(ValueKey)/sizeof(int));
    idTable = new Pad_HashTable(sizeof(IdKey)/sizeof(int));
}
