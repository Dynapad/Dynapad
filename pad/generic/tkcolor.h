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

#ifndef _PAD_TK_COLOR_H
#define _PAD_TK_COLOR_H 1

#include "defs.h"

/*
 * If a colormap fills up, attempts to allocate new colors from that
 * colormap will fail.  When that happens, we'll just choose the
 * closest color from those that are available in the colormap.
 * One of the following structures will be created for each "stressed"
 * colormap to keep track of the colors that are available in the
 * colormap (otherwise we would have to re-query from the server on
 * each allocation, which would be very slow).  These entries are
 * flushed after a few seconds, since other clients may release or
 * reallocate colors over time.
 */

struct Pad_StressedCmap {
    Colormap colormap;			/* X's token for the colormap. */
    int numColors;			/* Number of entries currently active
					 * at *colorPtr. */
    XColor *colorPtr;			/* Pointer to malloc'ed array of all
					 * colors that seem to be available in
					 * the colormap.  Some may not actually
					 * be available, e.g. because they are
					 * read-write for another client;  when
					 * we find this out, we remove them
					 * from the array. */
    struct Pad_StressedCmap *nextPtr;	/* Next in list of all stressed
					 * colormaps for the display. */
};

class Pad_Display;

XColor *Pad_AllocXColor(Pad_Display *dpy, XColor *colorPtr);
void    Pad_FreeXColor(Pad_Display *dpy, XColor *color);
#endif
