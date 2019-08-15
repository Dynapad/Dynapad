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

#ifndef DEFS_H
#define DEFS_H 1


#  include "../unix/config.h"

#include "list.h"

extern "C" {	// for X11/Xlib.h in win95
#  include <X11/Xlib.h>
}


#define PAD_MAJOR_VERSION 0
#define PAD_MINOR_VERSION 9
#define PAD_VERSION "0.9"
#define PAD_PATCH_LEVEL 2

#define XMIN 0         // Bounding Box indices
#define YMIN 1
#define XMAX 2
#define YMAX 3

#define DOTIMES(j, cnt) for(j = 0; j < cnt; j++)

#define PAD_RELIEF_FLAT          0
#define PAD_RELIEF_GROOVE        1
#define PAD_RELIEF_RAISED        2
#define PAD_RELIEF_RIDGE         3
#define PAD_RELIEF_SOLID         4
#define PAD_RELIEF_SUNKEN        5

#define PAD_3D_FLAT_GC           1
#define PAD_3D_LIGHT_GC          2
#define PAD_3D_DARK_GC           3

#define VirtualEvent        (LASTEvent)
#define ActivateNotify      (LASTEvent + 1)
#define DeactivateNotify    (LASTEvent + 2)
#define MouseWheelEvent     (LASTEvent + 3)
#define myTK_LASTEVENT      (LASTEvent + 4)

#define MouseWheelMask      (1L << 28)

#define ActivateMask        (1L << 29)
#define VirtualEventMask    (1L << 30)

#ifndef NULL
#define NULL    0
#endif

typedef char *Pad_Uid;
typedef unsigned char Pad_Bool;  // Booleans
typedef void *voidPtr;           // Makes casting easier
typedef void *ClientData;        // Makes casting easier

#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif

// used for some settings to mean 'unset by user'
#define AUTOMATIC 2

//
// Layout types
//
#define PAD_LAYOUT_LEFT       1
#define PAD_LAYOUT_RIGHT      2
#define PAD_LAYOUT_TOP        3
#define PAD_LAYOUT_BOTTOM     4
#define PAD_LAYOUT_HORIZONTAL 5
#define PAD_LAYOUT_VERTICAL   6
#define PAD_LAYOUT_COORDS     7
#define PAD_LAYOUT_WIDTH      8
#define PAD_LAYOUT_HEIGHT     9

///////////////////////////////////////////////////////////////////////////////////////
//
// Useful constants
//
///////////////////////////////////////////////////////////////////////////////////////

#ifndef PI
#define PI		3.141592
#endif
#define PITIMES2	6.283185307179586    // 2 * pi

///////////////////////////////////////////////////////////////////////////////////////
//
// One-argument macros
//
///////////////////////////////////////////////////////////////////////////////////////

// Absolute value of a
#define ABS(a)		(((a)<0) ? -(a) : (a))

// Take binary sign of a, either -1, or 1 if >= 0
#define SIGN(a)		(((a)<0) ? -1 : 1)

#define FSIGN(x)     ((x) >= 0 ? 1 : -1)
#define DEG2RAD(x)   ((x)*((PI)/(180.000000)))
#define POW2(x)      ((x) * (x))

///////////////////////////////////////////////////////////////////////////////////////
//
// Two-argument macros
//
///////////////////////////////////////////////////////////////////////////////////////

// Find minimum of a and b
#define MIN(a,b)	(((a)<(b))?(a):(b))	

// Find maximum of a and b
#define MAX(a,b)	(((a)>(b))?(a):(b))	

// Linear interpolation from a (when t=0) to b (when t=1)
#define LERP(t, a, b)   ((a) + (t) * ((b) - (a)))

//
// RGBA pixel management
//
#define CC_RED(rgba)   ((rgba) & 0xff)
#define CC_GREEN(rgba) (((rgba) >> 8) & 0xff)
#define CC_BLUE(rgba)  (((rgba) >> 16) & 0xff)
#define CC_ALPHA(rgba) ((rgba) >> 24)
#define CC_RGBA(r,g,b,a) ((r) | ((g) << 8) | ((b) << 16) | ((a) << 24))


#define MM      0		// Measurement Units
#define INCHES  1
#define POINTS  2
#define PIXELS  3

				// 
				// Convert units to internal value and vice versa
				// 
				// If no coordinate frame,
				//   Units may be any of the above Measurement Units.
				// Else
				//   Units in range (0.0, 1.0) relative to frame.
				// Internal value is pixels.
				// 
				// 

extern Pad_List Pad_coordFrames; // List of coordinate frames for hierarchical coords

/* linear interpolation from l (when a=0) to h (when a=1)*/
/* (equal to (a*h)+((1-a)*l) */
#ifndef LERP
#define LERP(a,l,h)	((l)+(((h)-(l))*(a)))
#endif

inline float coordframe_to_x(float coord)
{
    float *frame = (float *)Pad_coordFrames.First();
    return (LERP(coord, frame[XMIN], frame[XMAX]));
}

inline float coordframe_to_y(float coord)
{
    float *frame = (float *)Pad_coordFrames.First();
    return (LERP(coord, frame[YMIN], frame[YMAX]));
}

inline float coordframe_to_value(float unit)
{
    float *frame = (float *)Pad_coordFrames.First();
    float avgdim = 0.5 * ((frame[XMAX] - frame[XMIN]) + (frame[YMAX] - frame [YMIN]));
    return (unit * avgdim);
}

inline float x_to_coordframe(float x)
{
    float *frame = (float *)Pad_coordFrames.First();
    return ((x - frame[XMIN]) / (frame[XMAX] - frame[XMIN]));
}

inline float y_to_coordframe(float y)
{
    float *frame = (float *)Pad_coordFrames.First();
    return ((y - frame[YMIN]) / (frame[YMAX] - frame[YMIN]));
}

inline float value_to_coordframe(float value)
{
    float *frame = (float *)Pad_coordFrames.First();
    float avgdim = 0.5 * ((frame[XMAX] - frame[XMIN]) + (frame[YMAX] - frame [YMIN]));
    return (value / avgdim);
}

#define UNITSTOX(win, u) (Pad_coordFrames.Is_empty() ? ((u) * win->xfac) : coordframe_to_x(u))
#define UNITSTOY(win, u) (Pad_coordFrames.Is_empty() ? ((u) * win->yfac) : coordframe_to_y(u))
#define UNITSTOVALUE(win, unit) (Pad_coordFrames.Is_empty() ? \
				 ((unit) * (0.5 * (win->xfac + win->yfac))) : \
				 coordframe_to_value(unit))

#define XTOUNITS(win, x) (Pad_coordFrames.Is_empty() ? ((x) / win->xfac) : x_to_coordframe(x))
#define YTOUNITS(win, y) (Pad_coordFrames.Is_empty() ? ((y) / win->yfac) : y_to_coordframe(y))
#define VALUETOUNITS(win, value) (Pad_coordFrames.Is_empty() ? \
				  ((value) / (0.5 * (win->xfac + win->yfac))) : \
				  value_to_coordframe(value))

#define ATOXF(win, str) (UNITSTOX(win, atof(str)))
#define ATOYF(win, str) (UNITSTOY(win, atof(str)))
#define ATOVALUEF(win, str) (UNITSTOVALUE(win, atof(str)))
#define ATOXFE(win, str, end) (UNITSTOX(win, strtod(str, end)))
#define ATOYFE(win, str, end) (UNITSTOY(win, strtod(str, end)))

#define PAD_FIND_AND              0
#define PAD_FIND_ERROR            1
#define PAD_FIND_OR               2
#define PAD_FIND_REGEXP           3
#define PAD_FIND_EXACT            4
#define PAD_FIND_GLOB             5
#define PAD_SEARCH_ALL            6
#define PAD_SEARCH_ABOVE          7
#define PAD_SEARCH_BELOW          8
#define PAD_SEARCH_CLOSEST        9
#define PAD_SEARCH_ENCLOSED       10
#define PAD_SEARCH_OVERLAPPING    11
#define PAD_SEARCH_WITHTAG        12
#define PAD_SEARCH_WITHTEXT       13
#define PAD_SEARCH_WITHNAME       14
#define PAD_SEARCH_WITHTYPE       15
#define PAD_SEARCH_WITHLAYER      16
#define PAD_SEARCH_WITHINFO       17
#define PAD_SEARCH_WITHSTICKY     18

//
// The macro below is used to modify a "char" value (e.g. by casting
// it to an unsigned character) so that it can be used safely with
// macros such as isspace.
//

#define UCHAR(c) ((unsigned char) (c))

#endif
