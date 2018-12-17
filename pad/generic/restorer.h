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

#ifndef RESTORER_H
#define RESTORER_H 1


//
// Pad_Restorer
//
// This object manages restoration of changed part of Pad surface, by process
// of refinement. Each restorers encapsulates a region that it is restoring
// (implemented using the X Region facility). Logical operations on restorers
// allow the window to manage what each restorer is working on.

#include "defs.h"
#include "list.h"
#include "plist.h"
#include "misc.h"


extern "C" {
#  include <X11/Xutil.h>
}

#include <iostream>
using namespace std;


class Pad_Win;

enum Pad_Restore_Mode {
    RESTORE_UNDEFINED,
    RESTORE_NOW,
    RESTORE_WHEN_IDLE,
    RESTORE_REFINE,
    RESTORE_REPAIR
};

//
// Delay before repairing a screen (e.g. after a pan)
//
#define REPAIR_DELAY 1000

//
// delay to use if rescheduling a restore (e.g. because of interrupt)
//
#define RESCHEDULE_DELAY 1000

//
// Delay between refinements
//
#define REFINE_DELAY         50	        // subsequent refinements - fast

#define IDLE_DELAY           0          // Delay for damage renders

class Pad_Restorer
{

    friend ostream &operator<<(ostream &, Pad_Restorer &);
 public:
    Pad_Win *win;         // Window I am working on
    int refinementLevel;  // How far refinement has got (-1 = nowhere)
    int delay;		  // Delay for timer
    Pad_TimerToken timer; // timer for next refinement

    Pad_Bool full;             // True when restoring whole window

    Pad_Bool should_flush;     // True if this restorer should call swapbuffers
    Pad_Bool flush_all;        // If true, render should copy whole back buffer to window

    Pad_Restore_Mode mode;     // Restorer type

    Region xregion;            // Region that the restorer is working on
    Pad_List clip_stack;       // Stack of clip planes

    Pad_Restorer(Pad_Win *win, Pad_Bool is_full);
    Pad_Restorer(Pad_Restorer *restorer);
    ~Pad_Restorer();

    Pad_Bool Is_refiner()     // True if this wants refining
        {
            return (mode == RESTORE_REFINE);
        }

    Pad_Bool Is_repairer()     // True if this is repairing
        {
            return (mode == RESTORE_REPAIR);
        }

    Pad_Bool Is_idler()        // True if this is an idler
        {
            return (mode == RESTORE_WHEN_IDLE);
        }

    Pad_Bool Is_full()        // True if doing full restoration
        {
            return full;
        }

    Pad_Bool Is_empty()       // True when restorer's region becomes empty
        {
            return ( (!full) && XEmptyRegion(xregion));
        }

    //
    // Logical operations on restorers
    //
    Pad_Restorer & operator += (const Pad_Restorer &other);
    Pad_Restorer & operator += (XRectangle xrect);
    Pad_Restorer & operator -= (const Pad_Restorer &other);
    Pad_Restorer & operator &= (const Pad_Restorer &other);

    //
    // Move restorer's origin (needed for panning)
    //
    void Translate(int x, int y)
        {
            if (!full)
                XOffsetRegion(xregion, x, y);
        }

    //
    // Add rectangle to a restorer's region (assume matrix set up). Clips against
    // active screen clipping mask.
    //
    void Add_rect_to_refiner(float bbox[4]);

    // Same as add_rect_to_refiner, but doesn't clip against active screen clipping mask
    void Add_rect(float bbox[4]);

    // Adds rect (in pad pixel coordinates) to a restorer. Doesn't use current transform.
    void Add_rect(float xmin, float ymin, float xmax, float ymax);



    //
    // True if a rectangle overlaps with a restorers region.
    // (assumes transform matrix set up)
    //
    Pad_Bool Contains_rect(float bbox[4]);

    // True if rectangle is entirely within restorers region.
    // (assumes transform matrix is set up)
    Pad_Bool Occludes_rect(float bbox[4]);

    //
    // Return bounding box for the whole of the restorer's region
    //
    void Get_clip_box(int &x, int &y, int &width, int &height);
    void Get_bbox_screen_coords(float *bbox, float *clipped, float *unclipped);
    void Get_visibility(Pad_Object *obj, float &xmin, float &ymin, float &xmax, float &ymax);

    // Sets the clip mask of an X GC to a restorer's region
    void Push_clip(float bbox[4]);

    // JM -  used for nonrectangular portals
    void Push_clip(Pad_Point *points, int npoints);
    void Push_clip(Pad_PList &points);

    // JM - low level routine used by other Push_clip methods
    void Push_clip(Region region);

    void Pop_clip(void);
    void Reset_clip_stack(void);

    void Set_gc_clip(GC gc);
    void Unset_gc_clip(GC gc);

				// Set and unset clipping regions (and DCs on windows)
    void Initialize_clip_region(void);
    void Finalize_clip_region(void);

    // Return the current clipping region in use by a restorer, or NULL.
    Region Get_region(void);

    // activates a restoration process
    void Do_restore(void);
    void Reschedule(void);
    void Reschedule_copy(void);
    void Schedule(Pad_Restore_Mode mode, Pad_Restorer *current_restorer);

    // Make this restorer full
    void Make_full(void) {full = TRUE;}
};

// Utility functions
void Pad_AddRectToRegion(Region region, int x, int y, int width, int height);
void Pad_SetRoundedRect(XRectangle *rect, float x, float y, float width, float height);
#endif
