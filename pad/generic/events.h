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

#ifndef EVENT_H
#define EVENT_H 1

#include "defs.h"
#include "point.h"
#include "pad-string.h"
#include <X11/X.h>

class Pad_Win;
class Pad_Object;

//
// Special Pad++ events (and their masks)
//
#define Pad_PortalInterceptNotify       TK_LASTEVENT
#define Pad_CreateNotify                (TK_LASTEVENT + 1)
#define Pad_ModifyNotify                (TK_LASTEVENT + 2)
#define Pad_DeleteNotify                (TK_LASTEVENT + 3)
#define Pad_WriteNotify                 (TK_LASTEVENT + 4)
#define PAD_LASTEVENT                   (TK_LASTEVENT + 5)

#define Pad_PortalInterceptMask   (1L<<25)
#define Pad_CreateMask            (1L<<26)
#define Pad_ModifyMask            (1L<<27)
#define Pad_DeleteMask            (1L<<28)
#define Pad_WriteMask             (1L<<29)

extern int         motion_type;
extern int         button_press_type;
extern int         button_release_type;
extern int         key_press_type;
extern int         key_release_type;
extern int         proximity_in_type;
extern int         proximity_out_type;
#define MotionType        LASTEvent+1
#define ButtonPressType   LASTEvent+2
#define ButtonReleaseType LASTEvent+3
#define ProximityInType   LASTEvent+4
#define ProximityOutType  LASTEvent+5

#include <X11/extensions/XInput.h>
extern XDeviceInfo *xinputdevices;
extern int num_xinputdevices;

class Pad_Event
{
  public:
    Pad_Point   stickyPt;	// Sticky coordinates of event
    Pad_Point   pt;		// Coordinates of event
    float       mag;		// Magnification of event
    Pad_Point   objPt;		// Coordinates of event in object coordinates, used for bind macro expansion %U
    float       objMag;		// Magnification of event in object coordinates, used for bind macro expansion %V
    Pad_Win *   sourceWin;	// Surface that event started on
    Pad_Win *   win;		// Surface that event lands on
    Pad_List    portals;	// List of portals that event went through
    Pad_List    objects;	// List of objects that event was transformed by
    unsigned long serial;	// Id # for this event
    XEvent *    eventPtr;	// Related X event
    Pad_String  info;		// Info about this event
    Pad_String *result;		// String returned by event
    int         resultCode;	// Tcl result code of this event
    Pad_Bool    extension;      // True if event comes from an input extension

    void Do(void);
    void Do(Pad_Object *obj);
    void Do(char *tag);
    int  Get_divisible(void);
    void Pick_enter_leave(void);
    void Set_divisible(int divisible);
    void Update_coords(int x, int y);          // Set event to X coordinates (x, y)
    void Update_pad_coords(float x, float y);  // Set event to pad coordinates (x, y)

    static Pad_Object *Get_current(void);       // Return current item under pointer
    static void Set_current(Pad_Object *obj);   // Set the current item under pointer

   ~Pad_Event();
    Pad_Event(Pad_Win *win);
    Pad_Event(Pad_Win *win, XEvent *eventPtr);
    Pad_Event(Pad_Win *win, int x, int y);
    Pad_Event(Pad_Event &event);

 private:
   int _divisible;		// Specifies if should treat groups as divisible: True, False, or Auto
};

void     Pad_Bind_proc(ClientData clientData, XEvent *eventPtr);
void     Pad_EventObjectDeleted(Pad_Object *obj);
int      Pad_GenerateEvent(Pad_Win *win, int type, char *tag, Pad_String &result);

#endif
