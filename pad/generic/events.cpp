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
#include "events.h"
#include "object.h"
#include "win.h"
#include "view.h"
#include "portal.h"
#include "bind.h"
#include "global.h"
#include "api.h"

#include <stdlib.h>

#include <X11/extensions/XInput.h>

// List of portals and objects the currently grabbed
// event passed through.  Necessary for grabbing.
static Pad_List savePortals, saveObjects;
static Pad_Win *saveWin = NULL; // Event that ButtonPress landed on to be reused for motions and releases
static Pad_Bool deleted = FALSE;
static unsigned long staticSerial = 1;

static Pad_ObjHandle current;     // Current item under pointer

static XEvent pickEvent;    // The event upon which the current choice of 'current' is based.
// Must be saved so that if the current item is deleted, we
// can pick another.
static Pad_Bool buttonDown = FALSE;  // True when mouse button is pressed

//
// Return current object under pointer, or NULL if there is none.
//
Pad_Object *
Pad_Event::Get_current(void) {
    return (current._Get_object());
}

//
// Set the current object under pointer.
//
void
Pad_Event::Set_current(Pad_Object *obj) {
    current.Attach(obj);
}

Pad_Event::~Pad_Event() {
    // <result> gets deleted by creator
    Pad_events.Remove(this);
}

Pad_Event::Pad_Event(Pad_Win *w) {
    serial = staticSerial++;
    sourceWin = w;
    win = w;
    eventPtr = NULL;
    stickyPt.x = 0.0;
    stickyPt.y = 0.0;
    pt.x = 0.0;
    pt.y = 0.0;
    objPt.x = 0.0;
    objPt.y = 0.0;
    mag = 1.0;
    objMag = 1.0;
    _divisible = AUTOMATIC;
    result = NULL;
    resultCode = PAD_OK;
    Pad_events.Push(this);
    extension = FALSE;
}

extern unsigned int Pad_Get_x(XEvent *, Pad_Event *);

extern unsigned int Pad_Get_y(XEvent *, Pad_Event *);

Pad_Event::Pad_Event(Pad_Win *w, XEvent *e) {
    serial = staticSerial++;
    sourceWin = w;
    win = w;
    eventPtr = e;
    _divisible = AUTOMATIC;
    result = NULL;
    resultCode = PAD_OK;
    Update_coords(Pad_Get_x(e, this), Pad_Get_y(e, this));
    Pad_events.Push(this);
    extension = FALSE;
}

Pad_Event::Pad_Event(Pad_Win *w, int x, int y) {
    serial = staticSerial++;
    sourceWin = w;
    win = w;
    eventPtr = NULL;
    _divisible = AUTOMATIC;
    result = NULL;
    resultCode = PAD_OK;
    Update_coords(x, y);
    Pad_events.Push(this);
    extension = FALSE;
}

Pad_Event::Pad_Event(Pad_Event &event) {
    Pad_Object *obj;
    Pad_Portal *portal;
    Pad_Iterator oi;

    stickyPt = event.stickyPt;
    pt = event.pt;
    objPt = event.objPt;
    mag = event.mag;
    objMag = event.objMag;
    sourceWin = event.sourceWin;
    win = event.win;
    _divisible = event._divisible;
    DOLIST(oi, event.portals, Pad_Portal, portal) {
        portals.Push_last(portal);
    }
    DOLIST(oi, event.objects, Pad_Portal, obj) {
        objects.Push_last(obj);
    }
    serial = staticSerial++;
    eventPtr = event.eventPtr;
    if (event.result) {
        result = new Pad_String(event.result);
    } else {
        result = NULL;
    }
    resultCode = event.resultCode;
    Pad_events.Push(this);
    extension = FALSE;
}

//
// Given X coords (x, y), update pad coordinates
//
void
Pad_Event::Update_coords(int x, int y) {
    stickyPt.x = x - (win->width * 0.5);
    stickyPt.y = (win->height - y) - (win->height * 0.5);
    pt.x = (stickyPt.x / win->view->zoom) + win->view->xview;
    pt.y = (stickyPt.y / win->view->zoom) + win->view->yview;
    mag = win->view->zoom;
    objPt = pt;
    objMag = mag;
}

//
// Given pad coords (x, y), update event coordinates
//
void
Pad_Event::Update_pad_coords(float x, float y) {
    stickyPt.x = (x - win->view->xview) * win->view->zoom;
    stickyPt.y = (y - win->view->yview) * win->view->zoom;
    pt.x = x;
    pt.y = y;
    mag = win->view->zoom;
    objPt = pt;
    objMag = mag;
}

//
// Access divisible flag.
// Divisible specifies whether to descend into groups.
// Automatic means to use the groups definition.
//
void
Pad_Event::Set_divisible(int divisible) {
    _divisible = divisible;
}

int
Pad_Event::Get_divisible(void) {
    return (_divisible);
}

//
// This function gets called whenever an objects gets deleted.
// It makes sure that this object isn't used by the current event.
// It might be if the event passed through this object (if it
// is a group or portal).
//
void
Pad_EventObjectDeleted(Pad_Object *obj) {
    saveObjects.Remove(obj);
    if (savePortals.Remove(obj)) {
        // Kill grabbed events going through deleted portal
        deleted = TRUE;
    }
}

/*
 *--------------------------------------------------------------
 *
 * Pad_Bind_proc --
 *
 *	This procedure is invoked by the Tk dispatcher to handle
 *	events associated with bindings on items.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Depends on the command invoked as part of the binding
 *	(if there was any).
 *
 *--------------------------------------------------------------
 */

void
Pad_Bind_proc(ClientData clientData, XEvent *eventPtr) {
    int mask, state;
    Pad_Win *win = (Pad_Win *) clientData;
    Pad_Event *padEvent;

    //Preserve((ClientData) win);

    padEvent = new Pad_Event(win, eventPtr);

    /*
     * This code below keeps track of the current modifier state.
     * This information is used to defer repicks of
     * the current item while buttons are down.
     */

    if ((eventPtr->type == ButtonPress) || (eventPtr->type == ButtonRelease)) {
        switch (eventPtr->xbutton.button) {
            case Button1:
                mask = Button1Mask;
                break;
            case Button2:
                mask = Button2Mask;
                break;
            case Button3:
                mask = Button3Mask;
                break;
            case Button4:
                mask = Button4Mask;
                break;
            case Button5:
                mask = Button5Mask;
                break;
            default:
                mask = 0;
                break;
        }

        /*
         * For button press events, repick the current item using the
         * button state before the event, then process the event.  For
         * button release events, first process the event, then repick
         * the current item using the button state *after* the event
         * (the button has logically gone up before we change the
         * current item).
         */

        if (eventPtr->type == ButtonPress) {
            /*
             * On a button press, first repick the current item using
             * the button state before the event, the process the event.
             */
            padEvent->Pick_enter_leave();
            saveWin = padEvent->win;    // Event might hit another surface, and need to remember that
            state = eventPtr->xbutton.state & ~mask;
            buttonDown = (state & (Button1Mask | Button2Mask | Button3Mask | Button4Mask | Button5Mask)) ? TRUE : FALSE;
            padEvent->Do();
        } else {
            /*
             * Button release: first process the event, with the button
             * still considered to be down.  Then repick the current
             * item under the assumption that the button is no longer down.
             */
            // Check to make sure object hasn't been deleted
            if (Pad_Event::Get_current() && (!deleted)) {
                // Need to recalculate coordinates for the portal and object lists
                padEvent->portals = savePortals;
                padEvent->objects = saveObjects;
                if (saveWin) {
                    padEvent->win = saveWin;
                    win->view->pad->Find_pick(padEvent, savePortals, saveObjects);
                    padEvent->Do();
                }
            }
            eventPtr->xbutton.state ^= mask;
            buttonDown = (eventPtr->xbutton.state &
                          (Button1Mask | Button2Mask | Button3Mask | Button4Mask | Button5Mask)) ? TRUE : FALSE;
            delete padEvent;
            padEvent = new Pad_Event(win, eventPtr);   // Create a new event to pick from scratch
            padEvent->Pick_enter_leave();
            eventPtr->xbutton.state ^= mask;
            deleted = FALSE;
        }
    } else if ((eventPtr->type == EnterNotify)
               || (eventPtr->type == LeaveNotify)) {
        // Turn off pad "active" when window loses focus,
        // and then restore it when window re-entered
        if (eventPtr->type == EnterNotify) {
            win->view->pad->Set_active(TRUE);
        } else {
            win->view->pad->Set_active(FALSE);
        }
        if (Pad_focus && (Pad_focus != win->view->pad)) {
            Pad_focus->Damage();
        }
        padEvent->portals = savePortals;
        padEvent->objects = saveObjects;
        padEvent->Pick_enter_leave();
    } else if (eventPtr->type == MotionNotify) {
        if (!deleted) {
            buttonDown = (eventPtr->xmotion.state &
                          (Button1Mask | Button2Mask | Button3Mask | Button4Mask | Button5Mask)) ? TRUE : FALSE;
            if (buttonDown) {
                padEvent->portals = savePortals;
                padEvent->objects = saveObjects;
                if (saveWin) {
                    padEvent->win = saveWin;
                }
            }
            padEvent->Pick_enter_leave();
            padEvent->Do();
        }
    } else if (eventPtr->type == MotionType) {
        XDeviceMotionEvent *e = (XDeviceMotionEvent *) eventPtr;
        padEvent->extension = TRUE;
        padEvent->portals = savePortals;
        padEvent->objects = saveObjects;
        padEvent->Do();
    } else if (eventPtr->type == ButtonPressType ||
               eventPtr->type == ButtonReleaseType) {
        XDeviceButtonEvent *e = (XDeviceButtonEvent *) eventPtr;
        padEvent->extension = TRUE;
        padEvent->portals = savePortals;
        padEvent->objects = saveObjects;
        padEvent->Do();
    } else if (eventPtr->type == ProximityInType ||
               eventPtr->type == ProximityOutType) {
        XProximityNotifyEvent *e = (XProximityNotifyEvent *) eventPtr;
        padEvent->extension = TRUE;
        padEvent->portals = savePortals;
        padEvent->objects = saveObjects;
        padEvent->Do();
    } else {
        padEvent->portals = savePortals;
        padEvent->objects = saveObjects;
        padEvent->Do();
    }
    delete padEvent;

    //Release((ClientData) win);
}

/*
 *--------------------------------------------------------------
 *
 * DoItem --
 *
 *	This is a utility procedure called by FindItems.  It
 *	either adds itemPtr's id to the result forming in interp,
 *	or it adds a new tag to itemPtr, depending on the value
 *	of tag.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	If tag is NULL then itemPtr's id is added as a list element
 *	to interp->result;  otherwise tag is added to itemPtr's
 *	list of tags.
 *
 *--------------------------------------------------------------
 */

static void
DoItem(Pad_Object *itemPtr, Pad_Uid tag) {
    Pad_Uid t;
    Pad_Iterator oi;

    DOLISTI(oi, itemPtr->tags, Pad_Uid, t) {
        if (tag == t) {
            return;
        }
    }

    /*
     * Add in the new tag.
     */

    itemPtr->Add_tag(tag);
}

/*
 *--------------------------------------------------------------
 *
 * Pick_enter_leave
 *
 *	Find the topmost item in a pad that contains a given
 *	location and mark the the current item.  If the current
 *	item has changed, generate a fake exit event on the old
 *	current item and a fake enter event on the new current
 *	item.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The current item for win may change.  If it does,
 *	then the commands associated with item entry and exit
 *	could do just about anything.
 *
 *--------------------------------------------------------------
 */

void
Pad_Event::Pick_enter_leave(void) {
    Pad_ObjHandle closest;
    Pad_List origPortals, origObjects;
    Pad_List origSavePortals, origSaveObjects;
    Pad_Object *obj;
    Pad_Point origPt;
    float origMag;

    /*
     * Check whether or not a button is down.  If so, we'll log entry
     * and exit into and out of the current item, but not entry into
     * any other item.  This implements a form of grabbing equivalent
     * to what the X server does for windows.
     */

    if (!buttonDown) {
        sourceWin->flags &= ~WIN_LEFT_GRABBED_ITEM;
    }

    /*
     * Save information about this event.  The event is used for two purposes:
     *   1. Event bindings: if the current item changes, fake events are
     *      generated to allow item-enter and item-leave bindings to trigger.
     *   2. Reselection: if the current item gets deleted, can use the
     *      saved event to find a new current item.
     * Translate MotionNotify events into EnterNotify events, since that's
     * what gets reported to item handlers.
     */

    if (eventPtr != &pickEvent) {
        if ((eventPtr->type == MotionNotify) || (eventPtr->type == ButtonRelease)) {
            pickEvent.xcrossing.type = EnterNotify;
            pickEvent.xcrossing.serial = eventPtr->xmotion.serial;
            pickEvent.xcrossing.send_event = eventPtr->xmotion.send_event;
            pickEvent.xcrossing.display = eventPtr->xmotion.display;
            pickEvent.xcrossing.window = eventPtr->xmotion.window;
            pickEvent.xcrossing.root = eventPtr->xmotion.root;
            pickEvent.xcrossing.subwindow = None;
            pickEvent.xcrossing.time = eventPtr->xmotion.time;
            pickEvent.xcrossing.x = eventPtr->xmotion.x;
            pickEvent.xcrossing.y = eventPtr->xmotion.y;
            pickEvent.xcrossing.x_root = eventPtr->xmotion.x_root;
            pickEvent.xcrossing.y_root = eventPtr->xmotion.y_root;
            pickEvent.xcrossing.mode = NotifyNormal;
            pickEvent.xcrossing.detail = NotifyNonlinear;
            pickEvent.xcrossing.same_screen = eventPtr->xmotion.same_screen;
            pickEvent.xcrossing.focus = False;
            pickEvent.xcrossing.state = eventPtr->xmotion.state;
        } else {
            pickEvent = *eventPtr;
        }
    }

    /*
     * If this is a recursive call (there's already a partially completed
     * call pending on the stack;  it's in the middle of processing a
     * Leave event handler for the old current item) then just return;
     * the pending call will do everything that's needed.
     */

    if (sourceWin->flags & WIN_REPICK_IN_PROGRESS) {
        return;
    }

    // Use serial # to avoid following portal cycles
    // Need to use our own serial #. X serial doesn't
    // change for pure motion events.
    serial = staticSerial++;

    /*
     * A LeaveNotify event automatically means that there's no current
     * object, so the check for closest item can be skipped.
     */

    // Need to make sure portal list doesn't change if
    // button is down.  Necessary for proper grabbing.
    origPortals = portals;
    origObjects = objects;
    origSavePortals = savePortals;
    origSaveObjects = saveObjects;
    origPt = pt;
    origMag = mag;
    if (pickEvent.type != LeaveNotify) {
        serial = staticSerial++;
        if (buttonDown) {
            // Check to see if object has been deleted
            if (Get_current() == NULL) {
                closest.Attach(NULL);
            } else {
                closest.Attach(sourceWin->view->pad->Find_pick(this, portals, objects));
            }
        } else {
            closest.Attach(sourceWin->view->pad->Find_pick(this));
            savePortals = portals;     // Save portal list for proper grabbing
            saveObjects = objects;     // Save object list for proper grabbing
        }
    }

    /*
     * Simulate a LeaveNotify event on the previous current item and
     * an EnterNotify event on the new current item.  Remove the "current"
     * tag from the previous current item and place it on the new current
     * item.
     */

    if ((closest != current)
        && (Get_current() != NULL)
        && !(sourceWin->flags & WIN_LEFT_GRABBED_ITEM)) {
        XEvent event;
        Pad_Event *padEvent;
        Pad_Object *itemPtr = Get_current();

        event = pickEvent;
        event.type = LeaveNotify;

        /*
         * If the event's detail happens to be NotifyInferior the
         * binding mechanism will discard the event.  To be consistent,
         * always use NotifyAncestor.
         */

        event.xcrossing.detail = NotifyAncestor;
        sourceWin->flags |= WIN_REPICK_IN_PROGRESS;
        padEvent = new Pad_Event(win, &event);

        if (itemPtr) {
            itemPtr->Pointer_out(padEvent); // Let the item that is about to get <Leave> event know about it.
        }

        padEvent->portals = origPortals;
        padEvent->objects = origObjects;
        padEvent->Do();                    // Generate LEAVE Event
        sourceWin->flags &= ~WIN_REPICK_IN_PROGRESS;
        delete padEvent;

        /*
         * The check below is needed because there could be an event
         * handler for <LeaveNotify> that deletes the current item.
         */

        obj = Get_current();
        if (itemPtr && (itemPtr == obj) && !buttonDown) {
            itemPtr->Delete_tag(Pad_currentUid);
        }

        if (pickEvent.type != LeaveNotify) {
            serial = staticSerial++;
            // Need to make sure portal list doesn't change if
            // button is down.  Necessary for proper grabbing.
            pt = origPt;
            mag = origMag;
            if (buttonDown) {
                // Check to see if object has been deleted
                if (Get_current() == NULL) {
                    closest.Attach(NULL);
                } else {
                    portals = origPortals;
                    objects = origObjects;
                    closest.Attach(sourceWin->view->pad->Find_pick(this, portals, objects));
                }
            } else {
                portals = origPortals;
                objects = origObjects;
                closest.Attach(sourceWin->view->pad->Find_pick(this));
                savePortals = portals;     // Save portal list for proper grabbing
                saveObjects = objects;     // Save object list for proper grabbing
            }
        }
    }
    // Restore objects and portals
    portals = origSavePortals;
    objects = origSaveObjects;

    if ((closest != current) && buttonDown) {
        sourceWin->flags |= WIN_LEFT_GRABBED_ITEM;
        return;
    }
    if ((closest == current)
        && !(sourceWin->flags & WIN_LEFT_GRABBED_ITEM)) {
        return;
    }
    sourceWin->flags &= ~WIN_LEFT_GRABBED_ITEM;
    current.Attach(closest);
    if (Get_current()) {
        XEvent event;
        Pad_Event *padEvent;

        DoItem(closest._Get_object(), Pad_currentUid);
        event = pickEvent;
        event.type = EnterNotify;
        event.xcrossing.detail = NotifyAncestor;
        sourceWin->flags |= WIN_REPICK_IN_PROGRESS;
        padEvent = new Pad_Event(win, &event);
        closest._Get_object()->Pointer_in(padEvent); // Let the item that is about to get <Enter> event know about it.
        padEvent->portals = portals;
        padEvent->objects = objects;
        padEvent->Do();                  // Generate ENTER Event
        sourceWin->flags &= ~WIN_REPICK_IN_PROGRESS;
        delete padEvent;
    }

    // Restore objects and portals
    portals = origPortals;
    objects = origObjects;
}

//
// Generate an event of type <type> on <tag>, and
// return the string returned by the event in <result>,
// and return the return code of the event.
//
int
Pad_GenerateEvent(Pad_Win *win, int type, char *tag, Pad_String &result) {
    int rc;
    XEvent event;
    Pad_Event *padEvent;
    ClientData objectPtr[1];
    Pad_ObjHandle saveCurrent;

    if (win->bindingTable == NULL) {
        return (PAD_ERROR);
    }

    event.type = type;
    padEvent = new Pad_Event(win, &event);
    result = "";
    padEvent->result = &result;

    saveCurrent = current;
    current.Attach(NULL);
    objectPtr[0] = Pad_GetUid(tag);

    Pad_BindEvent(win->bindingTable, &event, padEvent, 1, objectPtr, NULL);
    rc = padEvent->resultCode;
    delete padEvent;

    current = saveCurrent;

    return (rc);
}

//
// Fire event on the specified Pad_Object.
//
void
Pad_Event::Do(Pad_Object *obj) {
    Pad_ObjHandle saveCurrent;

    saveCurrent = current;
    if (obj) {
        current.Attach(obj);
    } else {
        current.Attach(NULL);
    }

    Do();

    current = saveCurrent;
}

/*
 *--------------------------------------------------------------
 *
 * Do
 *
 *	This procedure is called to invoke binding processing
 *	for a new event that is associated with the current item
 *	for a pad.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Depends on the bindings for the pad.
 *
 *--------------------------------------------------------------
 */

void
Pad_Event::Do(void) {
#define NUM_STATIC 3
    ClientData staticObjects[NUM_STATIC];
    ClientData *objectPtr;
    int numObjects, i;
    Pad_Object *itemPtr;
    Pad_Iterator oi;
    Pad_Uid tag;
    int numTags;

    if (win->bindingTable == NULL) {
        return;
    }

    if ((eventPtr->type == KeyPress) || (eventPtr->type == KeyRelease)) {
        itemPtr = Pad_focus;
    } else {
        itemPtr = Get_current();
    }
    if (itemPtr == NULL) {
        return;
    }

    /*
     * Set up an array with all the relevant objects for processing
     * this event.  The relevant objects are (a) the event's item,
     * (b) the tags associated with the event's item, and (c) the
     * tag "all".  If there are a lot of tags then malloc an array
     * to hold all of the objects.
     */

    numTags = itemPtr->tags.Length();
    numObjects = numTags + 2;
    if (numObjects <= NUM_STATIC) {
        objectPtr = staticObjects;
    } else {
        objectPtr = new ClientData[numObjects];
    }

    // Determine the order of event firings
    // (most general or specific first) by the
    // PAD_MASK_ID_EVENT_FIRST bit.
    objectPtr[0] = ((itemPtr->flags & PAD_MASK_ID_EVENT_FIRST) ? (ClientData) itemPtr : (ClientData) Pad_allUid);
    i = 1;
    DOLISTI(oi, itemPtr->tags, Pad_Uid, tag) {
        objectPtr[i] = (void *)tag;
        i++;
    }
    objectPtr[numTags + 1] =
        ((itemPtr->flags & PAD_MASK_ID_EVENT_FIRST) ? (ClientData) Pad_allUid : (ClientData) itemPtr);

    /*
     * Invoke the binding system, then free up the object array if
     * it was malloc-ed.
     */

    Pad_BindEvent(itemPtr->pad->view->win->bindingTable, eventPtr, this, numObjects, objectPtr, itemPtr);
    if (objectPtr != staticObjects) {
        delete[] objectPtr;
    }
}
