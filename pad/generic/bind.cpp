/* 
 * bind.C --
 *
 *	This file provides procedures that associate Tcl commands
 *	with X events or sequences of X events.
 *
 * Copyright (c) 1989-1994 The Regents of the University of California.
 * Copyright (c) 1994-1995 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
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

/*
  Modified 11/13/95 from tkBind.c to work with Pad++ (Ben Bederson - UNM).
  The following changes are made:
    - C++'ized
    - Extra macro expansions added
    - New events added: <PortalIntercept>, <Create>, <Configure>, <Delete>, <Write>
    - User-specified Modifiers added
    - Event handlers may be any language defined by Pad_Callback, including C++
*/

#include "defs.h"
#include "events.h"
#include "group.h"
#include "object.h"
#include "pad.h"
#include "view.h"
#include "win.h"
#include "bind.h"
#include "callback.h"
#include "portal.h"
#include "misc.h"
#include "display.h"
#include "global.h"

#include <ctype.h>
#include <iostream>
using namespace std;
#include <stdlib.h>
#  include <X11/keysym.h>

#include <X11/extensions/XInput.h>

/*
 * Structures of the following form are used as keys in the patternTable
 * for a binding table:
 */

typedef struct PatternTableKey {
    ClientData object;		/* Identifies object (or class of objects)
				 * relative to which event occurred.  For
				 * example, in the widget binding table for
				 * an application this is the path name of
				 * a widget, or a widget class, or "all". */
    int type;			/* Type of event (from X). */
    int detail;			/* Additional information, such as
				 * keysym or button, or 0 if nothing
				 * additional.*/
} PatternTableKey;

/*
 * The following structure defines a pattern, which is matched
 * against X events as part of the process of converting X events
 * into Tcl commands.
 */

typedef struct Pattern {
    int eventType;		/* Type of X event, e.g. ButtonPress. */
    int needMods;		/* Mask of modifiers that must be
				 * present (0 means no modifiers are
				 * required). */
    int detail;			/* Additional information that must
				 * match event.  Normally this is 0,
				 * meaning no additional information
				 * must match.  For KeyPress and
				 * KeyRelease events, a keySym may
				 * be specified to select a
				 * particular keystroke (0 means any
				 * keystrokes).  For button events,
				 * specifies a particular button (0
				 * means any buttons are OK). */
    int mode;			/* Mode that must match.
				 * 0 means that any mode will do. */
} Pattern;

/*
 * The structure below defines a pattern sequence, which consists
 * of one or more patterns.  In order to trigger, a pattern
 * sequence must match the most recent X events (first pattern
 * to most recent event, next pattern to next event, and so on).
 */

typedef struct PatSeq {
    int numPats;		/* Number of patterns in sequence
				 * (usually 1). */
    Pad_Callback *command;	/* Command to invoke when this
				 * pattern sequence matches (malloc-ed). */
    int flags;			/* Miscellaneous flag values;  see
				 * below for definitions. */
    struct PatSeq *nextSeqPtr;
				/* Next in list of all pattern
				 * sequences that have the same
				 * initial pattern.  NULL means
				 * end of list. */
    PatternTableKey key;	/* Hash table key, needed to find the
				 * head of the list of which nextSeqPtr
				 * forms a part. */
    ClientData object;		/* Identifies object with which event is
				 * associated (e.g. window). */
    struct PatSeq *nextObjPtr;
				/* Next in list of all pattern
				 * sequences for the same object
				 * (NULL for end of list).  Needed to
				 * implement Tk_DeleteAllBindings. */
    Pattern pats[1];		/* Array of "numPats" patterns.  Only
				 * one element is declared here but
				 * in actuality enough space will be
				 * allocated for "numPats" patterns.
				 * To match, pats[0] must match event
				 * n, pats[1] must match event n-1,
				 * etc. */
} PatSeq;

/*
 * Flag values for PatSeq structures:
 *
 * PAT_NEARBY		1 means that all of the events matching
 *			this sequence must occur with nearby X
 *			and Y mouse coordinates and close in time.
 *			This is typically used to restrict multiple
 *			button presses.
 */

#define PAT_NEARBY		1

/*
 * Constants that define how close together two events must be
 * in milliseconds or pixels to meet the PAT_NEARBY constraint:
 */

#define NEARBY_PIXELS		5
#define NEARBY_MS		500

static int initialized = 0;

/*
 * A hash table is kept to map from the string names of event
 * modifiers to information about those modifiers.  The structure
 * for storing this information, and the hash table built at
 * initialization time, are defined below.
 */

typedef struct {
    const char *name;   /* Name of modifier. */
    int mask;			/* Button/modifier mask value, such as Button1Mask. */
    int flags;			/* Various flags;  see below for definitions. */
} ModInfo;

/*
 * Flags for ModInfo structures:
 *
 * DOUBLE -		Non-zero means duplicate this event,
 *			e.g. for double-clicks.
 * TRIPLE -		Non-zero means triplicate this event,
 *			e.g. for triple-clicks.
 * USER -               Non-zero means there is a user-defined modifier
 */

#define DOUBLE		1
#define TRIPLE		2
#define USER            4
static int max_mode = USER;

/*
 * The following special modifier mask bits are defined, to indicate
 * logical modifiers such as Meta and Alt that may float among the
 * actual modifier bits.
 */

//
// User defined modifiers are allowed by first declaring them with
// the Pad_AddBindingModifier command.  Then, bindings can be declared
// with a maximum of 1 user-defined modifier.  A modifier can be made
// active with the Pad_SetBindingModifier command in which case only
// bindings with that modifier will fire.
//
// User defined modifiers are not added to modArray.  Instead, they are
// added to the hashtable modTable, and all uses of modArray in the binding
// code were changed to use modTable instead.  User-defined modifiers have
// the mask USER_MASK, and an integer in the <flags> field that specifies
// which modifier it is.  Note that the bottom two bigs of the flags field are
// used by the Double and Triple modifiers, so user-defined modifier flags
// are integers which don't use the bottom two bits.
//

#define META_MASK	(AnyModifier<<1)
#define ALT_MASK	(AnyModifier<<2)
#define USER_MASK	(AnyModifier<<3)

static ModInfo modArray[] = {
    {"Control",		ControlMask,	0},
    {"Shift",		ShiftMask,	0},
    {"Lock",		LockMask,	0},
    {"Meta",		META_MASK,	0},
    {"M",		META_MASK,	0},
    {"Alt",		ALT_MASK,	0},
    {"B1",		Button1Mask,	0},
    {"Button1",		Button1Mask,	0},
    {"B2",		Button2Mask,	0},
    {"Button2",		Button2Mask,	0},
    {"B3",		Button3Mask,	0},
    {"Button3",		Button3Mask,	0},
    {"B4",		Button4Mask,	0},
    {"Button4",		Button4Mask,	0},
    {"B5",		Button5Mask,	0},
    {"Button5",		Button5Mask,	0},
    {"Mod1",		Mod1Mask,	0},
    {"M1",		Mod1Mask,	0},
    {"Mod2",		Mod2Mask,	0},
    {"M2",		Mod2Mask,	0},
    {"Mod3",		Mod3Mask,	0},
    {"M3",		Mod3Mask,	0},
    {"Mod4",		Mod4Mask,	0},
    {"M4",		Mod4Mask,	0},
    {"Mod5",		Mod5Mask,	0},
    {"M5",		Mod5Mask,	0},
    {"Double",		0,		DOUBLE},
    {"Triple",		0,		TRIPLE},
    {"Any",		0,		0},	/* Ignored: historical relic. */
    {NULL,		0,		0}
};
static Pad_HashTable *modTable;

/*
 * This module also keeps a hash table mapping from event names
 * to information about those events.  The structure, an array
 * to use to initialize the hash table, and the hash table are
 * all defined below.
 */

typedef struct {
    const char *name;   /* Name of event. */
    int type;			/* Event type for X, such as ButtonPress. */
    int eventMask;		/* Mask bits (for XSelectInput) for this event type. */
} EventInfo;

/*
 * Note:  some of the masks below are an OR-ed combination of
 * several masks.  This is necessary because X doesn't report
 * up events unless you also ask for down events.  Also, X
 * doesn't report button state in motion events unless you've
 * asked about button events.
 */

static EventInfo eventArray[] = {
    {"MotionType",	  MotionType,  		1},
    {"ButtonPressType",	  ButtonPressType,  	1},
    {"ButtonReleaseType", ButtonReleaseType,  	1},
    {"ProximityInType",   ProximityInType,  	1},
    {"ProximityOutType",  ProximityOutType,  	1},
    {"Motion",		MotionNotify,  ButtonPressMask|PointerMotionMask},
    {"Button",		ButtonPress,		ButtonPressMask},
    {"ButtonPress",	ButtonPress,		ButtonPressMask},
    {"ButtonRelease",	ButtonRelease, ButtonPressMask|ButtonReleaseMask},
    {"Colormap",	ColormapNotify,		ColormapChangeMask},
    {"Enter",		EnterNotify,		EnterWindowMask},
    {"Leave",		LeaveNotify,		LeaveWindowMask},
    {"Expose",		Expose,			ExposureMask},
    {"FocusIn",		FocusIn,		FocusChangeMask},
    {"FocusOut",	FocusOut,		FocusChangeMask},
    {"Key",		KeyPress,		KeyPressMask},
    {"KeyPress",	KeyPress,		KeyPressMask},
    {"KeyRelease",	KeyRelease,    KeyPressMask|KeyReleaseMask},
    {"Property",	PropertyNotify,		PropertyChangeMask},
    {"Circulate",	CirculateNotify,	StructureNotifyMask},
    {"Configure",	ConfigureNotify,	StructureNotifyMask},
    {"Destroy",		DestroyNotify,		StructureNotifyMask},
    {"Gravity",		GravityNotify,		StructureNotifyMask},
    {"Map",		MapNotify,		StructureNotifyMask},
    {"Reparent",	ReparentNotify,		StructureNotifyMask},
    {"Unmap",		UnmapNotify,		StructureNotifyMask},
    {"Visibility",	VisibilityNotify,	VisibilityChangeMask},
    {"Activate",	ActivateNotify,		ActivateMask},
    {"Deactivate",	DeactivateNotify,	ActivateMask},
    {"PortalIntercept", Pad_PortalInterceptNotify, Pad_PortalInterceptMask},
    {"Create",          Pad_CreateNotify,          Pad_CreateMask},
    {"Modify",          Pad_ModifyNotify,          Pad_ModifyMask},
    {"Delete",          Pad_DeleteNotify,          Pad_DeleteMask},
    {"Write",           Pad_WriteNotify,           Pad_WriteMask},
    {(char *) NULL,	0,			0}
};
static Pad_HashTable *eventTable;

/*
 * The defines and table below are used to classify events into
 * various groups.  The reason for this is that logically identical
 * fields (e.g. "state") appear at different places in different
 * types of events.  The classification masks can be used to figure
 * out quickly where to extract information from events.
 */

#define KEY_BUTTON_MOTION	0x1
#define CROSSING		0x2
#define FOCUS			0x4
#define EXPOSE			0x8
#define VISIBILITY		0x10
#define BIND_CREATE		0x20
#define MAP			0x40
#define REPARENT		0x80
#define CONFIG			0x100
#define CONFIG_REQ		0x200
#define RESIZE_REQ		0x400
#define GRAVITY			0x800
#define PROP			0x1000
#define SEL_CLEAR		0x2000
#define SEL_REQ			0x4000
#define SEL_NOTIFY		0x8000
#define COLORMAP		0x10000
#define MAPPING			0x20000
#define ACTIVATE		0x40000

static int flagArray[myTK_LASTEVENT] = {
   /* Not used */		0,
   /* Not used */		0,
   /* KeyPress */		KEY_BUTTON_MOTION,
   /* KeyRelease */		KEY_BUTTON_MOTION,
   /* ButtonPress */		KEY_BUTTON_MOTION,
   /* ButtonRelease */		KEY_BUTTON_MOTION,
   /* MotionNotify */		KEY_BUTTON_MOTION,
   /* EnterNotify */		CROSSING,
   /* LeaveNotify */		CROSSING,
   /* FocusIn */		FOCUS,
   /* FocusOut */		FOCUS,
   /* KeymapNotify */		0,
   /* Expose */			EXPOSE,
   /* GraphicsExpose */		EXPOSE,
   /* NoExpose */		0,
   /* VisibilityNotify */	VISIBILITY,
   /* CreateNotify */		BIND_CREATE,
   /* DestroyNotify */		0,
   /* UnmapNotify */		0,
   /* MapNotify */		MAP,
   /* MapRequest */		0,
   /* ReparentNotify */		REPARENT,
   /* ConfigureNotify */	CONFIG,
   /* ConfigureRequest */	CONFIG_REQ,
   /* GravityNotify */		0,
   /* ResizeRequest */		RESIZE_REQ,
   /* CirculateNotify */	0,
   /* CirculateRequest */	0,
   /* PropertyNotify */		PROP,
   /* SelectionClear */		SEL_CLEAR,
   /* SelectionRequest */	SEL_REQ,
   /* SelectionNotify */	SEL_NOTIFY,
   /* ColormapNotify */		COLORMAP,
   /* ClientMessage */		0,
   /* MappingNotify */		MAPPING,
   /* Activate */		ACTIVATE,	    
   /* Deactivate */		ACTIVATE
};

/*
 * Prototypes for local procedures defined in this file:
 */

static void		ChangeScreen(char *dispName, int screenIndex);
static PatSeq *		FindSequence(BindingTable *bindPtr,
			    ClientData object,
			    const char *eventString, int create,
			    unsigned long *maskPtr);
static const char *	GetField(const char *p, char *copy, int size);
static KeySym		GetKeySym(Pad_Display *dispPtr,
			    XEvent *eventPtr);
static void		InitKeymapInfo(Pad_Display *dispPtr);
static PatSeq *		MatchPatterns(Pad_Display *dispPtr,
			    BindingTable *bindPtr, PatSeq *psPtr, int mode);
static void             PrintPatSeq(Pad_String *ds, PatSeq *psPtr);


static unsigned int
Get_state(XEvent *event)
{
    switch (event->type) {
	case MotionType:
	    return ((XDeviceMotionEvent *)event)->device_state;
	case ButtonPressType:
	case ButtonReleaseType:
	    return ((XDeviceButtonEvent *)event)->device_state;
	case ProximityInType:
	case ProximityOutType:
	    return ((XProximityNotifyEvent *)event)->device_state;
        default:
            return(event->xbutton.state);
    }
}

static unsigned int
Get_button(XEvent *event)
{
    switch (event->type) {
      case ButtonPressType:
      case ButtonReleaseType:
        return ((XDeviceButtonEvent *)event)->button;
    }
    return(event->xbutton.button);
}

unsigned int
Pad_Get_x(XEvent *event, Pad_Event *)
{
    switch (event->type) {
      case MotionType:
        return ((XDeviceMotionEvent *)event)->x;
      case ButtonPressType:
      case ButtonReleaseType:
        return ((XDeviceButtonEvent *)event)->x;
      case ProximityInType:
      case ProximityOutType:
        return ((XProximityNotifyEvent *)event)->x;
    }
    return(event->xbutton.x);
}

unsigned int
Pad_Get_y(XEvent *event, Pad_Event *)
{
    switch (event->type) {
      case MotionType:
	return ((XDeviceMotionEvent *)event)->y;
      case ButtonPressType:
      case ButtonReleaseType:
	return ((XDeviceButtonEvent *)event)->y;
      case ProximityInType:
      case ProximityOutType:
        return ((XProximityNotifyEvent *)event)->y;
    }
    return(event->xbutton.y);
}

/*
 *--------------------------------------------------------------
 *
 * Pad_GetEventName
 *
 *	Converts an event type into an event name
 *
 * Results:
 *      Event name
 *
 *--------------------------------------------------------------
 */

const char *
Pad_GetEventName(int type)
{
    const char *name = NULL;
    EventInfo *eiPtr;

    for (eiPtr = eventArray; eiPtr->name != NULL; eiPtr++) {
	if (eiPtr->type == type) {
	    name = eiPtr->name;
	    break;
	}
    }

    return(name);
}

/*
 *--------------------------------------------------------------
 *
 * Pad_AddBindingModifier --
 *
 *	Adds the specified modifier to the binding tables.
 *
 * Results:
 *      none
 *
 *--------------------------------------------------------------
 */

void
Pad_AddBindingModifier(const char *modifier)
{
    int rc;
    int new_modifier;
    ModInfo *modPtr;

	// First check to see if it's already there
    rc = Pad_GetBindingModifier(modifier);
    if (rc == 0) {
		modPtr = new ModInfo;

		char *name = new char[strlen(modifier) + 1];
		strcpy(name, modifier);

		modPtr->name = name;
		modPtr->mask = USER_MASK;
		modPtr->flags = max_mode;
		max_mode += USER;
		modTable->Set((void *)modPtr->name, (void *)modPtr);
    }
}

/*
 *--------------------------------------------------------------
 *
 * Pad_DeleteBindingModifier --
 *
 *	Deletes the specified modifier from the binding tables.
 *
 * Results:
 *	Returns TRUE if the modifier was successfully deleted,
 *      and FALSE otherwise.
 *
 *--------------------------------------------------------------
 */

Pad_Bool
Pad_DeleteBindingModifier(const char *modifier)
{
    int rc;

    if (!modTable->Remove((void *)modifier)) {
	rc = FALSE;
    } else {
	rc = TRUE;
    }

    return(rc);
}

/*
 *--------------------------------------------------------------
 *
 * Pad_GetBindingModifier --
 *
 *	Return one form of the a modifier given the other.
 *      I.e., pass it a string, and it returns the integer.
 *      Pass it the integer, and it returns the string.
 *
 * Results:
 *	Returns the other form of the modifier
 *
 *--------------------------------------------------------------
 */

int
Pad_GetBindingModifier(const char *modifier)
{
    int mode;
    ModInfo *modPtr;

    if (!(modPtr = (ModInfo *)modTable->Get((void *)modifier))) {
	mode = 0;
    } else {
	mode = modPtr->flags;
    }

    return(mode);
}

const char *
Pad_GetBindingModifier(int mode)
{
    const char *modifier;
    ModInfo *modPtr;
    Pad_HashSearch search;
    void *key;

    modifier = NULL;
    for (modPtr = (ModInfo *)modTable->Init(search, key);
	    modPtr != NULL;
	    modPtr = (ModInfo *)modTable->Next(search, key)) {
	if ((modPtr->mask == USER_MASK) && (modPtr->flags == mode)) {
	    modifier = modPtr->name;
	    break;
	}
    }

    return(modifier);
}

/*
 *--------------------------------------------------------------
 *
 * Pad_CreateBindingTable --
 *
 *	Set up a new domain in which event bindings may be created.
 *
 * Results:
 *	The return value is a token for the new table, which must
 *	be passed to procedures like Tk_CreatBinding.
 *
 * Side effects:
 *	Memory is allocated for the new table.
 *
 *--------------------------------------------------------------
 */

BindingTable *
Pad_CreateBindingTable()
{
    BindingTable *bindPtr;
    int i;

    /*
     * If this is the first time a binding table has been created,
     * initialize the global data structures.
     */

    if (!initialized) {
	ModInfo *modPtr;
	EventInfo *eiPtr;
	int dummy;

	initialized = 1;
    
	modTable = new Pad_HashTable(PAD_STRING_TABLE);
	for (modPtr = modArray; modPtr->name != NULL; modPtr++) {
	    modTable->Set((void *)modPtr->name, (void *)modPtr);
	}
    
	eventTable = new Pad_HashTable(PAD_STRING_TABLE);
	for (eiPtr = eventArray; eiPtr->name != NULL; eiPtr++) {
	    eventTable->Set((void *)eiPtr->name, (void *)eiPtr);
	}
    }

    /*
     * Create and initialize a new binding table.
     */

    bindPtr = new BindingTable;
    for (i = 0; i < EVENT_BUFFER_SIZE; i++) {
	bindPtr->eventRing[i].type = -1;
    }
    bindPtr->curEvent = 0;
    bindPtr->patternTable = new Pad_HashTable(sizeof(PatternTableKey)/sizeof(int));
    bindPtr->objectTable = new Pad_HashTable(PAD_VOID_TABLE);
    return bindPtr;
}

/*
 *--------------------------------------------------------------
 *
 * Pad_DeleteBindingTable --
 *
 *	Destroy a binding table and free up all its memory.
 *	The caller should not use bindingTable again after
 *	this procedure returns.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Memory is freed.
 *
 *--------------------------------------------------------------
 */

void
Pad_DeleteBindingTable(BindingTable *bindPtr)
{
    PatSeq *psPtr, *nextPtr;
    Pad_HashSearch search;
    void *key;

    /*
     * Find and delete all of the patterns associated with the binding
     * table.
     */

    for (psPtr = (PatSeq *)bindPtr->patternTable->Init(search, key);
            psPtr != NULL; 
	    psPtr = (PatSeq *)bindPtr->patternTable->Next(search, key)) {
	for ( ;
		psPtr != NULL; psPtr = nextPtr) {
	    nextPtr = psPtr->nextSeqPtr;
	    if (psPtr->command) {
		psPtr->command->Decrement_refcount();
		if (psPtr->command->Get_refcount() == 0) {
		    delete psPtr->command;
		}
	    }
	    free((char *) psPtr);
	}
    }

    /*
     * Clean up the rest of the information associated with the
     * binding table.
     */

    delete bindPtr->patternTable;
    delete bindPtr->objectTable;
    delete bindPtr;
}

/*
 *--------------------------------------------------------------
 *
 * Pad_CreateBinding --
 *
 *	Add a binding to a binding table, so that future calls to
 *	Tk_BindEvent may execute the command in the binding.
 *
 * Results:
 *	The return value is 0 if an error occurred while setting
 *	up the binding.  In this case, an error message will be
 *	left in interp->result.  If all went well then the return
 *	value is a mask of the event types that must be made
 *	available to Tk_BindEvent in order to properly detect when
 *	this binding triggers.  This value can be used to determine
 *	what events to select for in a window, for example.
 *
 * Side effects:
 *	The new binding may cause future calls to Tk_BindEvent to
 *	behave differently than they did previously.
 *
 *--------------------------------------------------------------
 */

unsigned long
Pad_CreateBinding(BindingTable *bindPtr, ClientData object, 
		  const char *eventString, Pad_Callback *callback)
{
    PatSeq *psPtr;
    unsigned long eventMask;

    psPtr = FindSequence(bindPtr, object, eventString, 1, &eventMask);
    if (psPtr == NULL) {
	return 0;
    }
    callback->Increment_refcount();
    if (psPtr->command) {
	psPtr->command->Decrement_refcount();
	if (psPtr->command->Get_refcount() == 0) {
	    delete psPtr->command;
	}
    }
    psPtr->command = callback;

    return eventMask;
}

/*
 *--------------------------------------------------------------
 *
 * Pad_DeleteBinding --
 *
 *	Remove an event binding from a binding table.
 *
 * Results:
 *	The result is a standard Tcl return value.  If an error
 *	occurs then interp->result will contain an error message.
 *
 * Side effects:
 *	The binding given by object and eventString is removed
 *	from bindingTable.
 *
 *--------------------------------------------------------------
 */

int
Pad_DeleteBinding(BindingTable *bindPtr, ClientData object, const char *eventString)
{
    PatSeq *psPtr, *prevPtr;
    unsigned long eventMask;

    psPtr = FindSequence(bindPtr, object, eventString, 0, &eventMask);
    if (psPtr == NULL) {
	Pad_resultString = "";
	return PAD_OK;
    }

    /*
     * Unlink the binding from the list for its object, then from the
     * list for its pattern.
     */

    if (!(prevPtr = (PatSeq *)bindPtr->objectTable->Get((void *)object))) {
	cerr << "Tk_DeleteBinding couldn't find object table entry" << endl;
	exit(1);
    }
    if (prevPtr == psPtr) {
	bindPtr->objectTable->Set((void *)object, (void *)prevPtr->nextObjPtr);
    } else {
	for ( ; ; prevPtr = prevPtr->nextObjPtr) {
	    if (prevPtr == NULL) {
		cerr << "Tk_DeleteBinding couldn't find on object list" << endl;
		exit(1);
	    }
	    if (prevPtr->nextObjPtr == psPtr) {
		prevPtr->nextObjPtr = psPtr->nextObjPtr;
		break;
	    }
	}
    }
    prevPtr = (PatSeq *)bindPtr->patternTable->Get((void *)&psPtr->key);
    if (prevPtr == psPtr) {
	if (psPtr->nextSeqPtr == NULL) {
	    bindPtr->patternTable->Remove((void *)&psPtr->key);
	} else {
	    bindPtr->patternTable->Set((void *)&psPtr->key,
	            (void *)psPtr->nextSeqPtr);
	}
    } else {
	for ( ; ; prevPtr = prevPtr->nextSeqPtr) {
	    if (prevPtr == NULL) {
		cerr << "Tk_DeleteBinding couldn't find on hash chain" << endl;
		exit(1);
	    }
	    if (prevPtr->nextSeqPtr == psPtr) {
		prevPtr->nextSeqPtr = psPtr->nextSeqPtr;
		break;
	    }
	}
    }
    psPtr->command->Decrement_refcount();
    if (psPtr->command->Get_refcount() == 0) {
	delete psPtr->command;
    }
    free((char *) psPtr);
    return PAD_OK;
}

/*
 *--------------------------------------------------------------
 *
 * Pad_GetBinding --
 *
 *	Return the command associated with a given event string.
 *
 * Results:
 *	The return value is a pointer to the command string
 *	associated with eventString for object in the domain
 *	given by bindingTable.  If there is no binding for
 *	eventString, or if eventString is improperly formed,
 *	then NULL is returned and an error message is left in
 *	interp->result.  The return value is semi-static:  it
 *	will persist until the binding is changed or deleted.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

Pad_Callback *
Pad_GetCallback(BindingTable *bindPtr, ClientData object, const char *eventString)
{
    PatSeq *psPtr;
    unsigned long eventMask;

    psPtr = FindSequence(bindPtr, object, eventString, 0, &eventMask);
    if (psPtr == NULL) {
	return NULL;
    }
    return psPtr->command;
}

/*
 *--------------------------------------------------------------
 *
 * Pad_GetAllBindings --
 *
 *	Return a list of event strings for all the bindings
 *	associated with a given object.
 *
 * Results:
 *	There is no return value.  Interp->result is modified to
 *	hold a Tcl list with one entry for each binding associated
 *	with object in bindingTable.  Each entry in the list
 *	contains the event string associated with one binding.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

void
Pad_GetAllBindings(BindingTable *bindPtr, ClientData object, Pad_List &list)
{
    PatSeq *psPtr;
    Pad_String *string;

    if (!(psPtr = (PatSeq *)bindPtr->objectTable->Get((void *)object))) {
	return;
    }
    for ( ; psPtr != NULL;
	    psPtr = psPtr->nextObjPtr) {
	string = new Pad_String();
	PrintPatSeq(string, psPtr);
	list.Push((void *)string);
    }
    return;
}


/*
 *--------------------------------------------------------------
 *
 * PrintPatSeq --
 *
 *      For the specified PatSeq (an event binding), output information
 *      about each of the patterns in its sequence.  The order of the 
 *      patterns in the sequence is backwards from the order in which they
 *      must be output.
 *
 * Results:
 *	There is no return value.  The specified Pad_String is modified to
 *	hold a a description of the PatSeq.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

static void
PrintPatSeq(Pad_String *ds, PatSeq *psPtr)
{
    Pattern *patPtr;
    char c, buffer[10];
    int patsLeft, needMods;
    ModInfo *modPtr;
    EventInfo *eiPtr;
    Pad_HashSearch searchPtr;
    void *key;

    *ds = "";
    for (patsLeft = psPtr->numPats,
	     patPtr = &psPtr->pats[psPtr->numPats - 1];
	 patsLeft > 0; patsLeft--, patPtr--) {

	/*
	 * Check for simple case of an ASCII character.
	 */

	if ((patPtr->eventType == KeyPress)
	    && (patPtr->needMods == 0)
	    && (patPtr->detail < 128)
	    && isprint(UCHAR(patPtr->detail))
	    && (patPtr->detail != '<')
	    && (patPtr->detail != ' ')) {

	    c = patPtr->detail;
	    *ds += c;
	    continue;
	}

	/*
	 * It's a more general event specification.  First check
	 * for "Double" or "Triple", then modifiers, then event type,
	 * then keysym or button detail.
	 */

	*ds += '<';
	if ((patsLeft > 1) && (memcmp((char *) patPtr,
				      (char *) (patPtr-1), sizeof(Pattern)) == 0)) {
	    patsLeft--;
	    patPtr--;
	    if ((patsLeft > 1) && (memcmp((char *) patPtr,
					  (char *) (patPtr-1), sizeof(Pattern)) == 0)) {
		patsLeft--;
		patPtr--;
		*ds += "Triple-";
	    } else {
		*ds += "Double-";
	    }
	}
	// Go through hash table instead of array
	// to access user-defined modifiers.
	needMods = patPtr->needMods;
	modPtr = (ModInfo *)modTable->Init(searchPtr, key);
	while (modPtr) {
	    if (needMods == 0) {
		break;
	    }
	    if (((modPtr->mask == USER_MASK) && (modPtr->flags == patPtr->mode)) ||
		((modPtr->mask != USER_MASK) && (modPtr->mask & needMods))) {
		needMods &= ~modPtr->mask;
		*ds += modPtr->name;
		*ds += '-';
	    }
	    modPtr = (ModInfo *)modTable->Next(searchPtr, key);
	}

	for (eiPtr = eventArray; eiPtr->name != NULL; eiPtr++) {
	    if (eiPtr->type == patPtr->eventType) {
		*ds += eiPtr->name;
		if (patPtr->detail != 0) {
		    *ds += '-';
		}
		break;
	    }
	}

	if (patPtr->detail != 0) {
	    if ((patPtr->eventType == KeyPress)
		|| (patPtr->eventType == KeyRelease)) {
		char *string;

		string = XKeysymToString((KeySym) patPtr->detail);
		if (string != NULL) {
		    *ds += string;
		}
	    } else {
		sprintf(buffer, "%d", patPtr->detail);
		*ds += buffer;
	    }
	}
	*ds += '>';
    }
}
/*
 *--------------------------------------------------------------
 *
 * Pad_DeleteAllBindings --
 *
 *	Remove all bindings associated with a given object in a
 *	given binding table.
 *
 * Results:
 *	All bindings associated with object are removed from
 *	bindingTable.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

void
Pad_DeleteAllBindings(BindingTable *bindPtr, ClientData object)
{
    PatSeq *psPtr, *prevPtr;
    PatSeq *nextPtr;

    if (!(psPtr = (PatSeq *)bindPtr->objectTable->Get((void *)object))) {
	return;
    }
    nextPtr = NULL;		// For compiler warnings only
    for ( ; psPtr != NULL;
	    psPtr = nextPtr) {
	nextPtr  = psPtr->nextObjPtr;

	/*
	 * Be sure to remove each binding from its hash chain in the
	 * pattern table.  If this is the last pattern in the chain,
	 * then delete the hash entry too.
	 */

	prevPtr = (PatSeq *)bindPtr->patternTable->Get((void *)&psPtr->key);
	if (prevPtr == psPtr) {
	    if (psPtr->nextSeqPtr == NULL) {
		bindPtr->patternTable->Remove((void *)&psPtr->key);
	    } else {
		bindPtr->patternTable->Set((void *)&psPtr->key,
		        (void *)psPtr->nextSeqPtr);
	    }
	} else {
	    for ( ; ; prevPtr = prevPtr->nextSeqPtr) {
		if (prevPtr == NULL) {
		    cerr << "Tk_DeleteAllBindings couldn't find on hash chain" << endl;
		    exit(1);
		}
		if (prevPtr->nextSeqPtr == psPtr) {
		    prevPtr->nextSeqPtr = psPtr->nextSeqPtr;
		    break;
		}
	    }
	}
	psPtr->command->Decrement_refcount();
	if (psPtr->command->Get_refcount() == 0) {
	    delete psPtr->command;
	}
	free((char *) psPtr);
    }
    bindPtr->objectTable->Remove((void *)object);
}

/*
 *--------------------------------------------------------------
 *
 * Pad_BindEvent --
 *
 *	This procedure is invoked to process an X event.  The
 *	event is added to those recorded for the binding table.
 *	Then each of the objects at *objectPtr is checked in
 *	order to see if it has a binding that matches the recent
 *	events.  If so, that binding is invoked and the rest of
 *	objects are skipped.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Depends on the command associated with the matching
 *	binding.
 *
 *--------------------------------------------------------------
 */

void
Pad_BindEvent(BindingTable *bindPtr, XEvent *eventPtr, Pad_Event *padEvent,
	      int numObjects, ClientData *objectPtr, Pad_Object *itemPtr)
{
    Pad_Display *dispPtr = padEvent->win->dpy;
    XEvent *ringPtr;
    PatSeq *matchPtr, *psPtr;
    PatternTableKey key;
    int detail, code, oldScreen;
    Pad_List callbacks;		// List of Pad_Callback's
    Pad_Callback *callback = NULL;
    Pad_Win *win;
    Pad_List debug_event_list;
    Pad_String *debug_event;
    Pad_String debug_notice;
    Pad_String debug_ds;
    Pad_Iterator di;

    code = PAD_OK;
    win = padEvent->win;

    /*
     * Ignore the event completely if it is an Enter, Leave, FocusIn,
     * or FocusOut event with detail NotifyInferior.  The reason for
     * ignoring these events is that we don't want transitions between
     * a window and its children to visible to bindings on the parent:
     * this would cause problems for mega-widgets, since the internal
     * structure of a mega-widget isn't supposed to be visible to
     * people watching the parent.
     */

    if ((eventPtr->type == EnterNotify)  || (eventPtr->type == LeaveNotify)) {
	if (eventPtr->xcrossing.detail == NotifyInferior) {
	    return;
	}
    }
    if ((eventPtr->type == FocusIn)  || (eventPtr->type == FocusOut)) {
	if (eventPtr->xfocus.detail == NotifyInferior) {
	    return;
	}
    }

    /*
     * Add the new event to the ring of saved events for the
     * binding table.  Two tricky points:
     *
     * 1. Combine consecutive MotionNotify events.  Do this by putting
     *    the new event *on top* of the previous event.
     * 2. If a modifier key is held down, it auto-repeats to generate
     *    continuous KeyPress and KeyRelease events.  These can flush
     *    the event ring so that valuable information is lost (such
     *    as repeated button clicks).  To handle this, check for the
     *    special case of a modifier KeyPress arriving when the previous
     *    two events are a KeyRelease and KeyPress of the same key.
     *    If this happens, mark the most recent event (the KeyRelease)
     *    invalid and put the new event on top of the event before that
     *    (the KeyPress).
     */

    if ((eventPtr->type == MotionNotify)
	    && (bindPtr->eventRing[bindPtr->curEvent].type == MotionNotify)) {
	/*
	 * Don't advance the ring pointer.
	 */
    } else if (eventPtr->type == KeyPress) {
	int i;
	for (i = 0; ; i++) {
	    if (i >= dispPtr->numModKeyCodes) {
		goto advanceRingPointer;
	    }
	    if (dispPtr->modKeyCodes[i] == eventPtr->xkey.keycode) {
		break;
	    }
	}
	ringPtr = &bindPtr->eventRing[bindPtr->curEvent];
	if ((ringPtr->type != KeyRelease)
		|| (ringPtr->xkey.keycode != eventPtr->xkey.keycode)) {
	    goto advanceRingPointer;
	}
	if (bindPtr->curEvent <= 0) {
	    i = EVENT_BUFFER_SIZE - 1;
	} else {
	    i = bindPtr->curEvent - 1;
	}
	ringPtr = &bindPtr->eventRing[i];
	if ((ringPtr->type != KeyPress)
		|| (ringPtr->xkey.keycode != eventPtr->xkey.keycode)) {
	    goto advanceRingPointer;
	}
	bindPtr->eventRing[bindPtr->curEvent].type = -1;
	bindPtr->curEvent = i;
    } else {
	advanceRingPointer:
	bindPtr->curEvent++;
	if (bindPtr->curEvent >= EVENT_BUFFER_SIZE) {
	    bindPtr->curEvent = 0;
	}
    }
    ringPtr = &bindPtr->eventRing[bindPtr->curEvent];
    memcpy((void *) ringPtr, (void *) eventPtr, sizeof(XEvent));
    detail = 0;
    bindPtr->detailRing[bindPtr->curEvent] = 0;
    if ((ringPtr->type == KeyPress) || (ringPtr->type == KeyRelease)) {
	detail = (int) GetKeySym(dispPtr, ringPtr);
	if (detail == NoSymbol) {
	    detail = 0;
	}
    } else if ((ringPtr->type == ButtonPress)
	    || (ringPtr->type == ButtonRelease)) {
	detail = Get_button(ringPtr);
    }
    bindPtr->detailRing[bindPtr->curEvent] = detail;

    /*
     * Loop over all the objects, finding the binding script for each
     * one.  Append all of the binding scripts, with %-sequences expanded,
     * to "callbacks".
     */

    if (win->debugEvent) {
	debug_ds = "";
    }
    for ( ; numObjects > 0; numObjects--, objectPtr++) {

	/*
	 * Match the new event against those recorded in the
	 * pattern table, saving the longest matching pattern.
	 * For events with details (button and key events) first
	 * look for a binding for the specific key or button.
	 * If none is found, then look for a binding for all
	 * keys or buttons (detail of 0).
	 */
    
	matchPtr = NULL;
	key.object = *objectPtr;
	key.type = ringPtr->type;
	key.detail = detail;
	if (psPtr = (PatSeq *)bindPtr->patternTable->Get((void *)&key)) {
	    matchPtr = MatchPatterns(dispPtr, bindPtr,
		    psPtr, win->view->pad->mode);
	}
	if ((detail != 0) && (matchPtr == NULL)) {
	    key.detail = 0;
	    if (psPtr = (PatSeq *)bindPtr->patternTable->Get((void *)&key)) {
		matchPtr = MatchPatterns(dispPtr, bindPtr,
			psPtr, win->view->pad->mode);
	    }
	}
    

	if (matchPtr != NULL) {
				// Debugging info to see events as they fire.
				// For each event for which an event handler is defined
				// on tagOrIds that are being tracked, store a nice
				// user-readable description of that event for later output.
	    if (win->debugEvent) {
		debug_event = new Pad_String();
		PrintPatSeq(&debug_ds, matchPtr);
		*debug_event += debug_ds;
		*debug_event += " ";
		if (itemPtr) {
		    *debug_event += itemPtr->id;
		} else {
		    *debug_event += "1";
		}
		*debug_event += " ";
		if (key.object != itemPtr) {
		    *debug_event += (char *)key.object;
		}
		// Hackery from Dan: adds window coords to debug string
		*debug_event += " sx=";
		*debug_event += padEvent->stickyPt.x;
		*debug_event += " sy=";
		*debug_event += padEvent->stickyPt.y;
		  //end Dan hackery

		debug_event_list.Push_last(debug_event);


	    }

				// check if callback
				// is a script, and if it is, expand '%' macros.
	    callback = matchPtr->command;

	    if (callback) {
                callbacks.Push_last(callback);
	    }
	}
    }

    //
    // If there is a global event callback, add it to the front of the callbacks list
    // so that it gets called first
    //
    if (win->globalEventCallback) {
      callbacks.Push(win->globalEventCallback);
    }

    if (callbacks.Is_empty()) {
	return;
    }


    /*
     * Now go back through and evaluate the script for each object,
     * in order, dealing with "break" and "continue" exceptions
     * appropriately.
     *
     * There are two tricks here:
     * 1. Bindings can be invoked from in the middle of Tcl commands,
     *    where interp->result is significant (for example, a widget
     *    might be deleted because of an error in creating it, so the
     *    result contains an error message that is eventually going to
     *    be returned by the creating command).  To preserve the result,
     *    we save it in a dynamic string.
     * 2. The binding's action can potentially delete the binding,
     *    so bindPtr may not point to anything valid once the action
     *    completes.  Thus we have to save bindPtr->interp in a
     *    local variable in order to restore the result.
     */


				// There can be multiple handlers, and the object
				// could be deleted by one of them - so keep a handle
				// and if it does get deleted, then stop firing the events.
    Pad_ObjectHandle handle(itemPtr);

    while (!callbacks.Is_empty()) {
	callback = (Pad_Callback *)callbacks.Pop();
				// Event handler is about to fire.
				// If debugging is on, then output debugging info
				// for this event
	if (win->debugEvent) {
	    debug_event = (Pad_String *)debug_event_list.Pop();
	    Pad_Debug_output(win, debug_event->Get());
	}

        code = callback->Eval(itemPtr, padEvent);
	
	if (itemPtr && handle.Get_object() == NULL) {
				// Object has been deleted, so don't fire any more events
	    break;
	}

				// Event handler just fired.
				// If debugging is on, then output debugging info
				// on event handler status
	if (win->debugEvent) {
	    if ((code == PAD_BREAK) || (code == PAD_ERROR)) {
		if (code == PAD_BREAK) {
		    debug_notice = "break";
		} else if (code == PAD_ERROR) {
		    debug_notice = "error";
		}
		if (!debug_event_list.Is_empty()) {
		    debug_notice += ": The following indented events were not fired";
		}
		Pad_Debug_output(win, debug_notice.Get());
		DOLIST(di, debug_event_list, Pad_String, debug_event) {
		    debug_notice = "   ";
		    debug_notice += debug_event;
		    Pad_Debug_output(win, debug_notice.Get());
		}
	    }
	}

				// Necessary because codes from the Tcl "return"
				// command aren't passed back unless this is evaluated
				// at the top-level, and it might not be because
				// the event could be generated from within 
				// a Tcl function (such as ".pad write")
	/*
	if (code == PAD_RETURN) {
	    code = ((Tcl_Interp *)interp)->returnCode;
	}
	*/
	if ((code != PAD_ERROR) && (padEvent->result)) {
	    //*padEvent->result += interp->result;
	    *padEvent->result += "\n";
	}
	if (code != PAD_OK) {
	    if ((code == PAD_CONTINUE) || (code == PAD_RETURN)) {
		/*
		 * Do nothing:  just go on to the next script.
		 */
	    } else if (code == PAD_BREAK) {
		break;
	    } else {
		Pad_Background_error("command bound to event");
		break;
	    }
	}
    }

    padEvent->resultCode = code;
}

/*
 *----------------------------------------------------------------------
 *
 * FindSequence --
 *
 *	Find the entry in a binding table that corresponds to a
 *	particular pattern string, and return a pointer to that
 *	entry.
 *
 * Results:
 *	The return value is normally a pointer to the PatSeq
 *	in patternTable that corresponds to eventString.  If an error
 *	was found while parsing eventString, or if "create" is 0 and
 *	no pattern sequence previously existed, then NULL is returned
 *	and interp->result contains a message describing the problem.
 *	If no pattern sequence previously existed for eventString, then
 *	a new one is created with a NULL command field.  In a successful
 *	return, *maskPtr is filled in with a mask of the event types
 *	on which the pattern sequence depends.
 *
 * Side effects:
 *	A new pattern sequence may be created.
 *
 *----------------------------------------------------------------------
 */

static PatSeq *
FindSequence(BindingTable *bindPtr, ClientData object, const char *eventString, 
	     int create, unsigned long *maskPtr)
{
    Pattern pats[EVENT_BUFFER_SIZE];
    int numPats;
    const char *p;
    Pattern *patPtr;
    PatSeq *psPtr, *found;
#define FIELD_SIZE 48
    char field[FIELD_SIZE];
    int flags, count, newc;
    size_t sequenceSize;
    unsigned long eventMask;
    PatternTableKey key;
    int user_modifier = 0;
    EventInfo *eiPtr;

    /*
     *-------------------------------------------------------------
     * Step 1: parse the pattern string to produce an array
     * of Patterns.  The array is generated backwards, so
     * that the lowest-indexed pattern corresponds to the last
     * event that must occur.
     *-------------------------------------------------------------
     */

    p = eventString;
    flags = 0;
    eventMask = 0;
    for (numPats = 0, patPtr = &pats[EVENT_BUFFER_SIZE-1];
	    numPats < EVENT_BUFFER_SIZE;
	    numPats++, patPtr--) {
	patPtr->eventType = -1;
	patPtr->needMods = 0;
	patPtr->detail = 0;
	patPtr->mode = 0;
	while (isspace(UCHAR(*p))) {
	    p++;
	}
	if (*p == '\0') {
	    break;
	}

	/*
	 * Handle simple ASCII characters.
	 */

	if (*p != '<') {
	    char string[2];

	    patPtr->eventType = KeyPress;
	    eventMask |= KeyPressMask;
	    string[0] = *p;
	    string[1] = 0;
	    patPtr->detail = XStringToKeysym(string);
	    if (patPtr->detail == NoSymbol) {
		if (isprint(UCHAR(*p))) {
		    patPtr->detail = *p;
		} else {
		    Pad_errorString.Printf("%s%s",
			    "bad ASCII character 0x%x", (unsigned char) *p);
		    return NULL;
		}
	    }
	    p++;
	    continue;
	}

	/*
	 * A fancier event description.  Must consist of
	 * 1. open angle bracket.
	 * 2. any number of modifiers, each followed by spaces
	 *    or dashes.
	 * 3. an optional event name.
	 * 4. an option button or keysym name.  Either this or
	 *    item 3 *must* be present;  if both are present
	 *    then they are separated by spaces or dashes.
	 * 5. a close angle bracket.
	 */

	count = 1;
	p++;
	while (1) {
	    ModInfo *modPtr;
	    p = GetField(p, field, FIELD_SIZE);
	    modPtr = (ModInfo *)modTable->Get((void *)field);
	    if (modPtr == NULL) {
		break;
	    }
	    if (modPtr->mask == USER_MASK) {
		if (user_modifier) {
		    Pad_errorString = "only one user-defined modifier is allowed";
		    return NULL;
		}
		user_modifier = 1;
		patPtr->mode = modPtr->flags;
	    }
	    patPtr->needMods |= modPtr->mask;
	    if (modPtr->flags & (DOUBLE|TRIPLE)) {
		flags |= PAT_NEARBY;
		if (modPtr->flags & DOUBLE) {
		    count = 2;
		} else {
		    count = 3;
		}
	    }
	    while ((*p == '-') || isspace(UCHAR(*p))) {
		p++;
	    }
	}
	if (eiPtr = (EventInfo *)eventTable->Get((void *)field)) {
	    patPtr->eventType = eiPtr->type;
	    eventMask |= eiPtr->eventMask;
	    while ((*p == '-') || isspace(UCHAR(*p))) {
		p++;
	    }
	    p = GetField(p, field, FIELD_SIZE);
	}
	if (*field != '\0') {
	    if ((*field >= '1') && (*field <= '5') && (field[1] == '\0')) {
		if (patPtr->eventType == -1) {
		    patPtr->eventType = ButtonPress;
		    eventMask |= ButtonPressMask;
		} else if ((patPtr->eventType == KeyPress)
			|| (patPtr->eventType == KeyRelease)) {
		    goto getKeysym;
		} else if ((patPtr->eventType != ButtonPress)
			&& (patPtr->eventType != ButtonRelease)) {
		    Pad_errorString.Printf("%s%s%s",
		        "specified button \"", field, "\" for non-button event");
		    return NULL;
		}
		patPtr->detail = (*field - '0');
	    } else {
		getKeysym:
		patPtr->detail = XStringToKeysym(field);
		if (patPtr->detail == NoSymbol) {
		    Pad_errorString.Printf("%s%s%s",
		        "bad event type or keysym \"", field, "\"");
		    return NULL;
		}
		if (patPtr->eventType == -1) {
		    patPtr->eventType = KeyPress;
		    eventMask |= KeyPressMask;
		} else if ((patPtr->eventType != KeyPress)
			&& (patPtr->eventType != KeyRelease)) {
		    Pad_errorString.Printf("%s%s%s",
		        "specified keysym \"", field, "\" for non-key event");
		    return NULL;
		}
	    }
	} else if (patPtr->eventType == -1) {
	    Pad_errorString = "no event type or button # or keysym";
	    return NULL;
	}
	while ((*p == '-') || isspace(UCHAR(*p))) {
	    p++;
	}
	if (*p != '>') {
	    Pad_errorString = "missing \">\" in binding";
	    return NULL;
	}
	p++;

	/*
	 * Replicate events for DOUBLE and TRIPLE.
	 */

	if ((count > 1) && (numPats < EVENT_BUFFER_SIZE-1)) {
	    patPtr[-1] = patPtr[0];
	    patPtr--;
	    numPats++;
	    if ((count == 3) && (numPats < EVENT_BUFFER_SIZE-1)) {
		patPtr[-1] = patPtr[0];
		patPtr--;
		numPats++;
	    }
	}
    }

    /*
     *-------------------------------------------------------------
     * Step 2: find the sequence in the binding table if it exists,
     * and add a new sequence to the table if it doesn't.
     *-------------------------------------------------------------
     */

    if (numPats == 0) {
	Pad_errorString = "no events specified in binding";
	return NULL;
    }
    patPtr = &pats[EVENT_BUFFER_SIZE-numPats];
    key.object = object;
    key.type = patPtr->eventType;
    key.detail = patPtr->detail;
    sequenceSize = numPats*sizeof(Pattern);
    if (found = (PatSeq *)bindPtr->patternTable->Get((void *)&key)) {
	for (psPtr = found; psPtr != NULL;
		psPtr = psPtr->nextSeqPtr) {
	    if ((numPats == psPtr->numPats)
		    && ((flags & PAT_NEARBY) == (psPtr->flags & PAT_NEARBY))
		    && (memcmp((char *) patPtr, (char *) psPtr->pats,
		    sequenceSize) == 0)) {
		goto done;
	    }
	}
    }
    if (!create) {
	Pad_errorString.Printf("%s%s%s",
	    "no binding exists for \"", eventString, "\"");
	return NULL;
    }
    psPtr = (PatSeq *) malloc((unsigned) (sizeof(PatSeq)
	    + (numPats-1)*sizeof(Pattern)));
    psPtr->numPats = numPats;
    psPtr->command = NULL;
    psPtr->flags = flags;
    psPtr->nextSeqPtr = found;
    bindPtr->patternTable->Set((void *)&key, (void *)psPtr);

    /*
     * Link the pattern into the list associated with the object.
     */

    psPtr->object = object;
    if (found = (PatSeq *)bindPtr->objectTable->Get((void *)object)) {
	psPtr->nextObjPtr = found;
    } else {
	psPtr->nextObjPtr = NULL;
    }
    bindPtr->objectTable->Set((void *)object, (void *)psPtr);

    memcpy((void *) psPtr->pats, (void *) patPtr, sequenceSize);
    memcpy((void *) &psPtr->key, (void *) &key, sizeof(PatternTableKey));

    done:
    *maskPtr = eventMask;
    return psPtr;
}

/*
 *----------------------------------------------------------------------
 *
 * GetField --
 *
 *	Used to parse pattern descriptions.  Copies up to
 *	size characters from p to copy, stopping at end of
 *	string, space, "-", ">", or whenever size is
 *	exceeded.
 *
 * Results:
 *	The return value is a pointer to the character just
 *	after the last one copied (usually "-" or space or
 *	">", but could be anything if size was exceeded).
 *	Also places NULL-terminated string (up to size
 *	character, including NULL), at copy.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static const char *
GetField(const char *p, char *copy, int size)
{
    while ((*p != '\0') && !isspace(UCHAR(*p)) && (*p != '>')
	    && (*p != '-') && (size > 1)) {
	*copy = *p;
	p++;
	copy++;
	size--;
    }
    *copy = '\0';
    return p;
}

/*
 *----------------------------------------------------------------------
 *
 * GetKeySym --
 *
 *	Given an X KeyPress or KeyRelease event, map the
 *	keycode in the event into a KeySym.
 *
 * Results:
 *	The return value is the KeySym corresponding to
 *	eventPtr, or NoSymbol if no matching Keysym could be
 *	found.
 *
 * Side effects:
 *	In the first call for a given display, keycode-to-
 *	KeySym maps get loaded.
 *
 *----------------------------------------------------------------------
 */

static KeySym
GetKeySym(Pad_Display *dispPtr, XEvent *eventPtr)
{
    KeySym sym;
    int index;

    /*
     * Refresh the mapping information if it's stale
     */

    if (dispPtr->bindInfoStale) {
	InitKeymapInfo(dispPtr);
    }

    /*
     * Figure out which of the four slots in the keymap vector to
     * use for this key.  Refer to Xlib documentation for more info
     * on how this computation works.
     */

    index = 0;
    if (Get_state(eventPtr) & dispPtr->modeModMask) {
	index = 2;
    }
    if ((Get_state(eventPtr) & ShiftMask)
	    || ((dispPtr->lockUsage != LU_IGNORE)
	    && (Get_state(eventPtr) & LockMask))) {
	index += 1;
    }
    sym = XKeycodeToKeysym(dispPtr->display, eventPtr->xkey.keycode, index);

    /*
     * Special handling:  if the key was shifted because of Lock, but
     * lock is only caps lock, not shift lock, and the shifted keysym
     * isn't upper-case alphabetic, then switch back to the unshifted
     * keysym.
     */

    if ((index & 1) && !(Get_state(eventPtr) & ShiftMask)
	    && (dispPtr->lockUsage == LU_CAPS)) {
	if (!(((sym >= XK_A) && (sym <= XK_Z))
		|| ((sym >= XK_Agrave) && (sym <= XK_Odiaeresis))
		|| ((sym >= XK_Ooblique) && (sym <= XK_Thorn)))) {
	    index &= ~1;
	    sym = XKeycodeToKeysym(dispPtr->display, eventPtr->xkey.keycode,
		    index);
	}
    }

    /*
     * Another bit of special handling:  if this is a shifted key and there
     * is no keysym defined, then use the keysym for the unshifted key.
     */

    if ((index & 1) && (sym == NoSymbol)) {
	sym = XKeycodeToKeysym(dispPtr->display, eventPtr->xkey.keycode,
		    index & ~1);
    }
    return sym;
}

/*
 *----------------------------------------------------------------------
 *
 * MatchPatterns --
 *
 *	Given a list of pattern sequences and a list of
 *	recent events, return a pattern sequence that matches
 *	the event list.
 *
 * Results:
 *	The return value is NULL if no pattern matches the
 *	recent events from bindPtr.  If one or more patterns
 *	matches, then the longest (or most specific) matching
 *	pattern is returned.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static PatSeq *
MatchPatterns(Pad_Display *dispPtr, BindingTable *bindPtr, PatSeq *psPtr, int mode)
{
    PatSeq *bestPtr = NULL;

    /*
     * Iterate over all the pattern sequences.
     */

    for ( ; psPtr != NULL; psPtr = psPtr->nextSeqPtr) {
	XEvent *eventPtr;
	Pattern *patPtr;
	Window window;
	int *detailPtr;
	int patCount, ringCount, flags, state;
	int modMask;

	/*
	 * Iterate over all the patterns in a sequence to be
	 * sure that they all match.
	 */

	eventPtr = &bindPtr->eventRing[bindPtr->curEvent];
	detailPtr = &bindPtr->detailRing[bindPtr->curEvent];
	window = eventPtr->xany.window;
	patPtr = psPtr->pats;
	patCount = psPtr->numPats;
	ringCount = EVENT_BUFFER_SIZE;
	while (patCount > 0) {
	    if (ringCount <= 0) {
		goto nextSequence;
	    }
	    if (eventPtr->xany.type != patPtr->eventType) {
		/*
		 * Most of the event types are considered superfluous
		 * in that they are ignored if they occur in the middle
		 * of a pattern sequence and have mismatching types.  The
		 * only ones that cannot be ignored are ButtonPress and
		 * ButtonRelease events (if the next event in the pattern
		 * is a KeyPress or KeyRelease) and KeyPress and KeyRelease
		 * events (if the next pattern event is a ButtonPress or
		 * ButtonRelease).  Here are some tricky cases to consider:
		 * 1. Double-Button or Double-Key events.
		 * 2. Double-ButtonRelease or Double-KeyRelease events.
		 * 3. The arrival of various events like Enter and Leave
		 *    and FocusIn and GraphicsExpose between two button
		 *    presses or key presses.
		 * 4. Modifier keys like Shift and Control shouldn't
		 *    generate conflicts with button events.
		 */

		if ((patPtr->eventType == KeyPress)
			|| (patPtr->eventType == KeyRelease)) {
		    if ((eventPtr->xany.type == ButtonPress)
			    || (eventPtr->xany.type == ButtonRelease)) {
			goto nextSequence;
		    }
		} else if ((patPtr->eventType == ButtonPress)
			|| (patPtr->eventType == ButtonRelease)) {
		    if ((eventPtr->xany.type == KeyPress)
			    || (eventPtr->xany.type == KeyRelease)) {
			int i;

			/*
			 * Ignore key events if they are modifier keys.
			 */

			for (i = 0; i < dispPtr->numModKeyCodes; i++) {
			    if (dispPtr->modKeyCodes[i]
				    == eventPtr->xkey.keycode) {
				/*
				 * This key is a modifier key, so ignore it.
				 */
				goto nextEvent;
			    }
			}
			goto nextSequence;
		    }
		}
		goto nextEvent;
	    }
	    if (eventPtr->xany.window != window) {
		goto nextSequence;
	    }

	    /*
	     * Note: it's important for the keysym check to go before
	     * the modifier check, so we can ignore unwanted modifier
	     * keys before choking on the modifier check.
	     */

	    if ((patPtr->detail != 0)
		    && (patPtr->detail != *detailPtr)) {
		/*
		 * The detail appears not to match.  However, if the event
		 * is a KeyPress for a modifier key then just ignore the
		 * event.  Otherwise event sequences like "aD" never match
		 * because the shift key goes down between the "a" and the
		 * "D".
		 */

		if (eventPtr->xany.type == KeyPress) {
		    int i;

		    for (i = 0; i < dispPtr->numModKeyCodes; i++) {
			if (dispPtr->modKeyCodes[i] == eventPtr->xkey.keycode) {
			    goto nextEvent;
			}
		    }
		}
		goto nextSequence;
	    }
	    flags = flagArray[eventPtr->type];
	    if (flags & KEY_BUTTON_MOTION) {
		state = Get_state(eventPtr);
	    } else if (flags & CROSSING) {
		state = eventPtr->xcrossing.state;
	    } else {
		state = 0;
	    }
	    if (patPtr->needMods != 0) {
		modMask = patPtr->needMods;
		if ((modMask & META_MASK) && (dispPtr->metaModMask != 0)) {
		    modMask = (modMask & ~META_MASK) | dispPtr->metaModMask;
		}
		if ((modMask & ALT_MASK) && (dispPtr->altModMask != 0)) {
		    modMask = (modMask & ~ALT_MASK) | dispPtr->altModMask;
		}
				// Check to make sure user-defined modifier matches
		if (modMask & USER_MASK) {
		    if (patPtr->mode != mode) {
			goto nextSequence;
		    }
		    modMask &= ~USER_MASK;
		}
		if ((state & modMask) != modMask) {
		    goto nextSequence;
		}
	    }
	    if (psPtr->flags & PAT_NEARBY) {
		XEvent *firstPtr;
		unsigned long timeDiff;

		firstPtr = &bindPtr->eventRing[bindPtr->curEvent];
		timeDiff = (Time) firstPtr->xkey.time - eventPtr->xkey.time;
		if ((firstPtr->xkey.x_root
			    < (eventPtr->xkey.x_root - NEARBY_PIXELS))
			|| (firstPtr->xkey.x_root
			    > (eventPtr->xkey.x_root + NEARBY_PIXELS))
			|| (firstPtr->xkey.y_root
			    < (eventPtr->xkey.y_root - NEARBY_PIXELS))
			|| (firstPtr->xkey.y_root
			    > (eventPtr->xkey.y_root + NEARBY_PIXELS))
			|| (timeDiff > NEARBY_MS)) {
		    goto nextSequence;
		}
	    }
	    patPtr++;
	    patCount--;
	    nextEvent:
	    if (eventPtr == bindPtr->eventRing) {
		eventPtr = &bindPtr->eventRing[EVENT_BUFFER_SIZE-1];
		detailPtr = &bindPtr->detailRing[EVENT_BUFFER_SIZE-1];
	    } else {
		eventPtr--;
		detailPtr--;
	    }
	    ringCount--;
	}

	/*
	 * This sequence matches.  If we've already got another match,
	 * pick whichever is most specific.  Detail is most important,
	 * then needMods.
	 */

	if (bestPtr != NULL) {
	    Pattern *patPtr2;
	    int i;

	    if (psPtr->numPats != bestPtr->numPats) {
		if (bestPtr->numPats > psPtr->numPats) {
		    goto nextSequence;
		} else {
		    goto newBest;
		}
	    }
	    for (i = 0, patPtr = psPtr->pats, patPtr2 = bestPtr->pats;
		    i < psPtr->numPats; i++, patPtr++, patPtr2++) {
		if (patPtr->detail != patPtr2->detail) {
		    if (patPtr->detail == 0) {
			goto nextSequence;
		    } else {
			goto newBest;
		    }
		}
		if (patPtr->needMods != patPtr2->needMods) {
		    if ((patPtr->needMods & patPtr2->needMods)
			    == patPtr->needMods) {
			goto nextSequence;
		    } else if ((patPtr->needMods & patPtr2->needMods)
			    == patPtr2->needMods) {
			goto newBest;
		    }
		}
	    }
	    goto nextSequence;	/* Tie goes to newest pattern. */
	}
	newBest:
	bestPtr = psPtr;

	nextSequence: continue;
    }
    return bestPtr;
}

/*
 *--------------------------------------------------------------
 *
 * InitKeymapInfo --
 *
 *	This procedure is invoked to scan keymap information
 *	to recompute stuff that's important for binding, such
 *	as the modifier key (if any) that corresponds to "mode
 *	switch".
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Keymap-related information in dispPtr is updated.
 *
 *--------------------------------------------------------------
 */

static void
InitKeymapInfo(Pad_Display *dispPtr)
{
    XModifierKeymap *modMapPtr;
    KeyCode *codePtr;
    KeySym keysym;
    int count, i, j, max, arraySize;
#define KEYCODE_ARRAY_SIZE 20

    dispPtr->bindInfoStale = 0;
    modMapPtr = XGetModifierMapping(dispPtr->display);

    /*
     * Check the keycodes associated with the Lock modifier.  If
     * any of them is associated with the XK_Shift_Lock modifier,
     * then Lock has to be interpreted as Shift Lock, not Caps Lock.
     */

    dispPtr->lockUsage = LU_IGNORE;
    codePtr = modMapPtr->modifiermap + modMapPtr->max_keypermod*LockMapIndex;
    for (count = modMapPtr->max_keypermod; count > 0; count--, codePtr++) {
	if (*codePtr == 0) {
	    continue;
	}
	keysym = XKeycodeToKeysym(dispPtr->display, *codePtr, 0);
	if (keysym == XK_Shift_Lock) {
	    dispPtr->lockUsage = LU_SHIFT;
	    break;
	}
	if (keysym == XK_Caps_Lock) {
	    dispPtr->lockUsage = LU_CAPS;
	    break;
	}
    }

    /*
     * Look through the keycodes associated with modifiers to see if
     * the the "mode switch", "meta", or "alt" keysyms are associated
     * with any modifiers.  If so, remember their modifier mask bits.
     */

    dispPtr->modeModMask = 0;
    dispPtr->metaModMask = 0;
    dispPtr->altModMask = 0;
    codePtr = modMapPtr->modifiermap;
    max = 8*modMapPtr->max_keypermod;
    for (i = 0; i < max; i++, codePtr++) {
	if (*codePtr == 0) {
	    continue;
	}
	keysym = XKeycodeToKeysym(dispPtr->display, *codePtr, 0);
	if (keysym == XK_Mode_switch) {
	    dispPtr->modeModMask |= ShiftMask << (i/modMapPtr->max_keypermod);
	}
	if ((keysym == XK_Meta_L) || (keysym == XK_Meta_R)) {
	    dispPtr->metaModMask |= ShiftMask << (i/modMapPtr->max_keypermod);
	}
	if ((keysym == XK_Alt_L) || (keysym == XK_Alt_R)) {
	    dispPtr->altModMask |= ShiftMask << (i/modMapPtr->max_keypermod);
	}
    }

    /*
     * Create an array of the keycodes for all modifier keys.
     */

    if (dispPtr->modKeyCodes != NULL) {
	delete [] dispPtr->modKeyCodes;
    }
    dispPtr->numModKeyCodes = 0;
    arraySize = KEYCODE_ARRAY_SIZE;
    dispPtr->modKeyCodes = new KeyCode[KEYCODE_ARRAY_SIZE];
    for (i = 0, codePtr = modMapPtr->modifiermap; i < max; i++, codePtr++) {
	if (*codePtr == 0) {
	    continue;
	}

	/*
	 * Make sure that the keycode isn't already in the array.
	 */

	for (j = 0; j < dispPtr->numModKeyCodes; j++) {
	    if (dispPtr->modKeyCodes[j] == *codePtr) {
		goto nextModCode;
	    }
	}
	if (dispPtr->numModKeyCodes >= arraySize) {
	    KeyCode *newk;

	    /*
	     * Ran out of space in the array;  grow it.
	     */

	    arraySize *= 2;
	    newk = new KeyCode[arraySize];
	    memcpy((void *) newk, (void *) dispPtr->modKeyCodes,
		    (dispPtr->numModKeyCodes * sizeof(KeyCode)));
	    delete [] dispPtr->modKeyCodes;
	    dispPtr->modKeyCodes = newk;
	}
	dispPtr->modKeyCodes[dispPtr->numModKeyCodes] = *codePtr;
	dispPtr->numModKeyCodes++;
	nextModCode: continue;
    }
    XFreeModifiermap(modMapPtr);
}
