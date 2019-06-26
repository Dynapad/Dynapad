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

#ifndef BIND_H
#define BIND_H 1

#include "defs.h"
#include "hashtab.h"
#include "events.h"
#include <X11/keysym.h>         // needed for KeySym declaration

/*
 * The structure below represents a binding table.  A binding table
 * represents a domain in which event bindings may occur.  It includes
 * a space of objects relative to which events occur (usually windows,
 * but not always), a history of recent events in the domain, and
 * a set of mappings that associate particular Tcl commands with sequences
 * of events in the domain.  Multiple binding tables may exist at once,
 * either because there are multiple applications open, or because there
 * are multiple domains within an application with separate event
 * bindings for each (for example, each canvas widget has a separate
 * binding table for associating events with the items in the canvas).
 *
 * Note: it is probably a bad idea to reduce EVENT_BUFFER_SIZE much
 * below 30.  To see this, consider a triple mouse button click while
 * the Shift key is down (and auto-repeating).  There may be as many
 * as 3 auto-repeat events after each mouse button press or release
 * (see the first large comment block within Tk_BindEvent for more on
 * this), for a total of 20 events to cover the three button presses
 * and two intervening releases.  If you reduce EVENT_BUFFER_SIZE too
 * much, shift multi-clicks will be lost.
 * 
 */

#define EVENT_BUFFER_SIZE 30
typedef struct BindingTable {
    XEvent eventRing[EVENT_BUFFER_SIZE];/* Circular queue of recent events
					 * (higher indices are for more recent
					 * events). */
    int detailRing[EVENT_BUFFER_SIZE];	/* "Detail" information (keySym or
					 * button or 0) for each entry in
					 * eventRing. */
    int curEvent;			/* Index in eventRing of most recent
					 * event.  Newer events have higher
					 * indices. */
    Pad_HashTable *patternTable;	/* Used to map from an event to a list
					 * of patterns that may match that
					 * event.  Keys are PatternTableKey
					 * structs, values are (PatSeq *). */
    Pad_HashTable *objectTable;		/* Used to map from an object to a list
					 * of patterns associated with that
					 * object.  Keys are ClientData,
					 * values are (PatSeq *). */
} BindingTable;

class Pad_Object;
class Pad_Callback;

//
// Declare function prototypes
//
void     Pad_AddBindingModifier(const char *modifier);
void     Pad_BindEvent(BindingTable *bindingTable, XEvent *eventPtr, Pad_Event *padEvent,
		       int numObjects, ClientData *objectPtr, Pad_Object *itemPtr);
BindingTable *Pad_CreateBindingTable();
unsigned long Pad_CreateBinding(BindingTable *bindingTable, ClientData object, 
				const char *eventString, Pad_Callback *callback);
void     Pad_DeleteAllBindings(BindingTable *bindingTable, ClientData object);
int      Pad_DeleteBinding(BindingTable *bindingTable, ClientData object, 
			   const char *eventString);
Pad_Bool Pad_DeleteBindingModifier(const char *modifier);
void     Pad_DeleteBindingTable(BindingTable *bindingTable);
Pad_Callback *   Pad_GetCallback(BindingTable *bindingTable, ClientData object, char *eventString);
void     Pad_GetAllBindings(BindingTable *bindingTable, ClientData object,
		Pad_List &list);
int      Pad_GetBindingModifier(const char *modifier);
const char *   Pad_GetBindingModifier(int mode);
const char *   Pad_GetEventName(int type);
void     Pad_Expand_event_macros(char input, Pad_Event *padEvent, KeySym keySym);

#endif
