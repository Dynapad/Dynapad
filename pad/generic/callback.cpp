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

#include "callback.h"
#include "global.h"
#include "events.h"
#include "misc.h"
#include "hashtab.h"

#include <stdlib.h>

////////////////////////////////////////////////////////////////////////////
//
// Pad_Callback definitions
//
////////////////////////////////////////////////////////////////////////////

Pad_Callback::Pad_Callback(Pad_CallbackProc *newFunc, ClientData newClientData)
{
    func = newFunc;
    eventFunc = NULL;
    clientData = newClientData;
    language = NULL;
    refCount = 0;
}

Pad_Callback::Pad_Callback(Pad_EventCallbackProc((*newEventFunc)), ClientData newClientData)
{
    func = NULL;
    eventFunc = newEventFunc;
    clientData = newClientData;
    language = NULL;
    refCount = 0;
}

Pad_Callback::Pad_Callback(Pad_Callback &callback)
{
    func = callback.func;
    eventFunc = callback.eventFunc;
    clientData = callback.clientData;
    language = callback.language;
    refCount = 0;
}

Pad_Callback::~Pad_Callback()
{
}

//
// Maintain reference count - which is the number of times
// a callback is used in an event binding
//
void
Pad_Callback::Increment_refcount(void)
{
    refCount++;
}

void
Pad_Callback::Decrement_refcount(void)
{
    refCount--;
}

int
Pad_Callback::Get_refcount(void)
{
    return(refCount);
}

int
Pad_Callback::Eval(void)
{
    int rc = PAD_OK;

    if (func) {
        rc = func(clientData);
    } else if (eventFunc) {
        rc = eventFunc((Pad_Object *)NULL, clientData, (Pad_Event *)NULL);
    }

    return(rc);
}

int
Pad_Callback::Eval(Pad_Object *obj, Pad_Event *padEvent)
{
    int rc = PAD_OK;

    if (func) {
        rc = func(clientData);
    } else if (eventFunc) {
        rc = eventFunc(obj, clientData, padEvent);
    }

    return(rc);
}

void     
Pad_Callback::Set(Pad_Callback *newCallback)
{
    func = newCallback->func;
    eventFunc = newCallback->eventFunc;
    clientData = newCallback->clientData;
    language = newCallback->language;
}
