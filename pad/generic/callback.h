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

#ifndef CALLBACK_H
#define CALLBACK_H 1

#include "bind.h"
#include "defs.h"
#include "pad-string.h"

class Pad_TkWin;
class Pad_Language;
class Pad_Event;
class Pad_Object;
class Pad_Callback;

typedef void (Pad_InitProc)          (Pad_Language *language, int argc, char **argv);
typedef int  (Pad_CommandProc)       (ClientData clientData, int argc, char **argv);
typedef int  (Pad_CompleteProc)      (char *cmd);
typedef void (Pad_CreateProc)        (Pad_Language *language, char *pathname, Pad_Win *win);
typedef void (Pad_PromptProc)        (Pad_Language *language, int partial, Pad_String &prompt);
typedef int  (Pad_EvalProc)          (Pad_Language *language, char *cmd);
typedef int  (Pad_CallbackProc)      (ClientData clientData);
typedef int  (Pad_EventCallbackProc) (Pad_Object *, ClientData, Pad_Event *);

//
// A Pad_Callback contains a function in any of the available scripting languages.
// It stores the function, the language, and contains methods for modification,
// access, and evaluation.
//
class Pad_Callback
{
  private:
    Pad_Language          *language;	 // Language of callback
    Pad_CallbackProc      *func;	     // C++ function pointer
    Pad_EventCallbackProc *eventFunc;  // C++ function pointer
    ClientData             clientData; // Data for function callback
    int                    refCount;	 // Number of time this callback used within an event binding

  public:
    int      Eval(void);	        // Evaluates script
    int      Eval(Pad_Object *obj, Pad_Event *padEvent);   // Evaluates script, and passes obj and event pointers
    inline ClientData GetClientData(void) {return clientData;}
    void     Set(Pad_Callback *newCallBack);                         // Replace callback with new one
    void     Increment_refcount(void);
    void     Decrement_refcount(void);
    int      Get_refcount(void);

    Pad_Callback(Pad_CallbackProc *newFunc, ClientData clientData=NULL);
    Pad_Callback(Pad_EventCallbackProc *newEventFunc, ClientData clientData=NULL);
    Pad_Callback(Pad_Callback &callback);
    ~Pad_Callback();
};

#endif
