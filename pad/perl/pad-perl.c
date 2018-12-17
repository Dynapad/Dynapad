/*
"(c) Copyright 1993-1995 Pad++ Consortium {University of New Mexico (UNM),
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


// The perl5 include files don't yet know about C++
// I seem to be getting conflicts with GNU iostream library
// So these #includes need to come after pad-string.h
extern "C" {
#define explicit expl  
#include "EXTERN.h"
#include "perl.h"
#undef explicit                 // These names are defined in perl and they
#undef Null                     // conflict with stuff elsewhere in Pad++
}

#undef bool
#include "../generic/api.h"
#include "../generic/bind.h"
#include "../generic/callback.h"
#include "../generic/defs.h"
#include "../generic/global.h"
#include "../generic/hashtab.h"
#include "../generic/pad-string.h"
#include "../generic/pad-tcl.h"
#include "../generic/tclInt.h"
#include "../generic/tkwin.h"
#include "pad-perl.h"

#include <string.h>
#include <iostream.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

////////////////////////////////////////
//
// Function pre-declarations
//
//
////////////////////////////////////////
static int Perl_Tcl_eval(ClientData clientData, Tcl_Interp *interp,
                         int argc, char **);
static int Perl_Tcl_load(ClientData clientData, Tcl_Interp *interp,
                         int argc, char **);
static int Perl_Eval(SV *cmd);
	  
static int PushCallbackArgs(SV **svp , Perl_CallbackType *object);
static int CallCallback (SV *callback, int flags);

////////////////////////////////////////
//
// Global variables
//
//
////////////////////////////////////////
static Pad_String command;	// Used to assemble lines of terminal input
				// into Perl commands.
static SV * evalSV = newSViv(0);// Used in perl_eval
static Pad_Language *Perl_lang;	// Needed for Bind()

Pad_HashTable *Perl_Table;
Tcl_Interp *Perl_tcl_interp;	// Used in creating new pad widgets and
				// packing pad widgets. Currently Tcl must
				// be used.

//////////////////////////////////////////////////////////////////////
//
// Functions accessible to the rest of Pad++
//
//////////////////////////////////////////////////////////////////////
/*
 *----------------------------------------------------------------------
 *
 * Pad_Perl_init
 *
 *   Called upon system startup to initialize the perl interpreter.
 *
 * Result:
 *   none.
 *
 * Side effects:
 *   Creates and inititalizes Perl interpreter. Creates global bindings
 *   to C++ constants that are accessible from within Perl. Alerts Perl
 *   to the C++ API through a call to xs_init().
 *
 *----------------------------------------------------------------------
 */
void
Pad_Perl_init(Pad_Language *lang, int, char **argv)
{
//     Pad_String path = getenv ("PADHOME");
//     if (path == "") {
//         cerr << "Error: PADHOME environment variable must be set\n" << endl;
//         exit (1);
//     }
//     path += "/perl/Pad.pm";

//     int fd = open (path.Get(), O_RDONLY);
//     if (fd == -1) {
//         cerr << "Error: couldn't open Pad.pm library at " << path.Get() << endl;
//         cerr << "Error: set PADHOME properly" << endl;
//         exit (1);
//     }
//     close (fd);
//     path = getenv ("PADHOME");
//     path += "/perl/tst.pl";
  
//     char *embedding[] = { "Pad++", "-we", "1"};

//     Perl_interp = perl_alloc();
//     perl_construct(Perl_interp);
  
//    perl_parse(Perl_interp, xs_init, 2, embedding, NULL);

        // Initialize the global values
    Perl_Table = new Pad_HashTable(PAD_STRING_TABLE);
    Perl_tcl_interp = lang->interp;
    Perl_lang = lang;

            // Let the user know that perl is available :-)
    Pad_Perl_eval (lang, "print \"Perl5 support enabled\n\";");
}

/*
 *----------------------------------------------------------------------
 *
 * Pad_Perl_create
 *
 *   Called every time a new pad widget is created.  It creates a new
 *   perl command associated with the pad widget, and registers the
 *   pad widget name in a hash table for future access.
 *
 * Result:
 *   none.
 *
 * Side effects:
 *   creates a new entry in the hash table for the new pad.
 *
 *----------------------------------------------------------------------
 */
void
Pad_Perl_create(Pad_Language *language, char *pathname, Pad_TkWin *win)
{
    PerlData *data = new PerlData();

    data->tkwin = win;
    data->interp = language->interp;
    Perl_Table->Set(pathname, data);
}

/*
 *----------------------------------------------------------------------
 *
 * Pad_Perl_complete
 *
 *   Gets called to determine if <cmd> is complete and is ready to be
 *   evaluated.  For perl, it always is FALSE until we see EOF, but we
 *   can't do that here, so we return TRUE irregardless. We could try
 *   and do something fancy here like checking to see if the line ends
 *   with a '\' but we'll save that for a rainy day...
 *
 * Result:
 *   Always TRUE.
 *
 * Side effects:
 *   none.
 *
 *----------------------------------------------------------------------
 */
int
Pad_Perl_complete(char *cmd)
{
    return(TRUE);
}

/*
 *----------------------------------------------------------------------
 *
 * Pad_Perl_callback
 *
 *   Is called by Pad_BindEvent when there is an event that has a
 *   Perl callback associated with it.
 *
 * Result:
 *   Currently it evals the argument as a perl code reference. Later
 *   something fancier should be done about checking the arguments for
 *   '%' macro substitution and the like. Returns TCL_OK or TCL_ERROR
 *   which are expected by Pad_BindEvent.
 *
 * Side effects:
 *   Whatever the callback does...
 *
 *----------------------------------------------------------------------
 */
int
Pad_Perl_callback (ClientData command)
{
    if (Perl_Eval((SV*)command)) {
            // TRUE and TCL_OK are NOT the same thing .... Grrrrrr.
        return TCL_OK;
    } else {
        return TCL_ERROR;
    }
}

int
Pad_Perl_callback_event (Pad_Object *,ClientData command,Pad_Event *padEvent)
{
            // Initialize the calling objects event
    Perl_CallbackType *call = (Perl_CallbackType*)command;
    SV *sv = call->command;
    int result;
    
    dSP;                      
    ENTER;
    SAVETMPS;
    PUTBACK; 
    result = PushCallbackArgs(&sv,call);
    if (result == TCL_OK){
        CallCallback(sv, G_DISCARD | G_EVAL);
        FREETMPS;        
    }
    LEAVE;           

    return TCL_OK;
}

static int
PushCallbackArgs(SV **svp , Perl_CallbackType *object)
{
    SV *sv = *svp;
    dSP;
    if (SvTYPE(SvRV(sv)) != SVt_PVCV)
            sv = SvRV(sv);
    PUSHMARK(sp);
    if (SvTYPE(sv) == SVt_PVAV) {
        AV *av = (AV *) sv;
        int n = av_len(av) + 1;
        SV **x = av_fetch(av, 0, 0);
        if (x) {
            int i;
            sv = *x;
                // Always put the object on the stack first
            if (!sv_isobject(sv) && object && object->obj) {
                XPUSHs(sv_mortalcopy(object->obj));
            }
            for (i = 1; i < n; i++) {
                x = av_fetch(av, i, 0);
                if (x) {
                    SV *arg = *x;
                    XPUSHs(sv_mortalcopy(arg));
                } else {
                    XPUSHs(&sv_undef);
                }
            }
        } else {
            croak ("No zero'th element in callback");
            sv = &sv_undef;
        }
    } else {
        if (object && object->obj) {
            XPUSHs(sv_mortalcopy(object->obj));
        }
    }
    *svp = sv; 
    PUTBACK;
    return TCL_OK;
}

static int
CallCallback (SV *sv, int flags)
{
    dSP;
    I32 myframe = TOPMARK;
    I32 count;
    ENTER;

    if (SvTYPE(sv) == SVt_PVCV) {
        count = perl_call_sv(sv, flags);
    } else if (SvROK(sv) && SvTYPE(SvRV(sv)) == SVt_PVCV) {
        count = perl_call_sv(SvRV(sv), flags);
    } else {
        SV **top = stack_base + myframe + 1;
        SV *obj = *top;
        if (SvPOK(sv) && SvROK(obj) && SvOBJECT(SvRV(obj))) {
            count = perl_call_method(SvPV(sv, na), flags);
        } else if (SvPOK(obj) && SvROK(sv) && SvOBJECT(SvRV(sv))) {
                /* We have obj method ... 
                       Used to be used instead of LangMethodCall()
                       */
            *top = sv;
            count = perl_call_method(SvPV(obj, na), flags);
        } else {
            croak ("Unknown callback");
        }
    }
    

        // This next bit is equivalent to "print $@ if $@;" in perl
            // i.e. it checks for an error and prints the error code 
    sv_setsv (evalSV, GvSV(errgv));
    if (SvTRUE(evalSV)) {   
        Pad_errorString = "Oops - ";
        Pad_errorString += (char*)SvPV(evalSV, na);
        warn (Pad_errorString.Get());
        return (FALSE);
    }

    LEAVE;
        // always return TCL_OK, otherwise Tk will pop up a useless dialogue
    return TCL_OK;
}

//         ENTER;
//         SAVETMPS;
//         PUSHMARK (sp);
//         XPUSHs ((SV*) command);
//         PUTBACK;
//         count = perl_call_pv ("Pad::Args_command", G_SCALAR|G_EVAL);
//         SPAGAIN;
//             // check the return value
//         if (count == 1) {
//                 // put the array of char's in a char* and pass it one
//                 // character at a time to Pad_Expand_event_macros
//             input = POPp;
//         }
//             // the char * referenced by input is local to this stack
//             // frame, so we have to do all the manipulation here, or
//             // make a copy of the char *
        
//             // Get an array of char's representing Event Macros
//             // that are in need of substitution
//         for (;*input != 0;input++) {
//             cout << "Input: " << *input << endl;
//             Tcl_DStringInit(&output);
//             Pad_Expand_event_macros(*input, &output, padEvent, 0);
//             cout << "Output: " << Tcl_DStringValue(&output) << endl;
//             Tcl_DStringFree(&output);
//         }
//             // Clean up the stack
//         PUTBACK;
//         FREETMPS;
//         LEAVE;
//     Perl_Eval((SV*)command);


/*
 *----------------------------------------------------------------------
 *
 * Pad_Perl_eval
 *
 *   This function casts <cmd> to a SV* and sends it off to Perl_Eval
 *
 * Result:
 *   Returns TCL_OK or TCL_ERROR.
 *
 * Side effects:
 *   Whatever the command does...
 *
 *----------------------------------------------------------------------
 */
int
Pad_Perl_eval (Pad_Language*, char *cmd)
{
    sv_setpv (evalSV, cmd);
    if (Perl_Eval (evalSV)) {
        return TCL_OK;
    } else {
        return TCL_ERROR;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * Pad_Perl_command
 *
 *   The Tcl interface to Perl
 *
 * Result:
 *   Returns TCL_OK or TCL_ERROR.
 *
 * Side effects:
 *   Whatever the command does...
 *
 *----------------------------------------------------------------------
 */
int
Pad_Perl_command(ClientData clientData, Tcl_Interp *interp, int argc, char **argv)
{
    int result;
    
    if (argc != 3) {
        TCL_RESULT(interp, "wrong # args: should be \"pathName perl command arg\"");
        return(TCL_ERROR);
    }

    if (!strncmp(argv[1], "eval", 1)) {
        result = Perl_Tcl_eval(clientData, interp, argc, argv);
    } else if (!strncmp(argv[1], "load", 1)) {
        result = Perl_Tcl_load(clientData, interp, argc, argv);
    } else  {
        TCL_RESULT(interp, "invalid command: should be \"eval or load\"");
        return(TCL_ERROR);
    }
    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * Pad_Perl_prompt
 *
 *   Generates the prompt when Perl is the Top level interpreter
 *
 * Result:
 *   None.
 *
 * Side effects:
 *   None.
 *
 *----------------------------------------------------------------------
 */
void
Pad_Perl_prompt(Pad_Language *, int partial, Pad_String &prompt)
{
    if (!partial) {
        prompt = " Perl> ";
    }
}


///////////////////////////////////////////////////////////////////////
//
// API helper functions
//
// The following functions are static and are only accessible from
//   within this file.
//
//////////////////////////////////////////////////////////////////////

/*
 *----------------------------------------------------------------------
 *
 * Perl_Eval
 *
 *   The heart of calling Perl from C++
 *
 * Result:
 *   This code could actually call a specific perl subroutine and fill
 *   in the argument stack using perl_call_*, but when I try that I
 *   get unreproducible results, so for the meanwhile, I'll just use
 *   perl_evalSV. The internal Perl error code is checked and an
 *   error is generated if necessary.
 *   TRUE or FALSE are returned.
 *
 * Side effects:
 *   Whatever the command does...
 *
 *----------------------------------------------------------------------
 */
static int
Perl_Eval(SV *cmdSV)
{
    perl_eval_sv(cmdSV, 0); // call perl to eval the SV*

      // This next bit is equivalent to "print $@ if $@;" in perl
      // i.e. it checks for an error and prints the error code 
    sv_setsv (evalSV, GvSV(errgv));
    if (SvTRUE(evalSV)) {   
        Pad_errorString = "Oops - ";
        Pad_errorString += (char*)SvPV(evalSV, na);
        warn (Pad_errorString.Get());
        return (FALSE);
    }
  
    return(TRUE);
}

//
// Implement pad "perl load" command
//
static int
Perl_Tcl_load(ClientData , Tcl_Interp *interp, int argc, char **argv)
{
    if (argc != 3) {
        TCL_RESULT(interp, "wrong # args: should be \"pathName perl load pathname\"");
        return(TCL_ERROR);
    }
  
    Pad_resultString = "do qq[";
    Pad_resultString += argv[2];
    Pad_resultString += "];";

    sv_setpv(evalSV,Pad_resultString.Get());
    if (Perl_Eval (evalSV)) {
        return TCL_OK;
    } else {
        return TCL_ERROR;
    }
}

//
// Implement pad "perl eval" command
//
static int
Perl_Tcl_eval(ClientData , Tcl_Interp *interp, int argc, char **argv)
{
    char *result = NULL;
    
    if (argc != 3) {
        TCL_RESULT(interp, "wrong # args: should be \"pathName perl eval perl_command\"");
        return(TCL_ERROR);
    }

    Pad_Perl_eval(Pad_topLevelLanguage, argv[2]);

    if (result) {
        Tcl_ResetResult(interp);
        Tcl_AppendResult(interp, result, NULL);
    }
  
    return(TCL_OK);
}


void
Pad_perl_foo (void) 
{
    cout << "Here I am, foo!" << endl;
}


