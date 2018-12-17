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

#ifndef PADPERL_H
#define PADPERL_H 1

#include "../generic/defs.h"

class Pad_TkWin;
class Pad_Language;
class Pad_String;
class Pad_Object;
class Pad_Event;
class Pad_HashTable;

extern Pad_HashTable *Perl_Table;
extern Tcl_Interp *Perl_tcl_interp;// Used in creating new pad widgets and

struct PerlData {
    Tcl_Interp *interp;
    Pad_TkWin *tkwin;
};


extern "C" {
#define explicit expl  
#include "EXTERN.h"
#include "perl.h"
#undef explicit                 // These names are defined in perl and they
#undef Null                     // conflict with stuff elsewhere in Pad++
}

struct Perl_CallbackType {
    SV *command;
    SV *obj;
};


int  Pad_Perl_callback      (ClientData command);
int  Pad_Perl_callback_event(Pad_Object *, ClientData command, Pad_Event*);
int  Pad_Perl_command       (ClientData clientData, Tcl_Interp *interp, int argc, char **);
int  Pad_Perl_complete      (char *cmd);
void Pad_Perl_create        (Pad_Language *language, char *pathname, Pad_TkWin *win);
int  Pad_Perl_eval          (Pad_Language *language, char *cmd);
void Pad_Perl_init          (Pad_Language *language, int argc, char **argv);
void Pad_Perl_prompt        (Pad_Language *language, int partial, Pad_String &prompt);

void Pad_perl_foo(void);

#endif
