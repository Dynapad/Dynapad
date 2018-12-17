
#include "kpl.h"

#ifndef PAD_WIN
#include "../../unix/config.h"
#endif

#ifdef PAD_WIN
#  include "../../win/windefs.h"
#endif

#ifdef TK4_1_H
#  include <tcl7.5.h>
#  include <tk4.1.h>
#else
#  include <tcl.h>
#  include <tk.h>
#endif

#include <assert.h>

#define STRLEN 100000

static float a[16], b[16], c[16];

static Tcl_Interp *interp;	/* the reigning Tcl interpreter */
static int rc;			/* last return value from Tcl_Eval */

static void wrap_tcl_use()
{
	int len;

	len = kpl_pop(a);  assert (len==1);
	interp = *(Tcl_Interp **)(&a[0]);
}

/*
  Access a global tcl variable.
  Top element on stack is variable name
  Returns variable contents
*/
static void wrap_tcl_get()
{
	int len;
	char name[STRLEN+1];
	char *r;

	len = kpl_pop(a);
	kpl_sprintvalue (name, a, len);

	r = Tcl_GetVar (interp, name, TCL_GLOBAL_ONLY);
	if (r) {
		len = kpl_sscanvalue (r, b);
		kpl_push (b, len);
	} else {
		b[0] = 0.0;
		kpl_push (b, 1);
	}
}

/*
  Access a global tcl array element.
  Top element on stack is array index
  Next element on stack is array name
  Returns array contents
*/
static void wrap_tcl_get2()
{
	int len;
	char name1[STRLEN+1], name2[STRLEN+1];
	char *r;

				/* Get array index */
	len = kpl_pop(a);
	kpl_sprintvalue (name2, a, len);
				/* Get array name */
	len = kpl_pop(a);
	kpl_sprintvalue (name1, a, len);

	r = Tcl_GetVar2 (interp, name1, name2, TCL_GLOBAL_ONLY);
	if (r) {
		len = kpl_sscanvalue (r, b);
		kpl_push (b, len);
	} else {
		b[0] = 0.0;
		kpl_push (b, 1);
	}
}

/*
  Set a global tcl variable.
  top element on stack is value
  next element is array name
*/
static void wrap_tcl_set()
{
	int len;
	char name[STRLEN+1], value[STRLEN+1];

	len = kpl_pop(b);
	kpl_sprintvalue (value, b, len);

	len = kpl_pop(a);
	kpl_sprintvalue (name, a, len);

	Tcl_SetVar (interp, name, value, TCL_GLOBAL_ONLY);
}

/*
  Set a global tcl array element.
  top element on stack is value
  next element is array element
  next element is array name
*/
static void wrap_tcl_set2()
{
	int len;
	char name1[STRLEN+1], name2[STRLEN+1], value[STRLEN+1];

	len = kpl_pop(b);
	kpl_sprintvalue (value, b, len);

	len = kpl_pop(a);
	kpl_sprintvalue (name2, a, len);

	len = kpl_pop(a);
	kpl_sprintvalue (name1, a, len);

	Tcl_SetVar2 (interp, name1, name2, value, TCL_GLOBAL_ONLY);
}

static void wrap_tcl_eval()
{
	int len;
	char script[STRLEN+1];

	len = kpl_pop(a);
	kpl_sprintvalue (script, a, len);

	rc = Tcl_Eval (interp, script);
	if (rc == TCL_ERROR) {
	    Tcl_AddErrorInfo(interp, "\n    (Pad KPL script)");
	    Tcl_BackgroundError(interp);
	}
	len = kpl_sscanvalue (interp->result, b);
	kpl_push (b, len);
}

void install_kpl_tcl(void)
{
	kpl_install("tcluse",		(Proc)wrap_tcl_use);
	kpl_install("tclget",		(Proc)wrap_tcl_get);
	kpl_install("tclget2",		(Proc)wrap_tcl_get2);
	kpl_install("tclset",		(Proc)wrap_tcl_set);
	kpl_install("tclset2",		(Proc)wrap_tcl_set2);
	kpl_install("tcleval",		(Proc)wrap_tcl_eval);
}

