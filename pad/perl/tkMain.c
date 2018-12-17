/* 
 * Modified from tkMain.c --
 *
 *	This file contains a generic main program for Tk-based applications.
 *	It can be used as-is for many applications, just by supplying a
 *	different appInitProc procedure for each specific application.
 *	Or, it can be used as a template for creating new main programs
 *	for Tk applications.
 *
 * Copyright (c) 1990-1994 The Regents of the University of California.
 * Copyright (c) 1994-1995 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * Modified by BBB 3/96
 *   - Handle other top-level languages generically
 *   - C++'ized it
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

// Perl defines bool=char which conflicts with iostream.h
#undef bool
#include "../generic/defs.h"
#include "../generic/api.h"
#include "../generic/pad-string.h"
#include "../generic/callback.h"
#include "../generic/global.h"
#include "pad-perl.h"

#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <iostream.h>

/*
 * Declarations for various library procedures and variables (don't want
 * to include tkInt.h or tkPort.h here, because people might copy this
 * file out of the Tk source directory to make their own modified versions).
 * Note: don't declare "exit" here even though a declaration is really
 * needed, because it will conflict with a declaration elsewhere on
 * some systems.
 */

extern "C" int		isatty _ANSI_ARGS_((int fd));
extern "C" int		read _ANSI_ARGS_((int fd, char *buf, size_t size));

/*
 * Global variables used by the main program:
 */

static Tcl_DString command;	/* Used to assemble lines of terminal input
				 * into Tcl commands. */
static Tcl_DString line;	/* Used to read the next line from 
				 * the terminal input. */
static int tty;			/* Non-zero means standard input is a
				 * terminal-like device.  Zero means it's
				 * a file. */

/*
 * Command-line options:
 */

static char *language_name = NULL;

static Tk_ArgvInfo argTable[] = {
    {"-language", TK_ARGV_STRING, (char *) NULL, (char *) &language_name,
        "Language to use for top-level interpreter"},
    {"-sharedmemory", TK_ARGV_INT, (char *) NULL, (char *)&Pad_sharedMemory,
        "Try to use X shared memory"},
    {"-norgb", TK_ARGV_CONSTANT, (char *) 0, (char *)&Pad_rgb,
        "Don't store RGB information with images"},
    {(char *) NULL, TK_ARGV_END, (char *) NULL, (char *) NULL,
	(char *) NULL}
};

/*
 * Forward declarations for procedures defined later in this file:
 */

static void		StdinProc _ANSI_ARGS_((ClientData clientData, int mask));

/*
 *----------------------------------------------------------------------
 *
 * Pad_Main --
 *
 *	Main program for Pad++.  Starts up top-level Tcl interpreter
 *      by default.  But also handles other interpreters.
 *
 * Results:
 *	None. This procedure never returns (it exits the process when
 *	it's done.
 *
 * Side effects:
 *	This procedure initializes the Tk world and then starts
 *	interpreting commands;  almost anything could happen, depending
 *	on the script being interpreted.
 *
 *----------------------------------------------------------------------
 */

void
Pad_Main(int argc, char **argv, Tcl_AppInitProc *appInitProc)
{
    char *args;
    char *fileName;
    char buf[20];
    int code;
    size_t length;
    Pad_Bool rc;
    Tcl_Interp *interp;
    Tcl_Channel inChannel, outChannel, errChannel, chan;
    Pad_String prompt;
    Pad_Language *language;
    Pad_HashTableIterator hi;

    Tcl_FindExecutable(argv[0]);
    interp = Tcl_CreateInterp();
    inChannel  = Tcl_GetStdChannel(TCL_STDIN);
    outChannel = Tcl_GetStdChannel(TCL_STDOUT);
    errChannel = Tcl_GetStdChannel(TCL_STDERR);

    /*
     * Parse command-line arguments.  A leading "-file" argument is
     * ignored (a historical relic from the distant past).  If the
     * next argument doesn't start with a "-" then strip it off and
     * use it as the name of a script file to process.
     */

    fileName = NULL;
    if (argc > 1) {
	length = strlen(argv[1]);
	if ((length >= 2) && (strncmp(argv[1], "-file", length) == 0)) {
	    argc--;
	    argv++;
	}
    }
    if ((argc > 1) && (argv[1][0] != '-')) {
	fileName = argv[1];
	argc--;
	argv++;
    }

    /*
     * Make command-line arguments available in the Tcl variables "argc"
     * and "argv".
     */

    args = Tcl_Merge(argc-1, argv+1);
    Tcl_SetVar(interp, "argv", args, TCL_GLOBAL_ONLY);
    ckfree(args);
    sprintf(buf, "%d", argc-1);
    Tcl_SetVar(interp, "argc", buf, TCL_GLOBAL_ONLY);
    Tcl_SetVar(interp, "argv0", (fileName != NULL) ? fileName : argv[0],
	    TCL_GLOBAL_ONLY);

    /*
     * For now, under Windows, we assume we are not running as a console mode
     * app, so we need to use the GUI console.  In order to enable this, we
     * always claim to be running on a tty.  This probably isn't the right
     * way to do it.
     */

#ifdef __WIN32__
    tty = 1;
#else
    tty = isatty(0);
#endif

    /*
     * Set the "tcl_interactive" variable.
     */

    Tcl_SetVar(interp, "tcl_interactive",
	    ((fileName == NULL) && tty) ? "1" : "0", TCL_GLOBAL_ONLY);

    /*
     * Invoke application-specific initialization.
     */

    code = (*appInitProc)(interp);

				// Handle Pad++ specific command-line options
    if ((Tk_ParseArgv(interp, (Tk_Window) NULL, &argc, argv, argTable, 0) != TCL_OK) ||
	(code != TCL_OK)) {
	if (errChannel) {
            Tcl_Write(errChannel,
		    "application-specific initialization failed: ", -1);
            Tcl_Write(errChannel, interp->result, -1);
            Tcl_Write(errChannel, "\n", 1);
        }
	Tcl_Exit(1);
    }

				// Initialize perl language
    new Pad_Language(interp, "perl", "perl", Pad_Perl_init,
		     Pad_Perl_create, 
		     Pad_Perl_command, Pad_Perl_prompt, Pad_Perl_complete, 
		     Pad_Perl_eval);

				// Initialize languages
    DOTABLE(hi, Pad_languages, Pad_Language, language) {
	if (language->initProc) {
	    language->initProc(language, argc, argv);
	}
    }

				// Initialize top-level language
    if (language_name == NULL) {
	Pad_GetLanguage("perl", &Pad_topLevelLanguage);
    } else {
	rc = Pad_GetLanguage(language_name, &Pad_topLevelLanguage);
	if (!rc) {
	    if (errChannel) {
		Tcl_Write(errChannel, Pad_errorString.Get(), -1);
		Tcl_Write(errChannel, "\n", 1);
	    }
	    Tcl_Exit(1);
	}
    }

    /*
     * Invoke the script specified on the command line, if any.
     */

    if (fileName != NULL) {
	code = Tcl_EvalFile(interp, fileName);
	if (code != TCL_OK) {
	    /*
	     * The following statement guarantees that the errorInfo
	     * variable is set properly.
	     */
	    Tcl_AddErrorInfo(interp, "");
	    if (errChannel) {
		Tcl_Write(errChannel, Tcl_GetVar(interp, "errorInfo", TCL_GLOBAL_ONLY),
			  -1);
		Tcl_Write(errChannel, "\n", 1);
	    }
	    Tcl_DeleteInterp(interp);
	    Tcl_Exit(1);
	}
	tty = 0;
    } else {
	/*
	 * Commands will come from standard input, so set up an event
	 * handler for standard input.  Evaluate the .rc file, if one
	 * has been specified, set up an event handler for standard
	 * input, and print a prompt if the input device is a terminal.
	 */

	fileName = Tcl_GetVar(interp, "tcl_rcFileName", TCL_GLOBAL_ONLY);

	if (fileName != NULL) {
	    Tcl_DString buffer;
	    char *fullName;
    
	    fullName = Tcl_TranslateFileName(interp, fileName, &buffer);
	    if (fullName == NULL) {
		if (errChannel) {
                    Tcl_Write(errChannel, interp->result, -1);
                    Tcl_Write(errChannel, "\n", 1);
                }
	    } else {
// 		if (strncmp(fileName,"~",1)) {
// 		    fileName++;
// 		    Pad_resultString = getenv("HOME");
// 		    Pad_resultString << fileName;
// 		    strcpy(fileName,Pad_resultString.Get());
// cout << "File: " << fileName << endl;
// 		}
		Pad_resultString = "do \"";
		Pad_resultString << fullName << "\";";		
		Pad_Perl_eval(Pad_topLevelLanguage,Pad_resultString.Get());
            }
            
	    Tcl_DStringFree(&buffer);
	}
				// Set up a channel handler to process stdin
	if (inChannel) {
	    Tcl_CreateChannelHandler(inChannel, TCL_READABLE, StdinProc, NULL);
	}
				// Generate the first prompt
	if (tty) {
	    Pad_topLevelLanguage->promptProc(Pad_topLevelLanguage, 0, prompt);
	    if (outChannel) {
		Tcl_Write(outChannel, prompt.Get(), -1);
	    }
	}
    }

    if (outChannel) {
	Tcl_Flush(outChannel);
    }
    Tcl_DStringInit(&command);
    Tcl_DStringInit(&line);
    Tcl_ResetResult(interp);

    /*
     * Loop infinitely, waiting for commands to execute.  When there
     * are no windows left, Tk_MainLoop returns and we exit.
     */

    Tk_MainLoop();
    Tcl_DeleteInterp(interp);
    Tcl_Exit(0);
}

/*
 *----------------------------------------------------------------------
 *
 * StdinProc --
 *
 *	This procedure is invoked by the event dispatcher whenever
 *	standard input becomes readable.  It grabs the next line of
 *	input characters, adds them to a command being assembled, and
 *	executes the command if it's complete.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Could be almost arbitrary, depending on the command that's
 *	typed.
 *
 *----------------------------------------------------------------------
 */

static void
StdinProc(ClientData, int)
{
    static int gotPartial = 0;
    char *cmd;
    int code, count;
    Pad_String promptStr;
    Tcl_Channel inChannel, outChannel;

    inChannel = Tcl_GetStdChannel(TCL_STDIN);
    outChannel = Tcl_GetStdChannel(TCL_STDOUT);

    count = Tcl_Gets(inChannel, &line);

    if (count < 0) {
	if (!gotPartial) {
	    if (tty) {
		Tcl_Eval(Pad_topLevelLanguage->interp, "exit");
		exit(1);
	    } else {
		Tcl_DeleteChannelHandler(inChannel, StdinProc, (ClientData) inChannel);
	    }
	    return;
	} else {
	    count = 0;
	}
    }
    Tcl_DStringAppend(&command, Tcl_DStringValue(&line), -1);
    cmd = Tcl_DStringAppend(&command, "\n", -1);
    Tcl_DStringFree(&line);

    if (!Pad_topLevelLanguage->completeProc(cmd)) {
	gotPartial = 1;
    } else {
	gotPartial = 0;

	/*
	 * Disable the stdin file handler while evaluating the command;
	 * otherwise if the command re-enters the event loop we might
	 * process commands from stdin before the current command is
	 * finished.  Among other things, this will trash the text of the
	 * command being evaluated.
	 */

	Tcl_CreateChannelHandler(inChannel, 0, StdinProc, NULL);
	code = Pad_topLevelLanguage->evalProc(Pad_topLevelLanguage, cmd);
	Tcl_CreateChannelHandler(inChannel, TCL_READABLE, StdinProc, NULL);
	Tcl_DStringFree(&command);
	if (*Pad_topLevelLanguage->interp->result != 0) {
	    if ((code != TCL_OK) || (tty)) {
		Tcl_Write(outChannel, Pad_topLevelLanguage->interp->result, -1);
		Tcl_Write(outChannel, "\n", 1);
		Tcl_Flush(outChannel);
	    }
	}
    }

    /*
     * Output a prompt.
     */
    if (tty) {
	Pad_topLevelLanguage->promptProc(Pad_topLevelLanguage, gotPartial, promptStr);
	Tcl_Write(outChannel, promptStr.Get(), -1);
	Tcl_Flush(outChannel);
    }
    Tcl_ResetResult(Pad_topLevelLanguage->interp);
}


