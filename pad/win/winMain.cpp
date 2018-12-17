/* 
 * winMain.c --
 *
 *	Main entry point for wish and other Tk-based applications.
 *
 * Copyright (c) 1995 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * SCCS: @(#) winMain.c 1.28 96/07/23 16:58:12
 */

#include <tk.h>
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef WIN32_LEAN_AND_MEAN
#include <malloc.h>
#include <locale.h>

#include "../generic/defs.h"
#include "../generic/tkwin.h" 
#include "../generic/pad.h"
#include "../generic/global.h"
#include "../generic/pad-string.h"

/*
 * The following declarations refer to internal Tk routines.  These
 * interfaces are available for use, but are not supported.
 */

EXTERN void		TkConsoleCreate _ANSI_ARGS_((void));
EXTERN int		TkConsoleInit _ANSI_ARGS_((Tcl_Interp *interp));

/*
 * Forward declarations for procedures defined later in this file:
 */

static void Pad_Main(int argc, char **argv, Tcl_AppInitProc *appInitProc);
extern int Pad_Init(Tcl_Interp *interp);


/*
 *----------------------------------------------------------------------
 *
 * WinMain --
 *
 *	Main entry point from Windows.
 *
 * Results:
 *	Returns false if initialization fails, otherwise it never
 *	returns. 
 *
 * Side effects:
 *	Just about anything, since from here we call arbitrary Tcl code.
 *
 *----------------------------------------------------------------------
 */

int APIENTRY
WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpszCmdLine, int nCmdShow)
{
    char **argv, **argvlist, *p;
    int argc, size, i;
    char buffer[MAX_PATH];

    /*
     * Set up the default locale to be standard "C" locale so parsing
     * is performed correctly.
     */

    setlocale(LC_ALL, "C");


    /*
     * Increase the application queue size from default value of 8.
     * At the default value, cross application SendMessage of WM_KILLFOCUS
     * will fail because the handler will not be able to do a PostMessage!
     * This is only needed for Windows 3.x, since NT dynamically expands
     * the queue.
     */
    SetMessageQueue(64);

    /*
     * Create the console channels and install them as the standard
     * channels.  All I/O will be discarded until TkConsoleInit is
     * called to attach the console to a text widget.
     */

    TkConsoleCreate();

    /*
     * Precompute an overly pessimistic guess at the number of arguments
     * in the command line by counting non-space spans.  Note that we
     * have to allow room for the executable name and the trailing NULL
     * argument.
     */

    for (size = 3, p = lpszCmdLine; *p != '\0'; p++) {
	if (isspace(*p)) {
	    size++;
	    while (isspace(*p)) {
		p++;
	    }
	    if (*p == '\0') {
		break;
	    }
	}
    }
    argvlist = (char **) ckalloc((unsigned) (size * sizeof(char *)));
    argv = argvlist;

    /*
     * Parse the Windows command line string.  If an argument begins with a
     * double quote, then spaces are considered part of the argument until the
     * next double quote.  The argument terminates at the second quote.  Note
     * that this is different from the usual Unix semantics.
     */

    for (i = 1, p = lpszCmdLine; *p != '\0'; i++) {
	while (isspace(*p)) {
	    p++;
	}
	if (*p == '\0') {
	    break;
	}
	if (*p == '"') {
	    p++;
	    argv[i] = p;
	    while ((*p != '\0') && (*p != '"')) {
		p++;
	    }
	} else {
	    argv[i] = p;
	    while (*p != '\0' && !isspace(*p)) {
		p++;
	    }
	}
	if (*p != '\0') {
	    *p = '\0';
	    p++;
	}
    }
    argv[i] = NULL;
    argc = i;

    /*
     * Since Windows programs don't get passed the command name as the
     * first argument, we need to fetch it explicitly.
     */

    GetModuleFileName(NULL, buffer, sizeof(buffer));
    argv[0] = buffer;

    Pad_Main(argc, argv, Tcl_AppInit);
    return 1;
}



/*
 *----------------------------------------------------------------------
 *
 * Tcl_AppInit --
 *
 *	This procedure performs application-specific initialization.
 *	Most applications, especially those that incorporate additional
 *	packages, will have their own version of this procedure.
 *
 * Results:
 *	Returns a standard Tcl completion code, and leaves an error
 *	message in interp->result if an error occurs.
 *
 * Side effects:
 *	Depends on the startup script.
 *
 *----------------------------------------------------------------------
 */

int
Tcl_AppInit(Tcl_Interp *interp)
{
    /*
     * Call the init procedures for included packages.  Each call should
     * look like this:
     *
     * if (Mod_Init(interp) == TCL_ERROR) {
     *     return TCL_ERROR;
     * }
     *
     * where "Mod" is the name of the module.
     */

    if (Tcl_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
    if (Tk_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
	   /*
     * Initialize the console only if we are running as an interactive
     * application.
     */

    if (strcmp(Tcl_GetVar(interp, "tcl_interactive", TCL_GLOBAL_ONLY), "1")
	    == 0) {
	if (TkConsoleInit(interp) == TCL_ERROR) {
	return TCL_ERROR;
	}
    }
    Tcl_StaticPackage(interp, "Tk", Tk_Init, (Tcl_PackageInitProc *) NULL);

				// Initialize Pad package
    if (Pad_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
    Tcl_StaticPackage(interp, "Pad", Pad_Init, (Tcl_PackageInitProc *) NULL);
    if (Tcl_PkgProvide(interp, "Pad", Pad_version) != TCL_OK) {
	return TCL_ERROR;
    }

    /*
     * Specify a user-specific startup file to invoke if the application
     * is run interactively.  Typically the startup file is "~/.apPad_prc"
     * where "app" is the name of the application.  If this line is deleted
     * then no user-specific startup file will be run under any conditions.
     */

    Tcl_SetVar(interp, "tcl_rcFileName", "padinit", TCL_GLOBAL_ONLY);

    return TCL_OK;
}


#include <ctype.h>
#include <stdlib.h>
#include <string.h>

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

static Tcl_Interp *interp;      /* Interpreter for this application. */
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

static void
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

#ifdef HAS_SCHEME
				// Initialize scheme language
    new Pad_Language(interp, "scheme", "scm", Scheme_InitProc, Scheme_CreateProc, 
		     Scheme_CommandProc, Scheme_PromptProc, Scheme_CompleteProc, 
		     Scheme_EvalProc);
#endif

				// Initialize languages
    DOTABLE(hi, Pad_languages, Pad_Language, language) {
	if (language->initProc) {
	    language->initProc(language, argc, argv);
	}
    }

				// Initialize top-level language
    if (language_name == NULL) {
	Pad_GetLanguage("tcl", &Pad_topLevelLanguage);
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

                /*
                 * NOTE: The following relies on O_RDONLY==0.
                 */
                
                chan = Tcl_OpenFileChannel(interp, fullName, "r", 0);
                if (chan != (Tcl_Channel) NULL) {
                    Tcl_Close(NULL, chan);
                    if (Tcl_EvalFile(interp, fullName) != TCL_OK) {
			if (errChannel) {
                            Tcl_Write(errChannel, interp->result, -1);
                            Tcl_Write(errChannel, "\n", 1);
                        }
                    }
                }
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

#ifdef HAS_SOUND 
    Pad_Sound::Init();
#endif /* HAS_SOUND */
    


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
    Pad_String prompt_str;
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
	Pad_topLevelLanguage->promptProc(Pad_topLevelLanguage, gotPartial, prompt_str);
	Tcl_Write(outChannel, prompt_str.Get(), -1);
	Tcl_Flush(outChannel);
    }
    Tcl_ResetResult(Pad_topLevelLanguage->interp);
}
