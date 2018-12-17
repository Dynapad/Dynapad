/*
 * Copyright 2001-2002 by GTCO-CalComp <calcomp_sales@gtcocalcomp.com>
 *                                                                            
 * derived from xf86Wacom.c by Frederic Lepied, whose original 
 * copyright message appears below : 
 * Copyright 1995-1999 by Frederic Lepied, France. <Lepied@XFree86.org>
 * 
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of GTCO-CalComp not be used in advertising
 * or publicity pertaining to distribution of the software without specific,
 * written prior permission. GTCO-CalComp makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * GTCO-CALCOMP DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
 * EVENT SHALL GTCO-CALCOMP BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 * TORTIOUS ACTIONS, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *
 */

/* xf86GtcoCC.c v1.01 2002/6/19 */

static const char identification[] = "$Identification: 1 $";

#include "xf86Version.h"

#if XF86_VERSION_CURRENT >= XF86_VERSION_NUMERIC(3,9,0,0,0)
#define XFREE86_V4 1
#endif

#ifdef LINUX_INPUT
#include <asm/types.h>
#include <linux/input.h>

/* 2.4.5 module support */
#ifndef EV_MSC
#define EV_MSC 0x04
#endif

#ifndef MSC_SERIAL
#define MSC_SERIAL 0x00
#endif

/* max number of input events to read in one read call */
#define MAX_EVENTS 50

/* keithp - a hack to avoid redefinitions of these in xf86str.h */
#ifdef BUS_PCI
#undef BUS_PCI
#endif
#ifdef BUS_ISA
#undef BUS_ISA
#endif
#endif

#ifdef XFREE86_V4
/* post 3.9 headers */

#ifndef XFree86LOADER
#include <unistd.h>
#include <errno.h>
#endif

#include "misc.h"
#include "xf86.h"
#define NEED_XF86_TYPES
#if !defined(DGUX)
#include "xf86_ansic.h"
#include "xisb.h"
#endif
#include "xf86_OSproc.h"
#include "xf86Xinput.h"
#include "exevents.h"		/* Needed for InitValuator/Proximity stuff */
#include "keysym.h"
#include "mipointer.h"

#ifdef XFree86LOADER
#include "xf86Module.h"
#endif

#define wait_for_fd(fd) xf86WaitForInput((fd), 1000000)
#define tcflush(fd, n) xf86FlushInput((fd))
#undef read
#define read(a,b,c) xf86ReadSerial((a),(b),(c))
#undef write
#define write(a,b,c) xf86WriteSerial((a),(char*)(b),(c))
#undef close
#define close(a) xf86CloseSerial((a))
#define XCONFIG_PROBED "(==)"
#define XCONFIG_GIVEN "(**)"
#define xf86Verbose 1
#undef PRIVATE
#define PRIVATE(x) XI_PRIVATE(x)

/* 
 * Be sure to set vmin appropriately for your device's protocol. You want to
 * read a full packet before returning
 */
static const char *default_gtco[] =
{
	"BaudRate", "9600",
	"StopBits", "1",
	"DataBits", "8",
	"Parity", "None",
	"Vmin", "1",
	"Vtime", "10",
	"FlowControl", "Xoff",
	NULL
};

static const char *default_mm_options[] =
{
	"BaudRate", "9600",
	"StopBits", "1",
	"DataBits", "8",
	"Parity", "Odd",
	"Vmin", "1",
	"Vtime", "10",
	"FlowControl", "Xoff",
	NULL
};

static const char *default_uiof_options[] =
{
	"BaudRate", "9600",
	"StopBits", "2",
	"DataBits", "7",
	"Parity", "Even",
	"Vmin", "1",
	"Vtime", "10",
	"FlowControl", "Xoff",
	NULL
};

static InputDriverPtr gtcDrv;

#else /* pre 3.9 headers */

#include "Xos.h"
#include <signal.h>
#include <stdio.h>

#define NEED_EVENTS
#include "X.h"
#include "Xproto.h"
#include "misc.h"
#include "inputstr.h"
#include "scrnintstr.h"
#include "XI.h"
#include "XIproto.h"
#include "keysym.h"

#if defined(sun) && !defined(i386)
#define POSIX_TTY
#include <errno.h>
#include <termio.h>
#include <fcntl.h>
#include <ctype.h>
#include <stdio.h>

#include "extio.h"
#else
#include "compiler.h"

#include "xf86.h"
#include "xf86Procs.h"
#include "xf86_OSlib.h"
#include "xf86_Config.h"
#include "xf86Xinput.h"
#include "atKeynames.h"
#include "xf86Version.h"
#endif

#if !defined(sun) || defined(i386)
#include "osdep.h"
#include "exevents.h"

#include "extnsionst.h"
#include "extinit.h"
#endif

#endif /* Pre 3.9 headers */

#if defined(__QNX__) || defined(__QNXNTO__)
#define POSIX_TTY
#endif

/******************************************************************************
 * debugging macro
 *****************************************************************************/
#ifdef DBG
#undef DBG
#endif
#ifdef DEBUG
#undef DEBUG
#endif

static int      debug_level = 0;
#define DEBUG 1
#if DEBUG
#define DBG(lvl, f) {if ((lvl) <= debug_level) f;}
#else
#define DBG(lvl, f)
#endif

#define ABS(x) ((x) > 0 ? (x) : -(x))

/******************************************************************************
 * GtcoDeviceRec flags
 *****************************************************************************/
#define DEVICE_ID(flags) ((flags) & 0x07)

#define CURSOR_ID		2   /* used with function calls - bg*/

#define EMULATE_MOUSE_FLAG      4   /* used with GtcoDevicePtr - bg*/
#define ABSOLUTE_FLAG		8
#define FIRST_TOUCH_FLAG	16
#define	KEEP_SHAPE_FLAG		32

#define GTCO_TYPE5              0
#define GTCO_CC                 1
#define SUMMA_MM                2
#define SUMMA_UIOF              3

/* one of these flags is set in common->gtcCCDevice */
#define CCDEVICE_NONE		0
#define CCDEVICE_STATION	1
#define CCDEVICE_CADPRO		2
#define CCDEVICE_OTHER		3

#define BAUD_19200_FLAG		64

/******************************************************************************
 * GtcoCommonRec flags
 *****************************************************************************/

typedef struct
{
    /* configuration fields */
    unsigned char	flags;		/* various flags (device type, absolute, first touch...) */
    int			topX;		/* X top */
    int			topY;		/* Y top */
    int			bottomX;	/* X bottom */
    int			bottomY;	/* Y bottom */
    double		factorX;	/* X factor */
    double		factorY;	/* Y factor */
    unsigned int	serial;	        /* device serial number */
    int			initNumber;     /* magic number for the init phasis */
    int			screen_no;	/* associated screen */
    
    struct _GtcoCommonRec *common;	/* common info pointer */
    
    /* state fields */
    int			oldX;		/* previous X position */
    int			oldY;		/* previous Y position */
    int			oldZ;		/* previous pressure */
    int			oldTiltX;	/* previous tilt in x direction */
    int			oldTiltY;	/* previous tilt in y direction */    
    int			oldButtons;	/* previous buttons state */
    int			oldProximity;	/* previous proximity */
} GtcoDeviceRec, *GtcoDevicePtr;

typedef struct _GtcoCommonRec 
{
    char		*gtcDevice;	/* device file name */
    int			gtcSuppress;	/* transmit position if increment is superior */
    unsigned char	gtcFlags;	/* various flags */
    int			gtcMaxX;	/* max X value */
    int			gtcMaxY;	/* max Y value */
    int			gtcMaxZ;	/* max Z value */
    int			gtcResolX;	/* X resolution in points/inch */
    int			gtcResolY;	/* Y resolution in points/inch */
    int			gtcResolZ;	/* Z resolution in points/inch */
    LocalDevicePtr	*gtcDevices;	/* array of all devices sharing the same port */
    int			gtcNumDevices;	/* number of devices */
    int			gtcIndex;	/* number of bytes read */
    int			gtcPktLength;	/* length of a packet */
    unsigned char	gtcData[10];	/* data read on the device */
    int			gtcThreshold;	/* Threshold for counting pressure as a button */
    int			gtcInitNumber;  /* magic number for the init phasis */
    unsigned int	gtcLinkSpeed;   /* serial link speed */
    Bool		(*gtcOpen)(LocalDevicePtr /*local*/); /* function used to open the line (serial or USB) */
} GtcoCommonRec, *GtcoCommonPtr;

/******************************************************************************
 * configuration stuff
 *****************************************************************************/
#define CURSOR_SECTION_NAME "gtcocursor"

#ifndef XFREE86_V4

#define PORT		1
#define DEVICENAME	2
#define THE_MODE	3
#define SUPPRESS	4
#define DEBUG_LEVEL     5
#define TILT_MODE	6
#define HISTORY_SIZE	7
#define ALWAYS_CORE	8
#define	KEEP_SHAPE	9
#define	TOP_X		10
#define	TOP_Y		11
#define	BOTTOM_X	12
#define	BOTTOM_Y	13
#define	SERIAL		14
#define	BAUD_RATE	15
#define	THRESHOLD	16
#define MAX_X		17
#define MAX_Y		18
#define MAX_Z		19
#define RESOLUTION_X	20
#define RESOLUTION_Y	21
#define RESOLUTION_Z	22
#define USB		23
#define SCREEN_NO	24
/* the following defines I added for parsing config options from XF86Config v3.x - bg*/
#define	EMULATE_MOUSE	25 
#define EMULATE_MOUSE_YES 26
#define DATA_FORMAT     27
#define GTCO_TYPE5_STR  28 /* GTCO_Type5 format */
#define GTCO_CC_STR     29 /* GTCO_CC format */
#define MM_STR          30 /* Summa_MM format - bg*/
#define UIOF_STR        31 /* Summa_UIOF format - bg*/
/* end - bg*/

#if !defined(sun) || defined(i386)
static SymTabRec GtcTab[] = {
  { ENDSUBSECTION,	"endsubsection" },
  { PORT,		"port" },
  { DEVICENAME,		"devicename" },
  { THE_MODE,		"mode" },
  { SUPPRESS,		"suppress" },
  { DEBUG_LEVEL,	"debuglevel" },
  { TILT_MODE,		"tiltmode" },
  { HISTORY_SIZE,	"historysize" },
  { ALWAYS_CORE,	"alwayscore" },
  { KEEP_SHAPE,		"keepshape" },
  { TOP_X,		"topx" },
  { TOP_Y,		"topy" },
  { BOTTOM_X,		"bottomx" },
  { BOTTOM_Y,		"bottomy" },
  { SERIAL,		"serial" },
  { BAUD_RATE,		"baudrate" },
  { THRESHOLD,		"threshold" },
  { MAX_X,		"maxx" },
  { MAX_Y,		"maxy" },
  { MAX_Z,		"maxz" },
  { RESOLUTION_X,	"resolutionx" },
  { RESOLUTION_Y,	"resolutiony" },
  { RESOLUTION_Z,	"resolutionz" },
  { USB,		"usb" },
  { SCREEN_NO,		"screenno" },
  { EMULATE_MOUSE,	"emulatemouse" },
  { EMULATE_MOUSE_YES,  "yes"},
  { DATA_FORMAT,        "dataformat"},
  { GTCO_TYPE5_STR,     "gtco_type5"},
  { GTCO_CC_STR,        "gtco_cc"},
  { MM_STR,             "summa_mm"},
  { UIOF_STR,           "summa_uiof"},
  { -1,			"" }
};

#define RELATIVE	1
#define ABSOLUTE	2

static SymTabRec ModeTabRec[] = {
  { RELATIVE,	"relative" },
  { ABSOLUTE,	"absolute" },
  { -1,		"" }
};

#endif

#endif /* Pre 3.9 headers */

/******************************************************************************
 * constant and macros declarations
 *****************************************************************************/
#define DEFAULT_MAXZ 240	/* default value of MaxZ when nothing is configured */
#define BUFFER_SIZE 256		/* size of reception buffer */
#define XI_CURSOR "CURSOR"	/* X device name for the cursor */
#define MAX_VALUE 100           /* number of positions */
#define MAX_COORD_RES 1270.0	/* Resolution of the returned MaxX and MaxY */
#define INVALID_THRESHOLD 30000 /* Invalid threshold value used to test if the user
				 * configured it or not */

#define SYSCALL(call) while(((call) == -1) && (errno == EINTR))

/* Commands sent out to put Summa tablets into stream mode - bg*/
#define MM_STREAM_MODE    "@"
#define UIOF_STREAM_MODE  "\x01BM0" /* <ESC>M0 */

/* Command to put Calcomp tablet into 10 byte mode (buttons, x, y, z, xt, yt) */
#define GTCO_CC_MODE    "\033%VV2\r%^20\r"
/* Command to put Calcomp tablet into 6 byte mode (buttons, x, y) */
#define GTCO_TYPE5_MODE    "\033%^23\r"

#define COMMAND_SET_MASK	0xc0
#define BAUD_RATE_MASK		0x0a
#define PARITY_MASK		0x30
#define DATA_LENGTH_MASK	0x40
#define STOP_BIT_MASK		0x80

#define HEADER_BIT	0x80
#define ZAXIS_SIGN_BIT	0x40
#define ZAXIS_BIT    	0x04
#define ZAXIS_BITS    	0x3f
#define POINTER_BIT     0x20
#define PROXIMITY_BIT   0x40
#define BUTTON_FLAG	0x08
#define BUTTONS_BITS	0x78
#define TILT_SIGN_BIT	0x40
#define TILT_BITS	0x3f

#define OTHER_PROX	1

#define mils(res) (res * 100 / 2.54) /* resolution */

/******************************************************************************
 * Function/Macro keys variables
 *****************************************************************************/
static KeySym gtco_map[] = 
{
    NoSymbol,	/* 0x00 */
    NoSymbol,	/* 0x01 */
    NoSymbol,	/* 0x02 */
    NoSymbol,	/* 0x03 */
    NoSymbol,	/* 0x04 */
    NoSymbol,	/* 0x05 */
    NoSymbol,	/* 0x06 */
    NoSymbol,	/* 0x07 */
    XK_F1,	/* 0x08 */
    XK_F2,	/* 0x09 */
    XK_F3,	/* 0x0a */
    XK_F4,	/* 0x0b */
    XK_F5,	/* 0x0c */
    XK_F6,	/* 0x0d */
    XK_F7,	/* 0x0e */
    XK_F8,	/* 0x0f */
    XK_F8,	/* 0x10 */
    XK_F10,	/* 0x11 */
    XK_F11,	/* 0x12 */
    XK_F12,	/* 0x13 */
    XK_F13,	/* 0x14 */
    XK_F14,	/* 0x15 */
    XK_F15,	/* 0x16 */
    XK_F16,	/* 0x17 */
    XK_F17,	/* 0x18 */
    XK_F18,	/* 0x19 */
    XK_F19,	/* 0x1a */
    XK_F20,	/* 0x1b */
    XK_F21,	/* 0x1c */
    XK_F22,	/* 0x1d */
    XK_F23,	/* 0x1e */
    XK_F24,	/* 0x1f */
    XK_F25,	/* 0x20 */
    XK_F26,	/* 0x21 */
    XK_F27,	/* 0x22 */
    XK_F28,	/* 0x23 */
    XK_F29,	/* 0x24 */
    XK_F30,	/* 0x25 */
    XK_F31,	/* 0x26 */
    XK_F32	/* 0x27 */
};

/* minKeyCode = 8 because this is the min legal key code */
static KeySymsRec gtco_keysyms = {
  /* map	minKeyCode	maxKC	width */
  gtco_map,	8,		0x27,	1
};

/******************************************************************************
 * external declarations
 *****************************************************************************/
#ifndef XFREE86_V4

#if defined(sun) && !defined(i386)
#define ENQUEUE suneqEnqueue
#else
#define ENQUEUE xf86eqEnqueue

extern void xf86eqEnqueue(
#if NeedFunctionPrototypes
    xEventPtr /*e*/
#endif
);
#endif

extern void miPointerDeltaCursor(
#if NeedFunctionPrototypes
    int /*dx*/,
    int /*dy*/,
    unsigned long /*time*/
#endif
);

#endif /* pre 3.9 declarations */

#if NeedFunctionPrototypes
static LocalDevicePtr xf86GtcAllocateCursor(void);
#endif

#ifndef XFREE86_V4
/*
 ***************************************************************************
 *
 * xf86GtcConfig --
 *	Configure the device.
 *
 ***************************************************************************
 */
static Bool
xf86GtcConfig(LocalDevicePtr    *array,
              int               inx,
              int               max,
	      LexPtr            val)
{
    LocalDevicePtr      dev = array[inx];
    GtcoDevicePtr	priv = (GtcoDevicePtr)(dev->private);
    GtcoCommonPtr	common = priv->common;
    int			token;
    int			mtoken;
    
    DBG(1, ErrorF("xf86GtcConfig\n"));

    if (xf86GetToken(GtcTab) != PORT) {
	xf86ConfigError("PORT option must be the first option of a Gtco SubSection");
    }
    
    if (xf86GetToken(NULL) != STRING)
	xf86ConfigError("Option string expected");
    else {
	int     loop;
		
	/* try to find another gtco device which share the same port */
	for(loop=0; loop<max; loop++) {
	    if (loop == inx)
		continue;
	    if ((array[loop]->device_config == xf86GtcConfig) &&
		(strcmp(((GtcoDevicePtr)array[loop]->private)->common->gtcDevice, val->str) == 0)) {
		DBG(2, ErrorF("xf86GtcConfig gtco port share between"
			      " %s and %s\n",
			      dev->name, array[loop]->name));
		xfree(common->gtcDevices);
		xfree(common);
		common = priv->common = ((GtcoDevicePtr) array[loop]->private)->common;
		common->gtcNumDevices++;
		common->gtcDevices = (LocalDevicePtr *) xrealloc(common->gtcDevices,
								 sizeof(LocalDevicePtr) * common->gtcNumDevices);
		common->gtcDevices[common->gtcNumDevices - 1] = dev;
		break;
	    }
	}
	if (loop == max) {
	    common->gtcDevice = strdup(val->str);
	    if (xf86Verbose)
		ErrorF("%s Gtco port is %s\n", XCONFIG_GIVEN,
		       common->gtcDevice);
	}
    }

    while ((token = xf86GetToken(GtcTab)) != ENDSUBSECTION) {
	switch(token) {
	case DEVICENAME:
	    if (xf86GetToken(NULL) != STRING)
		xf86ConfigError("Option string expected");
	    dev->name = strdup(val->str);
	    if (xf86Verbose)
		ErrorF("%s Gtco X device name is %s\n", XCONFIG_GIVEN,
		       dev->name);
	    break;	    
	    
	case THE_MODE:
	    mtoken = xf86GetToken(ModeTabRec);
	    if ((mtoken == EOF) || (mtoken == STRING) || (mtoken == NUMBER)) 
		xf86ConfigError("Mode type token expected");
	    else {
		switch (mtoken) {
		case ABSOLUTE:
		    priv->flags = priv->flags | ABSOLUTE_FLAG;
		    break;
		case RELATIVE:
		    priv->flags = priv->flags & ~ABSOLUTE_FLAG; 
		    break;
		default:
		    xf86ConfigError("Illegal Mode type");
		    break;
		}
	    }
	    break;
	    
	case SUPPRESS:
	    if (xf86GetToken(NULL) != NUMBER)
		xf86ConfigError("Option number expected");
	    common->gtcSuppress = val->num;
	    if (xf86Verbose)
		ErrorF("%s Gtco suppress value is %d\n", XCONFIG_GIVEN,
		       common->gtcSuppress);      
	    break;
	    
	case DEBUG_LEVEL:
	    if (xf86GetToken(NULL) != NUMBER)
		xf86ConfigError("Option number expected");
	    debug_level = val->num;
	    if (xf86Verbose) {
#if DEBUG
		ErrorF("%s Gtco debug level sets to %d\n", XCONFIG_GIVEN,
		       debug_level);      
#else
		ErrorF("%s Gtco debug level not sets to %d because"
		       " debugging is not compiled\n", XCONFIG_GIVEN,
		       debug_level);      
#endif
	    }
	    break;
	    
	case HISTORY_SIZE:
	    if (xf86GetToken(NULL) != NUMBER)
		xf86ConfigError("Option number expected");
	    dev->history_size = val->num;
	    if (xf86Verbose)
		ErrorF("%s Gtco Motion history size is %d\n", XCONFIG_GIVEN,
		       dev->history_size);      
	    break;

	case ALWAYS_CORE:
	    xf86AlwaysCore(dev, TRUE);
	    if (xf86Verbose)
		ErrorF("%s Gtco device always stays core pointer\n",
		       XCONFIG_GIVEN);
	    break;

	case KEEP_SHAPE:
	    priv->flags |= KEEP_SHAPE_FLAG;
	    if (xf86Verbose)
		ErrorF("%s Gtco keeps shape\n",
		       XCONFIG_GIVEN);
	    break;

	case TOP_X:
	    if (xf86GetToken(NULL) != NUMBER)
		xf86ConfigError("Option number expected");
	    priv->topX = val->num;
	    if (xf86Verbose)
		ErrorF("%s Gtco top x = %d\n", XCONFIG_GIVEN, priv->topX);
	    break;

	case TOP_Y:
	    if (xf86GetToken(NULL) != NUMBER)
		xf86ConfigError("Option number expected");
	    priv->topY = val->num;
	    if (xf86Verbose)
		ErrorF("%s Gtco top y = %d\n", XCONFIG_GIVEN, priv->topY);
	    break;

	case BOTTOM_X:
	    if (xf86GetToken(NULL) != NUMBER)
		xf86ConfigError("Option number expected");
	    priv->bottomX = val->num;
	    if (xf86Verbose)
		ErrorF("%s Gtco bottom x = %d\n", XCONFIG_GIVEN, priv->bottomX);
	    break;

	case BOTTOM_Y:
	    if (xf86GetToken(NULL) != NUMBER)
		xf86ConfigError("Option number expected");
	    priv->bottomY = val->num;
	    if (xf86Verbose)
		ErrorF("%s Gtco bottom y = %d\n", XCONFIG_GIVEN, priv->bottomY);
	    break;

	case SERIAL:
	    if (xf86GetToken(NULL) != NUMBER)
		xf86ConfigError("Option number expected");
	    priv->serial = val->num;
	    if (xf86Verbose)
		ErrorF("%s Gtco serial number = %u\n", XCONFIG_GIVEN,
		       priv->serial);
	    break;
	    
	case BAUD_RATE:
	    if (xf86GetToken(NULL) != NUMBER)
		xf86ConfigError("Option number expected");
	    switch(val->num) {
		case 19200:
		    common->gtcLinkSpeed = 19200;
		    break;
		case 9600:
		    common->gtcLinkSpeed = 9600;
		    break;
		default:
		    xf86ConfigError("Illegal speed value");
		    break;
	    }
	    if (xf86Verbose)
		ErrorF("%s Gtco baud rate of %u\n", XCONFIG_GIVEN,
		       val->num);
	    break;
	    
	case THRESHOLD:
	    if (xf86GetToken(NULL) != STRING)
		xf86ConfigError("Option string expected");

	    common->gtcThreshold = atoi(val->str);
	    
	    if (xf86Verbose)
		ErrorF("%s Gtco pressure threshold for button 1 = %d\n",
		       XCONFIG_GIVEN, common->gtcThreshold);
	    break;

	case MAX_X:
	    if (xf86GetToken(NULL) != NUMBER)
		xf86ConfigError("Option number expected");
	    common->gtcMaxX = val->num;
	    if (xf86Verbose)
		ErrorF("%s Gtco max x = %d\n", XCONFIG_GIVEN, common->gtcMaxX);
	    break;

	case MAX_Y:
	    if (xf86GetToken(NULL) != NUMBER)
		xf86ConfigError("Option number expected");
	    common->gtcMaxY = val->num;
	    if (xf86Verbose)
		ErrorF("%s Gtco max y = %d\n", XCONFIG_GIVEN, common->gtcMaxY);
	    break;

	case MAX_Z:
	    if (xf86GetToken(NULL) != NUMBER)
		xf86ConfigError("Option number expected");
	    common->gtcMaxZ = val->num;
	    if (xf86Verbose)
		ErrorF("%s Gtco max y = %d\n", XCONFIG_GIVEN, common->gtcMaxZ);
	    break;

	case RESOLUTION_X:
	    if (xf86GetToken(NULL) != NUMBER)
		xf86ConfigError("Option number expected");
	    common->gtcResolX = val->num;
	    if (xf86Verbose)
		ErrorF("%s Gtco resolution x = %d\n", XCONFIG_GIVEN, common->gtcResolX);
	    break;

	case RESOLUTION_Y:
	    if (xf86GetToken(NULL) != NUMBER)
		xf86ConfigError("Option number expected");
	    common->gtcResolY = val->num;
	    if (xf86Verbose)
		ErrorF("%s Gtco resolution y = %d\n", XCONFIG_GIVEN, common->gtcResolY);
	    break;

	case RESOLUTION_Z:
	    if (xf86GetToken(NULL) != NUMBER)
		xf86ConfigError("Option number expected");
	    common->gtcResolZ = val->num;
	    if (xf86Verbose)
		ErrorF("%s Gtco resolution y = %d\n", XCONFIG_GIVEN, common->gtcResolZ);
	    break;

	case USB:
	    ErrorF("USB not supported\n");
	    break;
	    
	case SCREEN_NO:
	    if (xf86GetToken(NULL) != NUMBER)
		xf86ConfigError("Option number expected");
	    priv->screen_no = val->num;
	    if (xf86Verbose)
		ErrorF("%s Gtco attached screen = %d\n", XCONFIG_GIVEN,
		       priv->screen_no);
	    break;

	case EMULATE_MOUSE:
	    mtoken = xf86GetToken(GtcTab);
	    if ((mtoken == EOF) || (mtoken == STRING) || (mtoken == NUMBER)){
		xf86ConfigError("Emulate Mouse option token expected");
	    } else if (mtoken == EMULATE_MOUSE_YES) {
	      priv->flags |= EMULATE_MOUSE_FLAG;
	    } else {
	      priv->flags &= ~EMULATE_MOUSE_FLAG;
	    }	    
	    if (xf86Verbose){
		ErrorF("%s Gtco Emulates Mouse Buttons\n", XCONFIG_GIVEN);
	    }
	    break;

	case DATA_FORMAT:
	    {
	    char format_given[16]; /* temporary variable for message - bg*/
	    mtoken = xf86GetToken(GtcTab);
	    
	    switch(mtoken){
	    case GTCO_TYPE5_STR:
		common->gtcFlags |= GTCO_TYPE5;
		strcpy(format_given, "Gtco_Type5");
		break;
	    case GTCO_CC_STR:
		common->gtcFlags |= GTCO_CC;
		strcpy(format_given, "Gtco_CC");
		break;
	    case MM_STR:
		common->gtcFlags |= SUMMA_MM;
		strcpy(format_given, "Summa_MM");
		break;
	    case UIOF_STR:
		common->gtcFlags |= SUMMA_UIOF;
		strcpy(format_given, "Summa_UIOF");
		break;
	    default:
		common->gtcFlags = 0;
		xf86ConfigError("DataFormat option required (GTCO_TYPE5, "
				"GTCO_CC, SUMMA_MM, or SUMMA_UIOF)");
		break;
	    }

	    if(xf86Verbose){
		ErrorF("%s GTCO: DataFormat %s\n", XCONFIG_GIVEN, format_given);
	    }
	    break;

	    }

	case EOF:
	    FatalError("Unexpected EOF (missing EndSubSection)");
	    break;
	    
	default:
	    xf86ConfigError("Gtco subsection keyword expected");
	    break;
	}
    }
    
    DBG(1, ErrorF("xf86GtcConfig name=%s\n", common->gtcDevice));
    
    return Success;
}
#endif /* Pre 3.9 stuff */

#ifndef XFREE86_V4
/*
 ***************************************************************************
 *
 * set_serial_speed --
 *
 *	Set speed of the serial port.
 *
 ***************************************************************************
 */
static int
set_serial_speed(int	fd,
		 int	speed_code)
{
    struct termios	termios_tty;
    int			err;
    
#ifdef POSIX_TTY
    SYSCALL(err = tcgetattr(fd, &termios_tty));

    if (err == -1) {
	ErrorF("Gtco tcgetattr error : %s\n", strerror(errno));
	return !Success;
    }
    termios_tty.c_iflag = IXOFF;
    termios_tty.c_oflag = 0;
    termios_tty.c_cflag = speed_code|CS8|CREAD|CLOCAL;
    termios_tty.c_lflag = 0;

    termios_tty.c_cc[VINTR] = 0;
    termios_tty.c_cc[VQUIT] = 0;
    termios_tty.c_cc[VERASE] = 0;
    termios_tty.c_cc[VEOF] = 0;
#ifdef VWERASE
    termios_tty.c_cc[VWERASE] = 0;
#endif
#ifdef VREPRINT
    termios_tty.c_cc[VREPRINT] = 0;
#endif
    termios_tty.c_cc[VKILL] = 0;
    termios_tty.c_cc[VEOF] = 0;
    termios_tty.c_cc[VEOL] = 0;
#ifdef VEOL2
    termios_tty.c_cc[VEOL2] = 0;
#endif
    termios_tty.c_cc[VSUSP] = 0;
#ifdef VDSUSP
    termios_tty.c_cc[VDSUSP] = 0;
#endif
#ifdef VDISCARD
    termios_tty.c_cc[VDISCARD] = 0;
#endif
#ifdef VLNEXT
    termios_tty.c_cc[VLNEXT] = 0; 
#endif
	
    /* minimum 1 character in one read call and timeout to 100 ms */
    termios_tty.c_cc[VMIN] = 1;
    termios_tty.c_cc[VTIME] = 10;

    SYSCALL(err = tcsetattr(fd, TCSANOW, &termios_tty));
    if (err == -1) {
	ErrorF("Gtco tcsetattr TCSANOW error : %s\n", strerror(errno));
	return !Success;
    }

#else
    Code for OSs without POSIX tty functions
#endif

    return Success;
}

/*
 ***************************************************************************
 *
 * wait_for_fd --
 *
 *	Wait one second that the file descriptor becomes readable.
 *
 ***************************************************************************
 */
static int
wait_for_fd(int	fd)
{
    int			err;
    fd_set		readfds;
    struct timeval	timeout;
    
    FD_ZERO(&readfds);
    FD_SET(fd, &readfds);
    
    timeout.tv_sec = 1;
    timeout.tv_usec = 0;
    SYSCALL(err = select(FD_SETSIZE, &readfds, NULL, NULL, &timeout));

    return err;
}

/*
 ***************************************************************************
 *
 * flush_input_fd --
 *
 *	Flush all input pending on the file descriptor.
 *
 ***************************************************************************
 */
static int
flush_input_fd(int	fd)
{
    int			err;
    fd_set		readfds;
    struct timeval	timeout;
    char		dummy[1];
    
    FD_ZERO(&readfds);
    FD_SET(fd, &readfds);

    do {
	timeout.tv_sec = 0;
	timeout.tv_usec = 0;
	SYSCALL(err = select(FD_SETSIZE, &readfds, NULL, NULL, &timeout));

	if (err > 0) {
	    SYSCALL(err = read(fd, &dummy, 1));
	    DBG(10, ErrorF("flush_input_fd: read %d bytes\n", err));
	}
    } while (err > 0);
    return err;
}
#endif /* Pre 3.9 stuff */

/*
 ***************************************************************************
 *
 * xf86GtcConvert --
 *	Convert valuators to X and Y.
 *
 ***************************************************************************
 */
static Bool
xf86GtcConvert(LocalDevicePtr	local,
	       int		first,
	       int		num,
	       int		v0,
	       int		v1,
	       int		v2,
	       int		v3,
	       int		v4,
	       int		v5,
	       int*		x,
	       int*		y)
{
    GtcoDevicePtr	priv = (GtcoDevicePtr) local->private;
    
    DBG(6, ErrorF("xf86GtcConvert\n"));

    if (first != 0 || num == 1)
      return FALSE;

#ifdef XFREE86_V4
    {
	ScreenPtr	pscr;
	
	if (priv->screen_no != -1) {
	    pscr = screenInfo.screens[priv->screen_no];
	} else {
	    pscr = miPointerCurrentScreen();
	}
    
	if (pscr == NULL)
	    return FALSE;
	
	priv->factorX = ((double) pscr->width)
	    / (priv->bottomX - priv->topX);
	priv->factorY = ((double) pscr->height)
	    / (priv->bottomY - priv->topY);
    }
#endif
    
    *x = v0 * priv->factorX + 0.5;
    *y = v1 * priv->factorY + 0.5;

    DBG(6, ErrorF("Gtco converted v0=%d v1=%d to x=%d y=%d\n",
		  v0, v1, *x, *y));
#ifdef XFREE86_V4
    if (priv->screen_no != -1) {
	xf86XInputSetScreen(local, priv->screen_no, *x, *y);
    }
#endif
    return TRUE;
}

/*
 ***************************************************************************
 *
 * xf86GtcReverseConvert --
 *	Convert X and Y to valuators.
 *
 ***************************************************************************
 */
static Bool
xf86GtcReverseConvert(LocalDevicePtr	local,
		      int		x,
		      int		y,
		      int		*valuators)
{
    GtcoDevicePtr	priv = (GtcoDevicePtr) local->private;

#ifdef XFREE86_V4
    priv->factorX = ((double) miPointerCurrentScreen()->width)
	/ (priv->bottomX - priv->topX);
    priv->factorY = ((double) miPointerCurrentScreen()->height)
	/ (priv->bottomY - priv->topY);
#endif
    
    valuators[0] = x / priv->factorX + 0.5;
    valuators[1] = y / priv->factorY + 0.5;

    DBG(6, ErrorF("Gtco converted x=%d y=%d to v0=%d v1=%d\n", x, y,
		  valuators[0], valuators[1]));

    return TRUE;
}
 
/*
 ***************************************************************************
 *
 * xf86GtcSendButtons --
 *	Send button events by comparing the current button mask with the
 *      previous one.
 *
 ***************************************************************************
 */
static void
xf86GtcSendButtons(LocalDevicePtr	local,
		   int                  buttons,
		   int                  rx,
		   int                  ry,
		   int                  rz,
		   int                  rtx,
		   int                  rty)
		   
{
    int             button;
    GtcoDevicePtr  priv = (GtcoDevicePtr) local->private;

    for (button=1; button<=16; button++) {
	int mask = 1 << (button-1);
	
	if ((mask & priv->oldButtons) != (mask & buttons)) {
	    DBG(4, ErrorF("xf86GtcSendButtons button=%d state=%d\n", 
			  button, (buttons & mask) != 0));
	    xf86PostButtonEvent(local->dev, 
				(priv->flags & ABSOLUTE_FLAG),
				button, (buttons & mask) != 0,
				0, 5, rx, ry, rz, rtx, rty);
	}
    }
}

/*
 ***************************************************************************
 *
 * xf86GtcSendEvents --
 *	Send events according to the device state.
 *
 ***************************************************************************
 */
static void
xf86GtcSendEvents(LocalDevicePtr	local,
		  int			is_button,
		  int			is_proximity,
		  int			x,
		  int			y,
		  int			z,
		  int			buttons,
		  int			tx,
		  int			ty)
{
    GtcoDevicePtr	priv = (GtcoDevicePtr) local->private;
    GtcoCommonPtr	common = priv->common;
    int			rx, ry, rz, rtx, rty;
    int			is_core_pointer, is_absolute;

    /* Translate coordinates according to Top and Bottom points
     * if we are outside the zone do as a ProximityOut event.
     */

    if (x > priv->bottomX) {
	is_proximity = FALSE;
	buttons = 0;
	x = priv->bottomX;
    }
	    
    if (y > priv->bottomY) {
	is_proximity = FALSE;
	buttons = 0;
	y = priv->bottomY;
    }

    DBG(10, ErrorF("topX=%d topY=%d\n", priv->topX, priv->topY));

    x = x - priv->topX;
    y = y - priv->topY;

    if (x < 0) {
	is_proximity = FALSE;
	buttons = 0;
	x = 0;
    }
    
    if (y < 0) {
	is_proximity = FALSE;
	buttons = 0;
	y = 0;
    }
    
    is_absolute = (priv->flags & ABSOLUTE_FLAG);
    is_core_pointer = xf86IsCorePointer(local->dev);

    DBG(6, ErrorF("%s prox=%s\tx=%d\ty=%d\tz=%d\tbutton=%s\tbuttons=%d\n",
		  is_absolute ? "abs" : "rel",
		  is_proximity ? "true" : "false",
		  x, y, z,
		  is_button ? "true" : "false", buttons));

    if (((is_proximity && priv->oldProximity) ||
	 ((is_proximity == 0) && (priv->oldProximity == 0))) &&
	(buttons == priv->oldButtons) &&
	(ABS(x - priv->oldX) <= common->gtcSuppress) &&
	(ABS(y - priv->oldY) <= common->gtcSuppress) &&
	 (ABS(z - priv->oldZ) < 3) &&
	(ABS(tx - priv->oldTiltX) < 3) &&
	(ABS(ty - priv->oldTiltY) < 3)) {
	
	DBG(10, ErrorF("tablet filtered\n"));

	return;
    }
	       
    /* sets rx and ry according to the mode */
    if (is_absolute) {
	rx = x;
	ry = y;
	rz = z;
	rtx = tx;
	rty = ty;
    } else {
	rx = x - priv->oldX;
	ry = y - priv->oldY;
	rz = z - priv->oldZ;  
	rtx = tx - priv->oldTiltX;
	rty = ty - priv->oldTiltY;
    }

    /* coordinates are ready we can send events */
    if (is_proximity) {

	if (!priv->oldProximity) {
	    xf86PostProximityEvent(local->dev, 1, 0, 5, rx, ry, z, tx, ty);

	    priv->flags |= FIRST_TOUCH_FLAG;
	    DBG(4, ErrorF("xf86GtccSendEvents FIRST_TOUCH_FLAG set\n"));

	    priv->oldProximity = OTHER_PROX;

	}


	DBG(4, ErrorF("xf86GtccSendEvents rx=%d ry=%d rz=%d buttons=%d\n", rx, ry, rz, buttons));
	

	if ((priv->oldX != x) ||
	    (priv->oldY != y) ||
	    (priv->oldZ != z) ||
	    (tx != priv->oldTiltX || ty != priv->oldTiltY)) {
	    if (!is_absolute && (priv->flags & FIRST_TOUCH_FLAG)) {
		priv->flags -= FIRST_TOUCH_FLAG;
		DBG(4, ErrorF("xf86GtccSendEvents FIRST_TOUCH_FLAG unset\n"));
	    } else {
		xf86PostMotionEvent(local->dev, is_absolute, 0, 5, rx, ry, rz,
				    rtx, rty); 
	    }
	}

	if (priv->oldButtons != buttons) {
	    xf86GtcSendButtons (local, buttons, rx, ry, rz, rtx, rty);
	}
	
	priv->oldButtons = buttons;
	priv->oldX = x;
	priv->oldY = y;
	priv->oldZ = z;
	priv->oldTiltX = tx;
	priv->oldTiltY = ty;
    }
    else { /* !PROXIMITY */
	/* reports button up when the device has been down and becomes out of proximity */
	if (priv->oldButtons) {
	    xf86GtcSendButtons (local, 0, rx, ry, rz, rtx, rty);
	    priv->oldButtons = 0;
	}
	if (!is_core_pointer) {
	    /* macro button management */
	    if (buttons) {

		int	macro = z / 2;

		DBG(6, ErrorF("macro=%d buttons=%d gtco_map[%d]=%x\n",
			      macro, buttons, macro, gtco_map[macro]));

		/* First available Keycode begins at 8 => macro+7 */
		xf86PostKeyEvent(local->dev, macro+7, 1,
				 is_absolute, 0, 5,
				 0, 0, buttons, rtx, rty);
		xf86PostKeyEvent(local->dev, macro+7, 0,
				 is_absolute, 0, 5,
				 0, 0, buttons, rtx, rty);
	    }
	    if (priv->oldProximity) {
		xf86PostProximityEvent(local->dev, 0, 0, 5, rx, ry, rz,
				       rtx, rty);
	    }
	}
	priv->oldProximity = 0;
    }
}

/*
 ***************************************************************************
 *
 * xf86ReadGtco_Type5 --
 *	ReadInput function specific to dataformat.
 *
 ***************************************************************************
 */
static void
xf86ReadGtco_Type5(LocalDevicePtr         local)
{
    GtcoDevicePtr	priv = (GtcoDevicePtr) local->private;
    GtcoCommonPtr	common = priv->common;
    int dbg_count; /* used for debugging messages only - bg*/
    int			len, loop, idx;
    int			is_16button_cursor, is_button, is_proximity;

    int			x, y, z, buttons = 0, tx = 0, ty = 0;
    unsigned char	buffer[BUFFER_SIZE];

    DBG(7, ErrorF("xf86ReadGtco_Type5 BEGIN device=%s fd=%d\n",
		  common->gtcDevice, local->fd));

    SYSCALL(len = read(local->fd, buffer, sizeof(buffer)));

    if (len <= 0) {
	ErrorF("Error reading Gtco device : %s\n", strerror(errno));
	return;
    } else {
	DBG(10, ErrorF("xf86ReadGtco_Type5 read %d bytes\n", len));
    }
   

    dbg_count = 0;  /* used for debugging messages only - bg*/
    for(loop=0; loop<len; loop++) {
      int bBufferProcessed = 1;
	/* Data Packet format
	Byte 1 
	bit 7       MSB always 1
	bits 6-2    C4-C0      Buttons
	bits 1-0    X15-X14    X Coords

	Byte 2 
	bit 7       Always 0
	bits 6-0    X13-X7

	Byte 3 
	bit 7       Always 0
	bits 6-0 =  X6-X0

	Byte 4
	bits 7-6    Always 0
       	bit 5       Proximity 0 = in prox, 1 = out of prox
	bits 4-3    X17-X16
	bits 2-0    Y16-Y14    Y Coords

	Byte 5
	bit 7       Always 0
	bits 6-0    Y13 - Y7

	Byte 6
	bit 7       Always 0
	bits 6-0    Y6 - Y0
	*/

	/* loop through buffer until we find first byte of a new packet - bg*/
	if ((common->gtcIndex == 0) && !(buffer[loop] & HEADER_BIT)) { /* magic bit is not OK */
	    DBG(6, ErrorF("xf86ReadGtco_Type5 bad magic number 0x%x (pktlength=%d) %d\n",
			buffer[loop], common->gtcPktLength, loop));
	    continue;
	} else { /* magic bit at wrong place */
	    if ((common->gtcIndex != 0) && (buffer[loop] & HEADER_BIT)) {
		DBG(6, ErrorF("xf86ReadGtco_Type5 magic number 0x%x detetected at index %d loop=%d\n",
				(unsigned int) buffer[loop], common->gtcIndex, loop));
		common->gtcIndex = 0;
		common->gtcData[common->gtcIndex++] = buffer[loop];
	    } else {
	      common->gtcData[common->gtcIndex++] = buffer[loop];
	    }
	}

	/* displays packets when in prox*/
	DBG(2, if((common->gtcIndex == 6) && !(common->gtcData[3] & 0x20)){
	  for(dbg_count = 0; dbg_count < common->gtcIndex; ++dbg_count){
	    ErrorF("Byte %d = %02X\n", dbg_count, common->gtcData[dbg_count]);
	  }
	  ErrorF("\n");
	});
	DBG(4, if(common->gtcData[0] & 0x7c){ /* displays button data - bg*/
	  ErrorF("Byte %d = %02X\n", 0, ((common->gtcData[0] & 0x7c)>>2));});

	if (common->gtcIndex == common->gtcPktLength) {
    
	    /* the packet is OK */

	    /* reset char count for next read */
	    common->gtcIndex = 0;

	    x = (((common->gtcData[3] & 0x18) << 13) + ((common->gtcData[0] & 0x3) << 14) + (common->gtcData[1] << 7) + common->gtcData[2]);
	    y = (((common->gtcData[3] & 0x07) << 14) + (common->gtcData[4] << 7) + common->gtcData[5]);

	    /* this device does not support z or tilt */
	    z = 0;
	    tx = 0;
	    ty = 0;

	    /* check which device we have pen/4 button cursor, 16 button cursor, or mouse - bg*/
	    is_button = (((common->gtcData[0] & 0x7c) != 0) ? 1 : 0);

	    /* mouse can, and 16 button cursor does, have leading bit set - bg*/
	    is_16button_cursor = ((common->gtcData[0] & 0x40) >> 6);
	    is_proximity = !((common->gtcData[3] & 0x20) >> 5);

	    /* set button values - bg*/
	    if(is_button){
	      if(is_16button_cursor){ 
		buttons = 1 << ((common->gtcData[0] & 0x3c) >> 2);
		DBG(2, ErrorF("GTCO: 16 button map in place - buttons = 0x%02X\n", buttons));
	      }else{ /* pen and 4 button cursor */
		  /* 4 button cursor */
		  buttons = ((common->gtcData[0] & 0x3c) >> 2);
		  DBG(2, ErrorF("GTCO: 4 button map in place - buttons = 0x%02X\n", buttons));
	      }
	    }
	    DBG(4, if(common->gtcData[0] & 0x7c){
	      ErrorF("Raw button : 0x%02X, is_button = %d, %d\n", (common->gtcData[0] & 0x7c)>>2, is_button, buttons);
	    });

	    for(idx=0; idx<common->gtcNumDevices; idx++) {
	        LocalDevicePtr  local_dev = common->gtcDevices[idx];
		int		temp_buttons = buttons;
		int		temp_is_proximity = is_proximity;
		
		DBG(7, ErrorF("xf86ReadGtco_Type5 trying to send to %s\n",
			      local_dev->name));

		DBG(3, ErrorF("temp_buttons = %d\n", temp_buttons));


		y = common->gtcMaxY - y; /* correct origin of tablet - bg*/


    	    	xf86GtcSendEvents(common->gtcDevices[idx],
				   is_button,
				   temp_is_proximity,
				   x, y, z, temp_buttons,
				   tx, ty);
	    }
	}

	/* since the packet was padded, save the current buffer value at the beginning of the next packet */
	if (!bBufferProcessed) {
	  common->gtcIndex = 0;
	  common->gtcData[common->gtcIndex++] = buffer[loop];
	}

    }/* end of buffer processing for loop - bg*/
    DBG(7, ErrorF("xf86ReadGtco_Type5 END   local=0x%x priv=0x%x index=%d\n",
		  local, priv, common->gtcIndex));
}/* end xf86ReadGtco_Type5 - bg*/

/*
 ***************************************************************************
 *
 * xf86ReadGtco_CC --
 *	ReadInput function specific to dataformat.
 *
 ***************************************************************************
 */
static void
xf86ReadGtco_CC(LocalDevicePtr         local)
{
    GtcoDevicePtr	priv = (GtcoDevicePtr) local->private;
    GtcoCommonPtr	common = priv->common;
    int dbg_count; /* used for debugging messages only - bg*/
    int			len, loop, idx;
    int			is_16button_cursor, is_mouse, is_button, is_proximity;

    int			x, y, z, buttons = 0, tx = 0, ty = 0;
    unsigned char	buffer[BUFFER_SIZE];

    DBG(7, ErrorF("xf86ReadGtco_CC BEGIN device=%s fd=%d\n",
		  common->gtcDevice, local->fd));

    SYSCALL(len = read(local->fd, buffer, sizeof(buffer)));

    if (len <= 0) {
	ErrorF("Error reading Gtco device : %s\n", strerror(errno));
	return;
    } else {
	DBG(10, ErrorF("xf86ReadGtco_CC read %d bytes\n", len));
    }
   

    dbg_count = 0;  /* used for debugging messages only - bg*/
    for(loop=0; loop<len; loop++) {
      int bBufferProcessed = 1;

	/* Data Packet format
	Byte 1 
	bit 7       MSB always 1
	bits 6-2    C4-C0      Buttons
	bits 1-0    X15-X14    X Coords

	Byte 2 
	bit 7       Always 0
	bits 6-0    X13-X7

	Byte 3 
	bit 7       Always 0
	bits 6-0 =  X6-X0

	Byte 4
	bits 7-6    Always 0
       	bit 5       Proximity 0 = in prox, 1 = out of prox
	bits 4-3    X17-X16
	bits 2-0    Y16-Y14    Y Coords

	Byte 5
	bit 7       Always 0
	bits 6-0    Y13 - Y7

	Byte 6
	bit 7       Always 0
	bits 6-0    Y6 - Y0

	Byte 7
	bit 7       Always 0
	bits 6-0    XT6-XT0    X-Tilt

	Byte 8
	bit 7       Always 0
	bits 6-0    YT6-YT0    Y-Tilt
       
	Byte 9
	bit 7       Always 0
	bits 6-0    P6-P0      Pressure

	Byte 10
	bit 7       Always 0
	bits 6-0    H6-H0      Height - bg*/

	/* loop through buffer until we find first byte of a new packet - bg*/
	if ((common->gtcIndex == 0) && !(buffer[loop] & HEADER_BIT)) { /* magic bit is not OK */
	  DBG(6, ErrorF("xf86ReadGtco_CC bad magic number 0x%x (pktlength=%d) %d\n",
			buffer[loop], common->gtcPktLength, loop));
	  continue;
	} else { /* magic bit at wrong place */
	  if ((common->gtcIndex != 0) && (buffer[loop] & HEADER_BIT)) {
	    if (common->gtcIndex == 6) { /* format #23 has a 6 byte packet, driver requires 10 bytes - bg*/
	      while (common->gtcIndex < common->gtcPktLength) {
		common->gtcData[common->gtcIndex++] = 0; /* pad the remaining 4 bytes - bg*/
	      }
	      bBufferProcessed = 0;
	    } else {
	      DBG(6, ErrorF("xf86ReadGtco_CC magic number 0x%x detetected at index %d loop=%d\n",
			    (unsigned int) buffer[loop], common->gtcIndex, loop));
	      common->gtcIndex = 0;
	      common->gtcData[common->gtcIndex++] = buffer[loop];
	    }
	  } else {
	    common->gtcData[common->gtcIndex++] = buffer[loop];
	  }
	}

	/* displays packets when in prox*/
	DBG(2, if((common->gtcIndex == 10) && !(common->gtcData[3] & 0x20)){
	  for(dbg_count = 0; dbg_count < common->gtcIndex; ++dbg_count){
	    ErrorF("Byte %d = %02X\n", dbg_count, common->gtcData[dbg_count]);
	  }
	  ErrorF("\n");
	});
	DBG(4, if(common->gtcData[0] & 0x7c){ /* displays button data - bg*/
	  ErrorF("Byte %d = %02X\n", 0, ((common->gtcData[0] & 0x7c)>>2));});


	if (common->gtcIndex == common->gtcPktLength) {
    
	    /* the packet is OK */

	    /* reset char count for next read */
	    common->gtcIndex = 0;

	    x = (((common->gtcData[3] & 0x18) << 13) + ((common->gtcData[0] & 0x3) << 14) + (common->gtcData[1] << 7) + common->gtcData[2]);
	    y = (((common->gtcData[3] & 0x07) << 14) + (common->gtcData[4] << 7) + common->gtcData[5]);

	    /* check which device we have pen/4 button cursor, 16 button cursor, or mouse - bg*/
	    is_button = (((common->gtcData[0] & 0x7c) != 0) ? 1 : 0);
	    is_mouse = (priv->flags & EMULATE_MOUSE_FLAG);

	    /* 16 button cursor has leading bit set */
	    is_16button_cursor = ((common->gtcData[0] & 0x40) >> 6) && !is_mouse ? 1 : 0;
	    is_proximity = !((common->gtcData[3] & 0x20) >> 5);

	    /* set button values - bg*/
	    if(is_button){
	      if(is_16button_cursor){ 
		buttons = 1 << ((common->gtcData[0] & 0x3c) >> 2);
		DBG(2, ErrorF("GTCO: 16 button map in place - buttons = 0x%02X\n", buttons));
	      }else{ /* pen and 4 button cursor or mouse */
		if(is_mouse){
		  /* remap buttons for EmulateMouse option to get mouse-like button behavior by swapping buttons 2 and 3
		     raw data		->  new map
		     button 1		->  button 1 (=left button)
		     button 2		->  button 3 (=right button)
		     button 3		->  button 4 (=scroll up)
		     button 4		->  button 5 (=scroll down)	
		     ***button 5		->  button 2 (=middle button) */	
		  int raw = ((common->gtcData[0] & 0x3c) >> 2); /* raw button data */

		  buttons  = 0;
		  buttons |= (raw & 0x1) ? 0x1 : 0;
		  buttons |= (raw & 0x2) ? 0x4 : 0;
		  buttons |= (raw & 0x4) ? 0x8 : 0;
		  buttons |= (raw & 0x8) ? 0x10 : 0;
		  /*buttons |= (raw & 0x10) ? 0x2 : 0;*/

		  if (raw & ~0xF)
		    DBG(1, ErrorF("GTCO: unhandled button bits = 0x%02X\n", raw & ~0xF));

		  DBG(2, ErrorF("GTCO: Mouse button map in place - buttons = 0x%02X\n", buttons));
		}else{ /* not mouse */
		  buttons = ((common->gtcData[0] & 0x3c) >> 2);
		  DBG(2, ErrorF("GTCO: 4 button map in place - buttons = 0x%02X\n", buttons));
		}
	      }
	    }
	    DBG(4, if(common->gtcData[0] & 0x7c){
	      ErrorF("Raw button : 0x%02X, is_button = %d, %d\n", (common->gtcData[0] & 0x7c)>>2, is_button, buttons);
	    });

	    z = (common->gtcData[8] & 0x7f) << 2;

	    /* if we have lsbs and we're not at max in 1st 7 bits.... */
	    if ((common->gtcData[3] & 0x40) &&
	       ((common->gtcData[9] & 0x70) == 0x00) &&
	       (z != 0x1FC)) {
		z |= common->gtcData[9] & 0x3;
	    }

	    /* set tilt values - bg*/
	    tx = common->gtcData[6] & 0x7f;
	    if (tx & 0x40)
		tx |= 0xFFFFFFC0;
	    ty = common->gtcData[7] & 0x7f;
	    if (ty & 0x40)
		ty |= 0xFFFFFFC0;

	    for(idx=0; idx<common->gtcNumDevices; idx++) {
	        LocalDevicePtr  local_dev = common->gtcDevices[idx];
		int		temp_buttons = buttons;
		int		temp_is_proximity = is_proximity;
		
		DBG(7, ErrorF("xf86ReadGtco_CC trying to send to %s\n",
			      local_dev->name));

	    DBG(4, ErrorF("Pressure = %d\n", z));
	    DBG(3, ErrorF("temp_buttons = %d\n", temp_buttons));


	    y = common->gtcMaxY - y; /* correct origin of tablet - bg*/


    		xf86GtcSendEvents(common->gtcDevices[idx],
				   is_button,
				   temp_is_proximity,
				   x, y, z, temp_buttons,
				   tx, ty);
	    }
	}

	/* since the packet was padded, save the current buffer value at the beginning of the next packet */
	if (!bBufferProcessed) {
	  common->gtcIndex = 0;
	  common->gtcData[common->gtcIndex++] = buffer[loop];
	}

    }/* end of buffer processing for loop - bg*/
    DBG(7, ErrorF("xf86ReadGtco_CC END   local=0x%x priv=0x%x index=%d\n",
		  local, priv, common->gtcIndex));
}/* end xf86ReadGtco_CC - bg*/


/*
 ***************************************************************************
 *
 * xf86ReadSumma_MM --
 *	ReadInput function specific to dataformat.
 *
 ***************************************************************************
 */
static void/* function #2, format #30 - bg*/
xf86ReadSumma_MM(LocalDevicePtr         local)
{
    GtcoDevicePtr	priv = (GtcoDevicePtr) local->private;
    GtcoCommonPtr	common = priv->common;
    int dbg_count; /* used for debugging messages only - bg*/
    int			len, loop, idx;
    int			is_button, is_proximity;
    int			x, y, z = 0, buttons = 0, tx = 0, ty = 0;
    unsigned char	buffer[BUFFER_SIZE];


    DBG(7, ErrorF("xf86ReadSumma_MM BEGIN device=%s fd=%d\n",
		  common->gtcDevice, local->fd));

    SYSCALL(len = read(local->fd, buffer, sizeof(buffer)));

    if (len <= 0) {
	ErrorF("Error reading Gtco device : %s\n", strerror(errno));
	return;
    } else {
	DBG(10, ErrorF("xf86ReadSumma_MM read %d bytes\n", len));
    }
   

    dbg_count = 0;  /* used for debugging messages only - bg*/
    for(loop=0; loop<len; loop++) {
      int bBufferProcessed = 1;

	/* Data Packet for Format #30
	Byte 1 
	bit 7       MSB always 1
	bit 6       Proximity    0 = in prox, 1 = out of prox
	bit 5       Reserved     (Tablet status)
	bit 4       X14          high/low 1=pos 0=neg, ignored - bg
	bit 3       Y14          high/low 1=pos 0=neg, ignored - bg
	bits 2-0    C2-C0        Buttons

	Byte 2 
	bit 7       Always 0
	bits 6-0    X6-X0

	Byte 3 
	bit 7       Always 0
	bits 6-0    X13-X7

	Byte 4
	bit 7       Always 0
	bits 6-0    Y6-Y0

	Byte 5
	bit 7       Always 0
	bits 6-0    Y13 - Y7

	*/

	/* loop through buffer until we find first byte of a new packet - bg*/
	if ((common->gtcIndex == 0) && !(buffer[loop] & HEADER_BIT)) { /* magic bit is not OK */
	  DBG(6, ErrorF("xf86ReadSumma_MM bad magic number 0x%x (pktlength=%d) %d\n",
			buffer[loop], common->gtcPktLength, loop));
	  continue;
	} else { /* magic bit at wrong place */
	  if ((common->gtcIndex != 0) && (buffer[loop] & HEADER_BIT)) {
	    if (common->gtcIndex == 5) { /* format #30 has a 5 byte packet, driver requires 10 bytes - bg*/
	      while (common->gtcIndex < common->gtcPktLength) {
		common->gtcData[common->gtcIndex++] = 0; /* pad the remaining bytes - bg*/
	      }
	      bBufferProcessed = 0;
	    } else {
	      DBG(6, ErrorF("xf86ReadSumma_MM magic number 0x%x detetected at index %d loop=%d\n",
			    (unsigned int) buffer[loop], common->gtcIndex, loop));
	      common->gtcIndex = 0;
	      common->gtcData[common->gtcIndex++] = buffer[loop];
	    }
	  } else {
	    common->gtcData[common->gtcIndex++] = buffer[loop];
	  }
	}

	/* displays packets when in prox*/
	DBG(2, if((common->gtcIndex == 10) && !(common->gtcData[3] & 0x20)){
	  for(dbg_count = 0; dbg_count < common->gtcIndex; ++dbg_count){
	    ErrorF("Byte %d = %02X\n", dbg_count, common->gtcData[dbg_count]);
	  }
	  ErrorF("\n");
	});
	DBG(2, if(common->gtcData[0] & 0x07){ /* displays button data - bg*/
	  ErrorF("Byte %d = %02X\n", 0, ((common->gtcData[0] & 0x07)));});


	if (common->gtcIndex == common->gtcPktLength) {
    
	    /* the packet is OK */

	    /* reset char count for next read */
	    common->gtcIndex = 0;

	    x = (common->gtcData[2] << 7) + common->gtcData[1];
	    y = (common->gtcData[4] << 7) + common->gtcData[3];
	    x *= 2; /* compensate for lpi 500 resolution which is the default for format 30 - bg*/
	    y *= 2;

	    is_button = (((common->gtcData[0] & 0x07) != 0) ? 1 : 0);
	    is_proximity = !((common->gtcData[0] & 0x40) >> 6);

	    /* set button values - bg*/
	    if(is_button){
	      buttons = 1 << ((common->gtcData[0] & 0x07) - 1);
	    }
	    DBG(4, if(common->gtcData[0] & 0x07){
	      ErrorF("Raw button : 0x%02X, is_button = %d, button# = %d\n", (common->gtcData[0] & 0x07), is_button, buttons);
	    });

	    for(idx=0; idx<common->gtcNumDevices; idx++) {
	        LocalDevicePtr  local_dev = common->gtcDevices[idx];
		int		temp_buttons = buttons;
		int		temp_is_proximity = is_proximity;
		
		DBG(7, ErrorF("xf86ReadSumma_MM trying to send to %s\n",
			      local_dev->name));

	    DBG(4, ErrorF("Pressure = %d\n", z));
	    DBG(3, ErrorF("temp_buttons = %d\n", temp_buttons));


	    y = common->gtcMaxY - y; /* correct origin of tablet - bg*/


    		xf86GtcSendEvents(common->gtcDevices[idx],
				   is_button,
				   temp_is_proximity,
				   x, y, z, temp_buttons,
				   tx, ty);
	    }
	}
	/* since the packet was padded, save the current buffer value at the beginning of the next packet */
	if (!bBufferProcessed) {
	  common->gtcIndex = 0;
	  common->gtcData[common->gtcIndex++] = buffer[loop];
	}

    }/* end of buffer processing for loop - bg*/
    DBG(7, ErrorF("xf86ReadSumma_MM END   local=0x%x priv=0x%x index=%d\n",
		  local, priv, common->gtcIndex));
}/* end xf86ReadSumma_MM - bg*/

/*
 ***************************************************************************
 *
 * xf86ReadSumma_UIOF --
 *	ReadInput function specific to dataformat.
 *
 ***************************************************************************
 */
static void /* function #3, format #31 - bg*/
xf86ReadSumma_UIOF(LocalDevicePtr         local)
{
    GtcoDevicePtr	priv = (GtcoDevicePtr) local->private;
    GtcoCommonPtr	common = priv->common;
    int dbg_count; /* used for debugging messages only - bg*/
    int			len, loop, idx;
    int			is_button, is_proximity;
    int			x, y, z = 0, buttons = 0, tx = 0, ty = 0;
    unsigned char	buffer[BUFFER_SIZE];

    DBG(7, ErrorF("xf86ReadSumma_UIOF BEGIN device=%s fd=%d\n",
		  common->gtcDevice, local->fd));

    SYSCALL(len = read(local->fd, buffer, sizeof(buffer)));

    if (len <= 0) {
	ErrorF("Error reading Gtco device : %s\n", strerror(errno));
	return;
    } else {
	DBG(10, ErrorF("xf86ReadSumma_UIOF read %d bytes\n", len));
    }
   

    dbg_count = 0;  /* used for debugging messages only - bg*/
    for(loop=0; loop<len; loop++) {
      int bBufferProcessed = 1;

	/* Data Packet for Format #31 - 7 data bits
	Byte 1 
	bit 6       MSB always 1
	bits 5-4    not used
	bits 3-1    T2-T0        Tablet status, set by commands
	bit 0       Proximity    0 = in proximity, 1 = out of proximity

	Byte 2 
	bit 6       Always 0
	bit 5       not used
	bits 4-0    C4-C0        Buttons

	Byte 3 
	bit 6       Always 0
	bits 5-0 =  X5-X0

	Byte 4
	bit 6       Always 0
       	bits 5-0    X11-X6

	Byte 5
	bit 6       Always 0
	bit 5       not used
	bits 4-0    X16-X12

	Byte 6
	bit 6       Always 0
	bits 5-0    Y5-Y0

	Byte 7
	bit 6       Always 0
	bits 5-0    Y11-Y6

	Byte 8
	bit 6       Always 0
	bit 5       not used
        bits 4-0    Y16-Y12 - bg*/

	/* loop through buffer until we find first byte of a new packet - bg*/
	if ((common->gtcIndex == 0) && !(buffer[loop] & 0x40)) { /* magic bit is not OK -  HEADER_BIT*/
	  DBG(1, ErrorF("xf86ReadSumma_UIOF bad magic number 0x%x (pktlength=%d) %d\n",
			buffer[loop], common->gtcPktLength, loop));
	  continue;
	} else { /* magic bit at wrong place */
	  if ((common->gtcIndex != 0) && (buffer[loop] & 0x40)) {/* HEADER_BIT*/
	    if (common->gtcIndex == 8) { /* format #31 has a 8 byte packet, driver requires 10 bytes - bg*/
	      while (common->gtcIndex < common->gtcPktLength) {
		common->gtcData[common->gtcIndex++] = 0; /* pad the remaining bytes - bg*/
	      }
	      bBufferProcessed = 0;
	    } else {
	      DBG(1, ErrorF("xf86ReadSumma_UIOF magic number 0x%x detetected at index %d loop=%d\n",
			    (unsigned int) buffer[loop], common->gtcIndex, loop));
	      common->gtcIndex = 0;
	      common->gtcData[common->gtcIndex++] = buffer[loop];
	    }
	  } else {
	    common->gtcData[common->gtcIndex++] = buffer[loop];
	  }
	}

	/* displays packets when in prox*/
	DBG(3, if((common->gtcIndex == 10) && !(common->gtcData[0] & 0x01)){
	  for(dbg_count = 0; dbg_count < common->gtcIndex; ++dbg_count){
	    ErrorF("Byte %d = %02X\n", dbg_count, common->gtcData[dbg_count]);
	  }
	  ErrorF("\n");
	});
	DBG(3, if(common->gtcData[1] & 0x1f){ /* displays button data - bg*/
	  ErrorF("Byte %d = %02X\n", 0, (common->gtcData[1] & 0x1f));});


	if (common->gtcIndex == common->gtcPktLength) {
    
	    /* the packet is OK */

	    /* reset char count for next read */
	    common->gtcIndex = 0;

	    x = ((common->gtcData[4] & 0x1f) << 12) + ((common->gtcData[3] & 0x3f) << 6) + (common->gtcData[2] & 0x3f);
	    y = ((common->gtcData[7] & 0x1f) << 12) + ((common->gtcData[6] & 0x3f) << 6) + (common->gtcData[5] & 0x3f);

	    is_button = (((common->gtcData[1] & 0x1f) != 0) ? 1 : 0);
	    is_proximity = !(common->gtcData[0] & 0x01);

	    if(is_button){
	      buttons = 1 << ((common->gtcData[1] & 0x1f) - 1);
	    }
	    DBG(3, if(common->gtcData[1] & 0x1f){
	      ErrorF("Raw button : 0x%02X, is_button = %d, button# = %d\n", (common->gtcData[1] & 0x1f), is_button, buttons);
	    });

	    for(idx=0; idx<common->gtcNumDevices; idx++) {
	        LocalDevicePtr  local_dev = common->gtcDevices[idx];
		int		temp_buttons = buttons;
		int		temp_is_proximity = is_proximity;
		
		DBG(7, ErrorF("xf86ReadSumma_UIOF trying to send to %s\n",
			      local_dev->name));

	    DBG(4, ErrorF("Pressure = %d\n", z));
	    DBG(3, ErrorF("temp_buttons = %d\n", temp_buttons));


	    y = common->gtcMaxY - y; /* correct origin of tablet - bg*/


    		xf86GtcSendEvents(common->gtcDevices[idx],
				   is_button,
				   temp_is_proximity,
				   x, y, z, temp_buttons,
				   tx, ty);
	    }
	}

	/* since the packet was padded, save the current buffer value at the beginning of the next packet */
	if (!bBufferProcessed) {
	  common->gtcIndex = 0;
	  common->gtcData[common->gtcIndex++] = buffer[loop];
	}

    }/* end of buffer processing for loop - bg*/
    DBG(7, ErrorF("xf86ReadSumma_UIOF END   local=0x%x priv=0x%x index=%d\n",
		  local, priv, common->gtcIndex));
}/* end xf86ReadSumma_UIOF - bg*/

/*
 ***************************************************************************
 *
 * xf86GtcReadInput --
 *	Read the new events from the device, and enqueue them.
 *
 ***************************************************************************
 */
/* begin new readinput func - bg*/
static void
xf86GtcReadInput(LocalDevicePtr         local){
  GtcoDevicePtr	priv = (GtcoDevicePtr) local->private;
  GtcoCommonPtr	common = priv->common;

  switch(common->gtcFlags){
  case GTCO_TYPE5:
    xf86ReadGtco_Type5(local);
    break;
  case GTCO_CC:
    xf86ReadGtco_CC(local);
    break;
  case SUMMA_MM:
    xf86ReadSumma_MM(local);
    break;
  case SUMMA_UIOF:
    xf86ReadSumma_UIOF(local);
    break;
  default:
    DBG(1, ErrorF("GTCO: Unusable data format flag. Flags = 0x%02X\n", 
		  common->gtcFlags));
    break;
  }
}

/*
 ***************************************************************************
 *
 * xf86GtcControlProc --
 *
 ***************************************************************************
 */
static void
xf86GtcControlProc(DeviceIntPtr	device,
		   PtrCtrl	*ctrl)
{
  DBG(2, ErrorF("xf86GtcControlProc\n"));
}

/*
 ***************************************************************************
 *
 * xf86GtcOpen --
 *
 ***************************************************************************
 */

static Bool
xf86GtcOpen(LocalDevicePtr	local)
{
    int baud_val = 0;
#ifndef XFREE86_V4
    struct timeval	timeout;
#endif
    GtcoDevicePtr	priv = (GtcoDevicePtr)local->private;
    GtcoCommonPtr	common = priv->common;

    DBG(1, ErrorF("opening %s\n", common->gtcDevice));
    

#ifdef XFREE86_V4
    local->fd = xf86OpenSerial(local->options);
#else
    SYSCALL(local->fd = open(common->gtcDevice, O_RDWR|O_NDELAY, 0));
#endif
    if (local->fd < 0) {
	ErrorF("Error opening %s : %s\n", common->gtcDevice, strerror(errno));
	return !Success;
    }

    DBG(1, ErrorF("initializing tablet\n"));    

    /*set serial speed according to XF86Config, 9600 is default - bg*/
    if (19200 == common->gtcLinkSpeed) {
      /* Set the speed of the serial link to 19200 */
#ifdef XFREE86_V4
      if ((baud_val = xf86SetSerialSpeed(local->fd, 19200)) < 0) {
	return !Success;
      }
#else
      if (set_serial_speed(local->fd, B19200) == !Success)
        return !Success;
#endif
    }else{
      /* Set the speed of the serial link to 9600 */
#ifdef XFREE86_V4
      if ((baud_val =xf86SetSerialSpeed(local->fd, 9600)) < 0) {
	return !Success;
      }
#else
      if (set_serial_speed(local->fd, B9600) == !Success)
        return !Success;
#endif
    }


#ifndef XFREE86_V4 /*bg*/
    /* default port config was set in set_serial_speed function, only make adjustments here - bg*/
    if((common->gtcFlags) == SUMMA_MM || (common->gtcFlags) == SUMMA_UIOF){
      int err;
      struct termios termios_tty;
      SYSCALL(err = tcgetattr(local->fd, &termios_tty));
      if(err == -1){
	ErrorF("Gtco: tcgetattr error : %s\n", strerror(errno));
	return !Success;
      }
      if((common->gtcFlags) == SUMMA_MM){
	termios_tty.c_cflag |= PARENB | PARODD; /* set parity to odd - bg*/
      }else if((common->gtcFlags) == SUMMA_UIOF){
	termios_tty.c_cflag |= PARENB | CS7 | CSTOPB; /* set parity to even, data bits to 7, and stop bits to 2 - bg*/
      }else{ /* shouldn't get here - bg*/
	ErrorF("GTCO: Bad data format mask\n");
	return !Success;
      }
      SYSCALL(err = tcsetattr(local->fd, TCSANOW, &termios_tty));
      if(err == -1){
	ErrorF("Gtco: tcgetattr error : %s\n", strerror(errno));
	return !Success;
      }
    }
#endif /*bg*/

    /* MM and UIOF formats require the tablet be set to stream mode - bg*/
    if((common->gtcFlags) == SUMMA_MM || (common->gtcFlags) == SUMMA_UIOF){ 
      int err = 0;
      char initstr[8];
      if((common->gtcFlags) == SUMMA_MM){
	strcpy(initstr, MM_STREAM_MODE);
      }else if((common->gtcFlags) == SUMMA_UIOF){
	strcpy(initstr, UIOF_STREAM_MODE);
      }else{
	ErrorF("GTCO: Bad dataformat\n");
      }
      SYSCALL(err = write(local->fd, initstr, strlen(initstr)));
      if (err == -1) {
	ErrorF("GTCO: SummaSketch write failed\n");
	return !Success;
      }
    }

    if((common->gtcFlags) == GTCO_TYPE5) {
      int err = 0;
      char initstr[8];
      strcpy(initstr, GTCO_TYPE5_MODE);
      SYSCALL(err = write(local->fd, initstr, strlen(initstr)));
      if (err == -1) {
	ErrorF("GTCO: TYPE5_MODE write failed\n");
	return !Success;
      }
    }

    if((common->gtcFlags) == GTCO_CC) {
      int err = 0;
      char initstr[8];
      strcpy(initstr, GTCO_CC_MODE);
      SYSCALL(err = write(local->fd, initstr, strlen(initstr)));
      if (err == -1) {
	ErrorF("GTCO: CC_MODE write failed\n");
	return !Success;
      }
    }

    /* set the packet length to the correct format */
    switch (common->gtcFlags) {
    case GTCO_TYPE5:
	common->gtcPktLength = 6;
	break;
    case GTCO_CC:
	common->gtcPktLength = 10;
	break;
    case SUMMA_MM:
	common->gtcPktLength = 6;
	break;
    case SUMMA_UIOF:
	common->gtcPktLength = 8;
	break;
    }


#ifdef XFREE86_V4
    xf86FlushInput(local->fd);
#else
    flush_input_fd(local->fd);
#endif
    common->gtcMaxZ = 512;     
    DBG(1, ErrorF("setup is max X=%d max Y=%d resol X=%d resol Y=%d MaxZ=%d\n",
		  common->gtcMaxX, common->gtcMaxY, common->gtcResolX,
		  common->gtcResolY, common->gtcMaxZ));

    if (common->gtcSuppress < 0) {
	int	xratio = common->gtcMaxX/screenInfo.screens[0]->width;
	int	yratio = common->gtcMaxY/screenInfo.screens[0]->height;
	
	common->gtcSuppress = (xratio > yratio) ? yratio : xratio;
    }
    
    if (common->gtcSuppress > 100) {
	common->gtcSuppress = 99;
    }



    if (xf86Verbose){
      ErrorF("%s Gtco tablet maximum X=%d maximum Y=%d "
	     "X resolution=%d Y resolution=%d suppress=%d\n",
	     XCONFIG_GIVEN,
	     common->gtcMaxX, common->gtcMaxY,
	     common->gtcResolX, common->gtcResolY, common->gtcSuppress);
    }

    return Success;
}

/*
 ***************************************************************************
 *
 * xf86GtcOpenDevice --
 *	Open the physical device and init information structs.
 *
 ***************************************************************************
 */
static int
xf86GtcOpenDevice(DeviceIntPtr       pGtc)
{
    LocalDevicePtr	local = (LocalDevicePtr)pGtc->public.devicePrivate;
    GtcoDevicePtr	priv = (GtcoDevicePtr)PRIVATE(pGtc);
    GtcoCommonPtr	common = priv->common;
    double		screenRatio, tabletRatio;
    int			gap;
    int			loop;
    int			screen_idx = 0;
    
    if (local->fd < 0) {
        if (common->gtcInitNumber > 2 ||
	    priv->initNumber == common->gtcInitNumber) {
	    if (common->gtcOpen(local) != Success) {
	        if (local->fd >= 0) {
		    SYSCALL(close(local->fd));
	        }
	        local->fd = -1;
	    }
	    else {
	        /* report the file descriptor to all devices */
	        for(loop=0; loop<common->gtcNumDevices; loop++) {
		    common->gtcDevices[loop]->fd = local->fd;
	        }
	    }
	    common->gtcInitNumber++;
	    priv->initNumber = common->gtcInitNumber;
	}
	else {
	  priv->initNumber = common->gtcInitNumber;
	}
    }

    if (local->fd != -1 &&
	priv->factorX == 0.0) {
	
	if (priv->bottomX == 0) priv->bottomX = common->gtcMaxX;

	if (priv->bottomY == 0) priv->bottomY = common->gtcMaxY;

	/* Verify Box validity */

	if (priv->topX > common->gtcMaxX ||
	    priv->topX < 0) {
	    ErrorF("Gtco invalid TopX (%d) reseting to 0\n", priv->topX);
	    priv->topX = 0;
	}

	if (priv->topY > common->gtcMaxY ||
	    priv->topY < 0) {
	    ErrorF("Gtco invalid TopY (%d) reseting to 0\n", priv->topY);
	    priv->topY = 0;
	}

	if (priv->bottomX > common->gtcMaxX ||
	    priv->bottomX < priv->topX) {
	    ErrorF("Gtco invalid BottomX (%d) reseting to %d\n",
		   priv->bottomX, common->gtcMaxX);
	    priv->bottomX = common->gtcMaxX;
	}

	if (priv->bottomY > common->gtcMaxY ||
	    priv->bottomY < priv->topY) {
	    ErrorF("Gtco invalid BottomY (%d) reseting to %d\n",
		   priv->bottomY, common->gtcMaxY);
	    priv->bottomY = common->gtcMaxY;
	}

	if (priv->screen_no != -1 &&
	    (priv->screen_no >= screenInfo.numScreens ||
	     priv->screen_no < 0)) {
	    ErrorF("%s: invalid screen number %d, resetting to 0\n",
		   local->name, priv->screen_no);
	    priv->screen_no = 0;
	}

	/* Calculate the ratio according to KeepShape, TopX and TopY */

	if (priv->screen_no != -1) {
	    screen_idx = priv->screen_no;
	}
	
	if (priv->flags & KEEP_SHAPE_FLAG) {
	    screenRatio = ((double) screenInfo.screens[screen_idx]->width)
		/ screenInfo.screens[screen_idx]->height;

	    tabletRatio = ((double) (common->gtcMaxX - priv->topX))
		/ (common->gtcMaxY - priv->topY);

	    DBG(2, ErrorF("screenRatio = %.3g, tabletRatio = %.3g\n",
			  screenRatio, tabletRatio));

	    if (screenRatio > tabletRatio) {
		gap = common->gtcMaxY * (1 - tabletRatio/screenRatio);
		priv->bottomX = common->gtcMaxX;
		priv->bottomY = common->gtcMaxY - gap;
	    } else {
		gap = common->gtcMaxX * (1 - screenRatio/tabletRatio);
		priv->bottomX = common->gtcMaxX - gap;
		priv->bottomY = common->gtcMaxY;
	    }
	}
	priv->factorX = ((double) screenInfo.screens[0]->width)
	    / (priv->bottomX - priv->topX);
	priv->factorY = ((double) screenInfo.screens[0]->height)
	    / (priv->bottomY - priv->topY);
    
	if (xf86Verbose)
	    ErrorF("%s Gtco tablet top X=%d top Y=%d "
		   "bottom X=%d bottom Y=%d\n",
		   XCONFIG_PROBED, priv->topX, priv->topY,
		   priv->bottomX, priv->bottomY);
	
	DBG(2, ErrorF("X factor = %.3g, Y factor = %.3g\n",
		      priv->factorX, priv->factorY));
    }

    /* Check threshold correctness */
    DBG(2, ErrorF("Threshold=%d\n", common->gtcThreshold));
    
    if (common->gtcThreshold > common->gtcMaxZ ||
	common->gtcThreshold < 0) {
	common->gtcThreshold = common->gtcMaxZ / 3;
    }
    DBG(2, ErrorF("New threshold=%d\n", common->gtcThreshold));    

    /* Set the real values */
    InitValuatorAxisStruct(pGtc,
			   0,
			   0,		/* min val */
			   priv->bottomX - priv->topX, /* max val */
			   mils(common->gtcResolX), /* resolution */
			   0,		/* min_res */
			   mils(common->gtcResolX)); /* max_res */
    InitValuatorAxisStruct(pGtc,
			   1,
			   0,		/* min val */
			   priv->bottomY - priv->topY, /* max val */
			   mils(common->gtcResolY), /* resolution */
			   0,		/* min_res */
			   mils(common->gtcResolY)); /* max_res */
    InitValuatorAxisStruct(pGtc,
			   2,
			   0,		/* min val */
			   common->gtcMaxZ, /* max val */
			   mils(common->gtcResolZ), /* resolution */
			   0,		/* min_res */
			   mils(common->gtcResolZ)); /* max_res */
    InitValuatorAxisStruct(pGtc,
			   3,
			   -64,		/* min val */
			   63,		/* max val */
			   128,		/* resolution ??? */
			   0,
			   128);
    InitValuatorAxisStruct(pGtc,
			   4,
			   -64,		/* min val */
			   63,		/* max val */
			   128,		/* resolution ??? */
			   0,
			   128);
    
    return (local->fd != -1);
}

/*
 ***************************************************************************
 *
 * xf86GtcClose --
 *
 ***************************************************************************
 */
static void
xf86GtcClose(LocalDevicePtr	local)
{
    GtcoDevicePtr	priv = (GtcoDevicePtr)local->private;
    GtcoCommonPtr	common = priv->common;
    int			loop;
    int			num = 0;
    
    for(loop=0; loop<common->gtcNumDevices; loop++) {
	if (common->gtcDevices[loop]->fd >= 0) {
	    num++;
	}
    }
    DBG(4, ErrorF("Gtco number of open devices = %d\n", num));
    
    if (num == 1) {		    
	SYSCALL(close(local->fd));
    }
    
    local->fd = -1;
}

/*
 ***************************************************************************
 *
 * xf86GtcProc --
 *      Handle the initialization, etc. of a gtco
 *
 ***************************************************************************
 */
static int
xf86GtcProc(DeviceIntPtr       pGtc,
	    int                what)
{
    CARD8                 map[(32 << 4) + 1];
    int                   nbaxes;
    int                   nbbuttons;
    int                   loop;
    LocalDevicePtr        local = (LocalDevicePtr)pGtc->public.devicePrivate;
    GtcoDevicePtr        priv = (GtcoDevicePtr)PRIVATE(pGtc);
  
    switch (what)
	{
	case DEVICE_INIT: 
	    DBG(1, ErrorF("xf86GtcProc pGtc=0x%x what=INIT\n", pGtc));
      
	    nbaxes = 5;		/* X, Y, Pressure, Tilt-X, Tilt-Y */

	    nbbuttons = 16;
	    for(loop=1; loop<=nbbuttons; loop++) map[loop] = loop;

	    if (InitButtonClassDeviceStruct(pGtc,
					    nbbuttons,
					    map) == FALSE) {
		ErrorF("unable to allocate Button class device\n");
		return !Success;
	    }
      
	    if (InitFocusClassDeviceStruct(pGtc) == FALSE) {
		ErrorF("unable to init Focus class device\n");
		return !Success;
	    }
          
	    if (InitPtrFeedbackClassDeviceStruct(pGtc,
						 xf86GtcControlProc) == FALSE) {
		ErrorF("unable to init ptr feedback\n");
		return !Success;
	    }
	    
	    if (InitProximityClassDeviceStruct(pGtc) == FALSE) {
		ErrorF("unable to init proximity class device\n");
		return !Success;
	    }

	    if (InitKeyClassDeviceStruct(pGtc, &gtco_keysyms, NULL) == FALSE) {
		ErrorF("unable to init key class device\n"); 
		return !Success;
	    }

	    if (InitValuatorClassDeviceStruct(pGtc, 
					      nbaxes,
					      xf86GetMotionEvents, 
					      local->history_size,
					      ((priv->flags & ABSOLUTE_FLAG) 
					      ? Absolute : Relative) |
					      OutOfProximity)
		== FALSE) {
		ErrorF("unable to allocate Valuator class device\n"); 
		return !Success;
	    }
	    else {
		/* allocate the motion history buffer if needed */
		xf86MotionHistoryAllocate(local);
#ifndef XFREE86_V4
		AssignTypeAndName(pGtc, local->atom, local->name);
#endif
	    }

	    /* open the device to gather informations */
	    xf86GtcOpenDevice(pGtc);

	    break; 
      
	case DEVICE_ON:
	    DBG(1, ErrorF("xf86GtcProc pGtc=0x%x what=ON\n", pGtc));

	    if ((local->fd < 0) && (!xf86GtcOpenDevice(pGtc))) {
		return !Success;
	    }
#ifdef XFREE86_V4	    
	    xf86AddEnabledDevice(local);
#else
	    AddEnabledDevice(local->fd);
#endif
	    pGtc->public.on = TRUE;
	    break;
      
	case DEVICE_OFF:
	case DEVICE_CLOSE:
	    DBG(1, ErrorF("xf86GtcProc  pGtc=0x%x what=%s\n", pGtc,
			  (what == DEVICE_CLOSE) ? "CLOSE" : "OFF"));
	    if (local->fd >= 0) {
#ifdef XFREE86_V4	    
		xf86RemoveEnabledDevice(local);
#else
		RemoveEnabledDevice(local->fd);
#endif
		xf86GtcClose(local);
	    }
	    pGtc->public.on = FALSE;
	    break;
	    
	default:
	    ErrorF("gtco unsupported mode=%d\n", what);
	    return !Success;
	    break;
	}
    DBG(2, ErrorF("END   xf86GtcProc Success what=%d dev=0x%x priv=0x%x\n",
		  what, pGtc, priv));
    return Success;
}

/*
 ***************************************************************************
 *
 * xf86GtcChangeControl --
 *
 ***************************************************************************
 */
static int
xf86GtcChangeControl(LocalDevicePtr	local,
		     xDeviceCtl		*control)
{
    xDeviceResolutionCtl	*res;
    int				*resolutions;
    char			str[10];
  
    res = (xDeviceResolutionCtl *)control;
	
    if ((control->control != DEVICE_RESOLUTION) ||
	(res->num_valuators < 1))
	return (BadMatch);
  
    resolutions = (int *)(res +1);
    
    DBG(3, ErrorF("xf86GtcChangeControl changing to %d (suppressing under)\n",
		  resolutions[0]));

    sprintf(str, "SU%d\r", resolutions[0]);
    SYSCALL(write(local->fd, str, strlen(str)));
  
    return(Success);
}

/*
 ***************************************************************************
 *
 * xf86GtcSwitchMode --
 *
 ***************************************************************************
 */
static int
xf86GtcSwitchMode(ClientPtr	client,
		  DeviceIntPtr	dev,
		  int		mode)
{
    LocalDevicePtr        local = (LocalDevicePtr)dev->public.devicePrivate;
    GtcoDevicePtr        priv = (GtcoDevicePtr)local->private;

    DBG(3, ErrorF("xf86GtcSwitchMode dev=0x%x mode=%d\n", dev, mode));
  
    if (mode == Absolute) {
	priv->flags = priv->flags | ABSOLUTE_FLAG;
    }
    else {
	if (mode == Relative) {
	    priv->flags = priv->flags & ~ABSOLUTE_FLAG; 
	}
	else {
	    DBG(1, ErrorF("xf86GtcSwitchMode dev=0x%x invalid mode=%d\n", dev,
			  mode));
	    return BadMatch;
	}
    }
    return Success;
}

/*
 ***************************************************************************
 *
 * xf86GtcAllocate --
 *
 ***************************************************************************
 */
static LocalDevicePtr
xf86GtcAllocate(char *  name,
                int     flag)
{
    LocalDevicePtr        local;
    GtcoDevicePtr        priv;
    GtcoCommonPtr        common;

    priv = (GtcoDevicePtr) xalloc(sizeof(GtcoDeviceRec));
    if (!priv)
	return NULL;

    common = (GtcoCommonPtr) xalloc(sizeof(GtcoCommonRec));
    if (!common) {
	xfree(priv);
	return NULL;
    }

#ifdef XFREE86_V4
    local = xf86AllocateInput(gtcDrv, 0);
#else
    local = (LocalDevicePtr) xalloc(sizeof(LocalDeviceRec));
#endif
    if (!local) {
	xfree(priv);
	xfree(common);
	return NULL;
    }

    local->name = name;
    local->flags = 0;
#ifndef XFREE86_V4
    local->device_config = xf86GtcConfig;
#endif 
    local->device_control = xf86GtcProc;
    local->read_input = xf86GtcReadInput;
    local->control_proc = xf86GtcChangeControl;
    local->close_proc = xf86GtcClose;
    local->switch_mode = xf86GtcSwitchMode;
    local->conversion_proc = xf86GtcConvert;
    local->reverse_conversion_proc = xf86GtcReverseConvert;
    local->fd = -1;
    local->atom = 0;
    local->dev = NULL;
    local->private = priv;
    local->private_flags = 0;
    local->history_size  = 0;
    local->old_x = -1;
    local->old_y = -1;
    
    priv->flags = flag;			/* various flags (device type, absolute, first touch...) */
    priv->oldX = -1;			/* previous X position */
    priv->oldY = -1;			/* previous Y position */
    priv->oldZ = -1;			/* previous pressure */
    priv->oldTiltX = -1;		/* previous tilt in x direction */
    priv->oldTiltY = -1;		/* previous tilt in y direction */
    priv->oldButtons = 0;		/* previous buttons state */
    priv->oldProximity = 1;		/* previous proximity */
    priv->topX = 0;			/* X top */
    priv->topY = 0;			/* Y top */
    priv->bottomX = 0;			/* X bottom */
    priv->bottomY = 0;			/* Y bottom */
    priv->factorX = 0.0;		/* X factor */
    priv->factorY = 0.0;		/* Y factor */
    priv->common = common;		/* common info pointer */
    priv->oldProximity = 0;		/* previous proximity */
    priv->serial = 0;		        /* serial number */
    priv->initNumber = 0;	        /* magic number for the init phasis */
    priv->screen_no = -1;		/* associated screen */
    
    common->gtcDevice = "";		/* device file name */
    common->gtcSuppress = -1;		/* transmit position if increment is superior */
    common->gtcFlags = 0;		/* various flags */
    common->gtcDevices = (LocalDevicePtr*) xalloc(sizeof(LocalDevicePtr));
    common->gtcDevices[0] = local;
    common->gtcNumDevices = 1;		/* number of devices */
    common->gtcIndex = 0;		/* number of bytes read */
    common->gtcPktLength = 7;		/* length of a packet */
    common->gtcMaxX = 0;		/* max X value */
    common->gtcMaxY = 0;		/* max Y value */
    common->gtcMaxZ = DEFAULT_MAXZ;	/* max Z value */
    common->gtcResolX = 1000;		/* X resolution in points/inch */
    common->gtcResolY = 1000;		/* Y resolution in points/inch */
    common->gtcResolZ = 1270;		/* Z resolution in points/inch */
    common->gtcThreshold = INVALID_THRESHOLD; /* button 1 threshold for some tablet models */
    common->gtcInitNumber = 0;	        /* magic number for the init phasis */
    common->gtcLinkSpeed = 9600;        /* serial link speed */
    common->gtcOpen = xf86GtcOpen;	/* function used to open the line (serial or USB) */
    return local;
}

/*
 ***************************************************************************
 *
 * xf86GtcAllocateCursor --
 *
 ***************************************************************************
 */
static LocalDevicePtr
xf86GtcAllocateCursor()
{
    LocalDevicePtr        local = xf86GtcAllocate(XI_CURSOR, CURSOR_ID);

    if (local)
	local->type_name = "Gtco Cursor";
    return local;
}

/*
 ***************************************************************************
 *
 * Gtco Cursor device association --
 *
 ***************************************************************************
 */
DeviceAssocRec gtco_cursor_assoc =
{
    CURSOR_SECTION_NAME,		/* config_section_name */
    xf86GtcAllocateCursor		/* device_allocate */
};

#ifndef XFREE86_V4
#ifdef DYNAMIC_MODULE
/*
 ***************************************************************************
 *
 * entry point of dynamic loading
 *
 ***************************************************************************
 */
int
#ifndef DLSYM_BUG
init_module(unsigned long	server_version)
#else
init_xf86Gtco(unsigned long    server_version)
#endif
{
    xf86AddDeviceAssoc(&gtco_cursor_assoc);

    if (server_version != XF86_VERSION_CURRENT) {
	ErrorF("Warning: Gtco module compiled for version%s\n", XF86_VERSION);
	return 0;
    } else {
	return 1;
    }
}
#endif /* DYNAMIC_MODULE */

#else /* XFREE86_V4 */

/*
 * xf86GtcUninit --
 *
 * called when the driver is unloaded.
 */
static void
xf86GtcUninit(InputDriverPtr	drv,
	      LocalDevicePtr	local,
	      int flags)
{
    GtcoDevicePtr	priv = (GtcoDevicePtr) local->private;
    
    DBG(1, ErrorF("xf86GtcUninit\n"));
    
    xf86GtcProc(local->dev, DEVICE_OFF);
    
    xfree (priv);
    xf86DeleteInput(local, 0);    
}

/*
 * xf86GtcInit --
 *
 * called when the module subsection is found in XF86Config
 */
static InputInfoPtr
xf86GtcInit(InputDriverPtr	drv,
	    IDevPtr		dev,
	    int			flags)
{
    LocalDevicePtr	local = NULL;
    LocalDevicePtr	fakeLocal = NULL;
    GtcoDevicePtr	priv = NULL;
    GtcoCommonPtr	common = NULL;
    char		*s;
    LocalDevicePtr	localDevices;

    gtcDrv = drv;

    fakeLocal = (LocalDevicePtr) xcalloc(1, sizeof(LocalDeviceRec));
    if (!fakeLocal)
	return NULL;

    fakeLocal->conf_idev = dev;

    /* Force default serial port options to exist because the serial init
     * phasis is based on those values.
     */
    xf86CollectInputOptions(fakeLocal, default_gtco, NULL);

    local = xf86GtcAllocate(XI_CURSOR, CURSOR_ID);
    local->type_name = "Gtco Cursor";
    
    if (!local) {
	xfree(fakeLocal);
	return NULL;
    }

    priv = (GtcoDevicePtr) local->private;
    common = priv->common;

    local->options = fakeLocal->options;
    local->conf_idev = fakeLocal->conf_idev;
    local->name = dev->identifier;
    xfree(fakeLocal);
    
    /* Serial Device is mandatory */
    common->gtcDevice = xf86FindOptionValue(local->options, "Device");

    if (!common->gtcDevice) {
	xf86Msg (X_ERROR, "%s: No Device specified.\n", dev->identifier);
	goto SetupProc_fail;
    }

    /* Lookup to see if there is another gtco device sharing
     * the same serial line.
     */
    localDevices = xf86FirstLocalDevice();
    
    while(localDevices) {
	if ((local != localDevices) &&
	    (localDevices->device_control == xf86GtcProc) &&
	    (strcmp(((GtcoDevicePtr)localDevices->private)->common->gtcDevice,
		    common->gtcDevice) == 0)) {
		DBG(2, ErrorF("xf86GtcConfig gtco port share between"
			      " %s and %s\n",
			      local->name, localDevices->name));
		xfree(common->gtcDevices);
		xfree(common);
		common = priv->common = ((GtcoDevicePtr) localDevices->private)->common;
		common->gtcNumDevices++;
		common->gtcDevices = (LocalDevicePtr *) xrealloc(common->gtcDevices,
								 sizeof(LocalDevicePtr) * common->gtcNumDevices);
		common->gtcDevices[common->gtcNumDevices - 1] = local;
		break;
	}
	localDevices = localDevices->next;
    }

    /* Process the common options. */
    xf86ProcessCommonOptions(local, local->options);

    /* Optional configuration */

    xf86Msg(X_CONFIG, "%s serial device is %s\n", dev->identifier,
	    common->gtcDevice);

    debug_level = xf86SetIntOption(local->options, "DebugLevel", debug_level);
    if (debug_level > 0) {
	xf86Msg(X_CONFIG, "GTCO: debug level set to %d\n", debug_level);
    }

    s = xf86FindOptionValue(local->options, "Mode");

    if (s && (xf86NameCmp(s, "absolute") == 0)) {
	priv->flags = priv->flags | ABSOLUTE_FLAG;
    }
    else if (s && (xf86NameCmp(s, "relative") == 0)) {
	priv->flags = priv->flags & ~ABSOLUTE_FLAG;
    }
    else if (s) {
	xf86Msg(X_ERROR, "%s: invalid Mode (should be absolute or relative). Using default.\n",
		dev->identifier);
    }
    xf86Msg(X_CONFIG, "%s is in %s mode\n", local->name,
	    (priv->flags & ABSOLUTE_FLAG) ? "absolute" : "relative");	    

    common->gtcSuppress = xf86SetIntOption(local->options, "Suppress", common->gtcSuppress);
    if (common->gtcSuppress != -1) {
	xf86Msg(X_CONFIG, "GTCO: suppress value is %d\n", XCONFIG_GIVEN,
		common->gtcSuppress);      
    }

    s = xf86FindOptionValue(local->options, "DataFormat");
    if (s && (xf86NameCmp(s, "GTCO_Type5") == 0)) {
	common->gtcFlags |= GTCO_TYPE5;
	DBG(1, ErrorF("GTCO: GTCO_Type5 set\n"));
    } else if (s && (xf86NameCmp(s, "GTCO_CC") == 0)) {
	common->gtcFlags |= GTCO_CC;
	DBG(1, ErrorF("GTCO: GTCO_CC set\n"));
    } else if (s && (xf86NameCmp(s, "Summa_MM") == 0)) {
	common->gtcFlags |= SUMMA_MM;
	/* alters parity to odd - bg*/
	xf86CollectInputOptions(local, default_mm_options, NULL);
	DBG(1, ErrorF("GTCO: Summa_MM set\n"));
    } else if (s && (xf86NameCmp(s, "Summa_UIOF") == 0)) {
	common->gtcFlags |= SUMMA_UIOF;
	/* alters parity to even, stop bits to 2, and data bits to 7 - bg*/
	xf86CollectInputOptions(local, default_uiof_options, NULL);
	DBG(1, ErrorF("GTCO: Summa_UIOF set\n"));
    } else {
	xf86Msg(X_CONFIG, "GTCO: Bad DataFormat option. Expected: "
		"GTCO_CC, GTCO_Type5, Summa_MM, or Summa_UIOF\n",
		XCONFIG_GIVEN, s);
    }

    s = xf86FindOptionValue(local->options, "EmulateMouse");
    if(s && (xf86NameCmp(s, "yes") == 0)){
	priv->flags |= EMULATE_MOUSE_FLAG;
	DBG(1, ErrorF("Gtco: Set mouse flag to true\n"));
    }else{
	priv->flags &= ~EMULATE_MOUSE_FLAG;
	DBG(1, ErrorF("Gtco: Set mouse flag to false\n"));
    }

    if (xf86SetBoolOption(local->options, "KeepShape", 0)) {
	priv->flags |= KEEP_SHAPE_FLAG;
	xf86Msg(X_CONFIG, "%s: keeps shape\n", dev->identifier);
    }

    priv->topX = xf86SetIntOption(local->options, "TopX", 0);
    if (priv->topX != 0) {
	xf86Msg(X_CONFIG, "%s: top x = %d\n", dev->identifier, priv->topX);
    }
    priv->topY = xf86SetIntOption(local->options, "TopY", 0);
    if (priv->topY != 0) {
	xf86Msg(X_CONFIG, "%s: top x = %d\n", dev->identifier, priv->topY);
    }
    priv->bottomX = xf86SetIntOption(local->options, "BottomX", 0);
    if (priv->bottomX != 0) {
	xf86Msg(X_CONFIG, "%s: bottom x = %d\n", dev->identifier,
		priv->bottomX);
    }
    priv->bottomY = xf86SetIntOption(local->options, "BottomY", 0);
    if (priv->bottomY != 0) {
	xf86Msg(X_CONFIG, "%s: bottom x = %d\n", dev->identifier,
		priv->bottomY);
    }
    priv->serial = xf86SetIntOption(local->options, "Serial", 0);
    if (priv->bottomY != 0) {
	xf86Msg(X_CONFIG, "%s: serial number = %u\n", dev->identifier,
		priv->serial);
    }
    common->gtcThreshold = xf86SetIntOption(local->options, "Threshold", common->gtcThreshold);
    if (common->gtcThreshold != INVALID_THRESHOLD) {
	xf86Msg(X_CONFIG, "%s: threshold = %d\n", dev->identifier,
		common->gtcThreshold);
    }
    common->gtcMaxX = xf86SetIntOption(local->options, "MaxX", common->gtcMaxX);
    if (common->gtcMaxX != 0) {
	xf86Msg(X_CONFIG, "%s: max x = %d\n", dev->identifier,
		common->gtcMaxX);
    }
    common->gtcMaxY = xf86SetIntOption(local->options, "MaxY", common->gtcMaxY);
    if (common->gtcMaxY != 0) {
	xf86Msg(X_CONFIG, "%s: max x = %d\n", dev->identifier,
		common->gtcMaxY);
    }
    common->gtcMaxZ = xf86SetIntOption(local->options, "MaxZ", common->gtcMaxZ);
    if (common->gtcMaxZ != DEFAULT_MAXZ) {
	xf86Msg(X_CONFIG, "%s: max x = %d\n", dev->identifier,
		common->gtcMaxZ);
    }
    common->gtcResolX = xf86SetIntOption(local->options, "ResolutionX", common->gtcResolX);
    if (common->gtcResolX != 0) {
	xf86Msg(X_CONFIG, "%s: resol x = %d\n", dev->identifier,
		common->gtcResolX);
    }
    common->gtcResolY = xf86SetIntOption(local->options, "ResolutionY", common->gtcResolY);
    if (common->gtcResolY != 0) {
	xf86Msg(X_CONFIG, "%s: resol x = %d\n", dev->identifier,
		common->gtcResolY);
    }
    common->gtcResolZ = xf86SetIntOption(local->options, "ResolutionZ", common->gtcResolZ);
    if (common->gtcResolZ != 0) {
	xf86Msg(X_CONFIG, "%s: resol x = %d\n", dev->identifier,
		common->gtcResolZ);
    }

    {
	int	val;
	val = xf86SetIntOption(local->options, "BaudRate", 0);

	switch(val) {
	case 19200:
	    common->gtcLinkSpeed = 19200;
	    break;
	case 9600:
	    common->gtcLinkSpeed = 9600;
	    break;
	default:
	    xf86Msg(X_ERROR, "%s: Illegal speed value (must be 9600 or 19200).", dev->identifier);
	    break;
	}
	if (xf86Verbose)
	    xf86Msg(X_CONFIG, "%s: serial speed %u\n", dev->identifier,
		    val);
    }
    /* mark the device configured */
    local->flags |= XI86_POINTER_CAPABLE | XI86_CONFIGURED;

    /* return the LocalDevice */
    return (local);

  SetupProc_fail:
    if (common)
	xfree(common);
    if (priv)
	xfree(priv);
    if (local)
	xfree(local);
    return NULL;
}

#ifdef XFree86LOADER
static
#endif
InputDriverRec GTCO = {
    1,				/* driver version */
    "gtcc",			/* driver name */
    NULL,			/* identify */
    xf86GtcInit,		/* pre-init */
    xf86GtcUninit,		/* un-init */
    NULL,			/* module */
    0				/* ref count */
};

/*
 ***************************************************************************
 *
 * Dynamic loading functions
 *
 ***************************************************************************
 */
#ifdef XFree86LOADER
/*
 * xf86GtcUnplug --
 *
 * called when the module subsection is found in XF86Config
 */
static void
xf86GtcUnplug(pointer	p)
{
    DBG(1, ErrorF("xf86GtcUnplug\n"));
}

/*
 * xf86GtcPlug --
 *
 * called when the module subsection is found in XF86Config
 */
static pointer
xf86GtcPlug(pointer	module,
	    pointer	options,
	    int		*errmaj,
	    int		*errmin)
{
    xf86Msg(X_INFO, "Gtco driver level: %s\n", identification+strlen("$Identification: "));
	    
    xf86AddInputDriver(&GTCO, module, 0);

    return module;
}

static XF86ModuleVersionInfo xf86GtcVersionRec =
{
    "gtcc",
/*      MODULEVENDORSTRING, */
    "GTCO CalComp Peripherals",
    MODINFOSTRING1,
    MODINFOSTRING2,
    XF86_VERSION_CURRENT,
    1, 0, 0,
    ABI_CLASS_XINPUT,
    ABI_XINPUT_VERSION,
    MOD_CLASS_XINPUT,
    {0, 0, 0, 0}		/* signature, to be patched into the file by */
				/* a tool */
};

XF86ModuleData gtccModuleData = {&xf86GtcVersionRec,
				  xf86GtcPlug,
				  xf86GtcUnplug};

#endif /* XFree86LOADER */
#endif /* XFREE86_V4 */
