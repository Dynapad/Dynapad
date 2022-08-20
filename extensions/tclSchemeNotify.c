/* 
 * SchemeNotify.c --
 *
 *	notifier for DrScheme
 *
 */

#include "escheme.h"
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <X11/Xlib.h>
#include <fcntl.h>


typedef void *CallbackData;

typedef void (Pad_FileCallback)(CallbackData callbackData, int mask);

typedef void (Pad_TimerCallback)(CallbackData callbackData);

typedef void *Pad_TimerToken;
#define PAD_READABLE  1
#define PAD_WRITABLE  2
#define PAD_EXCEPTION 4

/*
 * This structure is used to keep track of the notifier info for a 
 * a registered file.
 */

typedef struct FileHandler {
    int fd;
    Display *display;
    int mask;            /* Mask of desired events: TCL_READABLE, etc. */
    Pad_FileCallback *proc;    /* Procedure to call, in the style of
				 * Tcl_CreateFileHandler. */
    CallbackData callbackData;    /* Argument to pass to proc. */
    struct FileHandler *nextPtr;/* Next in list of all files we care about. */
} FileHandler;

typedef struct TimerHandler {
    struct timeval endtime;    /* endtime of timeout */
    Pad_TimerCallback *proc;    /* Procedure to call, in the style of
				 * Tcl_CreateFileHandler. */
    CallbackData callbackData;    /* Argument to pass to proc. */
    struct TimerHandler *nextPtr;
} TimerHandler;

/*
 * The following static structure contains the state information for the
 * DrScheme based implementation of the notifier.
 */

static struct NotifierState {
    int timeout;            /* timeout in ms */
    FileHandler *firstFileHandlerPtr;    /* Pointer to head of file handler
					 * list. */
    TimerHandler *firstTimerHandlerPtr;    /* Pointer to head of timer handler
					 * list. */
} notifier;

static int debug = 0;

/*
 * The following static indicates whether this module has been initialized.
 */

static int initialized = 0;

/*
 * Static routines defined in this file.
 */

static int ready(Scheme_Object *data);

static void wakeup(Scheme_Object *data, void *fds);

static void timevaladd(struct timeval *, struct timeval *);

static void timevalsub(struct timeval *, struct timeval *);

static void timevalzero(struct timeval *);

static int timevaliszero(struct timeval *);

static int timevalle(struct timeval *, struct timeval *);

/*
 * Functions defined in this file for use by users of the Scheme Notifier:
 */


/*
 *----------------------------------------------------------------------
 *
 * Pad_CreateTimerHandler --
 *
 *	This procedure adds a timeout callback.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

Pad_TimerToken
Pad_CreateTimerHandler(delay_ms, proc, callbackData)
    int delay_ms;
    Pad_TimerCallback *proc;
    void *callbackData;
{
    TimerHandler *timerPtr, *p, *q;
    struct timeval timeout;

    if (debug) fprintf(stderr, "Pad_CreateTimerHandler %d\n", delay_ms);
    timerPtr = (TimerHandler *) malloc(sizeof(TimerHandler));
    timeout.tv_sec = delay_ms / 1000;
    timeout.tv_usec = (delay_ms % 1000) * 1000;
    gettimeofday(&timerPtr->endtime, NULL);
    timevaladd(&timerPtr->endtime, &timeout);
    timerPtr->proc = proc;
    timerPtr->callbackData = callbackData;

    for (p = notifier.firstTimerHandlerPtr, q = NULL;
         p != NULL;
         q = p, p = p->nextPtr) {
        if (timevalle(&timerPtr->endtime, &p->endtime)) {
            break;
        }
    }
    if (p == notifier.firstTimerHandlerPtr) {
        timerPtr->nextPtr = p;
        notifier.firstTimerHandlerPtr = timerPtr;
    } else {
        timerPtr->nextPtr = p;
        q->nextPtr = timerPtr;
    }

    if (debug) fprintf(stderr, "Pad_CreateTimerHandler returning\n");
    return (Pad_TimerToken *) timerPtr;
}

/*
 *----------------------------------------------------------------------
 *
 * Pad_DeleteTimerHandler --
 *
 *	This procedure removes a timeout callback.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */
void
Pad_DeleteTimerHandler(token)
    Pad_TimerToken token;
{
    TimerHandler *timerPtr, *prevPtr;

    if (debug) fprintf(stderr, "Pad_DeleteTimerHandler\n");
    for (prevPtr = NULL, timerPtr = notifier.firstTimerHandlerPtr;;
         prevPtr = timerPtr, timerPtr = timerPtr->nextPtr) {
        if (timerPtr == NULL) {
            return;
        }
        if (timerPtr == token) {
            break;
        }
    }

    /*
     * Clean up information in the callback record.
     */

    if (prevPtr == NULL) {
        notifier.firstTimerHandlerPtr = timerPtr->nextPtr;
    } else {
        prevPtr->nextPtr = timerPtr->nextPtr;
    }
    free((char *) timerPtr);
    if (debug) fprintf(stderr, "Pad_DeleteTimerHandler returning\n");
}

/*
 *----------------------------------------------------------------------
 *
 * TimerProc --
 *
 *	This procedure is the callback used to handle timeouts.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *      Processes all queued events.  loop???
 *
 *----------------------------------------------------------------------
 */

static void
TimerProc() {
    TimerHandler *timerPtr;

    if (debug) fprintf(stderr, "TimerProc\n");
    if ((timerPtr = notifier.firstTimerHandlerPtr)) {
        timerPtr->proc(timerPtr->callbackData);
        Pad_DeleteTimerHandler(timerPtr);
    }
    if (debug) fprintf(stderr, "TimerProc returning\n");
}

/*
 *----------------------------------------------------------------------
 *
 * SetTimer --
 *
 *	This procedure sets the current notifier timeout value.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Replaces any previous timer.
 *
 *----------------------------------------------------------------------
 */

static void
SetTimer() {
    TimerHandler *timerPtr;
    struct timeval timeout, *endtime;

    if ((timerPtr = notifier.firstTimerHandlerPtr))
        endtime = &timerPtr->endtime;

    if (debug) {
        if (timerPtr)
            fprintf(stderr,
                    "SetTimer %ld %d\n", endtime->tv_sec, endtime->tv_usec);
        else
            fprintf(stderr, "SetTimer NULL\n");
    }

    if (timerPtr) {
        gettimeofday(&timeout, NULL);
        timevalsub(&timeout, endtime);
        notifier.timeout = timeout.tv_sec * 1000 + timeout.tv_usec / 1000;
        notifier.timeout *= -1;
        if (notifier.timeout < 0)
            notifier.timeout = 0;
    } else {
        notifier.timeout = 0;
    }
    if (debug) fprintf(stderr, "SetTimer returning\n");
}

/*
 *----------------------------------------------------------------------
 *
 * CreateFileHandler --
 *
 *	This procedure registers a file handler with the Scheme notifier.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Creates a new file handler structure and registers one or more
 *	input procedures.
 *
 *----------------------------------------------------------------------
 */

void
Pad_CreateFileHandler(fd, display, mask, proc, callbackData)
    int fd;            /* Handle of stream to watch. */
    Display *display;
    int mask;                   /* OR'ed combination of PAD_READABLE,
				 * PAD_WRITABLE, and PAD_EXCEPTION:
                                 * indicates conditions under which
				 * proc should be called. */
    Pad_FileCallback *proc;    /* Procedure to call for each
				 * selected event. */
    CallbackData callbackData;    /* Arbitrary data to pass to proc. */
{
    FileHandler *filePtr;

    if (debug) fprintf(stderr, "CreateFileHandler %d %x\n", fd, mask);

    for (filePtr = notifier.firstFileHandlerPtr; filePtr != NULL;
         filePtr = filePtr->nextPtr) {
        if (filePtr->fd == fd) {
            break;
        }
    }
    if (filePtr == NULL) {
        filePtr = (FileHandler *) malloc(sizeof(FileHandler));
        filePtr->fd = fd;
        filePtr->display = display;
        filePtr->mask = 0;
        filePtr->nextPtr = notifier.firstFileHandlerPtr;
        notifier.firstFileHandlerPtr = filePtr;
    }
    filePtr->proc = proc;
    filePtr->callbackData = callbackData;
    filePtr->mask = mask;
    if (debug) fprintf(stderr, "CreateFileHandler returning\n");
}

/*
 *----------------------------------------------------------------------
 *
 * DeleteFileHandler --
 *
 *	Cancel a previously-arranged callback arrangement for
 *	a file.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	If a callback was previously registered on file, remove it.
 *
 *----------------------------------------------------------------------
 */

void
Pad_DeleteFileHandler(fd)
    int fd;            /* Stream id for which to remove
				 * callback procedure. */
{
    FileHandler *filePtr, *prevPtr;

    if (debug) fprintf(stderr, "DeleteFileHandler %d\n", fd);

    /*
     * Find the entry for the given file (and return if there
     * isn't one).
     */

    for (prevPtr = NULL, filePtr = notifier.firstFileHandlerPtr;;
         prevPtr = filePtr, filePtr = filePtr->nextPtr) {
        if (filePtr == NULL) {
            return;
        }
        if (filePtr->fd == fd) {
            break;
        }
    }

    /*
     * Clean up information in the callback record.
     */

    if (prevPtr == NULL) {
        notifier.firstFileHandlerPtr = filePtr->nextPtr;
    } else {
        prevPtr->nextPtr = filePtr->nextPtr;
    }
    free((char *) filePtr);
    if (debug) fprintf(stderr, "DeleteFileHandler returning\n");
}

/*
 *----------------------------------------------------------------------
 *
 * FileProc --
 *
 *	These procedures are called when a file becomes readable,
 *	writable, or has an exception.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Makes an entry on the Tcl event queue if the event is
 *	interesting.
 *
 *----------------------------------------------------------------------
 */

static void
FileProc(filePtr, mask)
    FileHandler *filePtr;
    int mask;
{
    FileHandler *fileEvPtr;

    if (debug) fprintf(stderr, "FileProc %d %x\n", filePtr->fd, mask);

    /*
     * Ignore unwanted or duplicate events.
     */

    if (!(filePtr->mask & mask)) {
        return;
    }

    /*
     * This is an interesting event, so run it.
     */

    filePtr->proc(filePtr->callbackData, mask);

    if (debug) fprintf(stderr, "FileProc returning\n");
}

/*
 *----------------------------------------------------------------------
 *
 * Pad_MainLoop --
 *
 *	Pad_MainLoop runs in a scheme thread and never returns.
 *	It registers the X Server file descriptor and any timeout
 *	with scheme, then blocks in scheme.  The block returns when
 *	X or a timeout is ready.  Pad_MainLoop dispatches the
 *	X or timeout callback, then loops back.
 *
 *----------------------------------------------------------------------
 */

#define TIMEOUT 0x4000

void
Pad_MainLoop(void) {
    static fd_set fdset[3];
    FileHandler *fhp;
    int n;

    if (debug) fprintf(stderr, "Pad_MainLoop\n");
#ifdef CYGWINDEBUG
    fprintf(stderr, "reopening stderr\n");
    {
        int fd;
        fd = open("/dev/tty0", 1);
        close(2);
        dup2(fd, 2);
        stderr = fdopen(2, "w");
        close(fd);
    }
    fprintf(stderr, "stderr works\n");
#endif

    if (debug) fprintf(stderr, "timeout %d\n", notifier.timeout);
    while (1) {
        SetTimer();
        if (notifier.timeout == 0) {
            if (ready((Scheme_Object *) fdset)) {
                goto process;
            } else {
                scheme_thread_block(0);
                scheme_making_progress();
                /*continue;*/
                goto process;
            }
        }
        process:
#ifdef CYGWIN
        n = scheme_block_until(ready, wakeup, (Scheme_Object *)fdset, 0.1);
#else
        n = scheme_block_until(ready, wakeup, (Scheme_Object *) fdset,
                               (float) notifier.timeout / 1000);
#endif

        if (n & ~TIMEOUT) {
            for (fhp = notifier.firstFileHandlerPtr; fhp; fhp = fhp->nextPtr) {
                if (FD_ISSET(fhp->fd, &fdset[0])) {
                    FileProc(fhp, PAD_READABLE);
                }
                if (FD_ISSET(fhp->fd, &fdset[1])) {
                    FileProc(fhp, PAD_WRITABLE);
                }
                if (FD_ISSET(fhp->fd, &fdset[2])) {
                    FileProc(fhp, PAD_EXCEPTION);
                }
            }
        }

        if (n & TIMEOUT) {
            TimerProc();
        }

        for (fhp = notifier.firstFileHandlerPtr; fhp; fhp = fhp->nextPtr) {
            if (fhp->display && QLength(fhp->display))
                FileProc(fhp, PAD_READABLE);
        }
    }
    if (debug) fprintf(stderr, "Pad_MainLoop returning 1\n");
}

static int
ready(Scheme_Object *data) {
    fd_set *fdset;
    int n = 0;
    FileHandler *fhp;
    struct timeval timeout, current;

    if (debug) fprintf(stderr, "ready\n");
    fdset = (fd_set *) data;
    FD_ZERO(&fdset[0]);
    FD_ZERO(&fdset[1]);
    FD_ZERO(&fdset[2]);
    for (fhp = notifier.firstFileHandlerPtr; fhp; fhp = fhp->nextPtr) {
        if (fhp->fd >= n) {
            n = fhp->fd + 1;
        }
        if (fhp->mask & PAD_READABLE) {
            FD_SET(fhp->fd, &fdset[0]);
        }
        if (fhp->mask & PAD_WRITABLE) {
            FD_SET(fhp->fd, &fdset[1]);
        }
        if (fhp->mask & PAD_EXCEPTION) {
            FD_SET(fhp->fd, &fdset[2]);
        }
    }
    timevalzero(&timeout);

    n = select(n, &fdset[0], &fdset[1], &fdset[2], &timeout);

    gettimeofday(&current, NULL);
    if (notifier.firstTimerHandlerPtr &&
        timevalle(&notifier.firstTimerHandlerPtr->endtime, &current)) {
        n |= TIMEOUT;
    }
    if (debug) fprintf(stderr, "ready returning 0x%x\n", n);
    return n;
}

static void
wakeup(Scheme_Object *data, void *fds) {
    FileHandler *fhp;
    void *fdset[3];

    if (debug) fprintf(stderr, "wakeup\n");
    fdset[0] = MZ_GET_FDSET(fds, 0);
    fdset[1] = MZ_GET_FDSET(fds, 1);
    fdset[2] = MZ_GET_FDSET(fds, 2);
    for (fhp = notifier.firstFileHandlerPtr; fhp; fhp = fhp->nextPtr) {
        if (fhp->mask & PAD_READABLE) {
            MZ_FD_SET(fhp->fd, (fd_set *) fdset[0]);
        }
        if (fhp->mask & PAD_WRITABLE) {
            MZ_FD_SET(fhp->fd, (fd_set *) fdset[1]);
        }
        if (fhp->mask & PAD_EXCEPTION) {
            MZ_FD_SET(fhp->fd, (fd_set *) fdset[2]);
        }
    }
    if (debug) fprintf(stderr, "wakeup returning\n");
}

/*
 * struct timeval arithmetic functions
 */

static void
timevalzero(t1)
    struct timeval *t1;
{
    t1->tv_sec = t1->tv_usec = 0;
}

static int
timevaliszero(t1)
    struct timeval *t1;
{
    return (t1->tv_sec == 0 && t1->tv_usec == 0);
}

static void
timevaladd(t1, t2)
    struct timeval *t1, *t2;
{
    t1->tv_sec += t2->tv_sec;
    t1->tv_usec += t2->tv_usec;
    if (t1->tv_usec >= 1000000) {
        t1->tv_sec++;
        t1->tv_usec -= 1000000;
    }
}

static void
timevalsub(t1, t2)
    struct timeval *t1, *t2;
{

    t1->tv_sec -= t2->tv_sec;
    t1->tv_usec -= t2->tv_usec;
    if (t1->tv_usec < 0) {
        t1->tv_sec--;
        t1->tv_usec += 1000000;
    }
}

static int
timevalle(t1, t2)
    struct timeval *t1, *t2;
{
    return (t1->tv_sec == t2->tv_sec) ? (t1->tv_usec <= t2->tv_usec) :
           (t1->tv_sec <= t2->tv_sec);
}
