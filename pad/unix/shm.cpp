/*
 > File:            shm.C
 > Purpose:         Shared memory interface
 > Author:          Jonathan Meyer, Mar  2 1992
 >                  Modified by Ben Bederson 9/24/95.
 >                     C++'ize, and made it portable
 */

/*
 * Shm.c:
 *
 * Interface to X11 MIT-SHM shared memory extension.
 *
 * This provides wrapper-procedures for the MIT-SHM library.
 *
 * Jonathan Meyer, 4 January 1992
 *
 */

#include <stdlib.h>
#include <stdio.h>
#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

#ifdef CYGWIN
#include <cygwin/ipc.h>
#include <cygwin/shm.h>
#else
#include <sys/ipc.h>
#include <sys/shm.h>
#endif
#include <X11/extensions/XShm.h>
#include "shm.h"

/*
   XImage *ShmCreateImage(display, width, height, depth, format)
     Attempts to create an XImage which uses shared memory. Returns the XImage
     if it succeeds, or NULL if it fails. The ximage->data field will point to
     the shared memory. An ximage returned by ShmCreateImage should be freed
     using ShmFreeImage. If format is 0, the appropriate format for the display
     is used.

   Status ShmFetchImage(display, drawable, shm_ximage,
			x, y, plane_mask, test_for_errors)
     Calls XShmGetImage to get the contents of a drawable and return it as
	 a shared ximage. Note that on some X servers, XShmGetImage only works
	 on Windows (not on Pixmaps). If test_for_errors is -True-, the X
     Error handler will be locally redefined to trap errors, and status will
     be -false- if an error occurs;

   Status ShmPutImage(display, drawable, gc, shm_ximage,
                      src_x, src_y, dst_x, dst_y, width, height, wait_mode)
     Copies a shared memory ximage as returned by ShmCreateImage
     onto a drawable. wait_mode is either:

          SHM_PUT_COMPLETION_IGNORE (False)
            The procedure returns as soon as it has put the image. It does
            not guarantee that the X Server has finished copying from the
			shared memory segment.
			
          SHM_PUT_COMPLETION_WAIT   (True)
			The procedure waits for the server to finish copying from the
            shared memory segment before returning.

          SHM_PUT_COMPLETION_SEND_EVENT (implementation defined)
			The procedure asks the server to send a completion event, but
            does not wait for the completion event to arrive. You should
            use ShmPutCompletionWait to wait for the event yourself.

		  SHM_PUT_TEST
			This locally redefines the X Error handler to trap any errors
            that occur during the put. If an error occurs then the status
            result of the put will be -False-

   ShmPutCompletionWait(display, drawable, shm_ximage)
     Returns when a Shm 'completion' event has been received for
     the specified drawable and shm_ximage. You should call this after you
     have done a ShmPut with a wait mode of SHM_PUT_COMPLETION_SEND_EVENT.

   ShmFreeImage(display, shm_ximage)
     Releases the shared memory segment and the frees local memory used by a
     shared ximage.

   Pixmap ShmAsPixmap(display, drawable, shm_ximage)
     Allocates an X Server pixmap which uses the same memory as the shm_ximage.
     Returns the pixmap as the result.
*/

static int shm_base;

typedef int (*Handler)(Display *, XErrorEvent *);

static int     (*orig_handler)(Display *, XErrorEvent *);
static Display *display;
static int      code;
static Bool     called;

static int handler(Display *disp, XErrorEvent *event)
{
				// If this is not the right sort of error, 
				//   call the old handler.
    if (disp != display || event->request_code != code ) {
	return orig_handler( disp, event );
    }
    
				// We did trap an error - flag it and return
    called = 1;
    return 0;
}

//
// Setup procedure to initialize a trap for specific Xlib major/minor code
//
void ErrorTrapSetup(Display *disp, int major_code, int )
{
				// Minor code is ignored for the moment
    called = 0;
    display = disp;
    code = major_code;
    orig_handler = XSetErrorHandler(handler);
}

//
// Procedure to test if there was an error - also resets old error handler
//
Status ErrorTrapSprung(int sync)
{
    if (!called) {
	if (sync) {
	    XSync( display, False );
	    XSync( display, False );
	}
    }
    XSetErrorHandler( orig_handler );
    return called;
}

XImage *ShmCreateImage(Display *display, int width, int height,
		       int depth, int format)
{
    int major_opcode, unused;
    if (XQueryExtension(display, "MIT-SHM", &major_opcode, &shm_base, &unused)) 
      {
	  XImage         *shm_image;
	  XShmSegmentInfo *shminfo;
	  Status          status;
	  unsigned int    image_size;
	
	  shminfo = (XShmSegmentInfo*)malloc(sizeof(XShmSegmentInfo));
	  if (!shminfo) return NULL;
	  if (!format)
	    format = ZPixmap;
				// Create shared memory XImage
	  if (!(shm_image = XShmCreateImage(display, NULL, depth,
					    format, NULL, shminfo,
					    width, height))) {
	      free(shminfo);
	      return NULL;
	  }
	  image_size = shm_image->bytes_per_line * shm_image->height;
	  
				// Get shared memory segment
	  shminfo->shmid = shmget(IPC_PRIVATE, image_size, IPC_CREAT | 0777);
	  if (shminfo->shmid < 0) {
	      XDestroyImage(shm_image);
	      free(shminfo);
	      return NULL;
	  }
	  
				// Map shared memory
	  shminfo->shmaddr = shm_image->data =
	    (char *)shmat(shminfo->shmid, 0, 0);
	  if (shminfo->shmaddr == (char *) -1) {
	      shm_image->data = NULL;
	      XDestroyImage(shm_image);
	      shmctl(shminfo->shmid, IPC_RMID, 0);
	      free(shminfo);
	      return NULL;
	  }
	  
	  shminfo->readOnly = False; // Might do a GetImage with this
	  
	  ErrorTrapSetup(display, major_opcode, X_ShmAttach);
	  
				// Register segment with X server
	  status = XShmAttach(display, shminfo);
	  
	  if (ErrorTrapSprung(1) || !status) {
	      XDestroyImage(shm_image);
	      shmdt(shminfo->shmaddr);
	      shmctl(shminfo->shmid, IPC_RMID, 0);
	      free(shminfo);
	      return NULL;
	  }
	  return shm_image;
      }
    else 
      {
	  // No doing... Return failure
	  return NULL;
      }
}

void ShmFreeImage(Display *display, XImage *shm_image)
{
    if (shm_image && shm_image->obdata) 
      {
	  XShmSegmentInfo *shminfo = (XShmSegmentInfo*)shm_image->obdata;
				// Free resources
	  if (display) {
	      if (!XShmDetach(display, shminfo)) {
		  fprintf(stderr, "XShmDetatch failed\n");
	      }
	  }
	  if (shmdt(shminfo->shmaddr) == -1) {
	      fprintf(stderr, "shmdt failed\n");
	  }
	  if (shmctl(shminfo->shmid, IPC_RMID, 0) == -1) {
	      fprintf(stderr, "shmctl failed\n");
	  }
	  XDestroyImage(shm_image);
	  free(shminfo);
      }
}

static Bool TestCompletion(Display *, XEvent *ev, XPointer clos)
{
    XShmCompletionEvent *event = (XShmCompletionEvent*)ev;
    XShmCompletionEvent *expected = (XShmCompletionEvent*)clos;
    return ((event->type == expected->type
	     && event->drawable == expected->drawable
	     && event->shmseg == expected->shmseg) ? True : False);
}

//
// This procedure implements waiting for a completion event after a put
//
void ShmPutCompletionWait(Display *display, Drawable drawable,
			  XImage *shm_image)
{
    XShmSegmentInfo *shminfo = (XShmSegmentInfo*)shm_image->obdata;
    XEvent expected_ev, result_ev;
    XShmCompletionEvent *expected = (XShmCompletionEvent*)&expected_ev;
    expected->type = shm_base + ShmCompletion;
    expected->drawable = drawable;
    expected->shmseg = shminfo->shmseg;
    XIfEvent(display, &result_ev, TestCompletion, (XPointer)expected);
}

Status ShmPutImage(Display *display, Drawable drawable, GC gc,
		   XImage *shm_image, int src_x, int src_y, int dst_x,
		   int dst_y, int width, int height, int wait_mode)
{
    
    int look_for_err = False, status;
				// Call X routine to do a CopyArea 
				//    to copy the image to drawable.
    if (wait_mode == SHM_PUT_TEST) {
	int major_opcode, unused;
	XQueryExtension(display, "MIT-SHM", &major_opcode, &shm_base, &unused);
	ErrorTrapSetup(display, major_opcode, X_ShmPutImage);
	look_for_err = True;
    }
    status = XShmPutImage(display, drawable, gc, shm_image,
			  src_x, src_y, dst_x, dst_y,
			  width, height, wait_mode ? True : False);
    
				// Test if we need to sync with the server
    if (look_for_err && ErrorTrapSprung(1)) {
	status = False;
    } else if (status && wait_mode && wait_mode != SHM_PUT_COMPLETION_SEND_EVENT) {
	ShmPutCompletionWait(display, drawable, shm_image);
    }

    return status;
}

Pixmap ShmAsPixmap(Display *display, Drawable drawable, XImage *shm_image)
{
    XShmSegmentInfo *shminfo = (XShmSegmentInfo*)shm_image->obdata;
    Pixmap p;
    int unused;
    Bool pixmaps;
    XShmQueryVersion(display, &unused, &unused, &pixmaps);
    if (pixmaps) {
	p = XShmCreatePixmap(display, drawable, shminfo->shmaddr, shminfo,
			     shm_image->width, shm_image->height, shm_image->depth);
    } else {
	p = None;
    }

    return(p);
}


Status ShmFetchImage(Display  *display, Drawable drawable,
		     XImage *shm_image, int x, int y, unsigned int plane_mask,
		     Bool test)
{
    Status status;
    if (test) {
	int major_opcode, unused;
	XQueryExtension(display, "MIT-SHM", &major_opcode, &unused, &unused);
	ErrorTrapSetup(display, major_opcode, X_ShmGetImage);
    }
    
    status = XShmGetImage(display, drawable, shm_image, x, y, plane_mask);
    
    if (test && ErrorTrapSprung(1)) {
	status = 0;
    }
    
    return(status);
}
