#ifndef SHM_H
#define SHM_H 1

#include "../generic/defs.h"


extern XImage *
ShmCreateImage(Display *display, int width, int height, int depth, int format);

void 
ShmFreeImage(Display *display, XImage *shm_image);

void 
ShmPutCompletionWait(Display *display, Drawable drawable, XImage *shm_image);

Status 
ShmPutImage(Display *display, Drawable drawable, GC gc,
	    XImage *shm_image, int src_x, int src_y, int dst_x,
	    int dst_y, int width, int height, int wait_mode);

Pixmap 
ShmAsPixmap(Display *display, Drawable drawable, XImage *shm_image);

Status 
ShmFetchImage(Display  *display, Drawable drawable,
	      XImage *shm_image, int x, int y, unsigned int plane_mask,
	      Bool test);

#define SHM_PUT_COMPLETION_IGNORE 0       // Don't wait for the server to finish
#define SHM_PUT_COMPLETION_WAIT 1         // Don't return until server finishes
#define SHM_PUT_COMPLETION_SEND_EVENT 2   // Send a completion event
#define SHM_PUT_TEST 4                    // Looks for XErrors

#endif
