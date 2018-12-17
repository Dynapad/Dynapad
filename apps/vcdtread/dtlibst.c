/* $Id: dtlibst.c,v 1.1.1.1 2005/10/09 04:28:49 stanonik Exp $ */

/*
 * Copyright 2002, 2003 Mitsubishi Electric Research Laboratories.
 * All Rights Reserved.
 * 
 * Permission to use, copy and modify this software and its
 * documentation for educational, research and non-profit
 * purposes, without fee, and without a written agreement is
 * hereby granted, provided that the above copyright notice and
 * the following three paragraphs appear in all copies, and the
 * software and documentation is not redistributed.
 * 
 * To request Permission to incorporate this software into
 * commercial products contact MERL - Mitsubishi Electric
 * Research Laboratories, 201 Broadway, Cambridge, MA 02139.
 * 
 * IN NO EVENT SHALL MERL BE LIABLE TO ANY PARTY FOR DIRECT,
 * INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES,
 * INCLUDING LOST PROFITS, ARISING OUT OF THE USE OF THIS
 * SOFTWARE AND ITS DOCUMENTATION, EVEN IF MERL HAS BEEN ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGES.
 * 
 * MERL SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT
 * NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
 * FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE PROVIDED
 * HEREUNDER IS ON AN "AS IS" BASIS, AND MERL HAS NO OBLIGATIONS
 * TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
 * MODIFICATIONS.
 *
 * Author:
 * Sam Shipman
 * shipman@merl.com
 * 617.621.7520
 */

#include <stdlib.h>
#include <stddef.h>
#include <limits.h>
#include <stdio.h>
#include <dtlib.h>
#include "escheme.h"

#define USER                     0
#define NUMBER_OF_FRAMES_TO_READ 300
#define STATIC_TOUCH_THRESHOLD   40

static dt_user_id user_id = USER;
static dt_device dtdev;
static dt_wm_calibration_data caldata;
static dt_table *tp;
static dt_frame *fp;
static dt_event *ep;
static dt_io_error ioerr;


Scheme_Object *
sch_dtopen(int argc, Scheme_Object **argv)
{
    int i;

    /* Get a pointer to the DiamondTouch table information. Because
       the table status is initially not OK, dt_get_table will try
       to find a DiamondTouch device and initialize the dt_table
       struct accordingly.  It will look for a USB table first, then
       for a Serial table. */
    tp = dt_get_table(user_id);

    if (tp->status != DT_TABLE_STATUS_OK) {
        fprintf(stderr, "dtlibst: can't find DiamondTouch device\n");
        return scheme_false;
    }

    /* This program behaves differently, depending on whether we're
       using the serial hardware or the (newer) USB hardware.  If it's
       a serial table, we only read the data for one user (because the
       device for user 0 is a serial port that only returns data for
       one user, user 0). If it's a USB table, we read data for all
       users, because the device for user 0 returns data for all
       users (i.e., there's only one device per table). */

    /* Open the DiamondTouch device for user 0. */
    dtdev = dt_device_open(tp, &(tp->devname[0][0]));
    if (dtdev == DT_BAD_DEVICE) {
        fprintf(stderr,
                "dtlibst: can't open device \"%s\"\n",
                &(tp->devname[0][0]));
        return scheme_false;
    }

    fp = dt_frame_new(tp);         /* Make a dt_frame. */
    if (!fp) {
		fprintf(stderr, "assert fp\n");
		return scheme_false;
	}

    /* Initialize frame interpretation for users. */
    for (i = 0; i < DT_MAX_USERS; i++) {
        dt_fi_user_initialize((dt_user_id)i);
    }
    

    /* Set various frame interpretation modes, parameters, etc.  (This
       is by way of illustration, the defaults would be fine.) */
    for (i = 0; i < DT_MAX_USERS; i++) {
        dt_set_touch_threshold_is_dynamic((dt_user_id)i, 0);
        dt_set_static_x_touch_threshold((dt_user_id)i, STATIC_TOUCH_THRESHOLD);
        dt_set_static_y_touch_threshold((dt_user_id)i, STATIC_TOUCH_THRESHOLD);
    }
    dt_fi_set_mode(DT_FI_MODE_NORMAL);

    /* Make a dt_event. */
    ep = (dt_event *)calloc(sizeof(dt_event), 1);
    if (!ep) {
		fprintf(stderr, "assert ep\n");
		return scheme_false;
	}

	return scheme_true;
}

Scheme_Object *
sch_dtread(int argc, Scheme_Object **argv)
{
	int user;
	Scheme_Object *suser, *sx, *sy, *ssig;

	if (!SCHEME_INTP(argv[0]))
		scheme_wrong_type("sch_dtread", "user integer", 0, argc, argv);

	user = SCHEME_INT_VAL(argv[0]);

    /* Read and process some frames. */

        /* For a serial table, we're only reading user 0's frames, and
           we must set the user id in the frame.  A USB table will
           give us the user_id in the frame it returns. */
		do {
			fp->user_id = 0;

			/* Read a frame from the device. */
			if (!dt_device_read(dtdev, tp, fp, &ioerr)) {
			    fprintf(stderr, "dtlibst: dt_device_read failed\n");
			    return scheme_false;
			}
		 } while (user != (int)fp->user_id);

        /* Start printing a line of information about this frame. */
		/* print user id */
        /*printf(" %1d:", (int)fp->user_id);*/
		suser = scheme_make_integer((int)fp->user_id);
        /* Print first ten (x) signal-strength values. */
		/*
        for (j = 0; j < 10; j++) {
            printf(" %3d", fp->x[j]);
        }
		*/

        /* Interpret the frame, producing a dt_event. */
        dt_fi_interpret_frame(ep, fp);

        /* Print the point of maximum signal strength, and whether the
           user is touching the DiamondTouch surface. */
		/*
        printf(" [%4d,%4d] %d", (int)ep->max_point.x,
               (int)ep->max_point.y, (int)ep->touch);
	    */
		sx = scheme_make_integer((int)ep->max_point.x);
		sy = scheme_make_integer((int)ep->max_point.y);
		ssig = scheme_make_integer((int)ep->touch);

		return scheme_make_pair(suser,
			scheme_make_pair(sx,
				scheme_make_pair(sy,
					scheme_make_pair(ssig, scheme_null))));
}

Scheme_Object *
sch_dtclose(int argc, Scheme_Object **argv)
{
    int i;
    /* Clean up. */
    free(ep);
    dt_frame_free(fp);
    for (i = 0; i < DT_MAX_USERS; i++) {
        dt_me_uninitialize(i);
        dt_fi_user_uninitialize((dt_user_id)i);
    }

    if (!dt_device_close(tp, dtdev)) {
        fprintf(stderr, "dtlibst: dt_device_close failed\n");
        return scheme_false;
    }

    return scheme_true;
}
