/* $Id */

/*
 * Copyright 2002 Mitsubishi Electric Research Laboratories.
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
 *
 * (See also below, for additional author(s).)
 */


#include "dt.h"
#include "dtusbio.h"
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/time.h>


static
int
getUsbDeviceName(char *device)
{
    strcpy(device, "/dev/DiamondTouch");
    return 1;
}


dt_device
DT_LIB_API
dt_usb_device_open(char *deviceName)
{
    int fd;

    if ((fd = open(deviceName, O_RDONLY)) < 0) {
        perror("open");
        return DT_BAD_DEVICE;
    }
    return fd;
}


int
DT_LIB_API
dt_usb_device_close(dt_device device)
{
    return (int)close(device);
}

typedef struct {
  unsigned int frame_number;
  unsigned char  user[4][224];
} dt_raw_frame;

dt_io_error
DT_LIB_API
dt_usb_device_read(dt_device h, dt_table *tp, dt_frame *fpa[])
{
    int cnt;
    dt_frame *fp;
    dt_raw_frame raw_frame;
    int i, user;
    struct timeval tv;

    /* read record with header */
    cnt = read(h, &raw_frame, sizeof(raw_frame));
    if (cnt != (sizeof(dt_raw_frame))) {
        return DT_IO_ERR_FAILURE;
    }


    for (user = 0; user < DT_MAX_USERS; user++) {
	fp = fpa[user];

        fp->user_id = user;

        for (i = 0; i < tp->xn; i++) {
	    fp->x[i] = raw_frame.user[user][tp->xn - i - 1]; /* reverse */
            if (fp->x[i] == 0) fp->x[i] = 1;   /* min sig strength is 1 */
        }

        for (i = 0; i < tp->yn; i++) {
	    fp->y[i] = raw_frame.user[user][tp->xn + i]; /* DON'T reverse */
            if (fp->y[i] == 0) fp->y[i] = 1;   /* min sig strength is 1 */
        }
        gettimeofday(&tv, 0);

        /* This wraps around every 2^32 milliseconds (about 49.71 days). */
        fp->time = (dt_timestamp)(tv.tv_sec*1000 + tv.tv_usec/1000);
    }
    return DT_IO_ERR_SUCCESS;
}

int
dt_usb_table_init(dt_table *tp)
{
    int r;
    r = getUsbDeviceName(&(tp->devname[0][0]));
    if (r) {
        tp->id = 0;
        tp->hw_if_type = DT_HW_IF_USB;
        tp->status = DT_TABLE_STATUS_OK; /* until proven guilty */
#if 0
        tp->xn = 160;
#endif
        tp->xn = 128;
        tp->yn = 96;
    }
    return r;
}
