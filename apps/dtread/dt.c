/* $Id: dt.c,v 1.1.1.1 2005/10/09 04:28:50 stanonik Exp $ */

/*
 * Copyright 2001, 2002 Mitsubishi Electric Research Laboratories.
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

/*
 * dt.c
 * 
 * DiamondTouch basic I/O support.
 *
 * This file contains (a little) Windows-dependent code.
 *
 */

/* This is needed because DT_X and DT_Y are used. */
#define DT_LIB_CORE 1

#include <stdlib.h>
#include "dt.h"
#include "dtusbio.h"

static
int
dt_table_init(dt_table *tp);


/*
 * Static Data
 */

/* The one and only table (shared among all users; read-only after
   initialization). */
static dt_table table = { 0, DT_HW_IF_SERIAL, 0, DT_X, DT_Y, };


/*
 * Exported Functions
 */

#if 0
void
dt_table_set_hw_if_type(dt_hw_if_type hw_if_type)
{
    table.hw_if_type = hw_if_type;
}
#endif

dt_table *
dt_get_table(dt_user_id id)
{
    /* We only support one table so far. */
    if (table.status != DT_TABLE_STATUS_OK) {
        if (dt_table_init(&table)) {
            table.status = DT_TABLE_STATUS_OK;
        }
        else {
            table.status = 0;
        }
    }
    return &table;
}


static
int
dt_table_init(dt_table *tp)
{
    int r;

    /* Look for usb table first */
    r = dt_usb_table_init(tp);
    if (r) return r;
}


/* Note that the dt_signal arrays are uninitialized. */
dt_frame *
dt_frame_new(dt_table *table)
{
    dt_frame *fp;

    fp = (dt_frame *)malloc(sizeof(dt_frame));
    if (!fp) return 0;

#ifndef DT_FIRMWARE
    fp->user_id = 0;
#endif /* !DT_FIRMWARE */
#ifdef DT_DYNAMIC_FRAME
    fp->time = 0;

    fp->x = (dt_signal *)malloc(sizeof(dt_signal) * (table->xn + table->yn));
    if (!fp->x) {
        free(fp);
        return 0;
    }
    fp->y = fp->x + table->xn;
#endif /* DT_DYNAMIC_FRAME */
    return fp;
}


void
dt_frame_free(dt_frame *fp)
{
#ifdef DT_DYNAMIC_FRAME
    free(fp->x);
#endif /* !DT_DYNAMIC_FRAME */
    free(fp);
}


void
dt_frame_copy(dt_table *table, dt_frame *dst, dt_frame *src)
{
    short i;

#ifndef DT_FIRMWARE
    dst->user_id = src->user_id;
#endif /* DT_FIRMWARE */
    dst->time = src->time;
    for (i = 0; i < table->xn; i++) {
        dst->x[i] = src->x[i];
    }
    for (i = 0; i < table->yn; i++) {
        dst->y[i] = src->y[i];
    }
}

/* Note: dt_frame_zero defined under "Internal Functions" below. */


dt_device
dt_device_open(dt_table *tp, char *deviceName)
{
    dt_device dev;

    if (tp->hw_if_type == DT_HW_IF_USB) {
        dev = dt_usb_device_open(deviceName);
    }
    else {
        dev = DT_BAD_DEVICE;
    }
    return dev;
}


int
dt_device_close(dt_table *tp, dt_device device)
{
    if (tp->hw_if_type == DT_HW_IF_USB) {
        return dt_usb_device_close(device);
    }
    else {
        return 0;
    }
}


int
dt_device_read(dt_device device, dt_table *tp, dt_frame *fp[],
               dt_io_error *ioerrp)
{
    dt_io_error    rv;

    if (tp->hw_if_type == DT_HW_IF_USB) {
        rv = dt_usb_device_read(device, tp, fp);
    }
    else {
        rv = DT_IO_ERR_BAD_HW_IF_TYPE;
    }
    if (ioerrp) {
        *ioerrp = rv;
    }
    return rv == DT_IO_ERR_SUCCESS;
}


/*
 * Internal Functions
 */

/* "zero" is a misnomer, because we can't violate the assumption that
   all valid dt_signal values are greater than zero.  Actually, it's
   a bad assumption, but it's assumed.  That needs to be rooted out of
   the code, at which point this function's semantics can be made
   consistent with its name. */
void
dt_frame_zero(dt_table *table, dt_frame *fp)
{
    short i;

    for (i = 0; i < table->xn; i++) {
        fp->x[i] = 1;
    }
    for (i = 0; i < table->yn; i++) {
        fp->y[i] = 1;
    }
}
