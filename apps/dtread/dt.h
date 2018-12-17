/* $Id: dt.h,v 1.1.1.1 2005/10/09 04:28:50 stanonik Exp $ */

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
 * dt.h
 * 
 * DiamondTouch low-level support.
 *
 * This file contains ANSI C code.
 */
#ifndef DT_H
#define DT_H

#ifndef DT_LIB_API
#ifdef _WIN32
#define DT_LIB_API __stdcall
#else
#define DT_LIB_API
#endif /* _WIN32 */
#endif /* DT_LIB_API */

#ifndef DT_FIRMWARE

#ifndef DT_STATIC_FRAME
#define DT_DYNAMIC_FRAME
#endif /* !DT_STATIC_FRAME */

#include <assert.h>
#ifndef ASSERT
#define ASSERT assert
#endif /* !ASSERT */

#endif /* !DT_FIRMWARE */


/* Maximum signal strength value.
 *
 * During each frame, the table hardware measures the strength of the
 * signal received via the chair (or other user contact) from each
 * antenna.  The signal strength value is in the range [1, 255].
 * (Zero is reserved as the frame separator.)
 */
#define DT_MAX_SIGNAL  255

/*
 * Maximum users supported.
 */
#define DT_MAX_USERS 4

/*
 * Hardware Interface Types (values for dt_hw_if_type)
 * (Yes, I know USB is a serial bus...)
 */
#define DT_HW_IF_SERIAL   0
#define DT_HW_IF_USB      1

/*
 * Table Status
 */
#define DT_TABLE_STATUS_OK 1

/*
 * Maximum length of a device name.
 * FIXME: This should be system dependent.
 */
#define DT_MAX_DEVNAME_LENGTH   255

/*
 * Table I/O Errors
 */
#define DT_IO_ERR_SUCCESS         0
#define DT_IO_ERR_BAD_HW_IF_TYPE  1
#define DT_IO_ERR_FAILURE         2
#define DT_IO_ERR_TIMED_OUT       3
#define DT_IO_ERR_LOST_SYNC       4

/* The value of type dt_device that indicates an invalid device. */
/* INVALID_HANDLE_VALUE */
#define DT_BAD_DEVICE    (-1)

#if !defined DT_DYNAMIC_FRAME || defined DT_LIB_CORE

/* Dimensions of the table.
 *
 * These dimensions are defined for various low-level uses and should
 * not be used by application software, which should consult the
 * dt_table struct for the actual dimensions of the table.
 *
 * The dimensions defined here can be considered as the maximum
 * dimensions of a table (for the current software release).
 *
 * This is the "hardware" resolution of the table, i.e., the actual
 * number of antennas in the horizontal and vertical dimensions.  This
 * is the table's resolution without interpolation (i.e., "raw").
 */
#define DT_X 160
#define DT_Y 96

#endif /* !DT_DYNAMIC_FRAME || DT_LIB_CORE */

/* DiamondTouch device handle (in the generic sense). */
typedef int dt_device;

/* I/O error type */
typedef unsigned long dt_io_error;

/* table hardware interface type */
typedef unsigned char dt_hw_if_type;

/* table ID */
typedef unsigned char dt_table_id;

/* user ID */
typedef unsigned char dt_user_id;

/* a signal-strength value */
typedef unsigned char dt_signal;

/* an index of an antenna (either vertical or horizontal) */
typedef unsigned char dt_antenna;

/* timestamp */
typedef unsigned long dt_timestamp;

/* DiamondTouch table data */
typedef struct {
    dt_table_id   id;    /* table id */
    dt_hw_if_type hw_if_type; /* hardware interface type */
    short         status;
    short         xn;    /* number of columns */
    short         yn;    /* number of rows */
    char          devname[DT_MAX_USERS][DT_MAX_DEVNAME_LENGTH + 1];
} dt_table;


/* Type of a frame of data from the DiamondTouch table.  Consists of a
   user ID, a timestamp, and signal strength values for each column
   (x[i]) and row (y[i]). */
typedef struct {
#ifndef DT_FIRMWARE
    dt_user_id   user_id;    /* user_id for this frame */
#endif /* !DT_FIRMWARE */
    dt_timestamp time;       /* timestamp (milliseconds) */
#ifdef DT_DYNAMIC_FRAME
    dt_signal    *x; /* pointer to first element of array of dt_signal */
    dt_signal    *y; /* pointer to first element of array of dt_signal */
#else /* DT_DYNAMIC_FRAME */
    dt_signal    x[DT_X]; /* array of dt_signal */
    dt_signal    y[DT_Y]; /* array of dt_signal */
#endif /* DT_DYNAMIC_FRAME */
} dt_frame;


/*
 * Exported Function Prototypes
 */
#if 0
void
DT_LIB_API
dt_table_set_hw_if_type(dt_hw_if_type hw_if_type);
#endif

dt_table *
DT_LIB_API
dt_get_table(dt_user_id id);

dt_frame *
DT_LIB_API
dt_frame_new(dt_table *table);

void
DT_LIB_API
dt_frame_free(dt_frame *fp);

void
DT_LIB_API
dt_frame_copy(dt_table *table, dt_frame *dst, dt_frame *src);

dt_device
DT_LIB_API
dt_device_open(dt_table *tp, char *deviceName);


int
DT_LIB_API
dt_device_close(dt_table *tp, dt_device device);


int
DT_LIB_API
dt_device_read(dt_device device, dt_table *tp, dt_frame *fp[],
               dt_io_error *ioerrp);

/*
 * Internal Function Prototypes
 */
#ifdef DT_LIB_CORE
void
DT_LIB_API
dt_initialize(void);

static
int
DT_LIB_API
dt_table_init(dt_table *tp);

void
DT_LIB_API
dt_frame_zero(dt_table *table, dt_frame *f);
#endif /* DT_LIB_CORE */

#endif /* DT_H */
