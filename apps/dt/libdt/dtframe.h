/* $Id: dtframe.h,v 1.1.1.1 2005/10/09 04:28:49 stanonik Exp $ */

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
 * dtframe.h
 * 
 * DiamondTouch frame interpreter.
 *
 * This file contains ANSI C code.
 */
#ifndef DTFRAME_H
#define DTFRAME_H

/*
 * Exported Macro Definitions
 */

/* Table coordinate interpolation factor.
 *
 * This is the number of discrete values interpolated between raw
 * table coordinates.  E.g., a table with a raw resolution of 160 x 96
 * would have an interpolated resolution, using an interpolation
 * factor of 16, of 2560 x 1536.
 *
 * It must be even.
 */
#define DT_INTER     16

/*
 * Frame interpretation modes.
 */
#define DT_FI_MODE_NORMAL            0
#define DT_FI_MODE_FINE_POSITIONING  1
#define DT_FI_MODE_ANTI_NOISE        2

#define DT_MAX_TOUCH_AREAS    160

/*
 * Internal Macro Definitions
 */
#ifdef DT_LIB_CORE
/*
 * Quality of touch.
 */
#define DT_TOUCH_NONE   0
#define DT_TOUCH        1

/* When to do median filtering. */
#define DT_MF_NEVER  0
#define DT_MF_ADAPT  1
#define DT_MF_ALWAYS 2

#define FRAME_HISTORY_LEN 3

#endif /* DT_LIB_CORE */


/*
 * Exported Data Types
 */

/* Coordinates of a(n interpolated) point. */
typedef struct {
    unsigned short    x;
    unsigned short    y;
} dt_point;

/* Coordinates of a point in raw (uninterpolated) coordinates. */
typedef struct {
    unsigned char    x;
    unsigned char    y;
} dt_raw_point;


/* dt_event */
typedef struct {
    dt_point       max_point;       /* point with max signal (interpolated)*/
    dt_raw_point   box_ul;          /* bounding box upper-left corner */
    dt_raw_point   box_lr;          /* bounding box lower-right corner */
    dt_timestamp   time;            /* timestamp */
    unsigned char  touch;           /* true if user is touching */
#ifndef DT_FIRMWARE
    dt_raw_point   max_raw;         /* point with max signal in raw coords */
    dt_user_id     user_id;         /* user id */
#endif /* !DT_FIRMWARE */
} dt_event;

#if 0
/* dt_touch_area */
typedef struct {
    dt_antenna    first;    /* first antenna in area touched */
    dt_antenna    max;      /* antenna with max signal strength in
                               area touched */
    dt_antenna    last;     /* last antenna in area touched */
    dt_signal     sfirst[3]; /* signal strength values for antennas
                               centered on first */
    dt_signal     smax[3];   /* signal strength values for antennas
                               centered on max */
    dt_signal     slast[3];  /* signal strength values for antennas
                               centered on last */
} dt_touch_area;

/* dt_touch_list */
typedef struct {
    dt_antenna       n;        /* number of distinct touched areas */
    dt_antenna       max;      /* area with max signal strength */
    dt_touch_area    touch[MAX_TOUCH_AREAS];
} dt_touch_area_list;

/* dt_touch */
typedef struct {
    dt_user_id       user_id;    /* user id */
    unsigned char    touch;      /* true if user is touching */
    dt_timestamp     time;       /* timestamp */
    dt_touch_list    x;          /* touch area list for vertical antennas */
    dt_touch_list    y;          /* touch area list for horizontal antennas */
} dt_touch;

#endif

/*
 * Internal Data Types
 */
#ifdef DT_LIB_CORE

/* frame history item */
typedef struct {
    dt_frame     *fp;
#ifndef DT_FIRMWARE
    int           ytouch;
    dt_point      max_point;
    dt_raw_point  max_raw;
    dt_raw_point  box_ul;
    dt_raw_point  box_lr;
#endif /* !DT_FIRMWARE */
} dt_fh_item;

/* frame history */
typedef struct {
    short       i;                        /* index of next item to store */
    dt_fh_item fhi[FRAME_HISTORY_LEN];
} dt_frame_history;

/* frame interpretation per-user information. */
typedef struct {
    dt_table          *table;
    /* hysteresis stuff */
    dt_point           rpl;
    dt_point           ipl;
    dt_point           rpbl;
    dt_point           ipbl;
    /* median filtering stuff */
    dt_frame_history   frame_hist;
    unsigned char      is_median_filtering;
    unsigned char      used_median_filtering; /* last time */
    /* adaptive touch threshold stuff */
    unsigned char      is_dynamic_touch_threshold;
    dt_signal          x_st_th;    /* x static touch threshold */
    dt_signal          y_st_th;    /* y static touch threshold */
    dt_signal          x_dy_th;    /* x dynamic touch threshold */
    dt_signal          y_dy_th;    /* y dynamic touch threshold */
    dt_signal          x_mean;
    dt_signal          y_mean;
    dt_signal          x_max;
    dt_signal          y_max;
#if 0
    dt_signal          x_mean_max;
    dt_signal          y_mean_max;
#endif
    dt_stats           x_stats;
    dt_stats           y_stats;
    dt_stats           x_stats_sorted;
    dt_stats           y_stats_sorted;
    dt_signal          x_sorted[DT_X];    /* FIXME: dynamic? */
    dt_signal          y_sorted[DT_Y];    /* FIXME: dynamic? */
    unsigned short     xk;
    unsigned short     yk;
    unsigned short     xtmax;
    unsigned short     ytmax;
    /* The user ID (an optimization). */
    dt_user_id         id;
} dt_fi_user;

#endif /* DT_LIB_CORE */


/*
 * Exported Function Prototypes
 */

short
DT_LIB_API
dt_get_touch_threshold_is_dynamic(dt_user_id user_id);

void
DT_LIB_API
dt_set_touch_threshold_is_dynamic(dt_user_id user_id,
                                     short is_dynamic);

dt_signal
DT_LIB_API
dt_get_static_x_touch_threshold(dt_user_id user_id);

void
DT_LIB_API
dt_set_static_x_touch_threshold(dt_user_id user_id, dt_signal new_threshold);

dt_signal
DT_LIB_API
dt_get_static_y_touch_threshold(dt_user_id user_id);

void
DT_LIB_API
dt_set_static_y_touch_threshold(dt_user_id user_id, dt_signal new_threshold);

dt_signal
DT_LIB_API
dt_get_x_touch_threshold(dt_user_id user_id);

dt_signal
DT_LIB_API
dt_get_y_touch_threshold(dt_user_id user_id);

dt_signal
DT_LIB_API
dt_get_noise_level(void);

int
DT_LIB_API
dt_fi_get_mode(void);

void
DT_LIB_API
dt_fi_set_mode(int mode);

void
DT_LIB_API
dt_fi_user_initialize(dt_user_id user_id);

void
DT_LIB_API
dt_fi_user_uninitialize(dt_user_id user_id);

#ifdef DT_FIRMWARE
void
DT_LIB_API
dt_fi_interpret_frame(dt_event *mp, dt_frame *fp, dt_user_id user_id);
#else
void
DT_LIB_API
dt_fi_interpret_frame(dt_event *mp, dt_frame *fp);
#endif /* DT_FIRMWARE */


/*
 * Internal Function Prototypes
 */
#ifdef DT_LIB_CORE

dt_fi_user *
DT_LIB_API
dt_fi_user_lookup(dt_user_id user_id);

unsigned char
DT_LIB_API
dt_fi_get_hysteresis(void);

void
DT_LIB_API
dt_fi_set_hysteresis(unsigned char new_hysteresis);

void
DT_LIB_API
dt_get_filtered_frame(dt_frame *fp, dt_user_id user_id);

unsigned char
DT_LIB_API
dt_fi_get_median_filter_when(void);

void
DT_LIB_API
dt_fi_set_median_filter_when(unsigned char new_when);

#ifndef DT_FIRMWARE
void
DT_LIB_API
dt_frame_spatial_median_filter(dt_frame *dfp, dt_frame *sfp);
#endif /* !DT_FIRMWARE */

#endif /* DT_LIB_CORE */

#endif /* DTFRAME_H */
