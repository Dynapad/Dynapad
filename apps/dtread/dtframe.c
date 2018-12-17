/* $id: dtframe.c,v 1.5 2002/04/17 07:32:07 shipman Exp $ */

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
 * dtframe.c
 * 
 * DiamondTouch frame interpreter
 *
 * This file contains ANSI C code.
 */

#define DT_LIB_CORE 1

#include <stdlib.h>
#include <math.h>
#include "dt.h"
#include "dtstats.h"
#include "dtframe.h"

#define DEFAULT_STATIC_TOUCH_THRESHOLD 60

/* User-specific data for frame interpretation. */
static dt_fi_user user[DT_MAX_USERS];

/* Default static thresholds. */
static dt_signal xTouchThreshold = DEFAULT_STATIC_TOUCH_THRESHOLD;
static dt_signal yTouchThreshold = DEFAULT_STATIC_TOUCH_THRESHOLD;

/* Current frame interpretation mode. */
static int fi_mode = DT_FI_MODE_NORMAL;

/* When to do median filtering. */
static unsigned char median_filter_when = DT_MF_NEVER;

/* True if hysteresis should be applied to max_point. */
static unsigned char hysteresis = 1;


/*
 * Static Function Prototypes
 */

static
unsigned short
dt_compute_threshold(int n, dt_signal *sig, dt_signal *sig_sorted,
                     dt_stats *stats, dt_stats *stats_sorted,
                     unsigned short *kp, unsigned short *tmaxp);

static
void
dt_update_dynamic_touch_threshold(dt_fi_user *up, dt_frame *fp);

static
unsigned short
interpolate(dt_signal *s, short n, short i);

static
void
dt_frame_history_init(dt_fi_user *up);

static
void
dt_frame_history_uninit(dt_fi_user *up);

static
void
dt_fi_user_add_frame_to_history(dt_fi_user *up, dt_frame *fp);

#ifndef DT_FIRMWARE
static
void
dt_median_filter_3(dt_frame *fpr, dt_fi_user *up, dt_frame *fp0,
                   dt_frame *fp1, dt_frame *fp2);

static
dt_signal
median3(dt_signal a, dt_signal b, dt_signal c);

static void
insertion_sort(int n, dt_signal *a);
#endif /* !DT_FIRMWARE */


/*
 * Exported Functions
 */

void
dt_fi_user_initialize(dt_user_id user_id)
{
    dt_fi_user *up;

    up = &user[user_id];
    up->id = user_id;
    up->table = dt_get_table(user_id);

    up->rpl.x = 0;
    up->rpl.y = 0;
    up->ipl.x = 0;
    up->ipl.y = 0;
    up->rpbl.x = 0;
    up->rpbl.y = 0;
    up->ipbl.x = 0;
    up->ipbl.y = 0;

    dt_frame_history_init(up);
    up->is_median_filtering = 0;
    up->used_median_filtering = 0;

    up->is_dynamic_touch_threshold = 0;
    up->x_st_th = xTouchThreshold;
    up->y_st_th = yTouchThreshold;
    up->x_dy_th = 0;
    up->y_dy_th = 0;
    up->x_mean = 0;
    up->y_mean = 0;
    up->x_max = 0;
    up->y_max = 0;
#if 0
    up->x_mean_max = 0;
    up->y_mean_max = 0;
#endif
    dt_stats_init(&up->x_stats);
    dt_stats_init(&up->y_stats);
    dt_stats_init(&up->x_stats_sorted);
    dt_stats_init(&up->y_stats_sorted);
}


void
dt_fi_user_uninitialize(dt_user_id user_id)
{
    dt_fi_user *up;

    up = &user[user_id];
    dt_frame_history_uninit(up);
}


short
dt_get_touch_threshold_is_dynamic(dt_user_id user_id)
{
    dt_fi_user *up;

    up = &user[user_id];
    return (up->is_dynamic_touch_threshold != 0);
}


void
dt_set_touch_threshold_is_dynamic(dt_user_id user_id,
                                          short is_dynamic)
{
    dt_fi_user *up;

    up = &user[user_id];
    up->is_dynamic_touch_threshold = (is_dynamic != 0);
}


dt_signal
dt_get_x_touch_threshold(dt_user_id user_id)
{
    dt_fi_user *up;

    up = &user[user_id];
    return up->is_dynamic_touch_threshold ? up->x_dy_th : up->x_st_th;
}


dt_signal
dt_get_y_touch_threshold(dt_user_id user_id)
{
    dt_fi_user *up;

    up = &user[user_id];
    return up->is_dynamic_touch_threshold ? up->y_dy_th : up->y_st_th;
}


dt_signal
dt_get_static_x_touch_threshold(dt_user_id user_id)
{
    dt_fi_user *up;

    up = &user[user_id];
    return up->x_st_th;
}


void
dt_set_static_x_touch_threshold(dt_user_id user_id,
                                dt_signal new_threshold)
{
    dt_fi_user *up;

    up = &user[user_id];
    up->x_st_th = new_threshold;
}


dt_signal
dt_get_static_y_touch_threshold(dt_user_id user_id)
{
    dt_fi_user *up;

    up = &user[user_id];
    return up->y_st_th;
}


void
dt_set_static_y_touch_threshold(dt_user_id user_id,
                                dt_signal new_threshold)
{
    dt_fi_user *up;

    up = &user[user_id];
    up->y_st_th = new_threshold;
}


dt_signal
dt_get_noise_level()
{
    /* FIXME: not implemented. */
    return 0;
}


int
dt_fi_get_mode()
{
    return fi_mode;
}


void
dt_fi_set_mode(int mode)
{
    fi_mode = mode;

    switch (mode) {
    case DT_FI_MODE_NORMAL:
    default:
        dt_fi_set_median_filter_when(DT_MF_NEVER);
        dt_fi_set_hysteresis(1);
        break;
    case DT_FI_MODE_FINE_POSITIONING:
        dt_fi_set_median_filter_when(DT_MF_ADAPT);
        dt_fi_set_hysteresis(1);
        break;
    case DT_FI_MODE_ANTI_NOISE:
        dt_fi_set_median_filter_when(DT_MF_ALWAYS);
        dt_fi_set_hysteresis(1);
        break;
    }
}


#ifdef DT_FIRMWARE
void
dt_fi_interpret_frame(dt_event *ep, dt_frame *fp, dt_user_id user_id)
#else
void
dt_fi_interpret_frame(dt_event *ep, dt_frame *fp)
#endif /* DT_FIRMWARE */
{
#ifndef DT_FIRMWARE
    dt_user_id user_id;
#endif /* !DT_FIRMWARE */
    dt_signal xm;         /* x value with max signal */
    dt_signal x_sig_max;  /* max x signal value */
    dt_signal ym;         /* y value with max signal */
    dt_signal y_sig_max;  /* max y signal value */
    short i;
    short first;        /* first ant. s.t. signal crosses touch threshold */
    short last;         /* first ant. s.t. signal crosses touch threshold */
    dt_fi_user *up;
    dt_signal x_touch_thresh;
    dt_signal y_touch_thresh;
    dt_point rnew;
    dt_point inew;
    dt_raw_point max_raw;
    unsigned short diff;
    dt_timestamp timestamp = 0;
#ifdef DT_FIRMWARE
    dt_frame mf;        /* median-filtered frame */
#endif /* DT_FIRMWARE */
#ifndef DT_FIRMWARE
    dt_fh_item *fhip;
    dt_fh_item *cfhip;
#endif /* !DT_FIRMWARE */

#ifndef DT_FIRMWARE
    user_id = fp->user_id;
    timestamp = fp->time;
#endif /* !DT_FIRMWARE */

    up = &user[user_id];

    /* Add the current frame to the frame history for this user. */
    dt_fi_user_add_frame_to_history(up, fp);

    if (up->is_median_filtering) {
#ifdef DT_FIRMWARE
        fp = &mf;
#else /* DT_FIRMWARE */
        fp = dt_frame_new(up->table);
#endif /* DT_FIRMWARE */
        fp->user_id = user_id;
        fp->time = timestamp;
        dt_get_filtered_frame(fp, user_id);
    }

    ep->time = fp->time;
#ifndef DT_FIRMWARE
    ep->user_id = fp->user_id;
#endif /* DT_FIRMWARE */

    if (up->is_dynamic_touch_threshold) {
        dt_update_dynamic_touch_threshold(up, fp);
    }

    /* For now, only x and y thresholds, no finer granularity. */
    x_touch_thresh = dt_get_x_touch_threshold(user_id);
    y_touch_thresh = dt_get_y_touch_threshold(user_id);

    /*
     * Find the point at which the user is touching the surface.
     * (For mouse emulation, we only care about a single point.)
     * Also, find the bounding box.
     */
    first = -1;
    last = -1;
    x_sig_max = 0;                          /* find max of x (columns) */
    for (i = 0; i < up->table->xn; i++) {
        if (fp->x[i] >= x_touch_thresh) {
            if (first == -1) first = i;
            last = i;
        }
        if (fp->x[i] > x_sig_max) {
            x_sig_max = fp->x[i];
            xm = (unsigned char)i;
        }
    }
    if (first < 0) {
        first = xm;
        last = xm;
    }
    /* The default bounding box (x values). */
    ep->box_ul.x = (unsigned char)first;
    ep->box_lr.x = (unsigned char)last;

    first = -1;
    last = -1;
    y_sig_max = 0;                          /* find max of y (rows) */
    for (i = 0; i < up->table->yn; i++) {
        if (fp->y[i] >= y_touch_thresh) {
            if (first == -1) first = i;
            last = i;
        }
        if (fp->y[i] > y_sig_max) {
            y_sig_max = fp->y[i];
            ym = (unsigned char)i;
        }
    }
    if (first < 0) {
        first = ym;
        last = ym;
    }
    /* The default bounding box (y values). */
    ep->box_ul.y = (unsigned char)last;
    ep->box_lr.y = (unsigned char)first;

    /* The default max x. */
    max_raw.x = xm;
    ep->max_point.x = interpolate(fp->x, up->table->xn, xm);
    /* The default max y. */
    max_raw.y = ym;
    ep->max_point.y = interpolate(fp->y, up->table->yn, ym);
#ifndef DT_FIRMWARE
    ep->max_raw = max_raw;
#endif /* !DT_FIRMWARE */

#ifndef DT_FIRMWARE
    /* Update current history item for this frame with the "real"
       y info. */
    i = up->frame_hist.i - 1;
    if (i < 0) i += FRAME_HISTORY_LEN;
    cfhip = &up->frame_hist.fhi[i];
    /* If the ym value we assign here is non-zero, then at least one
       of the y antennas registered a touch. */
    cfhip->ytouch = (y_sig_max >= y_touch_thresh);
    cfhip->max_point = ep->max_point;
    cfhip->max_raw = ep->max_raw;
    cfhip->box_ul = ep->box_ul;
    cfhip->box_lr = ep->box_lr;
#endif /* !DT_FIRMWARE */

    /* Check the touch thresholds. */
    ep->touch = DT_TOUCH_NONE;
    if (x_sig_max >= x_touch_thresh) {
        if (y_sig_max >= y_touch_thresh) {
            ep->touch = DT_TOUCH;
        }
#ifndef DT_FIRMWARE
        else {
            /* If the current max y value doesn't cross the touch
               threshold, look at the previous y value. */
            i = up->frame_hist.i - 2;
            if (i < 0) i += FRAME_HISTORY_LEN;
            fhip = &up->frame_hist.fhi[i];

            if (fhip->ytouch) {
                /* It was above threshold last time around, that
                   should be good enough. */
                ep->touch = DT_TOUCH;
                max_raw.y = fhip->max_raw.y;
                ep->max_raw.y = max_raw.y;
                ep->max_point.y = fhip->max_point.y;
                ep->box_ul.y = fhip->box_ul.y;
                ep->box_lr.y = fhip->box_lr.y;
            }
        }
#endif /* !DT_FIRMWARE */
    }

    if (hysteresis) {
        rnew = ep->max_point;
        inew = ep->max_point;

        if (((up->ipl.x - 2) <= rnew.x) && (rnew.x <= (up->ipl.x + 2))
            && (up->ipl.y - 2 <= rnew.y) && (rnew.y <= (up->ipl.y + 2))) {

            if ((rnew.x != up->rpl.x) || (rnew.y != up->rpl.y)
                || (rnew.x != up->rpbl.x) || (rnew.y != up->rpbl.y)) {

                inew = up->ipl;
            }
        }

        up->rpbl = up->rpl;
        up->ipbl = up->ipl;
        up->rpl = rnew;
        up->ipl = inew;

        ep->max_point = inew;
    }
    else {
        /* Keep this information to decide when to do median
           filtering. (Keep rp[b]l so hysteresis can be toggled
           smoothly.) */
        up->rpbl = up->rpl;
        up->ipbl = up->ipl;
        up->ipbl = up->ipl;
        up->rpl = ep->max_point;
        up->ipl = ep->max_point;
    }

#ifndef DT_FIRMWARE
    if (up->is_median_filtering) {
        /* Free the frame we allocated earlier. */
        dt_frame_free(fp);
    }
#endif /* !DT_FIRMWARE */    

    up->used_median_filtering = up->is_median_filtering;
    /* Decide whether to do median filtering next time around. */
    if (median_filter_when == DT_MF_NEVER) {
        up->is_median_filtering = 0;
    }
    else if (median_filter_when == DT_MF_ALWAYS) {
        up->is_median_filtering = 1;
    }
    else {
        if (ep->touch == DT_TOUCH) {
            up->is_median_filtering = 0;
            /* Do median filtering when the current touch is close to the
               previous touch (or non-touch, it doesn't much matter). */
            if (up->ipbl.x > up->ipl.x) {
                diff = up->ipbl.x - up->ipl.x;
            }
            else {
                diff = up->ipl.x - up->ipbl.x;
            }
            if (diff < (DT_INTER / 2)) {
                if (up->ipbl.y > up->ipl.y) {
                    diff = up->ipbl.y - up->ipl.y;
                }
                else {
                    diff = up->ipl.y - up->ipbl.y;
                }
                if (diff < (DT_INTER / 2)) {
                    up->is_median_filtering = 1;
                }
            }
        }
        else {
            up->is_median_filtering = 0;
        }
    }
    return;
}


#if 0
void
dt_fi_interpret_frame_touch(dt_touch *ep, dt_frame *fp)
{
    dt_user_id user_id;
    dt_signal xm;         /* x value with max signal */
    dt_signal x_sig_max;  /* max x signal value */
    dt_signal ym;         /* y value with max signal */
    dt_signal y_sig_max;  /* max y signal value */
    short i;
    short first;        /* first ant. s.t. signal crosses touch threshold */
    short last;         /* first ant. s.t. signal crosses touch threshold */
    dt_fi_user *up;
    dt_signal x_touch_thresh;
    dt_signal y_touch_thresh;
    dt_point rnew;
    dt_point inew;
    dt_raw_point max_raw;
    unsigned short diff;
    dt_timestamp timestamp = 0;
    dt_fh_item *fhip;
    dt_fh_item *cfhip;

    user_id = fp->user_id;
    timestamp = fp->time;

    up = &user[user_id];

    /* Add the current frame to the frame history for this user. */
    dt_fi_user_add_frame_to_history(up, fp);

    if (up->is_median_filtering) {
        fp = dt_frame_new(up->table);
        fp->user_id = user_id;
        fp->time = timestamp;
        dt_get_filtered_frame(fp, user_id);
    }

    ep->time = fp->time;
    ep->user_id = fp->user_id;

    if (up->is_dynamic_touch_threshold) {
        dt_update_dynamic_touch_threshold(up, fp);
    }

    /* For now, only x and y thresholds, no finer granularity. */
    x_touch_thresh = dt_get_x_touch_threshold(user_id);
    y_touch_thresh = dt_get_y_touch_threshold(user_id);

    /*
     * Find the point at which the user is touching the surface.
     * (For mouse emulation, we only care about a single point.)
     * Also, find the bounding box.
     */
    first = -1;
    last = -1;
    x_sig_max = 0;                          /* find max of x (columns) */
    for (i = 0; i < up->table->xn; i++) {
        if (fp->x[i] >= x_touch_thresh) {
            if (first == -1) first = i;
            last = i;
        }
        if (fp->x[i] > x_sig_max) {
            x_sig_max = fp->x[i];
            xm = (unsigned char)i;
        }
    }
    if (first < 0) {
        first = xm;
        last = xm;
    }
    /* The default bounding box (x values). */
    ep->box_ul.x = (unsigned char)first;
    ep->box_lr.x = (unsigned char)last;

    first = -1;
    last = -1;
    y_sig_max = 0;                          /* find max of y (rows) */
    for (i = 0; i < up->table->yn; i++) {
        if (fp->y[i] >= y_touch_thresh) {
            if (first == -1) first = i;
            last = i;
        }
        if (fp->y[i] > y_sig_max) {
            y_sig_max = fp->y[i];
            ym = (unsigned char)i;
        }
    }
    if (first < 0) {
        first = ym;
        last = ym;
    }
    /* The default bounding box (y values). */
    ep->box_ul.y = (unsigned char)last;
    ep->box_lr.y = (unsigned char)first;

    /* The default max x. */
    max_raw.x = xm;
    ep->max_point.x = interpolate(fp->x, up->table->xn, xm);
    /* The default max y. */
    max_raw.y = ym;
    ep->max_point.y = interpolate(fp->y, up->table->yn, ym);
    ep->max_raw = max_raw;

    /* Update current history item for this frame with the "real"
       y info. */
    i = up->frame_hist.i - 1;
    if (i < 0) i += FRAME_HISTORY_LEN;
    cfhip = &up->frame_hist.fhi[i];
    /* If the ym value we assign here is non-zero, then at least one
       of the y antennas registered a touch. */
    cfhip->ytouch = (y_sig_max >= y_touch_thresh);
    cfhip->max_point = ep->max_point;
    cfhip->max_raw = ep->max_raw;
    cfhip->box_ul = ep->box_ul;
    cfhip->box_lr = ep->box_lr;

    /* Check the touch thresholds. */
    ep->touch = DT_TOUCH_NONE;
    if (x_sig_max >= x_touch_thresh) {
        if (y_sig_max >= y_touch_thresh) {
            ep->touch = DT_TOUCH;
        }
        else {
            /* If the current max y value doesn't cross the touch
               threshold, look at the previous y value. */
            i = up->frame_hist.i - 2;
            if (i < 0) i += FRAME_HISTORY_LEN;
            fhip = &up->frame_hist.fhi[i];

            if (fhip->ytouch) {
                /* It was above threshold last time around, that
                   should be good enough. */
                ep->touch = DT_TOUCH;
                max_raw.y = fhip->max_raw.y;
                ep->max_raw.y = max_raw.y;
                ep->max_point.y = fhip->max_point.y;
                ep->box_ul.y = fhip->box_ul.y;
                ep->box_lr.y = fhip->box_lr.y;
            }
        }
    }

    if (hysteresis) {
        rnew = ep->max_point;
        inew = ep->max_point;

        if (((up->ipl.x - 2) <= rnew.x) && (rnew.x <= (up->ipl.x + 2))
            && (up->ipl.y - 2 <= rnew.y) && (rnew.y <= (up->ipl.y + 2))) {

            if ((rnew.x != up->rpl.x) || (rnew.y != up->rpl.y)
                || (rnew.x != up->rpbl.x) || (rnew.y != up->rpbl.y)) {

                inew = up->ipl;
            }
        }

        up->rpbl = up->rpl;
        up->ipbl = up->ipl;
        up->rpl = rnew;
        up->ipl = inew;

        ep->max_point = inew;
    }
    else {
        /* Keep this information to decide when to do median
           filtering. (Keep rp[b]l so hysteresis can be toggled
           smoothly.) */
        up->rpbl = up->rpl;
        up->ipbl = up->ipl;
        up->ipbl = up->ipl;
        up->rpl = ep->max_point;
        up->ipl = ep->max_point;
    }

    if (up->is_median_filtering) {
        /* Free the frame we allocated earlier. */
        dt_frame_free(fp);
    }

    up->used_median_filtering = up->is_median_filtering;
    /* Decide whether to do median filtering next time around. */
    if (median_filter_when == DT_MF_NEVER) {
        up->is_median_filtering = 0;
    }
    else if (median_filter_when == DT_MF_ALWAYS) {
        up->is_median_filtering = 1;
    }
    else {
        if (ep->touch == DT_TOUCH) {
            up->is_median_filtering = 0;
            /* Do median filtering when the current touch is close to the
               previous touch (or non-touch, it doesn't much matter). */
            if (up->ipbl.x > up->ipl.x) {
                diff = up->ipbl.x - up->ipl.x;
            }
            else {
                diff = up->ipl.x - up->ipbl.x;
            }
            if (diff < (DT_INTER / 2)) {
                if (up->ipbl.y > up->ipl.y) {
                    diff = up->ipbl.y - up->ipl.y;
                }
                else {
                    diff = up->ipl.y - up->ipbl.y;
                }
                if (diff < (DT_INTER / 2)) {
                    up->is_median_filtering = 1;
                }
            }
        }
        else {
            up->is_median_filtering = 0;
        }
    }
    return;
}
#endif

/*
 * Internal Functions
 */

dt_fi_user *
dt_fi_user_lookup(dt_user_id user_id)
{
    return &user[user_id];
}


unsigned char
dt_fi_get_hysteresis()
{
    return hysteresis;
}


void
dt_fi_set_hysteresis(unsigned char new_hysteresis)
{
    hysteresis = new_hysteresis;
}


unsigned char
dt_fi_get_median_filter_when()
{
    return median_filter_when;
}


void
dt_fi_set_median_filter_when(unsigned char new_when)
{
    median_filter_when = new_when;
}


/* The only reason there's a user_id parameter is for compatibility
   with defined(DT_FIRMWARE). */
void
dt_get_filtered_frame(dt_frame *fp, dt_user_id user_id)
{
    dt_fi_user *up;

    up = dt_fi_user_lookup(user_id);
    dt_median_filter_3(fp, up,
                       up->frame_hist.fhi[0].fp,
                       up->frame_hist.fhi[1].fp,
                       up->frame_hist.fhi[2].fp);
}


#ifndef DT_FIRMWARE
void
dt_frame_spatial_median_filter(dt_frame *dfp, dt_frame *sfp)
{
    int i;
    dt_table *table;
    table = dt_get_table(sfp->user_id);

    // x
    dfp->x[0] = median3(sfp->x[table->xn - 1],
                        sfp->x[0],
                        sfp->x[1]);
    for (i = 1; i < (table->xn - 1); i++) {
        dfp->x[i] = median3(sfp->x[i-1], sfp->x[i], sfp->x[i+1]);
    }
    dfp->x[table->xn - 1] = median3(sfp->x[table->xn - 2],
                                    sfp->x[table->xn - 1],
                                    sfp->x[0]);
    // y
    dfp->y[0] = median3(sfp->y[table->yn - 1],
                        sfp->y[0],
                        sfp->y[1]);
    for (i = 1; i < (table->yn - 1); i++) {
        dfp->y[i] = median3(sfp->y[i-1], sfp->y[i], sfp->y[i+1]);
    }
    dfp->y[table->yn - 1] = median3(sfp->y[table->yn - 2],
                                    sfp->y[table->yn - 1],
                                    sfp->y[0]);
}
#endif


/*
 * Static Functions
 */

static unsigned short
dt_compute_threshold(int n, dt_signal *sig, dt_signal *sig_sorted,
                     dt_stats *stats, dt_stats *stats_sorted,
                     unsigned short *kp, unsigned short *tmaxp)
{
    int i;
    int j;
    int k;
    unsigned short thresh;
    unsigned short diff;
    unsigned short median;
    unsigned short mmavg;
    unsigned short bigdiff;
    unsigned short mdi;
    unsigned short xmin;

    /* Copy the signal values to the array to be sorted. */
    for (i = 0; i < n; i++) {
        sig_sorted[i] = sig[i];
    }
    /* Sort the array. */
    insertion_sort(n, sig_sorted);

    /* Find median. */
    median = (unsigned short)sig_sorted[n / 2];

    /* FIXME: FOR DISPLAY (delete later) and fix up display code. */
    *stats_sorted = *stats;

    /* Find min value that isn't wedged. */
    for (i = 0; i < 5; i++) {
        if (sig_sorted[i] > 1) break;
    }
    j = i;    /* j is min, modulo broken inputs */
    xmin = sig_sorted[j];

    mdi = n;
    for (i = 10; i < n; i++) {
        bigdiff = ((sig_sorted[i] - xmin) * 10) / i;

        diff = sig_sorted[i] - sig_sorted[i - 10];
        if (diff > bigdiff) {
            if ((diff - bigdiff) > 26) {
                mdi = i;
                break;
            }
        }
    }
    k = mdi - 1;
    *kp = k;

    if ((k + 1) < n) {
        mmavg = (sig_sorted[k + 1] - sig_sorted[k]) / 2;
    }
    else {
        mmavg = 0;
    }

    thresh = sig_sorted[k] + 1;

    if (mmavg > stats->sigma) {
        thresh += mmavg;
    }
    else {
        thresh += (unsigned short)((stats->sigma * 7) / 4);
    }

    return thresh;
}


static
void
dt_update_dynamic_touch_threshold(dt_fi_user *up, dt_frame *fp)
{
    int i;
    unsigned short thresh;

    ASSERT(up->is_dynamic_touch_threshold);

    /* X */

    /* update running averages */
    dt_stats_init(&up->x_stats);
    for (i = 0; i < up->table->xn; i++) {
        dt_stats_update(&up->x_stats, (long)fp->x[i]);
    }
    if (up->x_mean == 0) {
        up->x_mean = (dt_signal)up->x_stats.mean;
    }
    else {
        up->x_mean = (dt_signal)(((int)up->x_mean + up->x_stats.mean) / 2);
    }
    if (up->x_stats.max > up->x_max) {
        up->x_max = (dt_signal)up->x_stats.max; /* update max */
    }
#if 0
    if (up->x_mean_max == 0) {
        up->x_mean_max = (dt_signal)up->x_stats.max;
    }
    else {
        up->x_mean_max = (dt_signal)((up->x_mean_max +
                                      up->x_stats.max) / 2);
    }
#endif

    thresh = dt_compute_threshold(up->table->xn,
                                  &fp->x[0],
                                  &up->x_sorted[0],
                                  &up->x_stats,
                                  &up->x_stats_sorted,
                                  &up->xk,
                                  &up->xtmax);

    if (thresh > DT_MAX_SIGNAL) thresh = DT_MAX_SIGNAL;/* FIXME: need this? */
#if 0
    /* FIXME: need something like this? */
    if (thresh > up->x_max) thresh = up->x_max;
#endif
    up->x_dy_th = (dt_signal)thresh;
#if 0
    if (up->x_dy_th == 0) {
        up->x_dy_th = (dt_signal)thresh;
    }
    else {
        up->x_dy_th = (dt_signal)((up->x_dy_th + thresh) / 2);
    }
#endif    

    /* Y */

    /* update running averages */
    dt_stats_init(&up->y_stats);
    for (i = 0; i < up->table->yn; i++) {
        dt_stats_update(&up->y_stats, (long)fp->y[i]);
    }
    if (up->y_mean == 0) {
        up->y_mean = (dt_signal)up->y_stats.mean;
    }
    else {
        up->y_mean = (dt_signal)(((int)up->y_mean + up->y_stats.mean) / 2);
    }
    if (up->y_stats.max > up->y_max) {
        up->y_max = (dt_signal)up->y_stats.max; /* update max */
    }

    thresh = dt_compute_threshold(up->table->yn,
                                  &fp->y[0],
                                  &up->y_sorted[0],
                                  &up->y_stats,
                                  &up->y_stats_sorted,
                                  &up->yk,
                                  &up->ytmax);

    if (thresh > DT_MAX_SIGNAL) thresh = DT_MAX_SIGNAL;/* FIXME: need this? */
#if 0
    /* FIXME: need something like this? */
    if (thresh > up->y_max) thresh = up->y_max;
#endif
    up->y_dy_th = (dt_signal)thresh;
}


/*
 * Static Functions
 */

#if 0

static
long
foo(dt_signal *s)
{
    long a;
    long b;
    long c;
    /* w should be even, and at least 4 */
    long w = 16;
    long minab;
    long maxab;
    long uab;
    long vab;
    long areaab;
    long minbc;
    long maxbc;
    long ubc;
    long vbc;
    long areabc;
    long minabc;
    long halfarea;
    long area;
    double x;


    a = (long)s[0];
    b = (long)s[1];
    c = (long)s[2];

    /* compute min(a,b) */
    minab = (a < b) ? a : b;
    /* compute min(b,c) */
    minbc = (b < c) ? b : c;
    /* compute min(a,b,c) */
    minabc = (minab < minbc) ? minab : minbc;

    /* subtract the minimum from each value */
    a -= minabc;
    b -= minabc;
    c -= minabc;
    minab -= minabc;
    minbc -= minabc;

    /* compute max(a,b) */
    maxab = (a > b) ? a : b;
    /* compute max(b,c) */
    maxbc = (b > c) ? b : c;

    /* compute u and v for ab */
    uab = minab;
    vab = maxab - uab;
    /* compute u and v for bc */
    ubc = minbc;
    vbc = maxbc - ubc;

    /* compute half of total area */
    halfarea = (w / 4) * (2 * (uab + ubc) + vab + vbc);
    /* compute area of ab */
    areaab = (w / 2) * (2 * uab + vab);

    area = halfarea;
    if (area > areaab) {
        area -= areaab;
        if (b > c) area = areabc - area;
        x = foobar(area, ubc, vbc, w);
        return (long)x;    /* round here? */
    }
    else {
        if (a > b) area = areaab - area;
        x = foobar(area, uab, ubc, w);
        return -(w - (long)x); /* round here? */
    }
}


double
foobar(long area, long u, long v, long w)
{
    long areaab;
    double x;

    areaab = (w / 2) * (2 * u + v);
    assert(computedarea >= areaab);

    if ((u == 0) && (v == 0)) {
        return w;
    }
    if (u == 0) {
        x = sqrt((double)(2 * w * area) / (double)v);
    }
    else if (v == 0) {
        x = (double)area / (double)u;
    }
    else {
        x = sqrt((double)(w * w * u * u + 2 * v * w * area)) -
            (double)(u * w);
        x /= (double)v;
    }
    return x;
}
#endif



#if 0
static
unsigned short
interpolate(dt_signal *s, short n, short i)
{
    unsigned short c;    /* current value */
    unsigned short p;    /* previous value */
    unsigned short q;    /* next value */
    unsigned short min;
    unsigned short x;
    unsigned short y;
    unsigned short d;
#if 0
    double diff;
#endif
    unsigned short scale0;
    short delta;

    c = (unsigned short)s[i];
    p = (i == 0) ? 0 : (unsigned short)s[i - 1];
    q = (i == (n - 1)) ? 0 : (unsigned short)s[i + 1];

    min = p;
    if (c < min) min = c;
    if (q < min) min = q;

    p = (p - min) * 16; /*  * 128; */
    c = (c - min) * 16; /*  * 128; */
    q = (q - min) * 16; /*  * 128; */

    x = (p + c + q) / 2;    /* half the total "area" */

    y = 0;
    if (p < x) {
        x -= p;
        y += 1;
        if (c < x) {
            x -= c;
            y += 1;
            d = q;
        }
        else {
            d = c;
        }
    }
    else {
        d = p;
    }

    scale0 = 1;
    if (x < 32768) scale0 = 2;
    if (x < 16384) scale0 = 4;
    if (x <  8192) scale0 = 8;
    if (x <  4096) scale0 = 16;
    if (x <  2048) scale0 = 32;
    if (x <  1024) scale0 = 64;
    if (x <   512) scale0 = 128;
    if (x <   256) scale0 = 256;
    if (x <   128) scale0 = 512;
    if (x <    64) scale0 = 1024;
    if (x <    32) scale0 = 2048;
    if (x <    16) scale0 = 4096;
    if (x <     8) scale0 = 8192;
    if (x <     4) scale0 = 16384;
    if (x <     2) scale0 = 32768;

    x *= scale0;
    x /= d;

    if (x < 4096) {
        x *= 16;
        x /= scale0;
    }
    else {
        if (scale0 <= 16) {
            x *= (16 / scale0);
        }
        else {
            x /= (scale0 / 16);
        }
    }

    if (y == 0) {
        delta = x - 24;
    }
    else if (y == 1) {
        delta = x - 8;
    }
    else if (y == 2) {
        delta = x + 8;
    }
    return i * DT_INTER + (x - 8);

#if 0
    diff = ((double)y - 1.5) + ((double)x / (double)d);
    return i * DT_INTER + (int)(diff * (double)DT_INTER);
#endif
}
#endif


static
unsigned short
interpolate(dt_signal *s, short n, short i)
{
    unsigned short c;    /* current value */
    unsigned short p;    /* previous value */
    unsigned short q;    /* next value */
    unsigned short min;
    unsigned short x;
    unsigned short y;
    unsigned short d;
#if 0
    double diff;
#endif
    unsigned short scale0;
    short delta;

    c = (unsigned short)s[i];
    p = (i == 0) ? 0 : (unsigned short)s[i - 1];
    q = (i == (n - 1)) ? 0 : (unsigned short)s[i + 1];

    min = p;
    if (c < min) min = c;
    if (q < min) min = q;

    p = (p - min) * 16; /*  * 128; */
    c = (c - min) * 16; /*  * 128; */
    q = (q - min) * 16; /*  * 128; */

    x = (p + c + q) / 2;    /* half the total "area" */

    y = 0;
    if (p < x) {
        x -= p;
        y += 1;
        if (c < x) {
            x -= c;
            y += 1;
            d = q;
        }
        else {
            d = c;
        }
    }
    else {
        d = p;
    }

    scale0 = 1;
    if (x < 32768) scale0 = 2;
    if (x < 16384) scale0 = 4;
    if (x <  8192) scale0 = 8;
    if (x <  4096) scale0 = 16;
    if (x <  2048) scale0 = 32;
    if (x <  1024) scale0 = 64;
    if (x <   512) scale0 = 128;
    if (x <   256) scale0 = 256;
    if (x <   128) scale0 = 512;
    if (x <    64) scale0 = 1024;
    if (x <    32) scale0 = 2048;
    if (x <    16) scale0 = 4096;
    if (x <     8) scale0 = 8192;
    if (x <     4) scale0 = 16384;
    if (x <     2) scale0 = 32768;

    x *= scale0;
    x /= d;

    if (x < 4096) {
        x *= 16;
        x /= scale0;
    }
    else {
        if (scale0 <= 16) {
            x *= (16 / scale0);
        }
        else {
            x /= (scale0 / 16);
        }
    }

    if (y == 0) {
        delta = x - 24;
    }
    else if (y == 1) {
        delta = x - 8;
    }
    else if (y == 2) {
        delta = x + 8;
    }
    return i * DT_INTER + (x - 8);

#if 0
    diff = ((double)y - 1.5) + ((double)x / (double)d);
    return i * DT_INTER + (int)(diff * (double)DT_INTER);
#endif
}


static
void
dt_frame_history_init(dt_fi_user *up)
{
    short i;
    dt_frame *fp;
    dt_frame_history *fhp;

    fhp = &up->frame_hist;
    fhp->i = 0;
    for (i = 0; i < FRAME_HISTORY_LEN; i++) {
        fp = dt_frame_new(up->table);
        dt_frame_zero(up->table, fp);
        fhp->fhi[i].fp = fp;
#ifndef DT_FIRMWARE
        fp->user_id = up->id;
        /* This guarantees that the y-info will be ignored. */
        fhp->fhi[i].ytouch = 0;
#endif /* !DT_FIRMWARE */
    }
}


static
void
dt_frame_history_uninit(dt_fi_user *up)
{
    short i;
    dt_frame_history *fhp;

    fhp = &up->frame_hist;
    fhp->i = 0;
    for (i = 0; i < FRAME_HISTORY_LEN; i++) {
        if (fhp->fhi[i].fp) {
            dt_frame_free(fhp->fhi[i].fp);
            fhp->fhi[i].fp = 0;
        }
    }
}


static
void
dt_fi_user_add_frame_to_history(dt_fi_user *up, dt_frame *fp)
{
    dt_frame_copy(up->table,
                  up->frame_hist.fhi[up->frame_hist.i].fp,
                  fp);

#ifndef DT_FIRMWARE
    /* This guarantees that the y-info will be ignored. */
    up->frame_hist.fhi[up->frame_hist.i].ytouch = 0;
#endif /* !DT_FIRMWARE */

    up->frame_hist.i = (up->frame_hist.i + 1) % FRAME_HISTORY_LEN;
}


#ifndef DT_FIRMWARE
static
void
dt_median_filter_3(dt_frame *fpr, dt_fi_user *up, dt_frame *fp0,
                   dt_frame *fp1, dt_frame *fp2)
{
    dt_table *table;
    short i;
    dt_signal lsr;
    dt_signal gtr;
    dt_signal med;

    table = dt_get_table(up->id);

    for (i = 0; i < table->xn; i++) {
        if (fp0->x[i] <= fp1->x[i]) {
            lsr = fp0->x[i];
            gtr = fp1->x[i];
        }
        else {
            lsr = fp1->x[i];
            gtr = fp0->x[i];
        }
        if (fp2->x[i] <= lsr) {
            med = lsr;
        }
        else if (fp2->x[i] <= gtr) {
            med = fp2->x[i];
        }
        else {
            med = gtr;
        }
#ifndef DT_FIRMWARE
        assert(med != 0);
#endif /* !DT_FIRMWARE */
        fpr->x[i] = med;
    }

    for (i = 0; i < table->yn; i++) {
        if (fp0->y[i] <= fp1->y[i]) {
            lsr = fp0->y[i];
            gtr = fp1->y[i];
        }
        else {
            lsr = fp1->y[i];
            gtr = fp0->y[i];
        }
        if (fp2->y[i] <= lsr) {
            med = lsr;
        }
        else if (fp2->y[i] <= gtr) {
            med = fp2->y[i];
        }
        else {
            med = gtr;
        }
#ifndef DT_FIRMWARE
        assert(med != 0);
#endif /* !DT_FIRMWARE */
        fpr->y[i] = med;
    }
}


dt_signal
median3(dt_signal a, dt_signal b, dt_signal c)
{
    dt_signal lsr;
    dt_signal gtr;

    if (a <= b) {
        lsr = a;
        gtr = b;
    }
    else {
        lsr = b;
        gtr = a;
    }
    if (c <= lsr) {
        return lsr;
    }
    else if (c <= gtr) {
        return c;
    }
    else {
        return gtr;
    }
}


static void
insertion_sort(int n, dt_signal *a)
{
    int    j;
    int    i;
    dt_signal  key;

    if (n <= 1) {
        return;
    }
    for (j = 1; j < n; j++) {
        key = a[j];
        /* Insert A[j] into the sorted sequence a[0 .. j-1]. */
        i = j - 1;
        while ((i >= 0) && (a[i] > key)) {
            a[i + 1] = a[i];
            i--;
        }
        a[i + 1] = key;
    }
}

#endif /* !DT_FIRMWARE */
