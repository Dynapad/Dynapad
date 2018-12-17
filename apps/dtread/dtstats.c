/* $Id: dtstats.c,v 1.1.1.1 2005/10/09 04:28:50 stanonik Exp $ */

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
 * dtstats.c
 * 
 * DiamondTouch statistics code.
 *
 * This file contains ANSI C code.
 */
#include <stdlib.h>
#include <math.h>
#include <limits.h>
#include "dt.h"
#include "dtstats.h"

/*
 * Local Function Prototypes
 */
#if 0
static
void
dt_fi_user_update_stats(dt_fi_user *up, dt_frame *fp);

static
dt_ut_stats *
dt_ut_stats_new(dt_table *table);
#endif


/*
 * Exported Functions
 */

void
dt_stats_init(dt_stats *sp)
{
    sp->n = 0;
    sp->sum = 0;
    sp->sumsqs = 0;
    sp->mean = 0;
    sp->sigma = 0;
    sp->min = LONG_MAX;
    sp->max = LONG_MIN;
    sp->sumabsdiff = 0;
    sp->meanabsdiff = 0;
    sp->previous = 0;
    sp->sumsqsabsdiff = 0;
    sp->sigmaabsdiff = 0;
}


void
dt_stats_update(dt_stats *sp, long value)
{
    double temp;
    long absdiff;

    sp->n++;
    sp->sum += value;
    sp->sumsqs += value * value;
    sp->mean = sp->sum / sp->n;
    /* TODO: Replace this with a fixed-point approximation. */
    temp = (double)(sp->sumsqs - sp->n * sp->mean * sp->mean)
        / (double)sp->n ;
    temp = sqrt(temp);
    sp->sigma = (long)temp;
    if (value < sp->min) sp->min = value;
    if (value > sp->max) sp->max = value;
    if (sp->n > 1) {
        absdiff = abs(value - sp->previous);
        sp->sumabsdiff += absdiff;
        sp->sumsqsabsdiff += absdiff * absdiff;
        sp->meanabsdiff = sp->sumabsdiff / (sp->n - 1);
        temp = (double)(sp->sumsqsabsdiff - ((sp->n - 1) *
                                             sp->meanabsdiff *
                                             sp->meanabsdiff)) /
                        (double)(sp->n - 1);
        temp = sqrt(temp);
        sp->sigmaabsdiff = (long)temp;
    }
    sp->previous = value;
}


#if 0

static void
dt_fi_user_update_stats(dt_fi_user *up, dt_frame *fp)
{
    int i;

    for (i = 0; i < up->table->xn; i++) {
        dt_stats_update(&up->stats->xs[i], (int)fp->x[i]);
    }
    for (i = 0; i < up->table->yn; i++) {
        dt_stats_update(&up->stats->ys[i], (int)fp->y[i]);
    }
}


static dt_ut_stats *
dt_ut_stats_new(dt_table *table)
{
    int i;
    dt_ut_stats *utsp;

    utsp = (dt_ut_stats *)malloc(sizeof(dt_ut_stats));
    if (!utsp) return 0;

    utsp->xs = malloc(sizeof(dt_stats) * table->xn);
    if (!utsp->xs) {
        free(utsp);
        return 0;
    }
    for (i = 0; i < table->xn; i++) {
        dt_stats_zero(&utsp->xs[i]);
    }
    utsp->ys = malloc(sizeof(dt_stats) * table->yn);
    if (!utsp->ys) {
        free(utsp);
        return 0;
    }
    for (i = 0; i < table->yn; i++) {
        dt_stats_zero(&utsp->ys[i]);
    }
    return utsp;
}
#endif
