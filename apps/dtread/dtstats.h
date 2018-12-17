/* $Id: dtstats.h,v 1.1.1.1 2005/10/09 04:28:50 stanonik Exp $ */

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
 * dtstats.h
 * 
 * DiamondTouch statistics code.
 *
 * This file contains ANSI C code.
 */
#ifndef DTSTATS_H
#define DTSTATS_H

/* Per-line (one row or column) statistics. */
typedef struct {
    long    n;
    long    sum;
    long    sumsqs;
    long    mean;
    long    sigma;
    long    min;
    long    max;
    long    previous;
    long    sumabsdiff;
    long    meanabsdiff;
    long    sumsqsabsdiff;
    long    sigmaabsdiff;
} dt_stats;

/* Statistics for (at least some of) the table lines for a particular user. */
typedef struct {
    dt_stats *xs; /* pointer to first element of array of dt_stats objects */
    dt_stats *ys; /* pointer to first element of array of dt_stats objects */
} dt_ut_stats;

void
DT_LIB_API
dt_stats_init(dt_stats *sp);

void
DT_LIB_API
dt_stats_update(dt_stats *sp, long value);

#endif /* DTSTATS_H */
