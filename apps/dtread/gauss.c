/* $Id: gauss.c,v 1.1.1.1 2005/10/09 04:28:50 stanonik Exp $ */

/*
 * Copyright 2003 Mitsubishi Electric Research Laboratories.
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
 */

/* Gaussian elimination and back substitution of a square matrix using
   partial pivoting and implicit pivoting.  There are n rows, each
   with n + 1 elements.  The last element is the right hand side for
   that row. */

#include <math.h>
#include <stdlib.h>
#include "gauss.h"

int gauss_backsub_pp(int n, double *rows[], double *x) {
  int i, j, k, ret;
  double *tmp, ip_tmp;
  double *ip = NULL; /* normalizations for the implicit pivots */

  ip = (double *) malloc(sizeof(double) * n);
  if (ip == NULL) {
    ret = -1;
    goto terminate;
  }

  /* find largest row element for implicit pivoting */
  for (i = 0; i < n; i++) {
    ip[i] = 0.0;
    for (j = 0; j < n; j++) {
      ip_tmp = fabs(rows[i][j]);
      if (ip_tmp > ip[i]) ip[i] = ip_tmp;
    }
    if (ip[i] == 0.0) {
      ret = -2;
      goto terminate;
    }
    ip[i] = 1.0 / ip[i];
  }

  /* iterate over the rows */
  for (i = 0; i < n; i++) {
    /* Perform pivoting.  Find the largest pivot below this row (if any)
       and swap the rows. */
    k = i;
    for (j = i + 1; j < n; j++) {
      if (fabs(rows[j][i] * ip[j]) > fabs(rows[k][i] * ip[k])) k = j;
    }
    /* swap the rows if necessary */
    if (k != i) {
      tmp = rows[i]; rows[i] = rows[k]; rows[k] = tmp;
      ip_tmp = ip[i]; ip[i] = ip[k]; ip[k] = ip_tmp;
    }

    /* make sure the pivot isn't zero -- should really check for
       sufficient precision here */
    if (rows[i][i] == 0.0) {
      ret = -3;
      goto terminate;
    }

    /* eliminate below the pivot */
    for (j = i + 1; j < n; j++) {
      for (k = i + 1; k <= n; k++) {
	rows[j][k] -= rows[j][i] * rows[i][k] / rows[i][i];
      }
      rows[j][i] = 0.0; /* remove in final code */
    }
  }

  /* back substitute */
  for (i = n - 1; i >= 0; i--) {
    x[i] = rows[i][n];
    for (j = n - 1; j > i; j--) {
      x[i] -= rows[i][j] * x[j];
    }
    x[i] /= rows[i][i];
  }

  ret = 0;

 terminate:
  if (ip != NULL) free(ip);
  return ret;
}
