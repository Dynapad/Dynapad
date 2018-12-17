/* $Id: dtcal.c,v 1.1.1.1 2005/10/09 04:28:49 stanonik Exp $ */

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

/* Calibration routine for DiamondTouch/Projector combo.  Given a set
   of at least four 2-D points whose coordinates in both the input
   (DiamondTouch) space and output (projector/display) space are
   known, computes a transformation matrix from one space to the
   other.  The routine uses least-squares if more than four points are
   provided.

   The transformation matrix is calculated as follows:

   [a b c][x1]   [s * x2]
   [d e f][y1] = [s * y2]
   [g h 1][ 1]   [  s   ]

   We want to solve for the variables a through h, so first we eliminate
   the variable s:

        a * x1 + b * y1 + c
   x2 = -------------------
        g * x1 + h * y1 + 1

        d * x1 + e * y1 + f
   y2 = -------------------
        g * x1 + h * y1 + 1

   Then we multiply through by the denominators and separate out the
   variables a through h so that we can solves then via a linear system.

   g * x1 * x2 + h * y1 * x2 + x2 = a * x1 + b * y1 + c
   g * x1 * y2 + h * y1 * y2 + y2 = d * x1 + e * y1 + f

   a(x1) + b(y1) + c(1) + d(0) + e(0) + f(0) + g(-x1*x2) + h(-y1*x2) = x2
   a(0) + b(0) + c(0) + d(x1) + e(y1) + f(1) + g(-x1*y2) + h(-y1*y2) = y2

   [x1  y1  1   0   0   0   -x1*x2  -y1*x2][a] = [ x2]
   [0   0   0   x1  y1  1   -x1*y2  -y1*y2][b] = [ y2]
   [                    .                 ][c] = [ . ]
   [                    .                 ][d] = [ . ]
   [                    .                 ][e] = [ . ]
   [                    .                 ][f] = [ . ]
   [                    .                 ][g] = [ . ]
   [                    .                 ][h] = [ . ]


  Note that there are two rows per set of coordinate pairs (x1,y1) and
  (x2,y2) because there are two equations that we must solve for.
  Also note that four sets of coordinate pairs will yield eight
  equations, and since there are eight unknowns, the system can be
  solved in closed form.  If we have more than four sets of coordiate
  pairs, it will be necessary to use least-squares to solve for the
  eight unknowns.

  Since there are more equations than unknowns, we have to solve this
  via least squares.  Ax=b can be converted to QAx = Qb where
  Q = transpose(A).  We then just solve the smaller, square system
  (QA)x = (Qb) for x.

*/

#include <stdlib.h>
#include "dtcal.h"
#include "gauss.h"

/* input parameters:
   "n" is the number of points provided.
   "xform" is an 8-element vector containing the resulting transformation
           variables "a" through "h".
   "x1" and "y1" are n-element vectors containing the x- and y- components
           of the calibration points in the first coordinate system.
   "x2" and "y2" are n-element vectors containing the x- and y- components
           of the calibration points in the second coordinate system.
*/
   
int
dt_compute_calibration(int n, double *xform,
		       double *x1, double *y1,
		       double *x2, double *y2) {
  int i, j, k, ret = 0;
  double (*a)[8] = 0, *b = 0;
  double ata[8][9];
  double *ata_rows[8];

  if (n < 4) return -101;

  /* allocate temporary matrices */
  a = (double (*)[8]) malloc(16 * n * sizeof(double));
  if (a == NULL) { ret = -1; goto terminate; }
  b = (double *) malloc(2 * n * sizeof(double));
  if (b == NULL) { ret = -1; goto terminate; }

  /* make pointers to rows of ata for gaussian elim routine */
  for (i = 0; i < 8; i++) ata_rows[i] = ata[i];

  /* fill matrix a and vector b */
  for (i = 0, j = 0; i < n; i++, j++) {
    a[j][0] = x1[i];
    a[j][1] = y1[i];
    a[j][2] = 1.0;
    a[j][3] = 0.0;
    a[j][4] = 0.0;
    a[j][5] = 0.0;
    a[j][6] = -x1[i] * x2[i];
    a[j][7] = -y1[i] * x2[i];

    b[j] = x2[i];

    j++;
    a[j][0] = 0.0;
    a[j][1] = 0.0;
    a[j][2] = 0.0;
    a[j][3] = x1[i];
    a[j][4] = y1[i];
    a[j][5] = 1.0;
    a[j][6] = -x1[i] * y2[i];
    a[j][7] = -y1[i] * y2[i];

    b[j] = y2[i];
  }

  /* calculate ata (transpose(a) times a) */
  for (i = 0; i < 8; i++) {
    for (j = 0; j < 8; j++) {
      ata[i][j] = 0.0;
      for (k = 0; k < 2 * n; k++) ata[i][j] += a[k][i] * a[k][j];
    }
  }

  /* calculate transpose(a) times b, which goies into the last column
     of ata for the gassian elimination program to work */
  for (i = 0; i < 8; i++) {
    ata[i][8] = 0.0;
    for (j = 0; j < 2 * n; j++) ata[i][8] += a[j][i] * b[j];
  }

  ret = gauss_backsub_pp(8, ata_rows, xform);

 terminate:
  free(b);
  free(a);
  return ret;
}

void dt_transform(double *xform, double x1, double y1, double *x2, double *y2) {
  double s;
  s = xform[6] * x1 + xform[7] * y1 + 1.0;
  *x2 = (xform[0] * x1 + xform[1] * y1 + xform[2]) / s;
  *y2 = (xform[3] * x1 + xform[4] * y1 + xform[5]) / s;
}
