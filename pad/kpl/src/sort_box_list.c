
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#ifdef PAD_WIN
#  include "../../win/windefs.h"
#endif

static debug = 0;

float **s = NULL;

static void face_center(float *a, float *b, float *c, float *d, float *e)
{
	int j;

	for (j = 0 ; j < 3 ; j++)
		e[j] = (a[j] + b[j] + c[j] + d[j]) / 4;
}

static float rr(float *a, float *b)
{
	float x, y;

	x = b[0] - a[0];
	y = b[1] - a[1];
	return x * x + y * y;
}

static float dot4(float *a, float *b)
{
	return a[0] * b[0] + a[1] * b[1] + a[2] * b[2] + a[3] * b[3];
}

static void preprocess_matrix(float *m, float *s, int i)
{
	float c[8][3], f[6][3], p[6][3], d[3];
	int j;

	for (j = 0 ; j < 16 ; j++)
		s[j] = m[j];

/* calculate corners of box */
	for (j = 0 ; j < 3 ; j++) {
		c[0][j] = m[4 * j + 3];
		c[1][j] = c[0][j] + m[4 * j];
		c[2][j] = c[0][j] + m[4 * j + 1];
		c[3][j] = c[1][j] + m[4 * j + 1];
		c[4][j] = c[0][j] + m[4 * j + 2];
		c[5][j] = c[1][j] + m[4 * j + 2];
		c[6][j] = c[2][j] + m[4 * j + 2];
		c[7][j] = c[3][j] + m[4 * j + 2];
	}

/* calculate face centers */
	face_center(c[0], c[1], c[2], c[3], f[0]);
	face_center(c[0], c[2], c[4], c[6], f[1]);
	face_center(c[0], c[4], c[1], c[5], f[2]);
	face_center(c[4], c[5], c[6], c[7], f[3]);
	face_center(c[1], c[3], c[5], c[7], f[4]);
	face_center(c[2], c[6], c[3], c[7], f[5]);

/* project face centers */
	for (j = 0 ; j < 6 ; j++) {
		p[j][0] = f[j][0] / f[j][2];
		p[j][1] = f[j][1] / f[j][2];
		p[j][2] = 1. / f[j][2];
	}

/* which local coordinate axis is the longest? */
	for (j = 0 ; j < 3 ; j++)
		d[j] = m[j]*m[j] + m[4+j]*m[4+j] + m[8+j]*m[8+j];

	j = (d[0] > d[1] && d[0] > d[2]) ? 0 :
	    (d[1] > d[0] && d[1] > d[2]) ? 1 : 2 ;

/* compute plane equation for longest axis */
	s[16] = p[j+3][1] - p[j][1];
	s[17] = p[j][0] - p[j+3][0];
	s[18] = 0.;
	s[19] = - ( s[16] * p[j][0] + s[17] * p[j][1] );

/* create parametric line equation */
	s[20] = p[j][0];
	s[21] = p[j][1];
	s[22] = p[j][2];
	s[23] = 1.;

	s[24] = p[j+3][0] - p[j][0];
	s[25] = p[j+3][1] - p[j][1];
	s[26] = p[j+3][2] - p[j][2];
	s[27] = 0.;

	s[28] = i;
}

static int compute_intersection(float *a, float *b, float *p)
{
	float t, w;
	int j;

	w = dot4(a + 8, b);
	if (fabs(w) < 0.001)
		return 0;

	t = - dot4(a + 4, b) / w;
	if (t > 1.) t = 1.;
	if (t < 0.) t = 0.;
	for (j = 0 ; j < 3 ; j++)
		p[j] = a[4 + j] + t * a[8 + j];
	return 1;
}

int compar(const void *a, const void *b)
{
	float p0[3], p1[3];

	if (compute_intersection(((float *)a) + 16, ((float *)b) + 16, p0) &&
	    compute_intersection(((float *)b) + 16, ((float *)a) + 16, p1))
	      return ( p0[2] < p1[2] ) ? 1 : -1;
	else
	      return ( ((float *)a)[22] < ((float *)b)[22] ) ? 1 : -1;
}

void sort_box_list(int n, float *m[])
{
	int i, j;

	if (s == NULL)
		s = (float **)calloc(256, sizeof(float *));
	for (i = 0 ; i < n ; i++)
		if (s[i] == NULL)
			s[i] = (float *)calloc(29, sizeof(float));
	for (i = 0 ; i < n ; i++)
		preprocess_matrix(m[i], s[i], i);
	qsort((void *)s, n, sizeof(float *), compar);
	for (i = 0 ; i < n ; i++)
		for (j = 0 ; j < 16 ; j++)
			m[i][j] = s[i][j];
}

