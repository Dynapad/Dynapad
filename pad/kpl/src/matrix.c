

/* useful matrix and math routines */

#include <math.h>

#ifdef PAD_WIN
#  include "../../win/windefs.h"
#endif

float dot(float u[3], float v[3]);
void cross(float u[3], float v[3], float w[3]);
float magnitude(float v[3]);
int normalize(float v[3]);

#define Lerp(t,a,b) ( (a) + (t) * ( (b) - (a) ) )

static void clear_mat(float m[16])
{
  int i;
	
  for (i=0 ; i < 16 ; i++)
    m[i] = 0.0;
}

void transpose(float m[16], float mt[16])
{
        int i,j;

	for (i = 0; i < 4; i++)
	for (j = 0; j < 4; j++) 
	        mt[4 * i + j] = m[4 * j + i];
}

static void transpose_3x3(float m[16], float mt[16])
{
        int i,j;

	clear_mat(mt);
	for (i = 0; i < 3; i++)
	for (j = 0; j < 3; j++) 
	        mt[4 * i + j] = m[4 * j + i];
	for (i = 0; i < 4; i++)
	        mt[4 * i + 3] = m[4 * i + 3];
}

void identity(float m[16])
{
        int i;
  
	for (i = 0 ; i < 16 ; i++)
	        m[i] = (i % 5) ? 0.0 : 1.0;
}

void movmat(float m[16])
{
        int i;
	float v[3];

	for (i = 0; i < 3; i++)
	        v[i] = m[i];
	identity(m);
	for (i = 0; i < 3; i++)
	        m[4 * i + 3] = v[i];
}

void rotmat(float m[16], float theta)
{
        float v[3], d1, d2, c, co , si;
        int i;

        for (i = 0; i < 3; i++)
                v[i] = m[i];
	d1 = sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2]);
        d2 = d1 * d1;
	co = cos(theta);
	si = sin(theta) / d1;
	c = (1 - co) / d2;
	clear_mat(m);
	m[0] = v[0] * v[0] * c + co;
	m[1] = v[0] * v[1] * c - v[2] * si;
	m[2] = v[0] * v[2] * c + v[1] * si;
	m[4] = v[1] * v[0] * c + v[2] * si;
	m[5] = v[1] * v[1] * c + co;
	m[6] = v[1] * v[2] * c - v[0] * si;
	m[8] = v[2] * v[0] * c - v[1] * si;
	m[9] = v[2] * v[1] * c + v[0] * si;
	m[10] = v[2] * v[2] * c + co;
	m[15] = 1.0;
}

void scamat(float m[16])
{
        float v[3];
	int i;

	for (i = 0; i < 3; i++)
	        v[i] = m[i];
	clear_mat(m);
	for (i = 0; i < 3; i++)
	        m[5 * i] = v[i];
	m[15] = 1.0;
}

void mult(float m[16], float m1[16])
{
        int i;
	float m2[16];
/*
	int j,k;

	clear_mat(m2);
	for (i = 0 ; i < 4 ; i++)
	  for (j = 0 ; j < 4 ; j++)
	    for (k = 0 ; k < 4 ; k++)
	            m2[4 * i + j] += m1[4 * i + k] * m[4 * k + j];
	for (i = 0 ; i < 16 ; i++)
	        m[i] = m2[i];
*/
	for (i = 0 ; i < 12 ; i++)
	        m2[i] = m[i];
	for (i = 0 ; i < 12 ; i += 4) {
	 m[i+0] = m1[i]*m2[0] + m1[i+1]*m2[4] + m1[i+2]*m2[8];
	 m[i+1] = m1[i]*m2[1] + m1[i+1]*m2[5] + m1[i+2]*m2[9];
	 m[i+2] = m1[i]*m2[2] + m1[i+1]*m2[6] + m1[i+2]*m2[10];
	 m[i+3] = m1[i]*m2[3] + m1[i+1]*m2[7] + m1[i+2]*m2[11] + m1[i+3];
	}
}        

void multsgi(float m[16], float m1[16])
{
#ifdef SGI
	g_init();
	loadmatrix(m);
	multmatrix(m1);
	getmatrix(m);
#else
	mult(m, m1);
#endif
}
     
void xform(float v[4], float m[16])
{
        float v_temp[4];
	int i,j;
	
	for (i = 0 ; i < 4 ; i++)
	        v_temp[i] = v[i];
	for (i = 0 ; i < 3 ; i++) {
	        v[i] = 0.0;
		for (j = 0 ; j < 4 ; j++)
		        v[i] += m[4 * i + j] * v_temp[j];
	}
}

void invert(float m[16])
{
      float m1[16], m2[16], d;
      int i, j;

      /* invert the non-translation part */
      transpose_3x3(m, m1);
      clear_mat(m);
      m[15] = 1.0;
      for (i = 0; i < 3; i++)
      for (j = 0; j < 3; j++)
	      m[4 * i + j] = m1[(i + 1) % 3 * 4 + (j + 1) % 3]                                            * m1[(i + 2) % 3 * 4 + (j + 2) % 3]                                            - m1[(i + 2) % 3 * 4 + (j + 1) % 3]                                            * m1[(i + 1) % 3 * 4 + (j + 2) % 3];
      
      /* determinant */
      d = m1[0] * m[0] + m1[1] * m[1] + m1[2] * m[2];

      if (d != 0.) {
	      for (i = 0; i < 3; i++)
	      for (j = 0; j < 3; j++)
		      m[4 * i + j] /= d;
      }
      else {
	      for (i = 0; i < 15; i++)
		      m[i] = 0;
      }

      /* invert the translation part */
      for (i = 0; i < 16; i++)
	      m2[i] = m[i];
      identity(m);
      for (i = 3; i < 15; i += 4)
	      m[i] = (-1.0) * m1[i];
      
      /* multiply the two results */
      mult( m, m2);
}
      
void orient(float m1[16], float m2[16])
{
	int i, j;
	float x[3], d;

	for (j = 0 ; j < 3 ; j++) {
		for (i = 0 ; i < 3 ; i++)
			x[i] = m1[4*i+j];
		d = magnitude(x);
		for (i = 0 ; i < 3 ; i++)
			m1[4*i+j] = d * m2[4*i+j];
	}
}

/* matscale: compute the scale info for a trasformation matrix
   by taking the descriminant of its 3x3 component */

#define aa m[0]
#define ab m[1]
#define ac m[2]
#define ba m[4]
#define bb m[5]
#define bc m[6]
#define ca m[8]
#define cb m[9]
#define cc m[10]

float matscale(float m[16])
{
	return	aa * (bb * cc - bc * cb) +
		ab * (ba * cc - ca * bc) +
		ac * (ba * cb - ca * bb) ;
}

static void
veer2(float m[12], float a[3], float da[3], float b[3], float db[3])
{
	int i;
	float x, y;

	x = m[0] * a[0] + m[4] * a[1] + m[8] * a[2];
	y = m[0] * b[0] + m[4] * b[1] + m[8] * b[2];
	for (i = 0 ; i < 3 ; i++)
		m[4*i] += x * da[i] + y * db[i];
}

void veer(float matrix[16], float p1[4], float p2[4], float t)
{
	int i;
	float axis[3], a[3], A[3], b[3], B[3], da[3], db[3];

	for (i = 0 ; i < 3 ; i++) {
		a[i] = p1[i] - p1[3] * matrix[4*i+3];
		A[i] = p2[i] - p2[3] * matrix[4*i+3];
	}

	if (normalize(a) == 0 || normalize(A) == 0)
		return;
	for (i = 0 ; i < 3 ; i++)
		A[i] = Lerp(t, a[i], A[i]);
	if (normalize(A) == 0)
		return;

	cross(a, A, axis);
	for (i = 0 ; i < 3 && normalize(axis) == 0 ; i++) {
		a[i] += .01;
		cross(a, A, axis);
	}
	cross(axis, a, b);
	cross(axis, A, B);
	for (i = 0 ; i < 3 ; i++) {
		da[i] = A[i] - a[i];
		db[i] = B[i] - b[i];
	}
	veer2(matrix    , a, da, b, db);
	veer2(matrix + 1, a, da, b, db);
	veer2(matrix + 2, a, da, b, db);
}

/* Transform the matrix so that its origin moves to distance `dist'
   from point or in direction `p'.  Do this by a fractional amount `t'. */

void todist(float p[4], float dist, float t, float matrix[16])
{
}

int normalize(float v[3])
{
	float s;

	s = dot(v, v);
	if (s == 0.)
		return 0;

	s = sqrt(s);
	v[0] = v[0] / s;
	v[1] = v[1] / s;
	v[2] = v[2] / s;

	return 1;
}

float magnitude(float v[3])
{
	return sqrt(dot(v,v));
}

float dot(float u[3], float v[3])
{
	return u[0] * v[0] + u[1] * v[1] + u[2] * v[2];
}

void cross(float u[3], float v[3], float w[3])
{
	w[0] = u[1] * v[2] - u[2] * v[1];
	w[1] = u[2] * v[0] - u[0] * v[2];
	w[2] = u[0] * v[1] - u[1] * v[0];
}

