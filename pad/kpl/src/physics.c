
/* compute attraction between two coils */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#ifdef PAD_WIN
#  include "../../win/windefs.h"
#endif

extern float dot(float u[3], float v[3]);
extern void cross(float u[3], float v[3], float w[3]);
extern int normalize(float v[]);

static void add_force(float f[], float dc[], float c1[], float d1[], float c2[], float d2[]);
static float *make_unit_direction_vectors(int n, float c[]);

void
coil_force(int n1, float c1[], int n2, float c2[], float f[])
{
	float *d1, *d2, dc[3];
	int i, j;
	float center[4];

/* initialize force to zero */

	for (j = 0 ; j < 6 ; j++)
		f[j] = 0.;
	if (n1 < 1 || n2 < 1)
		return;

/* compute center of coil1 - we'll need it to computer torque */

	center[0] = center[1] = center[2] = 0.0;
	for (i = 0 ; i < n1 ; i++)
	for (j = 0 ; j < 3 ; j++)
		center[j] += c1[3*i+j];
	for (j = 0 ; j < 3 ; j++)
		center[j] /= n1;

/* make an array of unit direction vectors along each coil */

	d1 = make_unit_direction_vectors(n1, c1);
	d2 = make_unit_direction_vectors(n2, c2);

/* accumulate forces at each point of each coil */

	for (i = 0 ; i < n1 ; i++) {
		for (j = 0 ; j < 3 ; j++)
			dc[j] = c1[j] - center[j];
		for (j = 0 ; j < n2 ; j++) 
			add_force(f, dc, c1+3*i, d1+3*i, c2+3*j, d2+3*j);
	}

/* normalize force, factoring out sampling density */

	for (i = 0 ; i < 6 ; i++)
		f[i] = f[i] / (n1 * n1);

	free((char *)d1);
	free((char *)d2);
}

static float *make_unit_direction_vectors(int n, float c[])
{
	float *d;
	int i, j;

	d = (float *)malloc(n * 3 * sizeof(float));
	for (i = 0 ; i < n-1 ; i++) {
		for (j = 0 ; j < 3 ; j++)
			d[3*i+j] = c[3*(i+1)+j] - c[3*i+j];
		normalize(d + 3*i);
	}
	for (j = 0 ; j < 3 ; j++)
		d[3*(n-1)+j] = d[3*(n-2)+j];
	return d;
}

static void
add_force(float f[], float dc[], float c1[], float d1[], float c2[], float d2[])
{
	float d[3], tmp[3], r_squared, r_cubed;
	float force_vector[3], torque_vector[3];

/* compute difference vector between the two points, divide by distance cubed */

	d[0] = c2[0] - c1[0];
	d[1] = c2[1] - c1[1];
	d[2] = c2[2] - c1[2];
	r_squared = dot(d, d);
	r_cubed = r_squared * sqrt(r_squared);
	d[0] /= r_cubed;
	d[1] /= r_cubed;
	d[2] /= r_cubed;

/* compute and add to linear force */

	cross(d1, d, tmp);
	cross(tmp, d2, force_vector);
	f[0] += force_vector[0];
	f[1] += force_vector[1];
	f[2] += force_vector[2];

/* compute and add to torque */

	cross(dc, force_vector, torque_vector);
	f[3] += torque_vector[0];
	f[4] += torque_vector[1];
	f[5] += torque_vector[2];
}

