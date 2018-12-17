/* Ken's special spline for controlling animation */

#include <math.h>

#ifdef PAD_WIN
#  include "../../win/windefs.h"
#endif

#define X(i) xy[2*(interval + (i))]
#define Y(i) xy[2*(interval + (i)) + 1]
#define DX(i) ( X(i) - X(i-1) )
#define DY(i) ( Y(i) - Y(i-1) )

static float eval_hermite(float x, float xi, float yi, float i_slope,
				   float xf, float yf, float f_slope);
static float eval_slope(float xy[], int interval);
static float end_slope(float dy_dx, float other_slope);

float eval_spline (int n, float xy[], float x)
{
	float slope0, slope1;
	int   interval;

	interval = n-1;
	if (x >= X(0))
		return Y(0);
	interval = 0;
	if (n == 1 || x < X(0))
		return Y(0);
	if (n == 2)
		return Y(0) + (x - X(0)) * DY(1) / DX(1);
	for (interval = 0 ; x >= X(1) ; interval++)
		;
	if (interval > 0)
		slope0 = eval_slope(xy, interval);
	if (interval < n-2)
		slope1 = eval_slope(xy, interval+1);
	if (interval == 0)
		slope0 = end_slope(DY(1) / DX(1), slope1);
	if (interval == n-2)
		slope1 = end_slope(DY(1) / DX(1), slope0);
	return eval_hermite(x, X(0), Y(0), slope0, X(1), Y(1), slope1);
}

static float eval_slope(float xy[], int interval)
{
	float d0, d1, s0, s1, slope;

	if (DY(0) * DY(1) < 0.)
		return  0.;
	d0 = DX(0) * DX(0) + DY(0) * DY(0);
	d1 = DX(1) * DX(1) + DY(1) * DY(1);
	s0 = DY(0) / DX(0);
	s1 = DY(1) / DX(1);
	slope = (d1 * s0 + d0 * s1) / (d0 + d1);
	if (fabs(slope) > 2 * fabs(s0))
		slope = 2 * s0;
	if (fabs(slope) > 2 * fabs(s1))
		slope = 2 * s1;
	return slope;
}

static float end_slope(float dy_dx, float other_slope)
{
	float slope = 2 * dy_dx - other_slope;

	return slope * other_slope < 0. ? 0. : slope;
}

static float eval_hermite (float x, float xi, float yi, float i_slope,
				    float xf, float yf, float f_slope)
{
	float t  = (x - xi) / (xf - xi);
	float R1 =  i_slope * (xf - xi);
	float R4 =  f_slope * (xf - xi);
	return t * ( t * ( t * (2*yi - 2*yf + R1 + R4)
                         +    -3*yi + 3*yf - 2*R1 - R4 ) + R1 ) + yi;
}

