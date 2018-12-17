
/* useful matrix and math routines */

#include <stdio.h>
#include <math.h>
#include "../../generic/tclInt.h"

#ifdef PAD_WIN
#  include "../../win/windefs.h"
#endif

extern float noise3(float[]);

double bias(double a, double b)
{
	if (a < .001)
		return 0.;
	else if (a > .999)
		return 1.;
	else if (b < .001)
		return 0.;
	else if (b > .999)
		return 1.;
	else
		return pow(a, log(b) / log(0.5));
}

double gain(double a, double b)
{
	double p;

	if (a < .001)
		return 0.;
	else if (a > .999)
		return 1.;
	b = (b < .001) ? .001 : (b > .999) ? .999 : b;
	p = log(1. - b) / log(0.5);
	if (a < 0.5)
		return pow(2 * a, p) / 2;
	else
		return 1. - pow(2 * (1. - a), p) / 2;
}

float turbulence(float *v, float freq)
{
	float t, vec[3];

	for (t = 0. ; freq >= 1. ; freq /= 2) {
		vec[0] = freq * v[0];
		vec[1] = freq * v[1];
		vec[2] = freq * v[2];
		t += fabs(noise3(vec)) / freq;
	}
	return t;
}

float ndot(float *a, int na, float *b, int nb)
{
	float f;
	int w;

	na = (nb < na) ? nb : na;
	f = 0.;
	for (w = na ; w-- > 0 ; )
		f += a[w] * b[w];
	return f;
}

double truncate(double a)
{
	return (int)a;
}

static Tcl_Time current_time;
static start_time = 0;
#define TICS_PER_SEC 10000

float seconds_elapsed(void)
{
	int time;

#if TCL_MAJOR_VERSION == 7 &&  TCL_MINOR_VERSION == 5 && !defined(TCL_PATCH_P1)
	TclGetTime(&current_time);
#else
	TclpGetTime(&current_time);
#endif
	time =	current_time.sec  * TICS_PER_SEC +
		current_time.usec / (1000000 / TICS_PER_SEC);
	if (start_time == 0)
		start_time = time;
	return (float)(time - start_time) / TICS_PER_SEC;
}

