static char rcsid[] = "$Header: /nas/backup/old-home/home/cvs/dynapad/pad/kpl/src/proj.c,v 1.1.1.1 2005/10/09 04:28:48 stanonik Exp $";
/* support routines to project a vector with clipping */

#include <stdio.h>
#include <math.h>

#ifdef PAD_WIN
#  include "../../win/windefs.h"
#endif

#define CONVERT_FOCAL(_f) ( - (_f) / 10.995 )

static int viewport[4] = {0,0,100,100}; /* lower and upper pixel bounds */
static float focal = CONVERT_FOCAL(35.0);

void project_pt(float a[3], int *b);

int fclip(int n, float *p, float *q, float *lo, float *hi);
int iclip(int n, int *p, int *q, int *lo, int *hi);

#define EPS (.001)

static void
zclip_ab(float a[3], float b[3])
{
	float t = (EPS - a[2]) / (b[2] - a[2]);

	a[0] = a[0] + t * (b[0] - a[0]);
	a[1] = a[1] + t * (b[1] - a[1]);
	a[2] = EPS;
}

static int zclip_vec(float a[3], float b[3])
{
	if (a[2] >= EPS && b[2] >= EPS)
		return 0;
	else {
		if (a[2] >= EPS)
			zclip_ab(a, b);
		else if (b[2] >= EPS)
			zclip_ab(b, a);
		return 1;
	}
}

int *project_vector(float a[3], float b[3], int *e)
{
	int i, lo[2], hi[2];
	float atemp[3], btemp[3];

	lo[0] = lo[1] = 0;
	hi[0] = viewport[2] - viewport[0];
	hi[1] = viewport[3] - viewport[1];
	for (i = 0 ; i < 3 ; i++) {
		atemp[i] = a[i];
		btemp[i] = b[i];
	}
	if (zclip_vec(atemp, btemp)) {
		project_pt(atemp, e);
		project_pt(btemp, e+2);
/*
		if (iclip(2, e, e+2, lo, hi))
			e += 4;
*/
		iclip(2, e, e+2, lo, hi);
		e += 4;
	}
	return e;
}

void project_pt(float a[3], int *b)
{
	int midx   = (viewport[0] + viewport[2])/2;
	int midy   = (viewport[1] + viewport[3])/2;
	int width  = (viewport[2] - viewport[0])/2;

	b[0] = midx + width * a[0] * focal / a[2];
	b[1] = midy + width * a[1] * focal / a[2];
}

void unproject_pt(int b[2], float a[3])
{
	int midx = (viewport[0] + viewport[2])/2;
	int midy = (viewport[1] + viewport[3])/2;
	int width = viewport[2] - viewport[0];

	a[0] = ((b[0] - midx) / focal) / -width;
	a[1] = ((b[1] - midy) / focal) /  width;
	a[2] = -1.;
}

void project_set_focal(float f)
{
	focal = CONVERT_FOCAL(f);
}

void project_set_viewport(int x0, int y0, int x1, int y1)
{
	viewport[0] = x0;
	viewport[1] = y0;
	viewport[2] = x1;
	viewport[3] = y1;
}

