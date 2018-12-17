/* summation vector */

#include <stdio.h>
#include <math.h>

#ifdef PAD_WIN
#  include <malloc.h>
#  include "../../win/windefs.h"
#endif

#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE 1
#endif

#define NI 30
#define NJ 3
#define NL 30

static int ni = 0;
static int layer = 0;

static float sumvec[NL][NI][NJ];
static float weight[NL][NI][NJ];

float *new_vec()
{
	return (float *)calloc(NI * NJ, sizeof(float));
}

void set_vec(float *v, int i, float vec[])
{
	int j;
	
	if (i >= ni)
		ni = i+1;
	for (j = 0 ; j < NJ ; j++)
	    v[NJ*i + j] = vec[j];
}

void get_vec(float *v, int i, float vec[])
{
	int j;
	
	for (j = 0 ; j < NJ ; j++)
	    vec[j] = v[NJ*i + j];
}

static void clear_layer(int l)
{
	int i, j;

	for (i = 0 ; i < NI ; i++)
	for (j = 0 ; j < NJ ; j++) {
	    weight[l][i][j] = 0.;
	    sumvec[l][i][j] = 0.;
	}
}

void begin_layer(int l)
{
	clear_layer(layer = l);
}

void lerp_vecs(float *a, float *b, float *t, float w)
{
	int i, j, k = 0;

	for (i = 0 ; i < NI ; i++, k += NJ)
	for (j = 0 ; j < NJ ; j++) {
	    sumvec[layer][i][j] += w * (a[k+j] + t[k+j]*(b[k+j] - a[k+j]));
	    weight[layer][i][j] += w;
	}
}

int lerp2_vecs(float *a, float *b, float *wt, float *t, float w)
{
	int i, j, k = 0, is_complete, is_vec;

	is_complete = TRUE;
	for (i = 0 ; i < ni ; i++, k += NJ) {

	    is_vec = FALSE;
	    for (j = 0 ; j < NJ ; j++) {
	        weight[layer][i][j] += w * wt[k+j];
	        sumvec[layer][i][j] += w * wt[k+j] * (a[k+j] + t[k+j]*(b[k+j] - a[k+j]));
		if (i == 0 || wt[k+j])
		    is_vec = TRUE;
	    }
	    if (is_vec == FALSE)
	        is_complete = FALSE;
	}

	return is_complete;
}

void get_sum(int i, float vec[])
{
	int l, j;
	float wgt[NJ], w;

	for (j = 0 ; j < NJ ; j++) {
	    vec[j] = 0.;
	    wgt[j] = 0.;
	}
	for (l = 0 ; l <= layer ; l++)
	for (j = 0 ; j < NJ ; j++)
	    if (w = weight[l][i][j]) {
		if (l > 0) {
		    vec[j] *= 1. - w;
		    wgt[j] *= 1. - w;
		}
		vec[j] += sumvec[l][i][j];
		wgt[j] += w;
            }
	for (j = 0 ; j < NJ ; j++)
	    if (wgt[j])
		vec[j] /= wgt[j];
}

