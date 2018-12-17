
/* kpl wrapper for summation vectors */

#include <stdio.h>
#include "kpl.h"

#ifdef PAD_WIN
#  include "../../win/windefs.h"
#endif

static float a[16], b[16], c[16], d[16], e[16];
static float **A = (float **)a;
static float **B = (float **)b;
static float **C = (float **)c;
static float **D = (float **)d;

extern void   begin_layer(int l);
extern void   get_sum(int i, float vec[]);
extern void   get_vec(float *v, int i, float vec[]);
extern void   lerp_vecs(float *a, float *b, float *t, float w);
extern int    lerp2_vecs(float *a, float *b, float *ww, float *t, float w);
extern float *new_vec();
extern void   set_vec(float *v, int i, float vec[]);

static void wrap_new_vec(void)		/* ~ => vv */
{
	A[0] = new_vec();
	kpl_push(a, 1);
}

static void wrap_set_vec(void)		/* x:y:z:w i vv ~ */
{
	kpl_pop(c);
	kpl_pop(b);
	kpl_pop(a);
	set_vec(C[0], (int)b[0], a);
}

static void wrap_get_vec(void)		/* i vv ~ => x:y:z */
{
	kpl_pop(c);
	kpl_pop(b);
	get_vec(C[0], (int)b[0], a);
	kpl_push(a, 3);
}

static void wrap_clear_sum(void)	/* ~ */
{
	begin_layer(0);
}
      
static void wrap_begin_layer(void)	/* layer ~ */
{
	kpl_pop(a);
	begin_layer((int)a[0]);
}

static void wrap_lerp_vecs(void)	/* aa bb tt w ~ */
{
	kpl_pop(d);
	kpl_pop(c);
	kpl_pop(b);
	kpl_pop(a);
	lerp_vecs(A[0], B[0], C[0], d[0]);
}
      
static void wrap_lerp2_vecs(void)	/* aa bb ww tt w ~ */
{
	kpl_pop(e);
	kpl_pop(d);
	kpl_pop(c);
	kpl_pop(b);
	kpl_pop(a);
	a[0] = lerp2_vecs(A[0], B[0], C[0], D[0], e[0]);
	kpl_push(a, 1);
}
      
static void wrap_normalize_sum(void)	/* kept only for backward compatibility */
{
}

static void wrap_get_sum(void)		/* i ~ => x:y:z */
{
	kpl_pop(a);
	get_sum((int)a[0], b);
	kpl_push(b, 3);
}
      
void install_kpl_sumvec(void)
{
	kpl_install("begin_layer",	(Proc)wrap_begin_layer);
	kpl_install("get_sum",		(Proc)wrap_get_sum);
	kpl_install("get_vec",		(Proc)wrap_get_vec);
	kpl_install("lerp_vecs",	(Proc)wrap_lerp_vecs);
	kpl_install("lerp2_vecs",	(Proc)wrap_lerp2_vecs);
	kpl_install("new_vec",		(Proc)wrap_new_vec);
	kpl_install("normalize_sum",	(Proc)wrap_normalize_sum);
	kpl_install("set_vec",		(Proc)wrap_set_vec);
}

