
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "kpl.h"

extern void
coil_force(int n, float c1[], int n2, float c2[], float f[]);

static float a[16], b[16];

static int get_coil_from_array(int id, float **coil)
{
	int n;

/*
	int i;
	n = kpl_asize(id);
	*coil = (float *)calloc(3 * n + 16, sizeof(float));
	for (i = 0 ; i < n ; i++)
		(void)kpl_aget(id, i, (*coil) + 3*i);
*/
	return n;
}

static void kpl_wrap_coil_force(void) /* c1 c2 coil_force => force */
{
	float f[6], *c1 = NULL, *c2 = NULL;
	int n1, n2;

	n1 = get_coil_from_array(kpl_pop(b), &c1);
	n2 = get_coil_from_array(kpl_pop(a), &c2);
	coil_force(n1, c1, n2, c2, f);
	kpl_push(f, 6);
	free((char *)c1);
	free((char *)c2);
}

void install_kpl_physics(void)
{
	kpl_install("coil_force",	(Proc)kpl_wrap_coil_force);
}

