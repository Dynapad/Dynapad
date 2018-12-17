
/* maintain vectors of kpl Values */

#include <stdio.h>

#ifdef PAD_WIN
#  include "../../win/windefs.h"
#  include <malloc.h>
#  include <memory.h>
#endif

typedef struct Value {
	int len;
	float data[16];
} Value;

typedef struct Vec {
	int len;
	char **data;
} Vec;

static int nvecs = 1;
static Vec *vec = NULL;

#define Vi(i)   vec->data[i]
#define V(i,j)  ((Vec *)Vi(i))->data[j]
#define Vlen(i) ((Vec *)Vi(i))->len

static void touch(Vec **addr, int i)
{
	Vec *a = *addr;
	char **data;

	if (a == NULL) {
		a = (Vec *)calloc(1, sizeof(Vec));
		a->data = (char **)calloc(a->len = 16, sizeof(char *));
	}
	while (a->len <= i) {
		data = (char **)calloc(2 * a->len, sizeof(char *));
		memcpy((char *)data, (char *)a->data, a->len * sizeof(char *));
		free((char *)a->data);
		a->len = 2 * a->len;
		a->data = data;
	}
	*addr = a;
}

int vcreate()
{
	return nvecs++;
}

void vdelete(int i)
{
	int j;

	if (vec != NULL && vec->len > i && Vi(i) != NULL) {
		for (j = 0 ; j < Vlen(i) ; j++)
			if (V(i,j))
				free(V(i,j));
		free(Vi(i));
		Vi(i) = NULL;
	}
}

Value *vaddr(int i, int j, int touching)
{
	if (touching) {
		touch(&vec, i);
		touch((Vec **)&Vi(i), j);
		if (V(i,j) == NULL)
			V(i,j) = (char *)calloc(1, sizeof(Value));
	}
	else if (vec == NULL || vec->len <= i ||
		 Vi(i) == NULL || Vlen(i) <= j || V(i,j) == NULL)
		return 0;
	return (Value *)V(i,j);
}

int vget(int i, int j, float *data)
{
	int k;
	Value *v = vaddr(i, j, 0);

	if (v == NULL)
		return 0;
	for (k = 0 ; k < v->len ; k++)
		data[k] = v->data[k];
	return v->len;
}

void vset(int i, int j, float *data, int len)
{
	int k;
	Value *v = vaddr(i, j, 1);

	v->len = len;
	for (k = 0 ; k < v->len ; k++)
		v->data[k] = data[k];
}

