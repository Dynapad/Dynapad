
/* arrays in kpl */

#include "kpl.h"

#ifdef PAD_WIN
#  include "../../win/windefs.h"
#endif

static float a[16], b[16], c[16];

extern int vcreate();
extern int vget(int id, int i, float *data);
extern void vset(int id, int i, float *data, int len);

static void wrap_vcreate()
{
	a[0] = vcreate();
	kpl_push(a, 1);
}

static void wrap_vget()
{
	(void)kpl_pop(b);
	(void)kpl_pop(a);
	kpl_push(a, vget((int)b[0], (int)a[0], a));
}

static void wrap_vset()
{
	(void)kpl_pop(c);
	(void)kpl_pop(b);
	vset((int)c[0], (int)b[0], a, kpl_pop(a));
}

void install_kpl_vec()
{
	kpl_install("vcreate"	, (Proc)wrap_vcreate);
	kpl_install("vget"	, (Proc)wrap_vget);
	kpl_install("vset"	, (Proc)wrap_vset);
}

