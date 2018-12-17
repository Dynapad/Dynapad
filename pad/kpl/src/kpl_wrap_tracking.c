#include <stdio.h>
#include "kpl.h"

#ifdef PAD_WIN
#  include "../../win/windefs.h"
#endif

extern void tracking(float threshold, int coords[3]); 
static float a[16];

static void wrap_get_track(void)  /* threshold get_track */
{
	int b[3];

	kpl_pop(a);
        tracking(a[0], b);
	a[0] = b[0];
	a[1] = b[1];
	a[2] = b[2];
	kpl_push(a,3);
}

void install_kpl_tracking(void)
{
	        kpl_install("get_track",  (Proc)wrap_get_track);
}

