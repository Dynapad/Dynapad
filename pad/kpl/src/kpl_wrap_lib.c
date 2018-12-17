
/* kpl wrapper for useful math routines */

#include <stdio.h>
#include <ctype.h>
#include <math.h>

#ifndef PAD_WIN
#  include <sys/time.h>
#endif

#ifdef PAD_WIN
#  include <string.h>
#  include <time.h>
#  include "../../win/windefs.h"
#endif

#include "kpl.h"

extern double bias(double a, double b);
extern void cross(float *a, float *b, float *c);
extern double gain(double a, double b);
extern void kpl_eval(int id);
extern int kpl_stack_set_level(int n);
extern float ndot(float *a, int na, float *b, int nb);
extern int no_hashing();
extern float noise(float vec[], int len);
extern double noise1(double t); 
extern float pnoise(float vec[], int len);
extern float seconds_elapsed(void);
extern double truncate(double a);
extern float turbulence(float *v, float freq);

static float a[16], b[16], c[16];
static void wrap_clock(void)
{

	
	a[0] = seconds_elapsed();
	kpl_push(a, 1);
}

static void wrap_noise(void)
{
	int len;

	len = kpl_pop(a);
	a[0] = noise(a, len);
	kpl_push(a, 1);
}

static void wrap_pnoise(void)
{
	int len;

	len = kpl_pop(a);
	a[0] = pnoise(a, len);
	kpl_push(a, 1);
}

static void wrap_turbulence(void)
{
	kpl_pop(b);
	kpl_pop(a);
	a[0] = turbulence(a, b[0]);
	kpl_push(a, 1);
}

static void wrap_cross(void)
{
	kpl_pop(b);
	kpl_pop(a);
	cross(a, b, c);
	kpl_push(c, 3);
}

static void wrap_dot(void)
{
	int la, lb;

	lb = kpl_pop(b);
	la = kpl_pop(a);
	a[0] = ndot(a, la, b, lb);
	kpl_push(a, 1);
}

static void wrap_length(void)
{
	a[0] = kpl_pop(a);
	kpl_push(a, 1);
}

static void wrap_strlen(void)
{
	char str[1024];

	kpl_sprintvalue(str, a, kpl_pop(a));
	a[0] = strlen(str);
	kpl_push(a, 1);
}

static void wrap_printstack(void)
{
	int i, n, len;
	char str[1024];

	n = kpl_stack_level();
	for (i = 0 ; i < n ; i++) {
		len = kpl_stack_get(i, a);
		kpl_sprintvalue(str, a, len);
		printf("%s ", str);
	}
	printf("\n");
}

static void wrap_strcat(void)
{
	char str1[1024], str2[1024];

	kpl_sprintvalue(str2, a, kpl_pop(a));
	kpl_sprintvalue(str1, a, kpl_pop(a));
	strcat(str1, str2);
	kpl_push(a, kpl_sscanvalue(str1, a));
}

static FILE *rfp;

static void wrap_rfile(void)
{
	char file[1024];
	int id;
	FILE *fp;

	id = kpl_pop(a);
	strcpy(file, kpl_token_name(abs(id)));
	if (fp = fopen(file, "r")) {
		if (rfp != stdin)
			fclose(rfp);
		rfp = fp;
	}
}

static FILE *wfp;

static void wrap_wfile(void)
{
	char file[1024];
	int id;
	FILE *fp;

	id = kpl_pop(a);
	strcpy(file, kpl_token_name(abs(id)));
	if (strcmp(file, "stdout") == 0)
		fp = stdout;
	else
		fp = fopen(file, "w");
	if (fp) {
		if (wfp != stdout)
			fclose(wfp);
		wfp = fp;
	}
}

void kpl_print(char *str)
{
	fprintf(wfp, "%s", str);
	if (str[strlen(str)-1] == '\n')
		fflush(wfp);
	else if (strcmp(str, "'") != 0 &&
	   !isspace(str[strlen(str)-1]))
		fprintf(wfp, " ");

}

static void wrap_read(void)
{
	char str[1024], *s = str;
	int c;

	while ((c = fgetc(rfp)) != EOF && isspace(c))
		;
	if (c != EOF) {
		if (c == '"')
			while ((c = fgetc(rfp)) != EOF && c != '"')
				*s++ = c;
		else {
			*s++ = c;
			while ((c = fgetc(rfp)) != EOF && ! isspace(c))
				*s++ = c;
		}
		*s = '\0';
		kpl_push(a, kpl_sscanvalue(str, a));
	}
	else
		kpl_push(a, 0);
}

static void wrap_input_file(void)
{
	kpl_push(a, kpl_sscanvalue(kpl_input_file(), a));
}

static void wrap_source(void)
{
	kpl_execPcode(
	   kpl_parsefile(
	      kpl_token_name( abs(kpl_pop(a)) ) ) );
}

static void wrap_imap(void)
{
	int level, la, lb;

	lb = kpl_pop(b);
	la = kpl_pop(a);

	level = kpl_stack_level();

/* RUN PROC A, ADDING N ITEMS TO THE STACK */
	kpl_eval(la);

/* RUN PROC B UNTIL THE NEW ITEMS ARE ALL GONE */
/* AT EACH ITERATION, PUSH THE LEVEL DIFFERENCE */
	while (kpl_stack_level() > level) {
		a[0] = (kpl_stack_level() - level) - 1;
		kpl_push(a, 1);
		kpl_eval(lb);
	}
}

static void wrap_map(void)
{
	int level, la, lb;

	lb = kpl_pop(b);
	la = kpl_pop(a);

	level = kpl_stack_level();

/* RUN PROC A, ADDING N ITEMS TO THE STACK */
	kpl_eval(la);

/* RUN PROC B UNTIL THE NEW ITEMS ARE ALL GONE */
	while (kpl_stack_level() > level)
		kpl_eval(lb);
}

static void wrap_loop(void)
{
	int la, lb, n, i;

	lb = kpl_pop(b);
	la = kpl_pop(a);

	n = b[0];
	for (i = 0 ; i < n ; i++) {
		b[0] = i;
		kpl_push(b, 1);
		kpl_eval(la);
	}
}

static void wrap_rmap(void)
{
	int i, diff, level, la, lb, l1, l2;
	float d1[16], d2[16];

	lb = kpl_pop(b);
	la = kpl_pop(a);

	level = kpl_stack_level();

/* RUN PROC A, ADDING N ITEMS TO THE STACK */

	kpl_eval(la);

/* COUNT NUMBER OF ITEMS PUSHED, AND RESET STACK */

	diff = kpl_stack_level() - level;

/* REVERSE ORDER OF THESE ITEMS */

	for (i = 0 ; i < diff/2 ; i++) {
		l1 = kpl_stack_get(i, d1);
		l2 = kpl_stack_get(diff-1-i, d2);
		kpl_stack_set(i, d2, l2);
		kpl_stack_set(diff-1-i, d1, l1);
	}

/* RUN PROC B UNTIL THE NEW ITEMS ARE ALL GONE */
	while (kpl_stack_level() > level)
		kpl_eval(lb);
}

static void wrap_map_join(void)
{
	int level, diff, la, i, n;
	float aa[32];

/* RUN PROCEDURE, ADDING ITEMS TO THE STACK */

	la = kpl_pop(a);
	level = kpl_stack_level();
	kpl_eval(la);

/* COUNT NUMBER OF ITEMS PUSHED, AND RESET STACK */

	diff = kpl_stack_level() - level;
	kpl_stack_set_level(level);

/* JOIN RESULTS INTO A SINGLE VECTOR AND PUSH IT */

	for (i = 1, n = 0 ; i <= diff && n < 16 ; i++)
		n += kpl_stack_get(-i, aa + n);
	if (n > 16)
		n = 16;
	kpl_push(aa, n);
}

void kpl_push_var(int id);
void kpl_pop_var(int id);
int kpl_stack_get_len(int i);

static void wrap_push_var(void)
{
	kpl_push_var(abs(kpl_stack_get_len(0)));
}

static void wrap_pop_var(void)
{
	kpl_pop_var(abs(kpl_stack_get_len(0)));
}

float eval_spline(int n, float keypts[], float t);

static void wrap_eval_spline(void)
{
	int len = kpl_pop(b);

	kpl_pop(a);
	a[0] = eval_spline(len/2, b, a[0]);
	kpl_push(a, 1);
}

static debug_state = 0;

int kpl_debug(void)
{
	return debug_state;
}

static void wrap_debug(void)
{
	kpl_pop(a);
	debug_state = a[0];
}

static void wrap_no_hashing(void)
{
	no_hashing();
}

extern void tracking(float threshold, int coords[3]); 

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

static void wrap_getenv(void)
{
	char str[100], *s, *getenv();

	kpl_sprintvalue(str, a, kpl_pop(a));
	s = getenv(str);
	kpl_push(a, s ? kpl_sscanvalue(s, a) : 0);
}

void install_kpl_lib(void)
{
	kpl_unop("abs",		(Proc)fabs);
	kpl_unop("acos",	(Proc)acos);
	kpl_unop("asin",	(Proc)asin);
	kpl_unop("atan",	(Proc)atan);
	kpl_unop("cos",		(Proc)cos);
	kpl_unop("noise1",	(Proc)noise1);
	kpl_unop("sin",		(Proc)sin);
	kpl_unop("sqrt",	(Proc)sqrt);
	kpl_unop("tan",		(Proc)tan);
	kpl_unop("truncate",	(Proc)truncate);

	kpl_binop("atan2",	(Proc)atan2);
	kpl_binop("bias",	(Proc)bias);
	kpl_binop("gain",	(Proc)gain);
	kpl_binop("mod",	(Proc)fmod);
	kpl_binop("pow",	(Proc)pow);

	kpl_install("clock",	(Proc)wrap_clock);
	kpl_install("cross",	(Proc)wrap_cross);
	kpl_install("debug",	(Proc)wrap_debug);
	kpl_install("dot",	(Proc)wrap_dot);
	kpl_install("eval_spline",(Proc)wrap_eval_spline);
	kpl_install("get_track",  (Proc)wrap_get_track);
	kpl_install("getenv", (Proc)wrap_getenv);
	kpl_install("imap",	(Proc)wrap_imap);
	kpl_install("input_file",(Proc)wrap_input_file);
	kpl_install("length",	(Proc)wrap_length);
	kpl_install("loop",	(Proc)wrap_loop);
	kpl_install("map",	(Proc)wrap_map);
	kpl_install("no_hashing", (Proc)wrap_no_hashing);
	kpl_install("noise",	(Proc)wrap_noise);
	kpl_install("strlen",	(Proc)wrap_strlen);
	kpl_install("pnoise",	(Proc)wrap_pnoise);
	kpl_install("pop_var"	,(Proc)wrap_pop_var);
	kpl_install("printstack",(Proc)wrap_printstack);
	kpl_install("push_var"	,(Proc)wrap_push_var);
	kpl_install("read",	(Proc)wrap_read);
	kpl_install("rfile",	(Proc)wrap_rfile);
	kpl_install("rmap",	(Proc)wrap_rmap);
	kpl_install("source",	(Proc)wrap_source);
	kpl_install("strcat",	(Proc)wrap_strcat);
	kpl_install("turbulence",(Proc)wrap_turbulence);
	kpl_install("map_join",	(Proc)wrap_map_join);
	kpl_install("wfile",	(Proc)wrap_wfile);
#ifdef PAD_UNIX
	rfp = stdin;
	wfp = stdout;
#endif
}

