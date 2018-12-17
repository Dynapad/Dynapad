
/* kpl wrapper for useful math routines */

#include <stdio.h>
#include <ctype.h>
#include <math.h>
#include <sys/time.h>
#include "kpl.h"

extern double bias(double a, double b);
extern double gain(double a, double b);
extern double truncate(double a);
extern double noise1(double t);

extern float ndot(float *a, int na, float *b, int nb);
extern float noise(float vec[], int len);
extern float pnoise(float vec[], int len);
extern float seconds_elapsed(void);
extern float turbulence(float *v, float freq);

static void wrap_clock(void)
{
	a[0] = seconds_elapsed();
	na = 1;
}

static void wrap_noise(void)
{
	a[0] = noise(a, na);
	na = 1;
}

static void wrap_pnoise(void)
{
	a[0] = pnoise(a, na);
	na = 1;
}

static void wrap_turbulence(void)
{
	a[0] = turbulence(a, b[0]);
	na = 1;
}

static void wrap_dot(void)
{
	a[0] = ndot(a, na, b, nb);
	na = 1;
}

static void wrap_length(void)
{
	a[0] = na;
	na = 1;
}

static void wrap_nchars(void)
{
	char str[256];

	kpl_sprintvalue(str, a, na);
	a[0] = strlen(str);
	na = 1;
}

static void wrap_strcat(void)
{
	char str1[256], str2[256];

	kpl_sprintvalue(str2, a, na);
	kpl_sprintvalue(str1, b, nb);
	strcat(str1, str2);
	na = kpl_sscanvalue(str1, a);
}

static FILE *rfp = stdin;

static void wrap_rfile(void)
{
	char file[256];
	FILE *fp;

	strcpy(file, kpl_token_name(abs(na)));
	if (fp = fopen(file, "r")) {
		if (rfp != stdin)
			fclose(rfp);
		rfp = fp;
	}
}

static FILE *wfp = stdout;

static void wrap_wfile(void)
{
	char file[256];
	FILE *fp;

	strcpy(file, kpl_token_name(abs(na)));
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

kpl_print(char *str)
{
	fprintf(wfp, "%s", str);
	if (str[strlen(str)-1] != '\n')
		fprintf(wfp, " ");
}

static void wrap_read(void)
{
	char str[256], *s = str;
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
		na = kpl_sscanvalue(str, a);
	}
	else
		na = 0;
}

static void wrap_source(void)
{
	kpl_execPcode(
	   kpl_parsefile(
	      kpl_token_name(abs(na)) ) );
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

	kpl_install("clock",	0, 1, (Proc)wrap_clock);
	kpl_install("dot",	2, 1, (Proc)wrap_dot);
	kpl_install("length",	1, 1, (Proc)wrap_length);
	kpl_install("noise",	1, 1, (Proc)wrap_noise);
	kpl_install("nchars",	1, 1, (Proc)wrap_nchars);
	kpl_install("pnoise",	1, 1, (Proc)wrap_pnoise);
	kpl_install("read",	0, 1, (Proc)wrap_read);
	kpl_install("rfile",	1, 0, (Proc)wrap_rfile);
	kpl_install("source",	1, 0, (Proc)wrap_source);
	kpl_install("strcat",	2, 1, (Proc)wrap_strcat);
	kpl_install("turbulence",1,1, (Proc)wrap_turbulence);
	kpl_install("wfile",	1, 0, (Proc)wrap_wfile);
}

