
#include <stdio.h>
#include <math.h>
#include <string.h>

#include "kpl.h"

typedef struct {
  int fd_in;
  int pid;
  FILE *fp_in;
  FILE *fp_out;
  char *buf;
} pump_handle;

extern void	collect_new_symbols();
extern void	delete_pcode(Pcode *pcode);
extern void	delete_symbol(int i);
extern int	get_new_symbol();
extern int	is_a_number(char *str);
extern int	parse_str_size();
extern void	pump_close (pump_handle *p);
extern void	pump_flush (pump_handle *p);
extern int      pump_getc (pump_handle *p);
extern pump_handle *pump_open (const char *cmd);
extern void     pump_putc (int c, pump_handle *p);
extern int      pump_read_any (pump_handle *p, char *str, int len);
extern int      read_any(int fd, char *buf, int len);

#define min(a,b) ( (a) < (b) ? (a) : (b) )
#define max(a,b) ( (a) > (b) ? (a) : (b) )

static float a[16], b[16];

/*
char **A = (char **)a;
char **B = (char **)b;
*/

pump_handle **A, **B;

static void wrap_pump_open(void)
{
	char *s;
	A[0] = pump_open(s = kpl_token_name(abs(kpl_pop(a))));
	kpl_push(a, 1);
}

static void wrap_pump_close(void)
{
	kpl_pop(a);
	pump_close(A[0]);
}

static int sscan_a(char *str, float *a)
{
	int len, offset, i;
	float v[64];

	if (! is_a_number(str))
		return -kpl_declare_token(str, P_VAR);

	len = sscanf(str,
"%g %g %g %g %g %g %g %g %g %g %g %g %g %g %g %g\
 %g %g %g %g %g %g %g %g %g %g %g %g %g %g %g %g\
 %g %g %g %g %g %g %g %g %g %g %g %g %g %g %g %g\
 %g %g %g %g %g %g %g %g %g %g %g %g %g %g %g %g\
 %g %g %g %g %g %g %g %g %g %g %g %g %g %g %g %g\
 %g %g %g %g %g %g %g %g %g %g %g %g %g %g %g %g\
 %g %g %g %g %g %g %g %g %g %g %g %g %g %g %g %g\
 %g %g %g %g %g %g %g %g %g %g %g %g %g %g %g %g",
	    v+0, v+1, v+2, v+3, v+4, v+5, v+6, v+7,
	    v+8, v+9, v+10,v+11,v+12,v+13,v+14,v+15,
	    v+16,v+17,v+18,v+19,v+20,v+21,v+22,v+23,
	    v+24,v+25,v+26,v+27,v+28,v+29,v+30,v+31,
	    v+32,v+33,v+34,v+35,v+36,v+37,v+38,v+39,
	    v+40,v+41,v+42,v+43,v+44,v+45,v+46,v+47,
	    v+48,v+49,v+50,v+51,v+52,v+53,v+54,v+55,
	    v+56,v+57,v+58,v+59,v+60,v+61,v+62,v+63);

	len    = max(0,  len);
	offset = max(0,  len - 16);
	len    = min(16, len);
	for (i = 0 ; i < len ; i++)
		a[i] = v[offset + i];

	return len;
}

static void wrap_pump_get(void)
{
	char str[1000];
	int i, c;

	kpl_pop(a);

	pump_flush(A[0]);
	for (i = 0 ; (c = pump_getc(A[0])) != '\n' ; i++)
		str[i] = c;
	str[i] = '\0';

	kpl_push(a, sscan_a(str, a));
}

static void wrap_pump_read(void)
{
	char str[1000];

	kpl_pop(a);
	str[pump_read_any(A[0], str, 1000)] = '\0';
	kpl_push(a, sscan_a(str, a));
}

static void wrap_pump_readevalget(void)
{
	char *pump_buf(), *str;
	int n, i;

	kpl_pop(a);

	str = pump_buf(A[0]);
	n = strlen(str);
	i = pump_read_any(A[0], str+n, 10000 - n);
	if (i > 0)
	    str[n+i] = '\0';
}

static void wrap_pump_readeval(void)
{
	char *pump_buf(), *parse_str(), *str = parse_str(), *s;
	int n, i;
	Pcode *pcode;

	kpl_pop(a);

/*
	str = pump_buf(A[0]);
	for (n = 0 ; n == 0 || str[n-1] != '\n' ; )
	   if ( (n += pump_read_any(A[0], str+n, 10000 - n)) <= 0)
		return;
*/
	for (n = 0 ; n == 0 || str[n-1] != '\n' ; )
	   if ( (n += pump_read_any(A[0], str+n, parse_str_size())) <= 0)
		return;
	str[n] = '\0';

	if (kpl_debug() == 1)
		printf("readeval: str = %s", str);

	collect_new_symbols();
	kpl_execPcode(pcode = kpl_parse(str));
	delete_pcode(pcode);
	while (i = get_new_symbol()) {
	    for (s = kpl_token_name(i) ; *s && *s != ' ' ; s++)
		;
	    if (*s == ' ')
		delete_symbol(i);
	}
	strcpy(str, "");
}

static void wrap_pump_put(void)
{
	int len, i;
	char str[1024], num[1024], *s;

	kpl_pop(b);

	len = kpl_pop(a);
	strcpy(str, "");
	if (len < 0)
		strcpy(str, kpl_token_name(abs(len)));
	else
		for (i = 0 ; i < len ; i++) {
			sprintf(num, "%g", a[i]);
			strcat(str, num);
			if (i < len-1)
			    strcat(str, ":");
		}

	for (s = str ; *s ; s++) {
		pump_putc(*s, B[0]);
		if (*s == '\n')
			pump_flush(B[0]);
	}
}

static void wrap_pump_flush(void)
{
	kpl_pop(a);
	pump_putc('\n', A[0]);
	pump_flush(A[0]);
}

static void wrap_pump_poke(void)
{
	kpl_pop(a);
	pump_putc('\n', A[0]);
}

void install_kpl_pump(void)
{
	kpl_install("pump_open",	(Proc)wrap_pump_open);
	kpl_install("pump_close",	(Proc)wrap_pump_close);
	kpl_install("pump_flush",	(Proc)wrap_pump_flush);
	kpl_install("pump_get",		(Proc)wrap_pump_get);
	kpl_install("pump_put",		(Proc)wrap_pump_put);
	kpl_install("pump_poke",	(Proc)wrap_pump_poke);
	kpl_install("pump_read",	(Proc)wrap_pump_read);
	kpl_install("pump_readeval",	(Proc)wrap_pump_readeval);
	kpl_install("pump_readevalget",	(Proc)wrap_pump_readevalget);
}

