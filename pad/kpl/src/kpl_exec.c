#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "kpl.h"

#ifdef PAD_WIN
#  include "../../win/windefs.h"
#endif

extern void kpl_print(char*);
extern char *kpl_symbol_value[];

#ifndef min
#define min(a,b) ( (a) < (b) ? (a) : (b) )
#endif
#ifndef max
#define max(a,b) ( (a) > (b) ? (a) : (b) )
#endif

#define si(i) stack[level-(i)]
#define vi(i) (si(i).data)
#define li(i) (si(i).len)

#define s0 si(0)
#define s1 si(1)
#define s2 si(2)

#define v0 vi(0)
#define v1 vi(1)
#define v2 vi(2)

#define l0 li(0)
#define l1 li(1)
#define l2 li(2)

#define f0 v0[0]
#define f1 v1[0]
#define f2 v2[0]

#define MAXLEVEL 2000

static level = 0;
static Value stack[MAXLEVEL];

static is_tron = FALSE;

#define EQSYMBOL(istrue) \
	if (l1 < 0 || l0 < 0) { \
		v1[0] = (istrue ? l1 == l0 : l1 != l0); \
		l1 = 1; \
		Pop(); \
		break; \
	}

#define BINOP(op) \
	p0 = v0; \
	p1 = v1; \
	if (l1 == 0) { l1 = 1; f1 = 0.; } \
	if (l0 == 0) { l0 = 1; f0 = 0.; } \
	if (l1 == 1 && l0 == 1) \
		f1 = f1 op f0; \
	else if (l1 == 1) \
		for (f = f1, w = l0 ; w-- > 0 ; ) \
			p1[w] = f op p0[w]; \
	else if (l0 == 1) \
		for (w = l1 ; w-- > 0 ; ) \
			p1[w] = p1[w] op f0; \
	else \
		for (w = l0 ; w-- > 0 ; ) \
			p1[w] = p1[w] op p0[w]; \
	l1 = (l1 > l0 ? l1 : l0); \
	Pop(); \
	break;

#define BINFUNC(func) \
	p0 = v0; \
	p1 = v1; \
	if (l1 == 0) { l1 = 1; f1 = 0.; } \
	if (l0 == 0) { l0 = 1; f0 = 0.; } \
	if (l1 == 1 && l0 == 1) \
		f1 = (func)(f1, f0); \
	else if (l1 == 1) \
		for (f = f1, w = l0 ; w-- > 0 ; ) \
			p1[w] = (func)(f, p0[w]); \
	else if (l0 == 1) \
		for (w = l1 ; w-- > 0 ; ) \
			p1[w] = (func)(p1[w], f0); \
	else \
		for (w = l0 ; w-- > 0 ; ) \
			p1[w] = (func)(p1[w], p0[w]); \
	l1 = (l1 > l0 ? l1 : l0); \
	Pop(); \
	break;

#define UNFUNC(func) \
	p0 = v0; \
	if (l0 == 1) \
		f0 = (func)(f0); \
	else \
		for (w = l0 ; w-- > 0 ; ) \
			p0[w] = (func)(p0[w]); \
	break;

int kpl_stack_level(void)
{
	return level;
}

int kpl_stack_set_level(int n)
{
	level = n;
	return level;
}

static Pcode *pcode;

#define Symbol_id ( (int) pcode->symbol->value )

#define Symbol_value ( kpl_token_value(Symbol_id) )

int exec_error(char *str)
{
	fprintf(stderr, "%s: %s at symbol `%s'\n", kpl_input_file(), str, pcode->symbol->name);
	exit(1);
	return 1;
}

/*------ routines that deal with a variable's nesting level -------*/

static char *vspace[MAXSYMBOL];
static short vlevel[MAXSYMBOL];
static short vnesting[MAXSYMBOL] = { 0 };

extern char *new_virtual_space(int pagesize);
extern char *virtual_page(char *client, int pagenumber);

static void declare_var(int id)
{
	Value *v;

	if (kpl_token_type(id) == P_VAR && kpl_symbol_value[id] != NULL)
		return;
	kpl_set_token_type(id, P_VAR);

	vspace[id] = new_virtual_space(sizeof(Value));
	kpl_symbol_value[id] = virtual_page(vspace[id], vlevel[id] = 0);

	v = (Value *)kpl_symbol_value[id];
	v->len = 1;
	v->data[0] = 0.;
}

static void set_var_level(int id, int level)
{
	declare_var(id);
	kpl_symbol_value[id] = virtual_page(vspace[id], vlevel[id] = level);
}

static int get_var_level(int id)
{
	declare_var(id);
	return vlevel[id];
}

void kpl_push_var(int id)
{
	set_var_level(id, vlevel[id]+1);
}

void kpl_pop_var(int id)
{
	set_var_level(id, vlevel[id]-1);
}

int kpl_stack_get_len(int i)
{
	return li(i);
}

float *kpl_stack_get_data(int i)
{
	return vi(i);
}

/*----------------------------------------------------------------*/

static int is_exported(int id)
{
	char *s;

	if (s = kpl_token_name(id))
		return *s != '#';
	printf("in is_exported: unexpected token id %d\n", id);
	return FALSE;
}

static int nesting_level(void);
static void push_local_var(int var_id);

static void adjust_var_level_up(int id)
{
	if (! is_exported(id) && nesting_level() > vnesting[id]) {
		kpl_push_var(id);
		*(int *)(kpl_symbol_value[id]) = vnesting[id];
		vnesting[id] = nesting_level();
		push_local_var(id);
	}
}

static void adjust_var_level_down(int id)
{
	if (! is_exported(id)) {
		vnesting[id] = *(int *)(kpl_symbol_value[id]);
		kpl_pop_var(id);
	}
}

int kpl_get(int id, float *data)
{
	int i;
	Value *v;

	if (id < 0 || id >= MAXSYMBOL)
		return 0;

	declare_var(id);
	adjust_var_level_up(id);
	v = (Value *)kpl_symbol_value[id];
	for (i = 0 ; i < v->len ; i++)
		data[i] = v->data[i];

	return v->len;
}

void kpl_set(int id, float *data, int len)
{
	int i;
	Value *v;

	if (id < 0 || id >= MAXSYMBOL)
		return;

	declare_var(id);
	adjust_var_level_up(id);
	v = (Value *)kpl_symbol_value[id];
	v->len = len;
	for (i = 0 ; i < v->len ; i++)
		v->data[i] = data[i];
}

#define Pop() ( level <= 0 ? exec_error("empty stack") : level-- )
#define Push() ( level >= MAXLEVEL-1 ? exec_error("full stack") : level++ )

void kpl_push(float *data, int len)
{
	int w;

	Push();
	l0 = len;
	for (w = 0 ; w < len ; w++)
		v0[w] = data[w];
}

int kpl_pop(float *data)
{
	int w, len;

	len = l0;
	for (w = 0 ; w < len ; w++)
		data[w] = v0[w];
	Pop();
	return len;
}

int kpl_stack_get(int i, float *data)
{
	int w, len;

	len = li(i);
	for (w = 0 ; w < len ; w++)
		data[w] = vi(i)[w];
	return len;
}

void kpl_stack_set(int i, float *data, int len)
{
	int w;

	li(i) = len;
	for (w = 0 ; w < len ; w++)
		vi(i)[w] = data[w];
}

static int is_zero_vector(float *vec, int len)
{
	if (len == 0)
		return TRUE;
	if (len < 0)
		return FALSE;
	while (--len >= 0)
		if (vec[len] != 0.)
			return FALSE;
	return TRUE;
}

static Pcode *return_stack[1000], **return_address = return_stack;

int local_var_stack[1000], *local = local_var_stack;

static int nesting_level(void)
{
	return return_address - return_stack;
}

static void push_local_var(int var_id)
{
	if (kpl_symbol_value[var_id] == NULL)
		kpl_set(var_id, NULL, 0);

	kpl_push_var(var_id);

	*local++ = var_id;
}

static float proc_total_time[MAXSYMBOL] = {0.};
static float proc_local_time[MAXSYMBOL] = {0.};
static int proc_parent[MAXSYMBOL] = {0};
extern float seconds_elapsed(void);
static int is_timing = FALSE;
static float times1[100], *total_time = times1;
static float times2[100], *local_time = times2;
static int pids[100], *pid = pids;

void do_timing(void)
{
	is_timing = TRUE;
}

void print_proc_times(void)
{
	int i;

	if (is_timing)
	for (i = 0 ; i < MAXSYMBOL ; i++)
		if (proc_total_time[i] > 0.) {
			printf("%7.3f %7.3f %20s",
			proc_local_time[i],
			proc_total_time[i],
			kpl_token_name(i));
			if (proc_parent[i])
				printf(" (%s)", kpl_token_name(proc_parent[i]));
			printf("\n");
		}
}

static void eval_metaproc(int proc_id, int var_id)
{
	*return_address++ = pcode;

	if (is_timing)
		if (*++pid = var_id)
			*++total_time = *++local_time = -seconds_elapsed();

	pcode = (Pcode *)kpl_token_value(proc_id);

	*local++ = 0;
}

static void return_from_metaproc()
{
	int var_id;
	float t;

	while (var_id = *--local) {
		kpl_pop_var(var_id);
		adjust_var_level_down(var_id);
	}

	if (is_timing)
		if (var_id = *pid--) {
			t = seconds_elapsed();
			proc_total_time[var_id] += (*total_time += t);
			proc_local_time[var_id] += (*local_time += t);
			proc_parent[var_id] = *pid;
			*--local_time -= *total_time--;
		}

	pcode = *--return_address;
}

static void eval_var(int id)
{
	int len;
	float data[16];

	len = kpl_get(id, data);
	if (len < 0 && kpl_token_type(-len) == P_METAPROC)
		eval_metaproc(-len, id);
	else
		kpl_push(data, len);
}

/*---------- Allow nested interpreters ------------*/

static Pcode *save_pcode[100];
static int pcode_level = 0;

static void exec_begin()
{
	save_pcode[pcode_level++] = pcode;
}

static void exec_end(void)
{
	pcode = save_pcode[--pcode_level];
}

static int exec_loop(Pcode *pcode_addr);

/*-------------------------------------------------*/

static float divide(float a, float b);
static Pcode *parent_pcodes[100] = {NULL}, **parent_pcode = parent_pcodes;

int kpl_execPcode(Pcode *pcode_addr)
{
	int status = 0;

	if (pcode_addr) {
		exec_begin();
		status = exec_loop(pcode_addr);
		exec_end();
	}
	return status;
}

static char str[4096];

static int exec_loop(Pcode *pcode_addr)
{
	
	float f, vec[16], *p0, *p1;
	int i, w, type;
	Value *v;
	char *s;

	for (pcode = pcode_addr->next ; pcode ; pcode = (pcode ? pcode->next : NULL) )
	{
		if (is_tron)
			if (pcode->symbol->type == P_VALUE) {
				v = pcode->symbol->value;
				kpl_sprintvalue(str, v->data, v->len);
				printf("%s\n", str);
			}
			else
				printf("%s\n", pcode->symbol->name);

		type = pcode->symbol->type;
		switch (type) {
		case P_TROFF:
			is_tron = FALSE;
			break;
		case P_TRON:
			is_tron = TRUE;
			break;
		case P_EXIT:
			i = f0;
			Pop();
			return i;
		case P_LEVEL:
			vec[0] = level;
			kpl_push(vec, 1);
			break;
		case P_PRINT:
			kpl_sprintvalue(str, v0, l0);
			kpl_print(str);
			Pop();
			break;
		case P_USERPROC:
			( (Proc) Symbol_value ) (0);
			break;
		case P_UNOP:
			s = Symbol_value;
			UNFUNC((FProc)s);
			break;
		case P_BINOP:
			s = Symbol_value;
			BINFUNC((FProc)s);
			break;
		case P_SYSTEM:
			if (l0 < 0) {
				kpl_sprintvalue(str, v0, l0);
				system(str);
			}
			Pop();
			break;
		case '{':
			kpl_push(vec, -Symbol_id);
			pcode = pcode->branch;
			break;
		case '|':
		case ']':
			pcode = pcode->branch;
			break;
		case '(':
			if (is_zero_vector(v0, l0))
				pcode = pcode->branch;
			Pop();
			break;
		case '?':
			if (is_zero_vector(v0, l0)) {
				pcode = pcode->branch;
				if (pcode->symbol->type == '[')
					pcode = pcode->branch;
				if (pcode->symbol->type == '{')
					return_from_metaproc();
			}
			Pop();
			break;
		case '}':
			return_from_metaproc();
			break;
		case P_GETV:
			f = get_var_level(abs(l0));
			Push();
			l0 = 1;
			f0 = f;
			break;
		case P_SETV:
			set_var_level(abs(l1), (int)f0);
			Pop();
			break;
		case P_LPUSH:
			push_local_var(abs(l0));
		case '=':
			kpl_set(abs(l0), v1, l1);
			Pop();
			Pop();
			break;
		case P_ASSIGN:
		case P_RPUSH:
			pcode = pcode->next;
			if (is_tron)
				printf("%s\n", pcode->symbol->name);
			if (type == P_RPUSH && is_exported(Symbol_id))
				push_local_var(Symbol_id);
			kpl_set(Symbol_id, v0, l0);
			Pop();
			break;
		case P_VALUE:
			Push();
			v = pcode->symbol->value;
			l0 = v->len;
			p0 = v0;
			for (w = l0 ; w-- > 0 ; )
				p0[w] = v->data[w];
			break;
		case P_VAR:
			eval_var(Symbol_id);
			break;
		case '`':	/* evaluate a symbol value on the stack */
			if ( (i = -l0) > 0 ) {
				Pop();
				s = kpl_token_value(i);
				switch (kpl_token_type(i)) {
				case P_VAR:
					eval_var(i);
					break;
				case P_METAPROC:
					eval_metaproc(i, 0);
					break;
				case P_USERPROC:
					((Proc)s)();
					break;
				case P_UNOP:
					UNFUNC((FProc)s);
					break;
				case P_BINOP:
					BINFUNC((FProc)s);
					break;
				default:
					Push();
					break;
				}
			}
			break;
		case P_INDEX:
			p0 = v0;
			p1 = v1;
			if (l1 < 0) {
				kpl_sprintvalue(str, v1, l1);
				for (w = 0 ; w < l0 ; w++)
					p0[w] = str[max(0,min(255,(int)p0[w]))];
			}
			else
				for (w = 0 ; w < l0 ; w++) {
					i = p0[w];
					p0[w] = (i >= 0 && i < l1) ? p1[i] : 0.;
				}
			l1 = l0;
			for (w = 0 ; w < l0 ; w++)
				p1[w] = p0[w];
			Pop();
			break;
		case ':':
			Push();
			l0 = 0;
			break;
		case P_JOIN:
			p0 = v0;
			p1 = v1 + l1;
			i = max(0, min(16 - l1, l0));
			for (w = 0 ; w < i ; w++)
				p1[w] = p0[w];
			l1 += i;
			Pop();
			break;
		case '+':	BINOP(+);
		case '-':	BINOP(-);
		case '*':	BINOP(*);
		case '/':	BINFUNC(divide);
		case '<':	EQSYMBOL(FALSE) BINOP(<);
		case '>':	EQSYMBOL(FALSE) BINOP(>);
		case P_LE:	EQSYMBOL(TRUE)  BINOP(<=);
		case P_GE:	EQSYMBOL(TRUE)  BINOP(>=);
		case P_NE:	EQSYMBOL(FALSE) BINOP(!=);
		case P_EQ:
			w = -1;
			if (l0 == l1)
			    if (l0 < 0)
				w = l0;
			    else
				for (w = 0 ; w < l0 ; w++)
					if (v0[w] != v1[w])
						break;
			v1[0] = (w == l0) ? 1. : 0.;
			l1 = 1;
			Pop();
			break;
		}
		if (pcode == *parent_pcode)
			break;
	}
	return 0;
}

static float divide(float a, float b)
{
	if (b == 0.) {
		printf("\tdivide by zero\n");
		return a;
	}
	else
		return a / b;
	
}

void kpl_eval(int id)
{
	*++parent_pcode = pcode;
	eval_metaproc(abs(id), 0);
	kpl_execPcode(pcode);
	pcode = *parent_pcode;
	*parent_pcode-- = NULL;
}

