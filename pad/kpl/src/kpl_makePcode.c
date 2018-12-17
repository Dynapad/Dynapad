/* read from input parser preprocessor */

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include "kpl.h"

#ifdef PAD_WIN
#  include "../../win/windefs.h"
#endif
extern void delete_symbol(int i);
extern int is_a_number(char *str);
extern int is_ephemeral(int id);
extern char *my_index(char *str, int c);


char *kpl_symbol_value[MAXSYMBOL] = { 0 };

static Pcode *first_pcode = NULL;
static Pcode *last_pcode;
static void init_read_input(char *input, int input_size);

static int read_input(char *data, int size);
static void addPcode(int type, char *data, char *name);

static char *alloc(int n)
{
	char *s;
	s = (char*)calloc(n, sizeof(char));
	if (s == NULL)
		printf("out of memory\n");
	return s;
}

static void parse_error(char *str)
{
	fprintf(stderr, "parse error: %s\n", str);
	exit(1);
}

void free_pcode()
{
	Pcode *pcode, *next_pcode;

	for (pcode = first_pcode ; pcode ; pcode = next_pcode) {
		next_pcode = pcode->next;
		free((char *)pcode->symbol);
		free((char *)pcode);
	}
	first_pcode = NULL;
}

static int proc_num, last_proc_num = 0;

void reset_code(level)
{
	if (level > 1)
		last_proc_num = 0;
	proc_num = last_proc_num;
}

void delete_pcode(Pcode *pcode)
{
	Pcode *next;

	for ( ; pcode ; pcode = next) {
		next = pcode->next;
		free((char *)pcode->symbol);
		free((char *)pcode);
	}
	reset_code(1);
}

Pcode *kpl_makePcode(char *input, int input_size)
{
	char name[1024];
	char *data;
	int type, i, trace;
	Value *v;

	trace = (getenv("TRACE") != NULL);
	init_read_input(input, input_size);

	first_pcode = (Pcode *)alloc(sizeof(Pcode));
	first_pcode->symbol = (Symbol *)alloc(sizeof(Symbol));
	first_pcode->symbol->type = 0;
	last_pcode = first_pcode;

	while (read_input((char *)&type, sizeof(int)) > 0) {

		switch (type) {
		case P_VALUE:
			(void)read_input((char *)&i, sizeof(int));
			v = (Value *)alloc(sizeof(int) + i * sizeof(float));
			v->len = i;
			data = (char *)v;
			(void)read_input(data + sizeof(int), i * sizeof(float));
			strcpy(name, "* value *");
			break;
		case '\'':
			(void)read_input((char *)&i, sizeof(int));
			(void)read_input((char *)&i, sizeof(int));
			v = (Value *)alloc(sizeof(int) + sizeof(float));
			v->len = -i;
			type = P_VALUE;
			data = (char *)v;
			strcpy(name, kpl_token_name(i));
			break;
		case P_VAR:
		case P_USERPROC:
		case P_UNOP:
		case P_BINOP:
			(void)read_input((char *)&i, sizeof(int));
			data = (char *) i;
			strcpy(name, kpl_token_name(i));
			break;
		default:
			data = 0;
			if (type < 256) {
				name[0] = type;
				name[1] = '\0';
			}
			else
				kpl_sprinttoken(name, type);
		}

		addPcode(type, data, name);
		if (trace)
			printf("%s\n", name);
	}

	if (last_proc_num == 0)
		last_proc_num = proc_num;

	return first_pcode;
}

static char *in0, *in;
static int nin;

static void init_read_input(char *input, int input_size)
{
	in = in0 = input;
	nin = input_size;
}

static int read_input(char *data, int size)
{
	while (size--)
		*data++ = *in++;
	return (in - in0 <= nin);
}

static level = 0;
static Pcode *stack[100];

static void addPcode(int type, char *data, char *name)
{
	Pcode *pcode = (Pcode *)alloc(sizeof(Pcode));
	int l, t, id;
	char token[10];

	/* make the new node the last thing on the linked list */
	last_pcode->next = pcode;
	last_pcode = pcode;

	pcode->symbol = (Symbol *)alloc(sizeof(Symbol));
	pcode->symbol->type = type;
	pcode->symbol->value = (Value *)data;
	strcpy((pcode->symbol->name = malloc(strlen(name)+1)), name);

	if (type == '{') {			/* Begin proc */
		sprintf(token, "#%d", ++proc_num);
		id = kpl_declare_token(token, P_METAPROC);
		kpl_symbol_value[id] = (char *)pcode;
		pcode->symbol->value = (Value *)id;
	}

	if (type == '(' || type == '{' || type == '[')
		stack[level++] = pcode;
	else if (type == '?') {			/* Conditional break */
		t = 0;
		for (l = level-1 ; l >= 0 ; l--) {	/* matches innermost */
			t = stack[l]->symbol->type;	/* loop or proc */
			if (t == '[' || t == '{')
				break;
		}
		switch (t) {
		case '[':
		case '{':
			pcode->branch = stack[l];
			break;
		default:
			parse_error("`?' outside of any loop or procedure");
		}
	}
	else if (type == ')' || type == '}' || type == ']') {
		if (level < 1) {
			printf("parse error: unmatched '");
			putchar(type);
			printf("'\n");
			exit(1);
		}
		stack[--level]->branch = pcode;
	}
	if (type == ']')			/* End loop */
		pcode->branch = stack[level];
	if (type == '|') {			/* Else */
	        stack[level-1]->branch = pcode;
		stack[level-1] = pcode;
	}
}

void kpl_sprintvalue(char *str, float data[], int len)
{
	int w, id;
	char tmpstr[1024];
	Pcode *pcode;
	Value *v;
	int level;

	if (len < -10000)
		sprintf(str, "<array>");
	else if ( (id = -len) > 0) {
		if (kpl_token_type(id) == P_METAPROC) {
			pcode = (Pcode *)kpl_token_value(id);
			strcpy(str, "");
			for (level = 0 ; pcode ; pcode = pcode->next) {
			    if (pcode->symbol->type == P_VALUE) {
				v = pcode->symbol->value;
				kpl_sprintvalue(tmpstr, v->data, v->len);
			    }
			    else
				strcpy(tmpstr, pcode->symbol->name);
			    strcat(tmpstr, " ");
			    strcat(str, tmpstr);
			    if (tmpstr[0] == '{') level++;
			    if (tmpstr[0] == '}') level--;
			    if (level == 0)
				break;
			}
		}
		else
			strcpy(str, kpl_token_name(id));
		if (is_ephemeral(id))
			delete_symbol(id);
	}
	else if (len == 0)
		sprintf(str, ":");
	else {
		strcpy(str, "");
		for (w = 0 ; w < len ; w++) {
			sprintf(tmpstr, "%g",data[w]);
			strcat(str, tmpstr);
			if (w < len-1)
				strcat(str, ":");
		}
	}
}

int kpl_sscanvalue(char *str, float data[])
{
	char *s;
	int w, len;

        if (*str != ':' && ! is_a_number(str))
		return -kpl_declare_token(str, P_VAR);
	len = 0;
	s = str;
	for (w = 0 ; w < 16 ; w++) {
		while (*s && *s == ':')
			s++;
		if (*s == '\0')
			break;
		data[w] = atof(s);
		len++;
		while (*s && *s != ':')
			s++;
		if (*s == '\0')
			break;
	}
	return len;
}

