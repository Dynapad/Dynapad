
/* input parser preprocessor for kpl */

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>
#include <fcntl.h>
#include "kpl.h"
#include "hash.h"

#ifdef PAD_WIN
#  include "../../win/windefs.h"
#  include <io.h>
#endif

typedef struct {
	int type;
	char *name;
} Token;

extern int exec_error(char *str);
extern char *kpl_symbol_value[];
extern void set_input_file(char *str);

static unsigned char ephemeral_flag[MAXSYMBOL] = { 0 };

int is_ephemeral(int id)
{
	return ephemeral_flag[abs(id)];
}

int ephemeral(int id)
{
	ephemeral_flag[abs(id)] = TRUE;
	return id;
}

/*-------- interface routines for string hashing --------*/

static int is_hashing = TRUE;

void no_hashing() { is_hashing = FALSE; }

static unsigned long dummy_hash_address;

static unsigned long *str_hash(char *str)
{
	unsigned long *hash();

	if (is_hashing)
		return (unsigned long *)get_hashtab_entry(str);

	dummy_hash_address = 0;
	return & dummy_hash_address;
}

static void str_unhash(char *str)
{
	if (is_hashing)
		free_hashtab_entry(str);
}

/*-----------------------------------------------------*/

char *my_index(char *str, int c)
{
	char *s;

	for (s = str ; *s ; s++)
		if (*s == c)
			return s;
	return NULL;
}

static Token name[MAXSYMBOL] = {
	{ 0		, "<null>"	 },
	{ P_ASSIGN	, "=>"		 },
	{ P_EQ		, "=="		 },
	{ P_EXIT	, "exit"	 },
	{ P_GE		, ">="		 },
	{ P_GETV	, "getv"	 },
	{ P_INDEX	, "index"	 },
	{ P_JOIN	, "join"	 },
	{ P_LE		, "<="		 },
	{ P_LEVEL	, "level"	 },
	{ P_LPUSH	, "<-"		 },
	{ P_NE		, "!="		 },
	{ P_PRINT	, "print"	 },
	{ P_RPUSH	, "->"		 },
	{ P_SETV	, "setv"	 },
	{ P_SYSTEM	, "system"	 },
	{ P_TROFF	, "troff"	 },
	{ P_TRON	, "tron"	 }
};

void kpl_sprinttoken(char *str, int type)
{
	int i;

	for (i = 0 ; i < 30 ; i++)
		if (name[i].type == type) {
			sprintf(str, name[i].name);
			break;
		}
}

int is_a_number(char *str);

static char *add_token(char *str, char *buf, int bufsize);
static char *output_token(int type, char *data, char *buf, int bufsize);
static char *write_output(char *data, int n, char *buf, int bufsize);

int kpl_name_id(char *str)
{
    int		i;

    for (i = 0; i < MAXSYMBOL; i++)
    {
	if (name[i].type == 0)
	    break;
	
	if (! strcmp (str, name[i].name))	/* found it */
	    return (i);
    }

    return -1;
}

static is_init_parse = FALSE;

static void init_parse()
{
	int i;

	if (is_init_parse == FALSE) {
		for (i = 0 ; name[i].name ; i++)
			*str_hash(name[i].name) = i;
		is_init_parse = TRUE;
	}
}

void kpl_install(char* str, Proc proc) {kpl_install_type(str,proc,P_USERPROC);}
void kpl_unop(char* str, Proc proc) { kpl_install_type(str, proc, P_UNOP);}
void kpl_binop(char* str, Proc proc) { kpl_install_type(str, proc, P_BINOP);}

static void eval_escape_chars(char *dst, char *src);

static int new_symbol[200], ns = 0;
static int collecting_new_symbols = FALSE;

void collect_new_symbols()
{
	collecting_new_symbols = TRUE;
	ns = 0;
}

int get_new_symbol()
{
	collecting_new_symbols = FALSE;
	return (ns > 0) ? new_symbol[--ns] : 0;
}

void delete_symbol(int i)
{
	str_unhash(name[i].name);
	name[i].name = NULL;
}

static void add_symbol(int i, char *str, int type)
{
	if (collecting_new_symbols)
	    new_symbol[ns++] = i;

	name[i].name = str;
	name[i].type = type;
}

int kpl_declare_token(char *str, int type)
{
	int i;
	char *buf = (char *)malloc(strlen(str)+1);
	unsigned long *h;

	eval_escape_chars(buf, str);
	if ( i = *(h = str_hash(buf)) )
		free(buf);
	else {
		for (i = 0 ; name[i].name ; i++)
			if (strcmp(name[i].name, buf) == 0)
				break;
		if (name[i].name == NULL)
			add_symbol(*h = i, buf, type);
		else
			free(buf);
	}
	return i;
}

void kpl_install_type(char *str, Proc proc, int type)
{
	int i;

	init_parse();
	i = kpl_declare_token(str, type);
	kpl_symbol_value[i] = (char *) proc;
}

int kpl_token_type(int id)
{
	if (id < 0 || id >= MAXSYMBOL)
		return -1;

	return name[id].type;
}

void kpl_set_token_type(int id, int type)
{
	if (id >= 0 && id < MAXSYMBOL)
		name[id].type = type;
}

char *kpl_token_value(int id)
{
	if (id < 0 || id >= MAXSYMBOL)
		return NULL;

	return kpl_symbol_value[id];
}

char *kpl_token_name(int id)
{
	if (id < 0 || id >= MAXSYMBOL)
		return NULL;

	return name[id].name;
}

#define STRSIZE 250000
#define BUFSIZE (2 * STRSIZE)

static char buf[BUFSIZE];

static void write_names(int fd)
{
	int i, n;

	for (i = 0 ; name[i].type != P_VAR ; i++)
		;
	for (n = 0 ; name[i+n].name ; n++)
		;
	write(fd, &n, sizeof(int));
	for ( ; name[i].name ; i++)
		write(fd, name[i].name, strlen(name[i].name) + 1);
}

static int read_char(int fd)
{
	char s[1];

	if (read(fd, s, 1) < 1)
		printf("EOF in read\n");
	printf("char = %d\n", s[0]);
	return s[0];
}

static void read_names(int fd)
{
	int i, n;
	char str[256], *s;

	for (i = 0 ; name[i].name ; i++)
		;
	read(fd, &n, sizeof(int));
	for ( ; n-- ; i++) {
		read(fd, s = str, 1);
		while (*s)
			read(fd, ++s, 1);
		add_symbol(i, strcpy(malloc(strlen(str)+1), str), P_VAR);
	}
}

static void write_kplcode(int n)
{
	int fd = open("kplcode", O_WRONLY | O_CREAT, 0666);

	write_names(fd);
	write(fd, &n, sizeof(int));
	write(fd, buf, n);
	close(fd);
}

static int read_kplcode()
{
	int n, fd = open("kplcode", O_RDONLY);

	read_names(fd);
	read(fd, &n, sizeof(int));
	read(fd, buf, n);
	close(fd);
	return n;
}

static void printtokens()
{
	int i;

	for (i = 0 ; name[i].name ; i++)
		printf("%d\t%s\n", name[i].type, name[i].name);
}

Pcode *kpl_parse(char *str)
{
	int n;

	if ((n = kpl_lex(str, buf, BUFSIZE)) < 0)
		return NULL;

	if (getenv("VERBOSE"))
		printtokens();
	if (getenv("KPLCODE"))
		write_kplcode(n);
	return kpl_makePcode(buf, n);
}

static int is_substring(char *s1, char *s2)
{
	while (*s1 || *s2)
		if (*s2 == '\0')
			return TRUE;
		else if (*s1++ != *s2++)
			break;
	return FALSE;
}

static void get_include_file_name(char *file, char *str)
{
	char *s1, *s2, *my_index();

	s1  = my_index(str, '"');
	s2  = my_index(++s1, '"');
	*s2 = '\0';
	strcpy(file, s1);
}

static char parse_str_buf[STRSIZE];

char *parse_str()
{
	return parse_str_buf;
}

int parse_str_size()
{
	return STRSIZE;
}

Pcode *kpl_parsefile(char *file)
{
	FILE *fp;
	char *s = parse_str(), str[1024], *tmp;
	int c;

	if (strcmp(file, "kplcode") == 0)
		return kpl_makePcode(buf, read_kplcode());

	fp = fopen(file, "r");
	if (fp == NULL && (tmp = getenv("PADHOME"))) {
		sprintf(str, "%s/kpl/lib/%s", tmp, file);
		fp = fopen(str, "r");
	}
	if (fp == NULL) {
		sprintf(str, "can't read file %s", file);
		exec_error(str);
		exit(1);
	}
	if (strcmp(kpl_input_file(), "") == 0)
		set_input_file(file);

	while ( (c = fgetc(fp)) != EOF )
		*s++ = c;
	*s = '\0';

	fclose(fp);
	return kpl_parse(parse_str());
}

static int lex_state = 0;

#define Comment_begin(s) (s[0] == '/' && s[1] == '*')
#define Comment_end(s)   (s[0] == '*' && s[1] == '/')
#define Comment_delim_size 2

int kpl_lex(char *str, char *buf, int bufsize)
{
	char token[1024], *s, *b;
	int j;

	b = buf;

	for (s = str ; *s ; ) {

		while (isspace(*s))
			s++;
		if (! *s)
			break;

		/* treat a string in double quotes as one quoted symbol */
		if (*s == '"') {
			token[0] = '\'';
			for (j = 1; *++s && *s != '"'; j++) {
				if (*s == '\\' && s[1] == '"')
					s++;
				token[j] = *s;
			}
			token[j] = '\0';
			b = add_token(token, b, bufsize - (b - buf));
			if (*s == '"')
				s++;
		}
		/* skip over C style comments */
		else if (Comment_begin(s)) {
			s += Comment_delim_size;
			while (*s && !Comment_end(s))
				s++;
			s += Comment_delim_size;
		}
		else {
			for (j = 0 ; *s && !isspace(*s) ; j++)
				token[j] = *s++;
			token[j] = '\0';
			b = add_token(token, b, bufsize - (b-buf));
		}
		if (b == NULL)
			return -1;
	}
	return b - buf;
}

static char *add_token(char *str, char *buf, int bufsize)
{
	int i, quoted;
	Value value;

	if (!isalnum(*str) && *str != ':' && *str != '\'' && str[1] == '\0')
		buf = output_token(*str, (char *)NULL, buf, bufsize);
	else if (*str == ':' || is_a_number(str)) {
		value.len = kpl_sscanvalue(str, value.data);
		buf = output_token(P_VALUE, (char *)&value, buf, bufsize);
	}
	else {
		if (quoted = (str[0] == '\''))
	    		buf = output_token(*str++, NULL, buf, bufsize);
		i = kpl_declare_token(str, P_VAR);
		buf = output_token(
			quoted ? P_VAR : name[i].type, (char *)&i, buf, bufsize);
	}
	return buf;
}

int is_a_number(char *str)
{        
        char *s = str;
        int status = FALSE;

        if (*s == 'e')
                return FALSE;

        for ( ; *s && !isspace(*s) ; s++)
                if (isdigit(*s) || my_index("-+.:e", *s))
                        status = TRUE;
                else
                        return FALSE;
        return status;
}

static char *
output_token(int type, char *data, char *buf, int bufsize)
{
	Value *value;

	buf = write_output((char *)&type, sizeof(int), buf, bufsize);
	switch (type) {
	case P_VALUE:
		value = (Value *)data;
		buf = write_output(data,
			sizeof(int) + value->len * sizeof(float),
				buf, bufsize);
		break;
	case P_VAR:
	case P_USERPROC:
	case P_UNOP:
	case P_BINOP:
		buf = write_output(data, sizeof(int), buf, bufsize);
		break;
	}
	return buf;
}

static char *
write_output(char *data, int n, char *buf, int bufsize)
{
	if (buf == NULL || n > bufsize)
		buf = NULL;
	else
		while (n--)
			*buf++ = *data++;
	return buf;
}

static void eval_escape_chars(char *dst, char *src)
{
	int c;
	char *d = dst, *s = src;

	while (c = *s++)
		if (c != '\\')
			*d++ = c;
		else
			switch (c = *s++) {
			case '\0':
				*d = '\0';
				return;
			case 'n':
				*d++ = '\n';
				break;
			case 't':
				*d++ = '\t';
				break;
			default:
				*d++ = c;
				break;
			}
	*d = '\0';
}

