#ifndef KPL_DEFINED
#define KPL_DEFINED

#include "kpl_tokens.h"

#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE 1
#endif

typedef int    (*Proc)();
typedef double (*FProc)();

typedef struct Value {
	int len;
	float data[16];
} Value;

typedef struct Symbol {
        char *name;
	int type;
	Value *value;
} Symbol;

typedef struct Pcode {
        Symbol *symbol;
	struct Pcode *next, *branch;
} Pcode;

#define MAXSYMBOL 6000
#ifdef __cplusplus
#define EXTERN extern "C"
#else
#define EXTERN extern
#endif

EXTERN int	kpl_acreate(void);
EXTERN void	kpl_adelete(int id);
EXTERN int	kpl_aget(int id, int i, float *data);
EXTERN void	kpl_aset(int id, int i, float *data, int len);
EXTERN int	kpl_asize(int id);
EXTERN void	kpl_binop(char* str, Proc proc);
EXTERN int	kpl_declare_token(char *str, int type);
EXTERN int	kpl_debug(void);
EXTERN int	kpl_execPcode(Pcode *pcode_addr);
EXTERN void	kpl_install(char* str, Proc proc);
EXTERN char	*kpl_input_file(void);
EXTERN void	kpl_install_type(char *str, Proc proc, int type);
EXTERN int	kpl_lex(char *str, char *buf, int bufsize);
EXTERN Pcode	*kpl_makePcode(char *input, int input_size);
EXTERN Pcode	*kpl_parse(char *str);
EXTERN Pcode	*kpl_parsefile(char *file);
EXTERN int	kpl_pop(float *data);
EXTERN void	kpl_push(float *data, int len);
EXTERN void	kpl_set_token_type(int id, int type);
EXTERN void	kpl_sprinttoken(char *str, int type);
EXTERN void	kpl_sprintvalue(char *str, float data[], int len);
EXTERN int	kpl_sscanvalue(char *str, float data[]);
EXTERN int	kpl_stack_get(int i, float data[]);
EXTERN int	kpl_stack_level(void);
EXTERN void	kpl_stack_set(int i, float data[], int len);
EXTERN char	*kpl_token_name(int id);
EXTERN int	kpl_token_type(int id);
EXTERN char	*kpl_token_value(int id);
EXTERN void	kpl_unop(char* str, Proc proc);

#endif

