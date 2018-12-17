
#include "kpl.h"
#include <stdio.h>

extern void	install_kpl_g(void);
extern void	install_kpl_g_raw(void);
extern void	install_kpl_graphics(void);
extern void	install_kpl_lib(void);
extern void	install_kpl_matrix(void);
extern void	install_kpl_physics(void);
extern void	install_kpl_pump(void);
extern void	install_kpl_sumvec(void);
extern void	install_kpl_vec(void);
extern void	install_kpl_w(void);

#define STRLEN 100000

static char str[STRLEN+1];
static void printstack(void);

static char input_file[256] = "";

char *kpl_input_file()
{
	return input_file;
}

void set_input_file(char *str)
{
	strcpy(input_file, str);
}

int main(int ac, char *av[])
{
	Pcode *pcode;
	char str[256];
	int status;

	install_kpl_lib();
	install_kpl_matrix();
	install_kpl_physics();
	install_kpl_pump();
	install_kpl_sumvec();
	install_kpl_vec();
	install_kpl_w();

	while (ac > 1 && av[1][0] == '-')
		switch (av[1][1]) {
		case 't':
			do_timing();
			ac--;
			av++;
			break;
		}

	if (ac == 1)
		while (fgets(str, 256, stdin)) {
			kpl_execPcode(kpl_parse(str));
			printstack();
		}
	else {
		pcode = kpl_parsefile(av[1]);
		while (status = kpl_execPcode(pcode)) {
			reset_code(status);
			if (status == 2)
				pcode = kpl_parsefile(av[1]);
		}
		printstack();
		print_proc_times();
	}
	exit(0);
}

static void printstack(void)
{
	float data[20];
	int len;

	while (kpl_stack_level() > 0) {
		len = kpl_pop(data);
		kpl_sprintvalue(str, data, len);
		printf("%s ", str);
	}
	printf("\n");
}

