
#include <stdio.h>
#include "hash.h"

main()
{
	char str[100], *s = str;
	int c, adding = 1;

	while ( (c = getchar()) != EOF ) {
		*s = c;
		if (c == '-' && s == str)
			adding = 0;
		else if (c == '\n') {
			*s = '\0';
			if (adding)
				printf("%d\n", hash(str, strlen(str)+1));
			else
				unhash(str, strlen(str)+1);
			s = str;
			adding = 1;
		}
		else
			s++;
	}
}

