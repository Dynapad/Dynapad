
/* evaluate the // slider, and replace its two
   args with a single interpolated arg */

#include <stdio.h>
#include <math.h>

static float value;

main(int ac, char **av)
{
	char str[256], *s;
	int c;

	if (ac <= 1) {
		printf("usage: %s value <infile >outfile\n", av[0]);
		exit(1);
	}
	value = atof(av[1]);

	for (c = 0 ; c != EOF ; ) {
	    for (s = str ; (c = getchar()) != EOF ; s++)
		if ( (*s = c) == '\n')
		    break;
	    *++s = '\0';
	    process_line(str);
	}
}

process_line(char *str)
{
	char *s, *a, *b, tmp[256], answer[256], trail[256];
	int nx, ny, n, i;
	float x[16], y[16];

	for (s = str ; *s ; s++)
	    if (s[0] == '/' && s[1] == '/') {
	        for (b = s-1 ; b > str && isspace(*b) ; b--)
		    ;
	        for ( ; b > str && ! isspace(*b) ; b--)
		    ;
	        for (a = b-1 ; a > str && isspace(*a) ; a--)
		    ;
	        for ( ; a > str && ! isspace(*a) ; a--)
		    ;
		if (isspace(*a))
		    a++;
		nx=sscanf(a, "%f:%f:%f:%f:%f:%f:%f:%f:%f:%f:%f:%f:%f:%f:%f:%f",
			x,x+1,x+2,x+3,x+4,x+5,x+6,x+7,x+8,
			x+9,x+10,x+11,x+12,x+13,x+14,x+15);
		ny=sscanf(b, "%f:%f:%f:%f:%f:%f:%f:%f:%f:%f:%f:%f:%f:%f:%f:%f",
			y,y+1,y+2,y+3,y+4,y+5,y+6,y+7,y+8,
			y+9,y+10,y+11,y+12,y+13,y+14,y+15);
		n = nx;
		if (ny > nx)
		    n = ny;
		strcpy(answer, "");
		for (i = 0 ; i < n ; i++) {
		    sprintf(tmp, "%g", x[i%nx] + value * (y[i%ny] - x[i%nx]));
		    strcat(answer, tmp);
		    if (i < n-1)
			strcat(answer, ":");
		}
		strcpy(trail, s+2);
		strcpy(a, answer);
		strcat(a, trail);
		if (a > str)
		   a--;
		s = a;
	    }
	printf("%s", str);
}

