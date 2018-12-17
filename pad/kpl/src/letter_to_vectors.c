/* Draw characters in a simple vector font */

#include <stdio.h>
#include <ctype.h>

#ifdef PAD_WIN
#  include "../../win/windefs.h"
#endif

static char *spec[] =
{
 "",        "neB",     "mjNk",    "ndOeJlGi","nbLjgifd","odMF",   "iedgnmjf","ok",	
 "njdb",    "nlfb",    "keJfLdGi","keGi",    "hea",     "gi",     "e",       "od",	
 "nliegjn", "jneDf",   "molgdf",  "mokifd",  "engi",    "omjlifd","omdflj",  "mohe",	
 "mofdmJl", "dfomjl",  "kE",      "kHea",    "ogc",     "jlGi",   "mia",     "jnlheB",	
 "fdmoihkl","dgnifGi", "dmnkJlfd","omdf",    "dmnlied", "omdfJk", "omdJk",   "omdfih",	
 "mdOfJl",  "moNeDf",  "ofdg",    "mdOjf",   "mdf",     "dmhof",  "dmfo",    "mofdm",	
 "dmolj",   "moiedmFh","dmoljf",  "omjlfd",  "moNe",    "mdfo",   "meo",     "mdkfo",
 "mfOd",    "mkoKe",   "modf",    "nmab",    "mf",      "nocb",   "jnl",     "ac",
 "mk",      "jlfdgi",  "mdflj",   "ljdf",    "ofdjl",   "fdjlig", "oneJl",   "fdjlca",
 "mdJlf",   "nKe",     "nKbad",   "mdLgf",   "mne",     "djlfKe", "jdGklf",  "djlfd",
 "ajlfd",   "cljdf",   "djl",     "ljgifd",  "nefJl",   "jdeiLf", "jel",     "jdkfl",
 "jfDl",    "jeLea",   "jldf",    "okecGh",  "nb",      "mkeaIh", "jnko"
};

#define AddPt(x,y,z) { *d++ = (x)/3.; *d++ = (y)/5.; *d++ = z; }

int letter_to_vectors(int key, float *data)
{
	int c, k, i, j, is_draw;
	float *d = data;
	char *s;

	if (key < 32 || key > 127)
		return 0;

	s = spec[key - 32];
	for (k = 0 ; c = s[k] ; k++) {
		is_draw = k && islower(c);
		c = tolower(c);
		i = (c - 'a') % 3;
		j = (c - 'a') / 3;
		AddPt(i, j, is_draw);
		if (!is_draw && (s[k+1] == '\0' || isupper(s[k+1]))) {
			AddPt(i-.25, j, 0);
			AddPt(i+.25, j, 1);
			AddPt(i, j-.25, 0);
			AddPt(i, j+.25, 1);
		}
	}
	return (d - data) / 3;
}

