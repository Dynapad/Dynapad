/*
"(c) Copyright 1993-1997 Pad++ Consortium {University of New Mexico (UNM),
and New York University (NYU)}, All Rights Reserved."  
Licensee can not remove or obscure any of the
copyright or trademark notices in this software.

IN NO EVENT SHALL THE AUTHORS OR DISTRIBUTORS BE LIABLE TO ANY PARTY
FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
ARISING OUT OF THE USE OF THIS SOFTWARE, ITS DOCUMENTATION, OR ANY
DERIVATIVES THEREOF, EVEN IF THE AUTHORS HAVE BEEN ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.

THE AUTHORS AND DISTRIBUTORS SPECIFICALLY DISCLAIM ANY WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE, AND NON-INFRINGEMENT.  THIS SOFTWARE
IS PROVIDED ON AN "AS IS" BASIS, AND THE AUTHORS AND DISTRIBUTORS HAVE
NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
MODIFICATIONS.

See the file "License" for general information on usage and
redistribution, and the file "LicenseTerms" for the specific license
agreement on usage and redistribution of this file, and the Pad++
software in general.
*/

#include "defs.h"
#include "misc.h"
#include "plist.h"
#include "point.h"
#include "list.h"
#include "win.h"
#include "global.h"
#include "restorer.h"
#include "hashtab.h"

#include <unistd.h>
#include <stdlib.h>
#include <math.h>
#include <errno.h>
#include <string.h>
#include <sys/time.h>

#if defined(sun)
extern "C" {
long random();
}
#endif

#ifndef HAVE_RANDOM
#  define random rand
#endif




//
// System independent way of getting the current
// time in seconds and microseconds.
//
void
Pad_Time::Update(void)
{

    gettimeofday(&_time, NULL);

}

long
Pad_Time::Get_sec(void)
{
    return(_time.tv_sec);
}

long
Pad_Time::Get_usec(void)
{
    return(_time.tv_usec);
}

//
// Convert a string to a Pad_Bool
//
Pad_Bool
Pad_Atob(char *string)
{
    int ival;
    Pad_Bool rc;

    ival = atoi(string);
    if (ival == 0) {
	rc = FALSE;
    } else {
	rc = TRUE;
    }

    return(rc);
}

//
// Return a random integer between <min> and <max>, inclusively
//
int
Pad_Random(int min, int max)
{
    int r;

    r = min + (random() % (max - min + 1));

    return(r);
}

//
// Map the input linear lerp (a linear interpolation from (0-1) to
// a slow-in, slow-out lerp.
//
float 
Pad_Siso_lerp(float t)
{
    float siso, t1, t2, l;

    t1 = t * t;
    t2 = 1 - (1 - t) * (1 - t);
    l = LERP(t, t1, t2);
    siso = LERP(l, t1, t2);
    
    return(siso);
}

//
// Output the specified message to the current debug output place.
// This may be stdout, or a Tcl variable, depending on the value
// of win->debugOut.  A carriage return is automatically added
// to the end of each message.
//
void
Pad_Debug_output(Pad_Win *win, char *msg) {

    if (msg) fprintf(stderr, "event %s\n", msg);
    if (win->debugOut) {
	    ; //???
    } else {
	    ; //???
    }
}

//
// Pad_Rotate receives a Pad_PList "coord_plist",
// an angle in degrees "theta", and a center of 
// rotation "center", and transforms coord_plist
// by rotating each of its points about "center"
// by angle "theta".  Positive rotate is counter-clockwise.
//  
// This is a utility function used by the "Rotate"
// functions for each object for which rotation is 
// defined.
//
void
Pad_Rotate(Pad_PList &coordPlist,
	   float theta,
	   Pad_Point &center) 
{

  Pad_Point *tmpPt;
  int pt, lenPlist;
  double thetaRad;
  float oldTmpx,oldTmpy;

  thetaRad = -DEG2RAD(theta);
  lenPlist = coordPlist.Length();

  for (pt=0;pt<lenPlist;pt++)
    {

      tmpPt = coordPlist.Nth(pt);
      oldTmpx = tmpPt->x;
      oldTmpy = tmpPt->y;

      tmpPt->x =
	center.x +
	((oldTmpx)-center.x)*cos(thetaRad) +
	((oldTmpy)-center.y)*sin(thetaRad);

      tmpPt->y =
	center.y +
	((oldTmpy)-center.y)*cos(thetaRad) -
	((oldTmpx)-center.x)*sin(thetaRad);
    }
}

//
// Compute the miter length between the three specified points
// with the specified line width
// 
float
Pad_Miter_length(float lineWidth, float x1, float y1, float x2, float y2, float x3, float y3)
{
    int cdeg;
    double c;
    double a2, b2, c2;
#define S 0.00001

				// Use cosine rule to convert lengths a,b,c to angle ABC
    a2 = (double)(POW2(x3 - x2) + POW2(y3 - y2));
    b2 = (double)(POW2(x2 - x1) + POW2(y2 - y1));
    c2 = (double)(POW2(x3 - x1) + POW2(y3 - y1));
    c = acos((a2 + b2 - c2) / (2.0 * sqrt(a2 + S) * sqrt(b2 + S)));
	
				// Coerce angle C to degrees to test for angles < 11
    cdeg = (int)((c * 360.0) / PITIMES2);
	
				// Miter length is line width * (1 / (sin(C / 2))
    c = sin(c / 2.0);
	
				// Cut off a bit early to be careful
    if ((cdeg % 360 <= 9) || (cdeg % 360 > 85) || (c == 0.0)) {
	return(0.0);
    } else {
	return(lineWidth * (0.5 / c));
    }
}

//
// Expand <name> into an absolute pathname, taking care
// of ~-expansion, and collapsing '.'s and '..'s.
// This fills in <fullName>, and returns TRUE if successfull,
// or FALSE otherwise.
//
Pad_Bool
Pad_Expand_pathname(char *name, Pad_String &fullName)
{
    int i, j;
    int index;
    Pad_String buf;
    Pad_String buffer;

    // Take care of ~-expansion ???

    fullName = name;
	
				// Expand relative pathname
    if (fullName.Get_char(0) != '/' &&
		( (fullName.Get_char(1) != ':') || (fullName.Get_char(2) != '/')) ) {
	//extern int errno; 
	char cwd[10240]; // yes, someday it will be too small - genghis ron
	if (!getcwd((char *)&cwd, sizeof(cwd))) {
	    fprintf(stderr, "Pad_Expand_pathname getcwd failed %s\n",
		strerror(errno));
	    return FALSE;
	}
	buf = cwd;
	buf += "/";
	buf += fullName;
	fullName = buf;
    }

				// Take care of '.'s and '..'s
    index = 0;
    do {
				// Find delimiting '/'s
	i = fullName.Strchr('/', index);
	j = fullName.Strchr('/', i + 1);
	if (i == -1) {
	    break;
	}
	if (j == -1) {
	    j = i + 1;
	}
	if ((j == i+2) && (fullName.Get_char(i+1) == '.')) {
				// '.' found
	    buf = fullName.Get(j);
	    fullName.Insert(&buf, i);
	    index = i;
	} else if ((j == i+3) && 
		   (fullName.Get_char(i+1) == '.') &&
		   (fullName.Get_char(i+2) == '.')) {
				// '..' found
	    
	    buf = fullName.Get(j);
	    fullName.Insert('\0', i);
	    i = fullName.Strrchr('/');
	    if (i == -1) {
		Pad_errorString = "Invalid filename, there are too many '..'s";
		return(FALSE);
	    }
	    fullName.Insert('\0', i);
	    fullName.Append(buf);
	    index = i;
	} else {
	    index = j;
	}
    } while(1);

    return(TRUE);
}

//
// Return the relative pathname of <newName> relative to <dir>.
// Put result in <relativeName>.  Returns TRUE if succcessfull,
// or FALSE otherwise.
//
Pad_Bool
Pad_Get_relative_pathname(char *dir, char *newName, Pad_String &relativeName)
{
    int i, j, k;
    int common;
    Pad_Bool rc;
    Pad_String name;
    Pad_String currentDir;

				// Compare filename with directory, one
				// dir at a time, finding which shared
				// dirs there are.
    currentDir = dir;
    rc = Pad_Expand_pathname(newName, name);  // Turn into a standard absolute pathname
    if (!rc) {
	return(FALSE);
    }
    common = 0;
    k = 0;
    do {
	i = currentDir.Strchr('/', k);
	j = name.Strchr('/', k);
	if ((i == j) && (i != -1) && (!currentDir.Strncmp(name, i))) {
	    common = i;
	    k = i + 1;
	} else {
	    break;
	}
    } while(1);

				// Now, build result, backing up as many
				// directories as are not shared
    k = common + 1;
    relativeName = "";
    do {
	i = currentDir.Strchr('/', k);
	if (i == -1) {
	    break;
	} else {
	    relativeName += "../";
	    k = i + 1;
	}
    } while (1);
    relativeName += name.Get(common + 1);

    return(TRUE);
}

//
// Data used by Pad_Background_error
//
static Pad_String errorStr;

//
// Pad_Background_error
//
//   Similar to Tk_BackgroundError, but works within a render by
//   forcing the actual background error to occur after a timer
//   (of zero seconds) expires.  Thus, the error occurs at the top
//   level.
//
void Pad_Background_error(const char *err)
{
    fprintf(stderr, "Pad_Background_error: %s\n", err);
}

//
// Swap the two specified numbers
//
void
Pad_Swap(float &a, float &b)
{
    float temp;

    temp = a;
    a = b;
    b = temp;
}

int
Pad_GetAnchor(char *string, Pad_Anchor *anchorPtr)
{
    switch (string[0]) {
	case 'n':
	    if (string[1] == 0) {
		*anchorPtr = PAD_ANCHOR_N;
		return PAD_OK;
	    } else if ((string[1] == 'e') && (string[2] == 0)) {
		*anchorPtr = PAD_ANCHOR_NE;
		return PAD_OK;
	    } else if ((string[1] == 'w') && (string[2] == 0)) {
		*anchorPtr = PAD_ANCHOR_NW;
		return PAD_OK;
	    }
	    goto error;
	case 's':
	    if (string[1] == 0) {
		*anchorPtr = PAD_ANCHOR_S;
		return PAD_OK;
	    } else if ((string[1] == 'e') && (string[2] == 0)) {
		*anchorPtr = PAD_ANCHOR_SE;
		return PAD_OK;
	    } else if ((string[1] == 'w') && (string[2] == 0)) {
		*anchorPtr = PAD_ANCHOR_SW;
		return PAD_OK;
	    } else {
		goto error;
	    }
	case 'e':
	    if (string[1] == 0) {
		*anchorPtr = PAD_ANCHOR_E;
		return PAD_OK;
	    }
	    goto error;
	case 'w':
	    if (string[1] == 0) {
		*anchorPtr = PAD_ANCHOR_W;
		return PAD_OK;
	    }
	    goto error;
	case 'c':
	    if (strncmp(string, "center", strlen(string)) == 0) {
		*anchorPtr = PAD_ANCHOR_CENTER;
		return PAD_OK;
	    }
	    goto error;
    }

    error:
        /*
        "bad anchor position \"", string,
        "\": must be n, ne, e, se, s, sw, w, nw, or center",
        */
    return PAD_ERROR;
}

char *
Pad_NameOfAnchor(Pad_Anchor anchor)
{
    switch (anchor) {
	case PAD_ANCHOR_N: return "n";
	case PAD_ANCHOR_NE: return "ne";
	case PAD_ANCHOR_E: return "e";
	case PAD_ANCHOR_SE: return "se";
	case PAD_ANCHOR_S: return "s";
	case PAD_ANCHOR_SW: return "sw";
	case PAD_ANCHOR_W: return "w";
	case PAD_ANCHOR_NW: return "nw";
	case PAD_ANCHOR_CENTER: return "center";
    }
    return "unknown anchor position";
}

char *
Pad_GetUid(const char *string)
{
	static int init;
	static Pad_HashTable *uidTable;
	char *value;

	if (!init) {
		uidTable = new Pad_HashTable(PAD_STRING_TABLE);
		init++;
	}
	if (value = (char *)uidTable->Get((void *)string))
		return value;
	value = strdup(string);
	uidTable->Set(value, value);

	return value;
}

//
// Case insensitive version of strstr
//

char *casestrstr(char *s1, char *s2) 
{
    char *p1, *p2;

    if (*s2 == 0) {
	return((char *)0);
    }
    while (*s1) {
	p1 = s1;
	p2 = s2;
	do {
	    if (*p2 == 0) {
		return(s1);
	    }
	    if (tolower(*p1) != tolower(*p2)) {
		break;
	    }
	    p1++;
	    p2++;
	} while (1);
	s1++;
    }

    return((char *)0);
}

#include <sys/types.h>
#include <regex.h>

int
Pad_StringMatch(char *string, char *pattern)
{
    int errcode;
    int errbuf_size = 1024;
    char errbuf[errbuf_size];
    char patbuf[1024];  // yes, someday it will be too small
    regex_t preg;
    int match = 0;

    sprintf(patbuf, "^%s$", pattern);

    if ((errcode = regcomp(&preg, patbuf, 0)) != 0) {
	regerror(errcode, &preg, errbuf, errbuf_size);
        fprintf(stderr, "regcomp(%s) failed %s\n", patbuf, errbuf);
        return 1;
    }

    if ((errcode = regexec(&preg, string, 0, 0, 0)) != 0) {
	regerror(errcode, &preg, errbuf, errbuf_size);
        //fprintf(stderr, "regexec(%s) failed %s\n", patbuf, errbuf);
	match = 1;
    }

    regfree(&preg);

    return match;
}

Pad_RegExp
Pad_RegExpCompile(char *pattern)
{
    regex_t *regexp;
    int errcode;
    int errbuf_size = 1024;
    char errbuf[errbuf_size];

    if ((regexp = (regex_t *)calloc(sizeof(regex_t), 1)) == NULL) {
        fprintf(stderr, "Pad_RegExpCompile: malloc failed\n");
	return NULL;
    }
    if ((errcode = regcomp(regexp, pattern, 0)) != 0) {
	regerror(errcode, regexp, errbuf, errbuf_size);
        fprintf(stderr, "Pad_RegExpCompile: regcomp(%s) failed %s\n",
	  pattern, errbuf);
        return NULL;
    }

    return (Pad_RegExp)regexp;
}

int
Pad_RegExpExec(Pad_RegExp regexp, char *string)
{
    int errcode;
    int errbuf_size = 1024;
    char errbuf[errbuf_size];
    int match = 0;

    if ((errcode = regexec(regexp, string, 0, 0, 0)) != 0) {
	regerror(errcode, regexp, errbuf, errbuf_size);
        fprintf(stderr, "Pad_RegExpExec: regexec(%s) failed %s\n",
	  string, errbuf);
	match = 1;
    }
    return match;
}

void
Pad_RegExpFree(Pad_RegExp regexp)
{
    if (regexp != NULL) {
        regfree(regexp);
	free(regexp);
    }
}
