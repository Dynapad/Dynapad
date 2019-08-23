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

#ifndef MISC_H
#define MISC_H 1

#include "defs.h"
#include "point.h"
#include "plist.h"
#include "pad-string.h"
#include <regex.h>

class  Pad_Win;
class  Pad;

class Pad_Time
{
  public:
    long Get_sec(void);
    long Get_usec(void);
    void Update(void);

  private:
    struct timeval _time;
};

///////////////////////////////////////////////////////////////////////
//
// Function Prototypes
//
///////////////////////////////////////////////////////////////////////

Pad_Bool Pad_Atob(char *string);
void     Pad_Background_error(const char *err);
void     Pad_Debug_output(Pad_Win *win, char *msg);
Pad_Bool Pad_Expand_pathname(char *name, Pad_String &fullName);
Pad_Bool Pad_Get_relative_pathname(char *currentName, char *newName,    
                                   Pad_String &relativeName);
float    Pad_Miter_length(float lineWidth, float x1, float y1, float x2, float y2, float x3, float y3);
void     Pad_Swap(float &a, float &b);
float    Pad_Siso_lerp(float t);

typedef void *CallbackData;
typedef void (Pad_TimerCallback)(CallbackData callbackData);
typedef void *Pad_TimerToken;
extern "C" Pad_TimerToken Pad_CreateTimerHandler(int delay_in_milliseconds,
                                      Pad_TimerCallback *timerCallback,
                                      CallbackData callbackData);
extern "C" void     Pad_DeleteTimerHandler(Pad_TimerToken token);
#define PAD_READABLE  1
#define PAD_WRITABLE  2
#define PAD_EXCEPTION 4
typedef void (Pad_FileCallback)(CallbackData callbackData, int mask);
extern "C" void     Pad_CreateFileHandler(int fd,
                               Display *display,
                               int mask,
                               Pad_FileCallback *fileCallback,
                               CallbackData callbackData);
void     Pad_DeleteFileHandler(int fd);

extern "C" void Pad_MainLoop(void);

// Used by Java/X version to change the routines used to manage timers
typedef Pad_TimerToken (Pad_CreateTimerProc)(int, Pad_TimerCallback *cb, void *);
typedef void (Pad_DeleteTimerProc)(Pad_TimerToken);
void     Pad_SetTimerProcs(Pad_CreateTimerProc *createProc, Pad_DeleteTimerProc *delProc);

int      Pad_Random(int min, int max);

char *   Pad_GetUid(const char *name);

void Pad_Rotate(Pad_PList &coords_plist,
               float theta,
               Pad_Point &center);



///////////////////////////////////////////////////////////////////////
//
// Some conversion macros
//
///////////////////////////////////////////////////////////////////////

//
// Convert float to short
//
inline short Pad_F2s(float x) 
{
                                // Important to round properly
    if (x > 0) {
        x += 0.5;
    } else {
        x -= 0.5;
    }       
    if (x < -32768) x = -32768;
    if (x > 32767) x = 32767;
    return (short)x;
}

//
// Convert float to short, and leave an extra 10,000
// pixels plus whatever the user requested for use with regions,
// and rendering.  X servers seem to crash sometimes when objects
// with coordinates even remotely close to +/- 32,768 are drawn.
// Safer to just clip 'em.
//
inline short Pad_F2rs(float x, int width) 
{
    int offset;

    offset = width + 10000;
                                // Important to round properly
    if (x > 0) {
        x += 0.5;
    } else {
        x -= 0.5;
    }       
    if ((x - offset) < -32768) x = -32768 + offset;
    if ((x + offset) > 32767) x = 32767 - offset;
    return (short)x;
}

typedef enum {
    PAD_ANCHOR_N, PAD_ANCHOR_NE, PAD_ANCHOR_E, PAD_ANCHOR_SE,
    PAD_ANCHOR_S, PAD_ANCHOR_SW, PAD_ANCHOR_W, PAD_ANCHOR_NW,
    PAD_ANCHOR_CENTER
} Pad_Anchor;

#define PAD_OK        0
#define PAD_ERROR     1
#define PAD_RETURN    2
#define PAD_BREAK     3
#define PAD_CONTINUE  4

#define PAD_RESULT_SIZE 200

int Pad_GetAnchor(char *string, Pad_Anchor *anchorPtr);
char *Pad_NameOfAnchor(Pad_Anchor anchor);

char *casestrstr(char *s1, char *s2);

int Pad_StringMatch(char *string, char *pattern);

typedef regex_t* Pad_RegExp;
Pad_RegExp Pad_RegExpCompile(char *pattern);
int Pad_RegExpExec(Pad_RegExp regexp, char *string);
void Pad_RegExpFree(Pad_RegExp regexp);

#endif


#ifndef HAVE_STRCASECMP_PROTO
// note: was previous throw ()/ noexcept(true)
// causes a compile error on systems that have /usr/include/strings.h from glibc
#ifdef __APPLE__
extern "C" int strcasecmp(const char *str1, const char *str2);
#endif
#endif
