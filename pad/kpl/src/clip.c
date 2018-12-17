static char rcsid[] = "$Header: /nas/backup/old-home/home/cvs/dynapad/pad/kpl/src/clip.c,v 1.1.1.1 2005/10/09 04:28:47 stanonik Exp $";
/* clip line segment pq to box with lo,hi corners a,b in n-space */

#include <stdio.h>

#define cliptype(type, clip, code, clipp, clipt)\
\
int code(int n, type *p, type *lo, type *hi)\
{\
        int pcode = 0;\
\
        while (n--)\
                pcode |= ((p[n] < lo[n]) | (p[n] > hi[n]) << 1) << (n+n);\
        return pcode;\
}\
\
static void clipt(int n, type *p, type *q, type a, type b)\
{\
        int i;\
\
        for (i = 0 ; i < n ; i++)\
                p[i] = q[i] + a * (p[i] - q[i]) / b;\
}\
\
static int clipp(int pcode, int n, type *p, type *q, type *lo, type *hi)\
{\
        int i, bit;\
\
        for (i = 0, bit = 1 ; i < n ; i++, bit <<= 2)\
                if (pcode & bit) {\
                        clipt(n, p, q, lo[i] - q[i], p[i] - q[i]);\
                        break;\
                }\
                else if (pcode & (bit << 1)) {\
                        clipt(n, p, q, hi[i] - q[i], p[i] - q[i]);\
                        break;\
                }\
        return code(n, p, lo, hi);\
}\
\
int clip(int n, type *p, type *q, type *lo, type *hi)\
{\
        int pcode = code(n, p, lo, hi);\
        int qcode = code(n, q, lo, hi);\
\
        while (pcode | qcode) {\
                if (pcode & qcode)\
                        return 0;\
                else if (pcode)\
                        pcode = clipp(pcode, n, p, q, lo, hi);\
                else\
                        qcode = clipp(qcode, n, q, p, lo, hi);\
        }\
        return 1;\
}

cliptype(int,   iclip, icode, iclipp, iclipt)
cliptype(float, fclip, fcode, fclipp, fclipt)
cliptype(double,dclip, dcode, dclipp, dclipt)

