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

#include "../generic/xdrutil.h"
#include "xdr.h"

typedef unsigned int   u_int;
typedef char *         caddr_t;
typedef unsigned long  u_long;
typedef unsigned short u_short;
typedef unsigned char  u_char;
typedef unsigned long  __uint64_t;

#define enum_t int
#define bool_t int

 void   xdrmem_create(XDR *, void *, u_int, enum xdr_op){}
 void	xdr_free(xdrproc_t, void *){}
 bool_t	xdr_void(XDR *, void *){return 0;}
 bool_t	xdr_int(XDR *, int *){return 0;}
 bool_t	xdr_u_int(XDR *, u_int *){return 0;}
 bool_t	xdr_long(XDR *, long *){return 0;}
 bool_t	xdr_u_long(XDR *, u_long *){return 0;}
 bool_t	xdr_short(XDR *, short *){return 0;}
 bool_t	xdr_u_short(XDR *, u_short *){return 0;}
 bool_t	xdr_char(XDR *, char *){return 0;}
 bool_t	xdr_u_char(XDR *, u_char *){return 0;}
 bool_t	xdr_bool(XDR *, bool_t *){return 0;}
 bool_t	xdr_enum(XDR *, enum_t *){return 0;}
 bool_t	xdr_array(XDR *, caddr_t *, u_int *, u_int, u_int, xdrproc_t){return 0;}
 bool_t	xdr_bytes(XDR *, char **, u_int *, u_int){return 0;}
 bool_t	xdr_opaque(XDR *, void *, u_int){return 0;}
 bool_t	xdr_string(XDR *, char **, u_int){return 0;}
 bool_t	xdr_union(XDR *, enum_t *, void *, struct xdr_discrim *, 
			xdrproc_t){return 0;}
 bool_t	xdr_reference(XDR *, caddr_t *, u_int, xdrproc_t){return 0;}
 bool_t	xdr_u_longlong_t(XDR *, __uint64_t *){return 0;}
 bool_t	xdr_uint32(XDR *, u_int *){return 0;}
 bool_t	xdr_uint64(XDR *, __uint64_t *){return 0;}

 bool_t	xdr_vector(XDR *, char *, u_int, u_int, xdrproc_t){return 0;}
 bool_t	xdr_float(XDR *, float *){return 0;}
 bool_t	xdr_double(XDR *, double *){return 0;}
 bool_t	xdr_pointer(XDR *, caddr_t *, u_int, xdrproc_t){return 0;}
 bool_t	xdr_wrapstring(XDR *, char **){return 0;}
