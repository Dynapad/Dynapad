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

#ifndef WINDEFS_H
#define WINDEFS_H 1

#pragma warning (disable : 4244)
#pragma warning (disable : 4245)
#pragma warning (disable : 4355) // 'this' : used in base member initializer list
								 // generic/text.cpp
#pragma warning (disable : 4065) // switch statement contains 'default' but no 'case' labels
								 // generic/text.cpp 
#pragma warning (disable : 4099) // 'Pcode' : type name first seen using 'class' now seen using 'struct'
								 // generic/tkpad.cpp kpl/src/kpl.h
#pragma warning (disable : 4018) // '!=' : signed/unsigned mismatch
								 // generic/win.cpp 
#pragma warning (disable : 4035) // kpl/ no return value 
#include <windef.h> 

#define HAVE_STRCASECMP_PROTO
#define HAVE_STRNCASECMP_PROTO

#define strncasecmp strnicmp
#define strcasecmp  stricmp

#endif
