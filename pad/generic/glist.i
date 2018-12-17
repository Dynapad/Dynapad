/*
"(c) Copyright 1993-1995 Pad++ Consortium {University of New Mexico (UNM),
New York University (NYU)}, All Rights Reserved."  Licensee can not remove 
or obscure any of the copyright or trademark notices in this software.

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

/* glist

Defines a generic array-style list class. Data is stored in packed 
arrays. The array size is doubled each time the array fills up. 

To avoid using C++ template classes (which aren't very portable) the code 
instead makes use of two macros:

    PAD_LIST_CLASS - the name of the C++ class for the list.

    LIST_TYPE - the type of items to be stored in the list. 
	By default, the data should fit into one word.  If it is
	larger than one word (such as a class), then references to
	the class instance must be passed in and returned.  In order
	to specify that this is the case, define the following two 
	macros:

	#define LIST_REF &
	#define LIST_PTR *

To use this mechanism to define a list class:

    1. Create a xlist.h header file which defines values for the two macros, 
       includes glist.h and then undefines these two macros. e.g.

            #define PAD_LIST_CLASS Pad_BList
            #define LIST_TYPE unsigned char
            #include "glist.h"
            #undef LIST_TYPE
            #undef PAD_LIST_CLASS

    2. Create a xlist.C code file which includes xlist.h, then defines
       the two macros (again) and includes "glist.i" (the implementation 
       portion of the list code). e.g.

           #include "xlist.h"
           #define PAD_LIST_CLASS Pad_BList
           #define LIST_TYPE unsigned char
           #include "glist.i"
           #undef LIST_TYPE
           #undef PAD_LIST_CLASS
*/

#include "defs.h"  // using ../win/windefs.h to get rid of warining in win95

#include <string.h>
#include <assert.h>

#ifndef LIST_PTR
#  define LIST_PTR
#endif
#ifndef LIST_REF
#  define LIST_REF
#endif

//
// Array-based list implementation.
// The array is automatically doubled in size when an item is
// added to it and there is not space for it.
//

PAD_LIST_CLASS::~PAD_LIST_CLASS()
{
    if (_data) {
	delete [] _data;
	_data = NULL;
	_length = 0;
	_space = 0;
    }
}

PAD_LIST_CLASS::PAD_LIST_CLASS()
{
    _length = 0;
    _space = 0;
    _data = NULL;
}

//
// Double space allocated for data,
// and copy list to new space.
//
void 
PAD_LIST_CLASS::extend(int n)
{
    LIST_TYPE *new_data;
    typedef LIST_TYPE Data;

    if (_space > n) {
	return;
    }
    while (_space <= n) {
	_space += MAX(2, _space);
    }
    new_data = new Data[_space];
    if (_data) {
	memcpy((char*)new_data, (char*)_data, sizeof(Data) * _length);
	delete [] _data;
    }
    _data = new_data;
}

void 
PAD_LIST_CLASS::Make_empty(void)
{
    _length = 0;
}

int 
PAD_LIST_CLASS::Is_empty(void)
{
    return(_length == 0);
}

void 
PAD_LIST_CLASS::Push(LIST_TYPE LIST_PTR obj)
{
    int i;

    extend(_length);
    for (i=0; i<_length; i++) {
	_data[i+1] = _data[i];
    }
    _data[0] = obj;
    _length++;
}

void 
PAD_LIST_CLASS::Push_last(LIST_TYPE LIST_PTR obj)
{
    extend(_length);
    _data[_length++] = obj;
}

void 
PAD_LIST_CLASS::Set(int n, LIST_TYPE LIST_PTR obj)
{
    extend(n);
    if (_length <= n) {
	_length = n + 1;
    }
    _data[n] = obj;
}

int 
PAD_LIST_CLASS::Push_new(LIST_TYPE LIST_PTR obj)
{
    if (!Member(obj)) {
	Push(obj);
	return(1);
    } else {
	return(0);
    }
}

int 
PAD_LIST_CLASS::Push_new_last(LIST_TYPE LIST_PTR obj)
{
    if (!Member(obj)) {
	Push_last(obj);
	return(1);
    } else {
	return(0);
    }
}

int 
PAD_LIST_CLASS::Member(LIST_TYPE LIST_PTR obj)
{
    int i;
    int rc = 0;

    for (i=0; i<_length; i++) {
	if (_data[i] == obj) {
	    rc = 1;
	    break;
	}
    }

    return(rc);
}

int 
PAD_LIST_CLASS::Remove(LIST_TYPE LIST_PTR obj)
{
    int i, j;
    int rc = 0;

    for (i=0; i<_length; i++) {
	if (_data[i] == obj) {
	    rc = 1;
	    _length--;
	    for (j=i; j<_length; j++) {
		_data[j] = _data[j+1];
	    }
	    break;
	}
    }

    return(rc);
}

LIST_TYPE LIST_PTR
PAD_LIST_CLASS::Pop(void)
{
    int i;
    LIST_TYPE LIST_PTR obj;

    if (_length) {
	obj = LIST_REF _data[0];
	_length--;
	for (i=0; i<_length; i++) {
	    _data[i] = _data[i+1];
	}
    } else {
	obj = 0;
    }

    return(obj);
}

LIST_TYPE LIST_PTR
PAD_LIST_CLASS::Pop_last(void)
{
    LIST_TYPE LIST_PTR obj;

    if (_length) {
	_length--;
	obj = LIST_REF _data[_length];
    } else {
	obj = 0;
    }

    return(obj);
}

LIST_TYPE LIST_PTR
PAD_LIST_CLASS::First(void)
{
    LIST_TYPE LIST_PTR obj;

    if (_length) {
	obj = LIST_REF _data[0];
    } else {
	obj = 0;
    }

    return(obj);
}

LIST_TYPE LIST_PTR
PAD_LIST_CLASS::Last(void)
{
    LIST_TYPE LIST_PTR obj;

    if (_length) {
	obj = LIST_REF _data[_length-1];
    } else {
	obj = 0;
    }

    return(obj);
}

LIST_TYPE LIST_PTR
PAD_LIST_CLASS::Nth(int n)
{
    LIST_TYPE LIST_PTR obj;

    assert((n >= 0) && (n < _length));

    obj = LIST_REF _data[n];

    return(obj);
}

LIST_TYPE *
PAD_LIST_CLASS::Pointer(void)
{
    return(_data);
}

LIST_TYPE *
PAD_LIST_CLASS::Pointer(int n)
{
    return(&_data[n]);
}

int 
PAD_LIST_CLASS::Length(void)
{
    return(_length);
}

#undef LIST_PTR
#undef LIST_REF
