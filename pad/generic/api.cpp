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

#include "api.h"
#include "global.h"
#include "group.h"
#include "pad.h"
#include "bind.h"
#include "callback.h"

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#define UCHAR(c) ((unsigned char) (c))
////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////
//
// Pad_Handle Class definition
//
////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////


//
// Alternative memory allocation for Pad_Handle's
// in an attempt to reduce memory fragmentation.
//
// Instead of allocating these small handles one at a time
// which would result in lots of little memory pieces, we
// allocated larger blocks of memory, and then dole them
// out for handles one at a time.  Note that this requires
// that all handles are the same size.  Also, note that
// handles are never returned to the system.  We assume
// that roughly a similar number of handles are used over
// and over.
//

static Pad_Handle *freelist = NULL;

void *
Pad_Handle::operator new(size_t size)
{
#define BLOCK_SIZE 512

    Pad_Handle *result;

    static int nused = BLOCK_SIZE;
    static Pad_Handle *freeblock = NULL;

    if (freelist) {
	// If there is a handle on the free list, use it
	result = freelist;
	freelist = freelist->_next;
    } else {
	// Otherwise, if we don't have a block of handles available, alloc up a
	// new block of them
	if (nused >= BLOCK_SIZE) {
	    freeblock = (Pad_Handle *)malloc(BLOCK_SIZE * size);
	    nused = 0;
	}
	// Take a handle off the block
	result = &freeblock[nused++];
    }

    return result;

#undef BLOCK_SIZE
}

void
Pad_Handle::operator delete(void *object)
{
    Pad_Handle *handle = (Pad_Handle *)object;

    handle->_obj = NULL;
    handle->_tag = NULL;
    handle->_next = freelist;
    freelist = handle;
}

/*
 *----------------------------------------------------------------------
 *
 * Pad_Handle::Pad_Handle --
 *
 *   Constructor for Pad_Handle
 *
 * Results:
 *   None
 *
 *----------------------------------------------------------------------
 */

Pad_Handle::Pad_Handle()
{
    _obj = NULL;
    _next = NULL;
    _tag = NULL;
    _data = NULL;
}

Pad_Handle::Pad_Handle(Pad_Handle &handle)
{
    *this = handle;
}

Pad_Handle::~Pad_Handle(void)
{
}

Pad_Handle & 
Pad_Handle::operator=(Pad_Handle &handle)
{
				// If assigning to self, then do nothing
    if (this == &handle) {
	return(*this);
    }

    _obj = handle._obj;
    _tag = handle._tag;
    _data = handle._data;
    _next = handle._next;
    handle._next = this;

    return (*this);
}

Pad_Bool
Pad_Handle::operator==(Pad_Handle &handle)
{
    if ((_obj == handle._obj) &&
	(_data == handle._data) &&
	((_tag && handle._tag && (*_tag == *handle._tag)) ||
	 (!_tag && !handle._tag))) {
	return(TRUE);
    } else {
	return(FALSE);
    }
}

Pad_Bool
Pad_Handle::operator!=(Pad_Handle &handle)
{
    return (!(*this == handle));
}

////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////
//
// Pad_ObjHandle Class definition
//
//    A handle to one or more Pad++ Objects.  A handle can specify either a
//    single object with very quick access, or a group of objects specified
//    by a tag with access through a hash table.  Handles are memory-safe,
//    even when the object the handle references has been deleted.
//
////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////


Pad_ObjHandle::Pad_ObjHandle()
{
}

Pad_ObjHandle::Pad_ObjHandle(Pad_ObjHandle &objHandle)
{
    Attach(objHandle);
}

Pad_ObjHandle::Pad_ObjHandle(Pad_Object *obj)
{
    Attach(obj);
}

Pad_ObjHandle & 
Pad_ObjHandle::operator=(Pad_ObjHandle &objHandle)
{
    Attach(objHandle);

    return (*this);
}

/*
 *----------------------------------------------------------------------
 *
 * Pad_ObjHandle::~Pad_ObjHandle --
 *
 *   Destructor for Pad_ObjHandle
 *   Remove this handle from linked list of handles that 
 *   share its object.  Leave object alone.
 *
 * Results:
 *   None
 *
 *----------------------------------------------------------------------
 */

Pad_ObjHandle::~Pad_ObjHandle(void)
{
    Attach((Pad_Object *)NULL);
}

/*
 *----------------------------------------------------------------------
 *
 * Pad_ObjHandle::Attach --
 *
 *   Set the object this handle refers to.
 *
 *   There are five methods:
 *
 *      one using an object pointer.
 *
 *      the other four are the permutations of the two ways of
 *      specifying an boject---either by integer ID, or by char *
 *      tagorid---and the two ways of specifying a surface---either by
 *      a surfaceHandle or by a char * surfaceName.
 *
 *   In any case, if the handle previously referenced an object, that
 *   reference will be properly removed.  If it is set to NULL, it
 *   will not reference any object.
 *
 * Results:
 *   Returns TRUE or FALSE. It is always TRUE for tags. For object ids
 *   it is TRUE if the id matches the id of an object an the surface,
 *   and for a Pad_Object* it is TRUE if the pointer is not NULL.
 *
 *----------------------------------------------------------------------
 */

Pad_Bool
Pad_ObjHandle::Attach(Pad_ObjHandle &objHandle)
{
    Pad_Bool rc;

				// If assigning to self, then do nothing
    if (this == &objHandle) {
	return(TRUE);
    }

    rc = Attach(objHandle._Get_object());

    return(rc);
}

Pad_Bool
Pad_ObjHandle::Attach(Pad_Object *obj)
{
				// First, dereference any objects
    if (_obj) {
	Pad_Handle *prev, *cur;

	prev = NULL;
	cur = _obj->_handle;
	while (cur) {
	    if (cur == this) {
		if (prev == NULL) {
		    _obj->_handle = _next;
		} else {
		    prev->_next = _next;
		}
		break;
	    }
	    prev = cur;
	    cur = cur->_next;
	}
	_obj = NULL;
	_next = NULL;
    }

				// Then, if there is a new one, reference it
    if (obj) {
	_obj = obj;
	_next = obj->_handle;
	obj->_handle = this;

	_tag = NULL;
        return TRUE;
    }
        // obj was NULL
    return FALSE;
}

/*
 *----------------------------------------------------------------------
 *
 * Pad_ObjHandle::_Get_object --
 *
 *   PRIVATE:
 *
 *   If this handle specifies an object directly, this returns
 *   that object.  If this handle specifies a tag or nothing, this 
 *   returns NULL.
 *
 * Results:
 *   Tag or NULL
 *
 *----------------------------------------------------------------------
 */

Pad_Object *
Pad_ObjHandle::_Get_object(void) const
{
    return(_obj);
}
