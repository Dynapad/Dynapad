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
#include "list.h"
#include "pad.h"
#include "point.h"
#include "image.h"
#include "view.h"
#include "line.h"
#include "text.h"
#include "renderer.h"
#include "noisedata.h"
#include "callback.h"
#include "win.h"
#include "global.h"
#include "trait.h"
#include "group.h"

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <errno.h>

#  include <unistd.h>

Pad_Bool
Pad_Object::Get_written(void)
{
   return ((flags & PAD_MASK_WRITTEN) ? TRUE : FALSE);
}

Pad_Bool
Pad_Object::Set_written(Pad_Bool val, Pad_Bool /*groupMembers*/)
{
   if (val) {
       flags |= PAD_MASK_WRITTEN;
   } else {
       flags &= ~PAD_MASK_WRITTEN;
   }

   return TRUE;
}

//
// Methods for dealing with traits

//
// Returns a trait.  If the trait is not set
// then NULL is returned.
//
Pad_Trait*
Pad_Object::_Get_trait(int id)
{
    Pad_Trait *trait=NULL;
    Pad_Iterator iter;
    if (_traitFlags & id) {
        DOLIST(iter, _traitList, Pad_Trait, trait) {
	   if (trait->Id() & id) {
	       break;
	   }
	}
    } 
    return trait;
}

//
// Removes a trait from the traitList and
// deallocates it.  Return TRUE if all goes well.
//
Pad_Bool
Pad_Object::_Remove_trait(int id)
{
    int rc=TRUE;
    Pad_Trait *trait = _Get_trait(id);
    if (trait) {
        _traitList.Remove(trait);
	delete trait;
	_traitFlags &= ~id;
    }
    return rc;
}


//
// Convinient method for getting the value of a string trait.
// If the trait does not exist then the value is set to NULL.
// Returns the trait if it's set and NULL otherwise.
//
Pad_Trait*
Pad_Object::_Get_trait(int id, char *&val)
{
    Pad_Trait *trait = _Get_trait(id);
    val = NULL;
    if (trait) {
        trait->Get_value(val);
    }

    return trait;
}

//
// Convinient method for setting the value of a string trait.
// It creates a trait for the option if it does not already exist.
// Returns TRUE if all goes well (if the trait could be set to
// the given value).
//
Pad_Bool
Pad_Object::_Set_trait(int id, char *val)
{
    Pad_Trait *trait = _Get_trait(id);
    Pad_Bool rc=TRUE;

    if (trait) {
        rc = trait->Set_value(val);
    } else {
        trait = new Pad_StringTrait(id, val);
	if (trait) {
	    _traitList.Push(trait);
            _traitFlags |= id;
	} else {
	    rc = FALSE;
	}
    }
    
    return rc;
}

Pad_Trait*
Pad_Object::_Get_trait(int id, Pad_String *&val)
{
    Pad_Trait *trait = _Get_trait(id);
    val = NULL;
    if (trait) {
        trait->Get_value(val);
    }

    return trait;
}

Pad_Bool
Pad_Object::_Set_trait(int id, Pad_String *val)
{
    Pad_Trait *trait = _Get_trait(id);
    Pad_Bool rc=TRUE;

    if (trait) {
        rc = trait->Set_value(val);
    } else {
        trait = new Pad_StringTrait(id, val);
	if (trait) {
	    _traitList.Push(trait);
            _traitFlags |= id;
	} else {
	    rc = FALSE;
	}
    }
    
    return rc;
}

Pad_Trait*
Pad_Object::_Get_trait(int id, Pad_Callback *&val)
{
    Pad_Trait *trait = _Get_trait(id);
    val = NULL;
    if (trait) {
        trait->Get_value(val);
    }

    return trait;
}

Pad_Bool
Pad_Object::_Set_trait(int id, Pad_Callback *val)
{
    Pad_Trait *trait = _Get_trait(id);
    Pad_Bool rc=TRUE;

    if (trait) {
        rc = trait->Set_value(val);
    } else {
        trait = new Pad_CallbackTrait(id, *val);
	if (trait) {
	    _traitList.Push(trait);
            _traitFlags |= id;
	} else {
	    rc = FALSE;
	}
    }
    
    return rc;
}

Pad_Trait*
Pad_Object::_Get_trait(int id, Pad_List *&val)
{
    Pad_Trait *trait = _Get_trait(id);
    val = NULL;
    if (trait) {
        trait->Get_value(val);
    }

    return trait;
}

Pad_Bool
Pad_Object::_Set_trait(int id, Pad_List *val)
{
    Pad_Trait *trait = _Get_trait(id);
    Pad_Bool rc=TRUE;

    if (trait) {
        rc = trait->Set_value(val);
    } else {
        trait = new Pad_ZoomActionTrait(id, *val);
	if (trait) {
	    _traitList.Push(trait);
            _traitFlags |= id;
	} else {
	    rc = FALSE;
	}
    }
    
    return rc;
}
