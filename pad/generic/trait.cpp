/*
"(c) Copyright 1993-1997 Pad++ Consortium {University of New Mexico (UNM),
New York University (NYU)}, All Rights Reserved." Licensee can not remove
or obscure any of the copyright or trademark notices in this software.

IN NO EVENT SHALL THE AUTHORS OR DISTRIBUTORS BE LIABLE TO ANY PARTY
FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
ARISING OUT OF THE USE OF THIS SOFTWARE, ITS DOCUMENTATION, OR ANY
DERIVATIVES THEREOF, EVEN IF THE AUTHORS HAVE BEEN ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.

THE AUTHORS AND DISTRIBUTORS SPECIFICALLY DISCLAIM ANY WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, AND
NON-INFRINGEMENT. THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, AND
THE AUTHORS AND DISTRIBUTORS HAVE NO OBLIGATION TO PROVIDE
MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.

See the file "License" for general information on usage and
redistribution, and the file "LicenseTerms" for the specific license
agreement on usage and redistribution of this file, and the Pad++
software in general.
*/

#include "trait.h"

//
// Stubs for the base class
//

Pad_Trait::Pad_Trait(int id)
          : _id(id)
{
}

Pad_Trait::~Pad_Trait()
{
}

int
Pad_Trait::Id()
{
    return _id;
}

Pad_Bool
Pad_Trait::Set_value(char*)
{
    return FALSE;
}

Pad_Bool
Pad_Trait::Get_value(char*&)
{
    return FALSE;
}

Pad_Bool
Pad_Trait::Set_value(Pad_String*)
{
    return FALSE;
}

Pad_Bool
Pad_Trait::Get_value(Pad_String*&)
{
    return FALSE;
}

Pad_Bool
Pad_Trait::Set_value(Pad_Callback*)
{
    return FALSE;
}

Pad_Bool
Pad_Trait::Get_value(Pad_Callback*&)
{
    return FALSE;
}

Pad_Bool
Pad_Trait::Set_value(Pad_List*)
{
    return FALSE;
}

Pad_Bool
Pad_Trait::Get_value(Pad_List*&)
{
    return FALSE;
}

Pad_Bool
Pad_Trait::Set_value(Pad_NoiseData*)
{
    return FALSE;
}

Pad_Bool
Pad_Trait::Get_value(Pad_NoiseData*&)
{
    return FALSE;
}

Pad_Bool
Pad_Trait::Set_value(Pad_Sticky*)
{
    return FALSE;
}

Pad_Bool
Pad_Trait::Get_value(Pad_Sticky*&)
{
    return FALSE;
}

Pad_Bool
Pad_Trait::Set_value(int*)
{
    return FALSE;
}

Pad_Bool
Pad_Trait::Get_value(int*&)
{
    return FALSE;
}

Pad_Bool
Pad_Trait::Set_value(float*)
{
    return FALSE;
}

Pad_Bool
Pad_Trait::Get_value(float*&)
{
    return FALSE;
}

Pad_Bool
Pad_Trait::Set_value(unsigned char*)
{
    return FALSE;
}

Pad_Bool
Pad_Trait::Get_value(unsigned char*&)
{
    return FALSE;
}


//
// Pad_String traits
//

Pad_StringTrait::Pad_StringTrait(int id) :
Pad_Trait(id)
{
}

Pad_StringTrait::Pad_StringTrait(int id, char *d) :
Pad_Trait(id),
value(d)
{
}

Pad_StringTrait::Pad_StringTrait(int id, int len) :
Pad_Trait(id),
value(len)
{
}

Pad_StringTrait::Pad_StringTrait(int id, Pad_String *ps) :
Pad_Trait(id),
value(ps)
{
}
                 
Pad_StringTrait::Pad_StringTrait(int id, Pad_String &ps) :
Pad_Trait(id),
value(ps)
{
}

Pad_StringTrait::Pad_StringTrait(int id, char *d, int len) :
Pad_Trait(id),
value(d, len)
{
}


Pad_Bool
Pad_StringTrait::Set_value(Pad_String *ps)
{
    int rc=TRUE;
    value.Set(ps);
    return rc;
}

Pad_Bool
Pad_StringTrait::Get_value(Pad_String *&ps)
{
    int rc=TRUE;
    ps = &value;
    return rc;
}

Pad_Bool
Pad_StringTrait::Set_value(char *ps)
{
    int rc=TRUE;
    value.Set(ps);
    return rc;
}

Pad_Bool
Pad_StringTrait::Get_value(char *&ps)
{
    int rc=TRUE;
    ps = value.Get();
    return rc;
}


//
// Pad_Callback traits
//

Pad_CallbackTrait::Pad_CallbackTrait(int id, Pad_Callback &callback) :
Pad_Trait(id),
value(callback)
{
}

Pad_Bool
Pad_CallbackTrait::Get_value(Pad_Callback *&val)
{
    Pad_Bool rc=TRUE;
    val = &value;
    return rc;
}

Pad_Bool
Pad_CallbackTrait::Set_value(Pad_Callback *val)
{
    Pad_Bool rc=TRUE;
    if ( !val ) {
      return FALSE;
    }
    value.Set(val);
    return rc;
}

//
// Pad_NoiseData traits
//

Pad_NoiseTrait::Pad_NoiseTrait(int id) :
Pad_Trait(id)
{
}

Pad_Bool
Pad_NoiseTrait::Get_value(Pad_NoiseData *&val)
{
    Pad_Bool rc=TRUE;
    val = &value;
    return rc;
}

Pad_Bool
Pad_NoiseTrait::Set_value(Pad_NoiseData *val)
{
    Pad_Bool rc=TRUE;
    if ( !val ) {
      return FALSE;
    }
    value = *val;
    return rc;
}

//
// Pad_List traits
//

Pad_ListTrait::Pad_ListTrait(int id) :
Pad_Trait(id)
{
}

Pad_ListTrait::Pad_ListTrait(int id, Pad_List &list) :
Pad_Trait(id),
value(list)
{
}

Pad_Bool
Pad_ListTrait::Get_value(Pad_List *&val)
{
    val = &value;
    return TRUE;
}

Pad_Bool
Pad_ListTrait::Set_value(Pad_List *val)
{
    Pad_Bool rc=TRUE;
    if ( !val ) {
      return FALSE;
    }
    value = *val;
    return rc;
}

//
// Pad_ZoomAction traits
//

Pad_ZoomActionTrait::Pad_ZoomActionTrait(int id) :
Pad_ListTrait(id)
{
}

Pad_ZoomActionTrait::Pad_ZoomActionTrait(int id, Pad_List &list) :
Pad_ListTrait(id, list)
{
}

Pad_Bool
Pad_ZoomActionTrait::Get_prevsizes(Pad_List *&val)
{
    val = &prevSizes;
    return TRUE;
}

//
// Pad_Sticky traits
//

Pad_StickyTrait::Pad_StickyTrait(int id) :
Pad_Trait(id)
{
}

Pad_Bool
Pad_StickyTrait::Get_value(Pad_Sticky *&val)
{
    Pad_Bool rc=TRUE;
    val = &value;
    return rc;
}

Pad_Bool
Pad_StickyTrait::Set_value(Pad_Sticky *val)
{
    Pad_Bool rc=TRUE;
    if ( !val ) {
      return FALSE;
    }
    value = *val;
    return rc;
}

// Pad_TreeNode traits

Pad_TreeNodeTrait::Pad_TreeNodeTrait(int id) :
Pad_Trait(id),
value(NULL)
{
}

Pad_Bool
Pad_TreeNodeTrait::Get_value(Pad_TreeNode *&val)
{
   val = value;
   return TRUE;
}

Pad_Bool
Pad_TreeNodeTrait::Set_value(Pad_TreeNode *val)
{
   value = val;
   return TRUE;
}

// Pad_TreeLayout traits

Pad_TreeLayoutTrait::Pad_TreeLayoutTrait(int id) :
Pad_Trait(id),
value(NULL)
{
}

Pad_Bool
Pad_TreeLayoutTrait::Get_value(Pad_TreeLayout *&val)
{
   val = value;
   return TRUE;
}

Pad_Bool
Pad_TreeLayoutTrait::Set_value(Pad_TreeLayout *val)
{
   value = val;
   return TRUE;
}

// usigned char traits

Pad_UCharTrait::Pad_UCharTrait(int id, unsigned char val) :
Pad_Trait(id),
value(val)
{
}

Pad_Bool
Pad_UCharTrait::Get_value(unsigned char *&val)
{
    val = &value;
    return TRUE;
}

Pad_Bool
Pad_UCharTrait::Set_value(unsigned char *val)
{
    if (!val) {
      return FALSE;
    }
    
    value = *val;
    return TRUE;
}

// float traits

Pad_FloatTrait::Pad_FloatTrait(int id, float val) :
Pad_Trait(id),
value(val)
{
}

Pad_Bool
Pad_FloatTrait::Get_value(float *&val)
{
    val = &value;
    return TRUE;
}

Pad_Bool
Pad_FloatTrait::Set_value(float *val)
{
    if (!val) {
      return FALSE;
    }
    
    value = *val;
    return TRUE;
}
    
