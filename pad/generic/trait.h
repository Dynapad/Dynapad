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

#ifndef PAD_TRAIT_H
#define PAD_TRAIT_H

#include "pad-string.h"
#include "callback.h"
#include "point.h"
#include "noisedata.h"
#include "object.h"
#include "tree.h"
#include "tree-layout.h"

//
// These are the trait classes for infrequently used options 
// of pad objects.  The abstract base class Pad_Trait has virutal
// Get() and Set() methods (for typical option value types) that
// derived classes can override.  The base class also has an integer
// bit field which should uniquely identify a trait.
//

// Type name macro that trait types can use
#define PAD_TRAIT_TYPE(N) \
  virtual char* Type_name(void) {return N;}


class Pad_Trait {
public:
    // methods for accessing trait value

    virtual Pad_Bool Get_value(char*&);
    virtual Pad_Bool Get_value(Pad_String*&);
    virtual Pad_Bool Get_value(Pad_Callback*&);
    virtual Pad_Bool Get_value(Pad_List*&);
    virtual Pad_Bool Get_value(Pad_NoiseData*&);
    virtual Pad_Bool Get_value(Pad_Sticky*&);
    virtual Pad_Bool Get_value(int*&);
    virtual Pad_Bool Get_value(float*&);
    virtual Pad_Bool Get_value(unsigned char*&);

    // methods for assigning values

    virtual Pad_Bool Set_value(char*);
    virtual Pad_Bool Set_value(Pad_String*);
    virtual Pad_Bool Set_value(Pad_Callback*);
    virtual Pad_Bool Set_value(Pad_List*);
    virtual Pad_Bool Set_value(Pad_NoiseData*);
    virtual Pad_Bool Set_value(Pad_Sticky*);
    virtual Pad_Bool Set_value(int*);
    virtual Pad_Bool Set_value(float*);
    virtual Pad_Bool Set_value(unsigned char*);

    // mehtod for checking trait option

    virtual int Id(void);

    // method for getting trait type information

    PAD_TRAIT_TYPE("Pad_Trait");

    virtual ~Pad_Trait();

protected:
    Pad_Trait(int id);
    int _id;   // the PAD_option_TRAIT bit flag for a trait
};


//
// Trait class for options with Pad_String values.
//
class Pad_StringTrait : public Pad_Trait {
public:
    Pad_Bool Get_value(char*&);
    Pad_Bool Get_value(Pad_String*&);

    Pad_Bool Set_value(char*);
    Pad_Bool Set_value(Pad_String*);

    Pad_StringTrait(int option_flag);
    Pad_StringTrait(int, char *d);
    Pad_StringTrait(int, int len);
    Pad_StringTrait(int, char *d, int len);
    Pad_StringTrait(int, Pad_String *ps);
    Pad_StringTrait(int, Pad_String &ps);
    PAD_TRAIT_TYPE("Pad_StringStrait");

private:
    Pad_String value;
};


//
// Trait class for options with Pad_Callback values.
//
class Pad_CallbackTrait : public Pad_Trait {
public:
    Pad_Bool Get_value(Pad_Callback*&);
    Pad_Bool Set_value(Pad_Callback*);

    Pad_CallbackTrait(int, Pad_Callback &callback);
    PAD_TRAIT_TYPE("Pad_CallbackTrait");

private:
    Pad_Callback value;
};


//
// Trait class for options with Pad_NoiseData values.
//
class Pad_NoiseTrait : public Pad_Trait {
public:
    Pad_Bool Get_value(Pad_NoiseData*&);
    Pad_Bool Set_value(Pad_NoiseData*);

    Pad_NoiseTrait(int);
    PAD_TRAIT_TYPE("Pad_NoiseTrait");

private:
    Pad_NoiseData value;
};

//
// Trait class for options with Pad_List values.
//
class Pad_ListTrait : public Pad_Trait {
public:
    virtual Pad_Bool Get_value(Pad_List*&);
    virtual Pad_Bool Set_value(Pad_List*);

    Pad_ListTrait(int);
    Pad_ListTrait(int, Pad_List &list);
    PAD_TRAIT_TYPE("Pad_ListTrait");

private:
    Pad_List value;
};

//
// Trait class for options with Pad_List values for zom actions and sizes.
//
class Pad_ZoomActionTrait : public Pad_ListTrait {
public:
    Pad_Bool Get_prevsizes(Pad_List *&list);

    Pad_ZoomActionTrait(int);
    Pad_ZoomActionTrait(int, Pad_List &list);
    PAD_TRAIT_TYPE("Pad_ZoomActionTrait");

private:
    Pad_List prevSizes;
};

//
// Trait class for Pad_Sticky values.
//
class Pad_StickyTrait : public Pad_Trait {
public:
    Pad_Bool Get_value(Pad_Sticky*&);
    Pad_Bool Set_value(Pad_Sticky*);

    Pad_StickyTrait(int);
    PAD_TRAIT_TYPE("Pad_StickyTrait");

private:
    Pad_Sticky value;
};

//
// Trait class for Pad_TreeNode values.
//
class Pad_TreeNodeTrait : public Pad_Trait {
public:
    Pad_Bool Get_value(Pad_TreeNode*&);
    Pad_Bool Set_value(Pad_TreeNode*);

    Pad_TreeNodeTrait(int);
    PAD_TRAIT_TYPE("Pad_TreeNodeTrait");

private:
    Pad_TreeNode *value;
};


//
// Trait class for Pad_TreeLayoutNode vlaues
//
class Pad_TreeLayoutTrait : public Pad_Trait {
public:
    Pad_Bool Get_value(Pad_TreeLayout*&);
    Pad_Bool Set_value(Pad_TreeLayout*);

    Pad_TreeLayoutTrait(int);
    PAD_TRAIT_TYPE("Pad_TreeLayoutTrait");

private:
    Pad_TreeLayout *value;
};

//
// Trait class for uchar values
//
class Pad_UCharTrait : public Pad_Trait {
public:
    Pad_Bool Get_value(unsigned char*&);
    Pad_Bool Set_value(unsigned char*);

    Pad_UCharTrait(int, unsigned char val);
    PAD_TRAIT_TYPE("Pad_UCharTrait");

private:
    unsigned char value;
};

//
// Trait class for float values
//
class Pad_FloatTrait : public Pad_Trait {
public:
    Pad_Bool Get_value(float*&);
    Pad_Bool Set_value(float*);

    Pad_FloatTrait(int, float val);
    PAD_TRAIT_TYPE("Pad_FloatTrait");

private:
    float value;
};

#endif

