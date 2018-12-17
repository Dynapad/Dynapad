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

#ifndef ALIAS_H
#define ALIAS_H 1

#include "defs.h"
#include "object.h"

#define ALIAS_REFERENCE_SET    (1<<0)  // Set if -reference has been configured

//
// Alias Area
// 
class Pad_Alias : public Pad_Object
{
  public:
    unsigned char aliasFlags;   // OR'd combination of flags
    Pad_Object *  reference;	// Object this alias refers to

    virtual void     Compute_bounding_box(void);
    virtual float    Compute_dimensions(float &global_width, float &global_height);
    virtual float    Compute_dimensions(Pad_Event *event, float &global_width, float &global_height);
    virtual float    Compute_dimensions(Pad_List &views, float &global_width, float &global_height);
    virtual Pad_Object *Get_reference(void);
    virtual Pad_Bool Render(void);
    virtual Pad_Bool Pick(Pad_Event *event, float halo);
    virtual Pad_Bool Set_reference(Pad_Object *obj);
    virtual void     Set_reference_default(void);

    Pad_Alias(Pad *pad);
    virtual ~Pad_Alias();

    PAD_TYPE("alias");
};

#endif
