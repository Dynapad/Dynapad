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
#include "alias.h"
#include "pad.h"
#include "global.h"
#include "win.h"

#include <stdlib.h>

//////////////////////////////////////////////
//              Alias definitions
//////////////////////////////////////////////  

#define ALIAS_DEFAULT_REFERENCE    NULL

Pad_Alias::~Pad_Alias()
{
    Generate_delete();
    if (reference) {
	reference->Remove_alias(this);
	reference = NULL;
    }
}

Pad_Alias::Pad_Alias(Pad *pad) :
Pad_Object(pad)
{
    _type = PAD_ALIAS;
    aliasFlags = PAD_NO_MASK;
    reference = NULL;
}

//
// Setters and getters for reference
//
Pad_Bool
Pad_Alias::Set_reference(Pad_Object *obj)
{
    if (obj && (obj->Type() == PAD_ALIAS)) {
	Pad_errorString = "Can't make an alias of an alias";
	return(FALSE);
    }

    aliasFlags |= ALIAS_REFERENCE_SET;
    if (reference) {
	reference->Remove_alias(this);
    }
    if (obj) {
	obj->Add_alias(this);
    }
    reference = obj;
    Update();
    
    return(TRUE);
}

void
Pad_Alias::Set_reference_default(void)
{
    Set_reference(ALIAS_DEFAULT_REFERENCE);
    aliasFlags &= ~ALIAS_REFERENCE_SET;
}

Pad_Object *
Pad_Alias::Get_reference(void)
{
    return(reference); 
}

void
Pad_Alias::Compute_bounding_box() 
{
    Pad_BBox bb;

    if (reference) {
	reference->Get_bbox(bb);
	if (!(optionFlags & PAD_WIDTH_SET)) {
	    Set_bbox_xmin(bb.Xmin());
	    Set_bbox_xmax(bb.Xmax());
	}
	if (!(optionFlags & PAD_HEIGHT_SET)) {
	    Set_bbox_ymin(bb.Ymin());
	    Set_bbox_ymax(bb.Ymax());
	}

	Pad_Object::Compute_bounding_box();
    } else {
        Set_bbox(0, 0, 0, 0);
    }
}

//
// Compute the dimensions of the alias based on the dimensions of
// its reference.  Also set the reference's dimensions to that
// of the alias (for proper rendering, etc.).
//
float
Pad_Alias::Compute_dimensions(float &global_width, float &global_height)
{
    float dz;

    if (reference) {
	dz = transform.Get_scale() / reference->transform.Get_scale();
	reference->Compute_dimensions(global_width, global_height);
	pixelDim = reference->pixelDim * dz;
	percentDim = reference->percentDim * dz;
	reference->pixelDim = pixelDim;
	reference->percentDim = percentDim;
    } else {
	percentDim = 0;
	pixelDim = 0;
    }

    return(pixelDim);
}

float
Pad_Alias::Compute_dimensions(Pad_Event *event, float &global_width, float &global_height)
{
    float dz;

    if (reference) {
	dz = transform.Get_scale() / reference->transform.Get_scale();
	reference->Compute_dimensions(event, global_width, global_height);
	pixelDim = reference->pixelDim * dz;
	percentDim = reference->percentDim * dz;
	reference->pixelDim = pixelDim;
	reference->percentDim = percentDim;
    } else {
	percentDim = 0;
	pixelDim = 0;
    }

    return(pixelDim);
}

float
Pad_Alias::Compute_dimensions(Pad_List &views, float &global_width, float &global_height)
{
    float dz;

    if (reference) {
	dz = transform.Get_scale() / reference->transform.Get_scale();
	reference->Compute_dimensions(views, global_width, global_height);
	pixelDim = reference->pixelDim * dz;
	percentDim = reference->percentDim * dz;
	reference->pixelDim = pixelDim;
	reference->percentDim = percentDim;
    } else {
	percentDim = 0;
	pixelDim = 0;
    }

    return(pixelDim);
}

//
// Determines if the specified point is within halo pixels of the 
// reference of this alias.
// Coordinates are local.
//
Pad_Bool
Pad_Alias::Pick(Pad_Event *event, float halo)
{
    Pad_Bool rc;

    if (reference) {
	rc = reference->Pick(event, halo);
    } else {
	rc = FALSE;
    }

    return(rc);
}

//
// Return true to continue rendering other objects, or false if rendering
// was interrupted.
//
Pad_Bool
Pad_Alias::Render(void)
{
    Pad_Bool rc;
    float globalWidth, globalHeight;

    Compute_dimensions(globalWidth, globalHeight);
    if (reference) {
				// There's a tricky issue here related to bboxes.
				// If the reference is a group, its members might
				// not get rendered because their global bounding
				// boxes might not be visible (despite the fact
				// that this alias is visible).  Thus, to correct
				// this situation, we will temporarily shift the
				// active bbox to be related to the reference instead
				// of this alias.
	Pad_BBox *activeBBox = &Pad_prc->activeBBox;
	Pad_BBox tmpBBox(*activeBBox);
	float dx = reference->Get_global_bbox()->Xmin() - Get_global_bbox()->Xmin();
	float dy = reference->Get_global_bbox()->Ymin() - Get_global_bbox()->Ymin();
	activeBBox->Translate(dx, dy);
	rc = reference->Render();
	Pad_prc->activeBBox = tmpBBox;
    } else {
	rc = TRUE;
    }

    return(rc);
}
