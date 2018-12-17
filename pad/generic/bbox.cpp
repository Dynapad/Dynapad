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
#include "bbox.h"
#include "point.h"

////////////////////////////////////////////////////////
//
// Pad_BBox
//
////////////////////////////////////////////////////////

Pad_BBox::Pad_BBox()
{
    bbox[XMIN] = 0.0;
    bbox[YMIN] = 0.0;
    bbox[XMAX] = 0.0;
    bbox[YMAX] = 0.0;
}

Pad_BBox::Pad_BBox(Pad_BBox &bb)
{
    bbox[XMIN] = bb.Xmin();
    bbox[YMIN] = bb.Ymin();
    bbox[XMAX] = bb.Xmax();
    bbox[YMAX] = bb.Ymax();
}

Pad_Bool
Pad_BBox::Contains(Pad_Point &point)
{
    if ((point.x >= bbox[XMIN]) &&
	(point.y >= bbox[YMIN]) &&
	(point.x <= bbox[XMAX]) &&
	(point.y <= bbox[YMAX])) {
	return(TRUE);
    } else {
	return(FALSE);
    }
}

Pad_Bool
Pad_BBox::Overlaps(Pad_BBox &bb)
{
    if ((bb.bbox[XMIN] < bbox[XMAX]) &&
	(bb.bbox[XMAX] > bbox[XMIN]) &&
	(bb.bbox[YMIN] < bbox[YMAX]) &&
	(bb.bbox[YMAX] > bbox[YMIN])) 
      {
	  return(TRUE);
      }
    else 
      {
	  return(FALSE);
      }
}

//
// True if <bb> is completely enclosed by <this>
//
Pad_Bool
Pad_BBox::Enclosed(Pad_BBox &bb)
{
    if ((bb.bbox[XMIN] > bbox[XMIN]) &&
	(bb.bbox[XMAX] < bbox[XMAX]) &&
	(bb.bbox[YMIN] > bbox[YMIN]) &&
	(bb.bbox[YMAX] < bbox[YMAX])) 
      {
	  return(TRUE);
      }
    else 
      {
	  return(FALSE);
      }
}

//
// True if <bb> is completely enclosed by <this>, or the same as <this>
//
Pad_Bool
Pad_BBox::Enclosed_or_same(Pad_BBox &bb)
{
    if ((bb.bbox[XMIN] >= bbox[XMIN]) &&
	(bb.bbox[XMAX] <= bbox[XMAX]) &&
	(bb.bbox[YMIN] >= bbox[YMIN]) &&
	(bb.bbox[YMAX] <= bbox[YMAX])) 
      {
	  return(TRUE);
      }
    else 
      {
	  return(FALSE);
      }
}

void 
Pad_BBox::Union(Pad_BBox &src_bbox)
{
    bbox[XMIN] = MIN(bbox[XMIN], src_bbox.bbox[XMIN]);
    bbox[YMIN] = MIN(bbox[YMIN], src_bbox.bbox[YMIN]);
    bbox[XMAX] = MAX(bbox[XMAX], src_bbox.bbox[XMAX]);
    bbox[YMAX] = MAX(bbox[YMAX], src_bbox.bbox[YMAX]);
}

void 
Pad_BBox::Intersection(Pad_BBox &src_bbox)
{
    bbox[XMIN] = MAX(bbox[XMIN], src_bbox.bbox[XMIN]);
    bbox[YMIN] = MAX(bbox[YMIN], src_bbox.bbox[YMIN]);
    bbox[XMAX] = MIN(bbox[XMAX], src_bbox.bbox[XMAX]);
    bbox[YMAX] = MIN(bbox[YMAX], src_bbox.bbox[YMAX]);
    if (bbox[XMIN] > bbox[XMAX]) {
	bbox[XMIN] = 0.5 * (bbox[XMIN] + bbox[XMAX]);
	bbox[XMAX] = bbox[XMIN];
    }
    if (bbox[YMIN] > bbox[YMAX]) {
	bbox[YMIN] = 0.5 * (bbox[YMIN] + bbox[YMAX]);
	bbox[YMAX] = bbox[YMIN];
    }
}

void 
Pad_BBox::Translate(float dx, float dy)
{
    bbox[XMIN] += dx;
    bbox[YMIN] += dy;
    bbox[XMAX] += dx;
    bbox[YMAX] += dy;
}

void
Pad_BBox::Get(float *bb)
{
    bb[XMIN] = bbox[XMIN];
    bb[YMIN] = bbox[YMIN];
    bb[XMAX] = bbox[XMAX];
    bb[YMAX] = bbox[YMAX];
}

void
Pad_BBox::Get(Pad_BBox &bb)
{
    bb.Set(bbox[XMIN], bbox[YMIN], bbox[XMAX], bbox[YMAX]);
}

void
Pad_BBox::Set(float *bb)
{
    bbox[XMIN] = bb[XMIN];
    bbox[YMIN] = bb[YMIN];
    bbox[XMAX] = bb[XMAX];
    bbox[YMAX] = bb[YMAX];
}

void
Pad_BBox::Set(Pad_BBox &bb)
{
    Set(bb.Get());
}

void
Pad_BBox::Set(float xmin, float ymin, float xmax, float ymax)
{
    bbox[XMIN] = xmin;
    bbox[YMIN] = ymin;
    bbox[XMAX] = xmax;
    bbox[YMAX] = ymax;
}

void
Pad_BBox::Set_x(float xmin, float xmax)
{
    bbox[XMIN] = xmin;
    bbox[XMAX] = xmax;
}

void
Pad_BBox::Set_xmin(float xmin)
{
    bbox[XMIN] = xmin;
}

void
Pad_BBox::Set_xmax(float xmax)
{
    bbox[XMAX] = xmax;
}

void
Pad_BBox::Set_y(float ymin, float ymax)
{
    bbox[YMIN] = ymin;
    bbox[YMAX] = ymax;
}

void
Pad_BBox::Set_ymin(float ymin)
{
    bbox[YMIN] = ymin;
}

void
Pad_BBox::Set_ymax(float ymax)
{
    bbox[YMAX] = ymax;
}

void
Pad_BBox::Set_zero(void)
{
    bbox[XMIN] = 0.0;
    bbox[YMIN] = 0.0;
    bbox[XMAX] = 0.0;
    bbox[YMAX] = 0.0;
}

/////////////////////////////////////////////////////////////////////////////
// Pad_BBox Operator: =
/////////////////////////////////////////////////////////////////////////////

Pad_BBox& Pad_BBox::operator=(Pad_BBox &bb)
{
    Set(bb);

    return(*this);
}

Pad_BBox& Pad_BBox::operator=(float *bb)
{
    Set(bb);

    return(*this);
}
