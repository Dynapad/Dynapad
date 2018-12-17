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

#ifndef BBOX_H
#define BBOX_H 1

#include "point.h"

//
// Pad_Is_overlapping
// 
// True if obb and vbb overlap.
//
inline int Pad_Is_overlapping(float obb[4], float vbb[4]) 
{
    if ((obb[XMIN] < vbb[XMAX]) &&
	(obb[XMAX] > vbb[XMIN]) &&
	(obb[YMIN] < vbb[YMAX]) &&
	(obb[YMAX] > vbb[YMIN])) {
	return(1);
    } else {
	return(0);
    }
}

//
// Make dst be the union of the bounding boxes s1 and s2
//
inline void Pad_Merge_bounds(float s1[4], float s2[4], float dst[4])
{
    dst[XMIN] = (s1[XMIN] < s2[XMIN] ? s1[XMIN] : s2[XMIN]);
    dst[XMAX] = (s1[XMAX] > s2[XMAX] ? s1[XMAX] : s2[XMAX]);
    dst[YMIN] = (s1[YMIN] < s2[YMIN] ? s1[YMIN] : s2[YMIN]);
    dst[YMAX] = (s1[YMAX] > s2[YMAX] ? s1[YMAX] : s2[YMAX]);
}

//
// Pad_BBox
//
//   Bounding box support
//
class Pad_BBox
{
  public:
    inline float
      Avg_dim(void) const {
	  return(0.5 * (Width() + Height()));
      }
    Pad_Bool Contains(Pad_Point &point);
    Pad_Bool Enclosed(Pad_BBox &bb);
    Pad_Bool Enclosed_or_same(Pad_BBox &bb);
    inline float *  
      Get(void) {
	  return(bbox);
      }
    void     Get(float *bb);
    void     Get(Pad_BBox &bb);
    inline float
      Height(void) const {
	  return(bbox[YMAX] - bbox[YMIN]);
      }
    void     Intersection(Pad_BBox &bb);
    Pad_Bool Overlaps(Pad_BBox &bb);
    void     Set(float *bb);
    void     Set(Pad_BBox &bb);
    void     Set(float xmin, float ymin, float xmax, float ymax);
    void     Set_x(float xmin, float xmax);
    void     Set_xmin(float xmin);
    void     Set_xmax(float xmax);
    void     Set_y(float ymin, float ymax);
    void     Set_ymin(float ymin);
    void     Set_ymax(float ymax);
    void     Set_zero(void);
    void     Translate(float dx, float dy);
    void     Union(Pad_BBox &bbox);
    inline float
      Width(void) const {
	  return(bbox[XMAX] - bbox[XMIN]);
      }
    inline float
      Xmin(void) const {
	  return(bbox[XMIN]);
      }
    inline float
      Ymin(void) const {
	  return(bbox[YMIN]);
      }
    inline float
      Xmax(void) const {
	  return(bbox[XMAX]);
      }
    inline float    
      Ymax(void) const {
	  return(bbox[YMAX]);
      }
    inline float
      Xctr(void) const {
	  return(bbox[XMIN] + 0.5 * (bbox[XMAX] - bbox[XMIN]));
    }
    inline float
      Yctr(void) const {
	  return(bbox[YMIN] + 0.5 * (bbox[YMAX] - bbox[YMIN]));
    }

    Pad_BBox& operator=(Pad_BBox &bb);
    Pad_BBox& operator=(float *bb);

    Pad_BBox();
    Pad_BBox(Pad_BBox &bb);

  private:
    float bbox[4];
};

#endif
