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

#ifndef POINT_H
#define POINT_H 1

#include "defs.h"

class Pad_PList;

class Pad_Point         // Simple floating-point Point.
{
  public:
    float x, y;

    float    Angle_between_points(Pad_Point &p1, Pad_Point &p2);
    float    Distance(Pad_Point &point);
    float    Distance_to_line_squared(Pad_Point &v1, Pad_Point &v2);
    float    Distance_to_bezier_squared(Pad_Point &p0, Pad_Point &p1, Pad_Point &p2, Pad_Point &p3);
    float    Dot(Pad_Point &point);
    Pad_Bool In_polygon(int length, Pad_Point *points);
    Pad_Bool In_polygon(Pad_PList &points);
    Pad_Bool On_line(int length, Pad_Point *points, float width, Pad_Bool closed);
    Pad_Bool On_line(Pad_PList &points, float width, Pad_Bool closed);
    Pad_Bool On_bezier(int length, Pad_Point *points, float width, Pad_Bool closed);
    Pad_Bool On_bezier(Pad_PList &points, float width, Pad_Bool closed);
    void     Rotate(float theta, Pad_Point &center);
    float    Vector_length(void);
    void     Vector_normalize(void);

    inline void 
      Set(float xa, float ya) {
	  x = xa;
	  y = ya;
      }

    Pad_Point& operator=(int n);
    Pad_Point& operator=(Pad_Point *point);
    Pad_Point& operator=(Pad_Point &point);

    Pad_Bool operator==(Pad_Point *point);
    Pad_Bool operator==(Pad_Point &point);

    Pad_Point() {
	x = 0.0;
	y = 0.0;
    }
    Pad_Point(float newX, float newY) {
	x = newX;
	y = newY;
    }
    Pad_Point(Pad_Point *point) {
	x = point->x;
	y = point->y;
    }
    Pad_Point(Pad_Point &point) {
	x = point.x;
	y = point.y;
    }
};

#endif


