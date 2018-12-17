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
#include "point.h"
#include "plist.h"

#include <math.h>
#include <assert.h>
#include <iostream>
using namespace std;

//
// Operator definitions
//
// Assigning a point to any integer initializes the point.
Pad_Point & 
Pad_Point::operator=(int)
{
    x = 0;
    y = 0;

    return(*this);
}

Pad_Point & 
Pad_Point::operator=(Pad_Point *point)
{
    x = point->x;
    y = point->y;

    return (*this);
}

Pad_Point & 
Pad_Point::operator=(Pad_Point &point)
{
    x = point.x;
    y = point.y;

    return (*this);
}

Pad_Bool
Pad_Point::operator==(Pad_Point *point)
{
    if ((x == point->x) && (y = point->y)) {
	return(TRUE);
    } else {
	return(FALSE);
    }
}

Pad_Bool
Pad_Point::operator==(Pad_Point &point)
{
    if ((x == point.x) && (y = point.y)) {
	return(TRUE);
    } else {
	return(FALSE);
    }
}

float
Pad_Point::Dot(Pad_Point &point)
{
    return((x * point.x) + (y * point.y));
}

float
Pad_Point::Distance(Pad_Point &point)
{
    return(sqrt(((point.x - x) * (point.x - x)) + ((point.y - y) * (point.y - y))));
}

float
Pad_Point::Vector_length(void)
{
    return(sqrt((x * x) + (y * y)));
}

void
Pad_Point::Vector_normalize(void)
{
    float len;

    len = Vector_length();
    x /= len;
    y /= len;
}


void
Pad_Point::Rotate(float theta, Pad_Point &center) 
{
    double thetaRad;
    float tmpx,tmpy;

    thetaRad = -DEG2RAD(theta);

    tmpx = x;
    tmpy = y;

    x = center.x +
	((tmpx)-center.x)*cos(thetaRad) +
	((tmpy)-center.y)*sin(thetaRad);

    y = center.y +
	((tmpy)-center.y)*cos(thetaRad) -
	((tmpx)-center.x)*sin(thetaRad);
	
}

//
// On_line
//
//   Returns TRUE if point, p, is on line of specified width, or FALSE otherwise.
//
//   Note that since the width of the line extends on both sides of the line,
//   for a point to be on the line, it must be within half the width of the line.
//   I.e., to pick a 10 pixel thick line, you must be within 5 pixels of the line
//   points.  This means, if this function is called with a halo, the halo must
//   be doubled before being added to the width.
//
Pad_Bool
Pad_Point::On_line(Pad_PList &points, float width, Pad_Bool closed)
{
    return(On_line(points.Length(), points.Pointer(), width, closed));
}

Pad_Bool
Pad_Point::On_line(int length, Pad_Point *points, float width, Pad_Bool closed)
{
    int i;
    float square_dist;
    float min_square_dist;
    Pad_Bool rc;

    if (length == 1) {
	min_square_dist = Distance_to_line_squared(points[0], points[0]);
    } else {
	min_square_dist = Distance_to_line_squared(points[0], points[1]);
    }
    for (i=1; i<length-1; i++)
      {
	  square_dist = Distance_to_line_squared(points[i], points[i+1]);
	  if (square_dist < min_square_dist) {
	      min_square_dist = square_dist;
	  }
      }
    if (closed) {
	square_dist = Distance_to_line_squared(points[length-1], points[0]);
	if (square_dist < min_square_dist) {
	    min_square_dist = square_dist;
	}
    }

    if (min_square_dist <= (0.5 * width) * (0.5 * width)) {
	rc = TRUE;
    } else {
	rc = FALSE;
    }

    return(rc);
}

//
// Returns 0 if point, p, is on spline of specified width,
// or the distance from the spline.
//
Pad_Bool
Pad_Point::On_bezier(Pad_PList &points, float width, Pad_Bool closed)
{
    return(On_bezier(points.Length(), points.Pointer(), width, closed));
}

Pad_Bool
Pad_Point::On_bezier(int length, Pad_Point *points, float width, Pad_Bool closed)
{
    int i;
    Pad_Bool rc;
    float square_dist;
    float min_square_dist;

    min_square_dist = Distance_to_bezier_squared(points[0], points[1], points[2], points[3]);
    for (i=3; i<length-3; i+=3)
      {
	  square_dist = Distance_to_bezier_squared(points[i], points[i+1], points[i+2], points[i+3]);
	  if (square_dist < min_square_dist) min_square_dist = square_dist;
      }
    if (closed) {
        square_dist = Distance_to_bezier_squared(points[i], points[i+1], points[i+2], points[0]);
	if (square_dist < min_square_dist) min_square_dist = square_dist;
    }

    if (min_square_dist <= (0.5 * width) * (0.5 * width)) {
	rc = TRUE;
    } else {
	rc = FALSE;
    }

    return(rc);
}

//
// Returns true if point, is inside polygon.
//
Pad_Bool
Pad_Point::In_polygon(Pad_PList &points)
{
    return(In_polygon(points.Length(), points.Pointer()));
}

Pad_Bool
Pad_Point::In_polygon(int length, Pad_Point *points)
{
    int i;
    float angle = 0.0;
    Pad_Bool rc;

    for (i=0; i<length-1; i++)
      {
	  angle += Angle_between_points(points[i], points[i+1]);
      }
    angle += Angle_between_points(points[length-1], points[0]);

				// Allow for a bit of rounding
				// Ideally, angle should be 2*pi.
    if (ABS(angle) > 6.2) {
	rc = TRUE;
    } else {
	rc = FALSE;
    }

    return (rc);
}

//
// Computes the distance between the point p, and the
// line (v1, v2).  Returns the square of the distance
//
float
Pad_Point::Distance_to_line_squared(Pad_Point &v1, Pad_Point &v2)
{
    float t;
    float tmp1, tmp2;
    float dx, dy, d2;
    float denom;
    Pad_Point q;
    
    tmp1 = v1.x - v2.x;
    tmp2 = v1.y - v2.y;
    denom = -tmp1*tmp1 - tmp2*tmp2;

    if (denom == 0.0) 
      {
	  d2 = (v1.x - x)*(v1.x - x) + (v1.y - y)*(v1.y - y);
	  return(d2);
      }
    else 
      {
	  t = ((x - v1.x) * tmp1 + (y - v1.y) * tmp2) / denom;
	  
	  if (t < 0.0) 
	    {
		dx = x - v1.x;
		dy = y - v1.y;
		d2 = (dx*dx) + (dy*dy);
		return(d2);
	    }
	  else if (t > 1.0)
	    {
		dx = x - v2.x;
		dy = y - v2.y;
		d2 = (dx*dx) + (dy*dy);
		return(d2);
	    }
	  else 
	    {
		q.x = v1.x + t * (v2.x - v1.x);
		q.y = v1.y + t * (v2.y - v1.y);
		dx = x - q.x;
		dy = y - q.y;
		d2 = (dx*dx) + (dy*dy);
		return(d2);
	    }
      }
}

float
Pad_Point::Distance_to_bezier_squared(Pad_Point &p0, Pad_Point &p1, Pad_Point &p2, Pad_Point &p3)
{
    Pad_Point	mid, q1, q2;
    float	dx, dy, sqdist1, sqdist2;

    mid.x = (p0.x + (3.0 * (p1.x + p2.x)) + p3.x) / 8.0;
    mid.y = (p0.y + (3.0 * (p1.y + p2.y)) + p3.y) / 8.0;
    dx = mid.x - ((p0.x + p3.x) / 2.0);
    dy = mid.y - ((p0.y + p3.y) / 2.0);

    if (((dx * dx) + (dy * dy)) < 1.0) {
	sqdist1 = Distance_to_line_squared(p0, mid);	
	sqdist2 = Distance_to_line_squared(mid, p3);
    }
    else {
	q1.x = (p0.x+p1.x)/2.0;  
	q1.y = (p0.y+p1.y)/2.0;
	q2.x = (p0.x+2*p1.x+p2.x)/4.0; 
	q2.y = (p0.y+2*p1.y+p2.y)/4.0;
	sqdist1 = Distance_to_bezier_squared(p0, q1, q2, mid);
	q1.x = (p1.x+2*p2.x+p3.x)/4.0;
	q1.y = (p1.y+2*p2.y+p3.y)/4.0;
	q2.x = (p2.x+p3.x)/2.0;
	q2.y = (p2.y+p3.y)/2.0;
	sqdist2 = Distance_to_bezier_squared(mid, q1, q2, p3);
    }
    if (sqdist1 < sqdist2) return sqdist1; else return sqdist2;
}

//
// Returns the angle at p in the triangle specified with p1 and p2
// from p1 to p2.  (Positive is counter-clockwise).
//
float
Pad_Point::Angle_between_points(Pad_Point &p1, Pad_Point &p2)
{
    Pad_Point v1, v2;
    float z, s;
    float theta;
    float t1, t2;

    v1.Set(p1.x - x, p1.y - y);
    v2.Set(p2.x - x, p2.y - y);

    z = (v1.x * v2.y) - (v1.y * v2.x);
    s = SIGN(z);
    t1 = v1.Vector_length() * v2.Vector_length();
    if (t1 == 0.0) {
	theta = 0.0;
    } else {
	t2 = v1.Dot(v2) / t1;
	if ((t2 < -1) || (t2 > 1)) {
	    theta = 0.0;
	} else {
	    theta = s * acos(t2);
	}
    }
    
    return theta;
}


