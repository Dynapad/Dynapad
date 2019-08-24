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
#include "line.h"
#include "noisedata.h"
#include "plist.h"

#include <math.h>
#include <stdlib.h>

//
// Noisy lines
// 

extern double noise1(double); // see noise.c

//
// Compute the bounding box of a noisy line.
//
void
Pad_NoiseData::Compute_bbox(Pad_PList &points, Pad_Bool closed, Pad_Bool miter,
                            float lineWidth, float bbox_ret[4])
{
    Compute_bbox(points.Pointer(), points.Length(), closed, miter, lineWidth, bbox_ret);
}

void
Pad_NoiseData::Compute_bbox(Pad_Point *points, int length, Pad_Bool closed, Pad_Bool miter,
                            float lineWidth, float bbox_ret[4])
{
    Pad_NoiseIterator ni;
    Pad_Point point;
    Pad_Point p1, p2, p3;
    float xmin, ymin, xmax, ymax;
    int i = 0;
    float miter_length;

    xmin = points[0].x;
    ymin = points[0].y;
    xmax = points[0].x;
    ymax = points[0].y;

    ni.Setup(this, points, length, closed);       

    while (ni.Next(point)) {
	if (point.x < xmin) xmin = point.x;
	if (point.y < ymin) ymin = point.y;
	if (point.x > xmax) xmax = point.x;
	if (point.y > ymax) ymax = point.y;
	if (miter) {
	    if (i == 0) {
		p1.x = point.x;
		p1.y = point.y;
	    } else if (i == 1) { 
		p2.x = point.x;
		p2.y = point.y;
	    } else {
		p3.x = point.x;
		p3.y = point.y;
		miter_length = Pad_Miter_length(lineWidth, p1.x, p1.y, 
					       p2.x, p2.y, p3.x, p3.y);
		if (p2.x - miter_length < xmin) xmin = p2.x - miter_length;
		if (p2.y - miter_length < ymin) ymin = p2.y - miter_length;
		if (p2.x + miter_length > xmax) xmax = p2.x + miter_length;
		if (p2.y + miter_length > ymax) ymax = p2.y + miter_length;
		
		p1.x = p2.x;
		p1.y = p2.y;
		p2.x = p3.x;
		p2.y = p3.y;
	    }
	    i++;
	}
    }
    bbox_ret[XMIN] = xmin;    
    bbox_ret[YMIN] = ymin;    
    bbox_ret[XMAX] = xmax;    
    bbox_ret[YMAX] = ymax;    
}

//
// Returns the points used for a noisy line
//
Pad_Point *
Pad_NoiseData::Get_points(Pad_PList &points, Pad_Bool closed, int &numgot) 
{
    return(Get_points(points.Pointer(), points.Length(), closed, numgot));
}

Pad_Point *
Pad_NoiseData::Get_points(Pad_Point *points, int length, Pad_Bool closed, int &numgot) 
{
    Pad_NoiseIterator ni;
    Pad_Point point;
    Pad_Point *pts = new Pad_Point[32];
    int npts = 32, nout = 0;

    ni.Setup(this, points, length, closed);       
    
    while (ni.Next(point)) {	
	if (nout >= npts) { 
	    int i, new_npts = npts * 2; 
	    Pad_Point *new_pts = new Pad_Point[new_npts]; 
	    for (i = 0; i < npts; i++) { 
		new_pts[i] = pts[i]; 
	    } 
	    delete[] pts;
	    pts = new_pts; 
	    npts = new_npts; 
	} 
	pts[nout].x = point.x;
	pts[nout].y = point.y;
	nout++;
    }

    numgot = nout;
    return pts;
}




//
// Noise Iterator
//


// A noise iterator is used to generate the points along a noisy line. Its usually
// used with a piece of code that looks like:
//
//    Pad_NoiseIterator ni;
//    Pad_Point point;
//    ni.Setup(noise_data, points, length, closed);
//    while (ni.Next(point)) { ... do something ... }
//

Pad_NoiseIterator::Pad_NoiseIterator()
{
    nd = NULL;
    points = NULL;
    closed = TRUE;
}

void
Pad_NoiseIterator::Setup(Pad_NoiseData *ndata, Pad_PList &pts, Pad_Bool closd) 
{
    Setup(ndata, pts.Pointer(), pts.Length(), closd);
}

void
Pad_NoiseIterator::Setup(Pad_NoiseData *ndata,
			 Pad_Point *pts, int len,  Pad_Bool closd) 
{
    nd = ndata;
    points = pts;
    length = len;
    closed = closd;
    pos = nd->pos;
    step = (1.0/nd->steps);    

    i = -1;
}


void
Pad_NoiseIterator::Setup_segment(int i)
{
    float dx, dy, theta, mag;

    x1 = points[i].x;
    y1 = points[i].y;

    if (i >= length - 1) {
	x2 = points[0].x;
	y2 = points[0].y;
    } else {
	x2 = points[i+1].x;
	y2 = points[i+1].y;
    }

    dx = (x2-x1);
    dy = (y2-y1);
    if ((dx == 0.0) && (dy == 0.0)) {
	theta = 0.0;
	mag = 0.0;
    } else {
	theta = atan2(dy, dx);
	mag = sqrt(dx*dx+dy*dy);
    }

    xamp = nd->amp * sin(theta)*mag;
    yamp = -nd->amp * cos(theta)*mag;
    a = step;
}

int
Pad_NoiseIterator::Next(Pad_Point &point)
{
    int should_continue = 1;

    if (i == -1) {
	point.x = points[0].x;
	point.y = points[0].y;
	i = 0;
	Setup_segment(i);
    } else if (i >= length) {
	// Reached end of line - terminate
	should_continue = 0;
    } else if (a >= 1) {
	// Reached end of a segment - move onto next
	i++;

	if (i == length) {
	    // return first point - next call will terminate
	    point.x = points[0].x;
	    point.y = points[0].y;	    
	} else {
	    point.x = points[i].x;
	    point.y = points[i].y;
	}

	if (i == length - 1) {
	    // now on last segment.
	    if (!closed) {
		i++; // force next call to terminate
		return(1);
	    }
	}

	Setup_segment(i);
    } else {
	// Evaluate a point
	float n = noise1(pos);
	point.x = LERP(a, x1, x2) + n * xamp;
	point.y = LERP(a, y1, y2) + n * yamp;    
	pos += nd->freq;
	a += step;
    }

    return(should_continue);
}



