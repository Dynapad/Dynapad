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

#ifndef TRANSFORM_H
#define TRANSFORM_H 1

#include "defs.h"
#include "bbox.h"
#include "misc.h"
#include "point.h"
#include "events.h"

class Pad_Object;

class Pad_Transform
{
  public:

    ///////////////////////////////////////////////////////////////////////////////
    //
    // The following Pad_Transform methods modify the parameters that 
    // get passed in.
    //
    ///////////////////////////////////////////////////////////////////////////////

				// Apply this transformation to the parameter
    inline void Apply(float &x, float &y) {
	x = (x * _scale) + _xoffset;
	y = (y * _scale) + _yoffset;
    }
    inline void Apply(float &x, float &y, float &s) {
	x = (x * _scale) + _xoffset;
	y = (y * _scale) + _yoffset;
	s *= _scale;
    }
    inline void Apply(float &s) {
	s *= _scale;
    }
    inline void Apply(Pad_Transform *transform) {Apply(transform->_xoffset, transform->_yoffset, transform->_scale);}
    inline void Apply(Pad_Transform &transform) {Apply(transform._xoffset, transform._yoffset, transform._scale);}
    inline void Apply(Pad_Point *point) {Apply(point->x, point->y);}
    inline void Apply(Pad_Point &point) {Apply(&point);}
    inline void Apply(Pad_Point *point, float &s) {Apply(point->x, point->y, s);}
    inline void Apply(Pad_Point &point, float &s) {Apply(&point, s);}
    inline void Apply(float *bb) {Apply(bb[XMIN], bb[YMIN]); Apply(bb[XMAX], bb[YMAX]);}
    inline void Apply(Pad_BBox &bb) {Apply(bb.Get());}
                                // BBB - why does inverting an event have to reverse the multiplication of magnitude?
    inline void Apply(Pad_Event *event) {Apply(event->pt); event->mag /= Get_scale();}
           void Apply(Pad_Object *obj);
    inline void Apply_x(float &x) {x = (x * _scale) + _xoffset;}
    inline void Apply_y(float &y) {y = (y * _scale) + _yoffset;}
    inline void Apply_x(short &x) {x = Pad_F2s((x * _scale) + _xoffset);}
    inline void Apply_y(short &y) {y = Pad_F2s((y * _scale) + _yoffset);}

				// Apply the inverted transform to the parameter
    inline void Invert(float &x, float &y) {
	x = (x - _xoffset) / _scale;
	y = (y - _yoffset) / _scale;
    }
    inline void Invert(float &x, float &y, float &s) {
	x = (x - _xoffset) / _scale;
	y = (y - _yoffset) / _scale;
	s /= _scale;
    }
    inline void Invert(float &s) {
	s /= _scale;
    }
    inline void Invert(Pad_Transform *transform) {Invert(transform->_xoffset, transform->_yoffset, transform->_scale);}
    inline void Invert(Pad_Transform &transform) {Invert(transform._xoffset, transform._yoffset, transform._scale);}
    inline void Invert(Pad_Point *point) {Invert(point->x, point->y);}
    inline void Invert(Pad_Point &point) {Invert(&point);}
    inline void Invert(Pad_Point *point, float &s) {Invert(point->x, point->y, s);}
    inline void Invert(Pad_Point &point, float &s) {Invert(&point, s);}
    inline void Invert(float *bb) {Invert(bb[XMIN], bb[YMIN]); Invert(bb[XMAX], bb[YMAX]);}
    inline void Invert(Pad_BBox &bb) {Invert(bb.Get());}
                                // BBB - why does inverting an event have to reverse the multiplication of magnitude?
    inline void Invert(Pad_Event *event) {Invert(event->pt); event->mag *= Get_scale();}
           void Invert(Pad_Object *obj);
    inline void Invert_x(float &x) {x = (x - _xoffset) / _scale;}
    inline void Invert_y(float &y) {y = (y - _yoffset) / _scale;}
    inline void Invert_x(short &x) {x = Pad_F2s((x - _xoffset) / _scale);}
    inline void Invert_y(short &y) {y = Pad_F2s((y - _yoffset) / _scale);}

    ///////////////////////////////////////////////////////////////////////////////
    //
    // The following Pad_Transform methods modify the transform based
    // on the parameters that get passed in.
    //
    ///////////////////////////////////////////////////////////////////////////////

				// Set transformation to specified values
    inline void Set(float x, float y, float s) {_xoffset = x; _yoffset = y; _scale = s;}
    inline void Set_scale(float s) {_scale = s;}
    inline void Set_offset(float x, float y) {_xoffset = x; _yoffset = y;}
    inline void Set_offset_x(float x) {_xoffset = x;}
    inline void Set_offset_y(float y) {_yoffset = y;}
				// Modify transformation
    inline void Scale(float s) {_scale *= s;}
    inline void Translate(float x, float y) {_xoffset += x * _scale; _yoffset += y * _scale;}
    inline void Translate_x(float x) {_xoffset += x * _scale;}
    inline void Translate_y(float y) {_yoffset += y * _scale;}

    inline Pad_Transform& operator=(Pad_Transform *transform) {
	_xoffset = transform->_xoffset;
	_yoffset = transform->_yoffset;
	_scale = transform->_scale;
	
	return (*this);
    }
    inline Pad_Transform& operator=(Pad_Transform &transform) {
	*this = &transform;
	return (*this);
    }

    ///////////////////////////////////////////////////////////////////////////////
    //
    // These retrieve transform components
    //
    ///////////////////////////////////////////////////////////////////////////////

				// Retrieve transform values
    inline void  Get(float &x, float &y, float &s) {x = _xoffset; y = _yoffset; s = _scale;}
    inline float Get_scale(void) {return(_scale);}
    inline void  Get_offset(float &x, float &y) {x = _xoffset; y = _yoffset;}
    inline float Get_offset_x(void) {return(_xoffset);}
    inline float Get_offset_y(void) {return(_yoffset);}


    //
    // Constructors
    // 
    inline Pad_Transform() {
	_xoffset = 0.0;
	_yoffset = 0.0;
	_scale = 1.0;
    }
    inline Pad_Transform(Pad_Transform *transform) {
	*this = transform;
    }

private:
    float _xoffset;
    float _yoffset;
    float _scale;
};

#endif
