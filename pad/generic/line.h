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

#ifndef LINE_H
#define LINE_H 1

#include "defs.h"
#include "object.h"
#include "view.h"
#include "plist.h"

class Pad_String;
class Pad_Event;

				     // Bits for objFlags
#define LINE_ARROW_SET       (1<<0)  // Set if -arrow      has been configured
#define LINE_ARROWSHAPE_SET  (1<<1)  // Set if -arrowshape has been configured
#define LINE_CAPSTYLE_SET    (1<<2)  // Set if -capstyle   has been configured
#define LINE_JOINSTYLE_SET   (1<<3)  // Set if -joinstyle  has been configured
#define LINE_PEN_SET         (1<<4)  // Set if -pen        has been configured
#define LINE_PENWIDTH_SET    (1<<5)  // Set if -penwidth   has been configured
#define LINE_FILL_SET        (1<<6)  // Set if -fill       has been configured (might be for derived types)

#define PTS_IN_ARROW 6		// Number of points in an arrowhead


//
// Arrow head types
//
#define ARROW_NONE    0
#define ARROW_FIRST   1
#define ARROW_LAST    2
#define ARROW_BOTH    3

//
// A line connecting a sequence of points
//
class Pad_Line : public Pad_Object
{
  public:
    Pad_PList     points;	// List of points
    float         lineWidth;	// Line width
    Pad_Bool	  absLineStyle; // Line width fixed pixels?
    Pad_ColorRef  penColor;	// Pen color
    unsigned char lineFlags;    // OR'd combination of flags
    unsigned char capStyle;	// X Cap style
    unsigned char joinStyle;	// X Join style
    float         arrowShapeA;	// Distance from tip of arrowhead to center.
    float         arrowShapeB;	// Distance from tip of arrowhead to trailing
				// point, measured along shaft.
    float         arrowShapeC;	//Distance of trailing points from outside
				// edge of shaft.
    Pad_Point *   firstArrowPtr; //Points to array of PTS_IN_ARROW points
				// describing polygon for arrowhead at first
				// point in line.  First point of arrowhead
				// is tip.  Malloc'ed.  NULL means no arrowhead
				// at first point.
    Pad_Point *   lastArrowPtr;	// Points to polygon for arrowhead at last
				// point in line (PTS_IN_ARROW points, first
				// of which is tip).  Malloc'ed.  NULL means
				// no arrowhead at last point.

    virtual Pad_Bool   Append_coords(Pad_PList &pts, Pad_Bool object_coords);
    virtual void       Compute_bounding_box(void);
    virtual int        Configure_arrows(int arrow);
    virtual int        Create_obj_args(int argc, char **argv);   // Called when an item is created with string args
    virtual char *     Get_capstyle(void);
    virtual int        Get_arrow(void);
    virtual char *     Get_arrowshape(void);
    virtual void       Get_coords(Pad_PList &pts, Pad_Bool object_coords);
    virtual char *     Get_joinstyle(void);
            void       Get_pen_name(Pad_String &penname);
            float      Get_penwidth(void);
            Pad_Bool   Get_abslinestyle(void);
    virtual Pad_Bool   Is_rotatable();
    virtual Pad_Bool   Pick(Pad_Event *event, float halo);
    virtual Pad_Bool   Render(void);
            void       Render_line(Pad_Bool closed);
            void       Render_polygon(void);
    virtual Pad_Bool   Rotate(float theta);
    virtual Pad_Bool   Rotate(float theta, Pad_Point &center);
    virtual void       Set_arrow(int);
    virtual void       Set_arrow_default(void);
    virtual void       Set_arrowshape(float a, float b, float c);
    virtual void       Set_arrowshape_default(void);
    virtual Pad_Bool   Set_capstyle(char *capstyle);
    virtual void       Set_capstyle_default(void);
    virtual Pad_Bool   Set_coords(Pad_PList &pts, Pad_Bool object_coords);
    virtual Pad_Bool   Set_height(float height);
    virtual Pad_Bool   Set_joinstyle(char *joinstyle);
    virtual void       Set_joinstyle_default(void);
            Pad_Bool   Set_pen(char *colorname);
            Pad_Bool   Set_pen(int red, int green, int blue);
            void       Set_pen_default(void);
            Pad_Bool   Set_penwidth(float penwidth, Pad_Bool abslinestyle = FALSE);
            void       Set_penwidth_default(void);
    virtual Pad_Bool   Set_width(float width);

    virtual ~Pad_Line();
    Pad_Line(Pad *pad);

    PAD_TYPE("line");
};

//
// A spline is a series of bezier curves - open splines have
// 3N+1 points, whereas closed splines have 3N points.
//
class Pad_Spline : public Pad_Line
{
  public:
    virtual int        Create_obj_args(int argc, char **argv);   // Called when an item is created with string args
    virtual Pad_Bool   Pick(Pad_Event *event, float halo);
    virtual Pad_Bool   Render(void);

    virtual ~Pad_Spline();
    Pad_Spline(Pad *pad);
    
    PAD_TYPE("spline");
};


//
// A polygon that connects and fills a sequence of points.
//
class Pad_Polygon : public Pad_Line
{
  public:
    Pad_ColorRef       fillColor;	// Fill color

    virtual void       Get_fillname(Pad_String &fillname);
    virtual Pad_Bool   Pick(Pad_Event *event, float halo);
    virtual Pad_Bool   Render(void);
    virtual Pad_Bool   Set_fill(char *colorname);
    virtual Pad_Bool   Set_fill(int red, int green, int blue);
    virtual void       Set_fill_default(void);

    virtual ~Pad_Polygon();
    Pad_Polygon(Pad *pad);

    PAD_TYPE("polygon");
};

//
// A rectangle is a limited kind of polygon that 
// gets rendered faster if it is not rotated.
// It is specified by lower left and upper right corners
// that is aligned with the coordinate system.
//
class Pad_Rectangle : public Pad_Polygon
{
  public:
    virtual Pad_Bool   Append_coords(Pad_PList &pts, Pad_Bool object_coords);
    virtual int        Create_obj_args(int argc, char **argv);   // Called when an item is created with string args
    virtual void       Get_coords(Pad_PList &pts, Pad_Bool object_coords);
    virtual Pad_Bool   Pick(Pad_Event *event, float halo);
    virtual Pad_Bool   Render(void);
    virtual Pad_Bool   Rotate(float theta);
    virtual Pad_Bool   Rotate(float theta, Pad_Point &center);
    virtual Pad_Bool   Set_coords(Pad_PList &pts, Pad_Bool object_coords);

    virtual ~Pad_Rectangle();
    Pad_Rectangle(Pad *pad);
    Pad_Rectangle(Pad *pad, float x1, float y1, float x2, float y2);
    
    PAD_TYPE("rectangle");
};

//
// An oval is a limited kind of polygon that 
// gets rendered faster if it is not rotated.
// It is specified by lower left and upper right corners
// that is aligned with the coordinate system.
//
class Pad_Oval : public Pad_Rectangle
{
  public:
    Pad_PList          renderpoints;	// List of actual oval points used to render

    virtual Pad_Bool   Pick(Pad_Event *event, float halo);
    virtual Pad_Bool   Render(void);
    virtual Pad_Bool   Set_coords(Pad_PList &pts, Pad_Bool object_coords);
    virtual Pad_Bool   Rotate(float theta);
    virtual Pad_Bool   Rotate(float theta, Pad_Point &center);
    virtual void       Compute_bounding_box(void);

    virtual ~Pad_Oval();
    Pad_Oval(Pad *pad);
    Pad_Oval(Pad *pad, float x1, float y1, float x2, float y2);
    
    PAD_TYPE("oval");
};

#endif
