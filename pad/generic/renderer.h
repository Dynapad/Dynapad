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

#ifndef RENDERER_H
#define RENDERER_H 1

#include "defs.h"
#include "list.h"
#include "point.h"
#include "transform.h"
#include "plist.h"
#include "color.h"
#include "border.h"
#include "font.h"

#include <X11/Xft/Xft.h>

//
// Forward reference classes
//
class Pad_FontData;
class Pad_BBox;
class Pad_String;
class Pad_Display;
class Pad_ImageData;
class Pad_RenderContext;
class Pad_Win;
class Pad_Transform;
class Pad_View;
class Pad_Restorer;

typedef struct Pad_3DBorder_ *Pad_3DBorder;

#define RENDERER_NO_FLAGS        0

//
// Pad_Renderer
//
//   Base class to perform all Pad++ rendering.
//
class Pad_Renderer
{
  public:
    virtual Pad_ImageData *Alloc_image(char *name, Pad_Bool *newflag) = 0;
    virtual Pad_ImageData *Alloc_image(unsigned char *data, int len) = 0;
    virtual Pad_ImageData *Alloc_image(void) = 0;
    virtual void        Begin_line(void) = 0;
    virtual void        Begin_oval(void) = 0;
    virtual void        Begin_polygon(void) = 0;
    virtual void        Begin_rectangle(void) = 0;
    virtual void        Draw_3d_horizontal_bevel(Pad_Point &min, Pad_Point &max, Pad_Bool leftIn, Pad_Bool rightIn, 
						 Pad_Bool topBevel, int relief) = 0;
    virtual void        Draw_3d_polygon(Pad_PList &points, float polyBorderWidth, int leftRelief) = 0;
    virtual void        Draw_3d_rectangle(Pad_Point &min, Pad_Point &max, float borderWidth, int relief) = 0;
    virtual void        Draw_3d_vertical_bevel(Pad_Point &min, Pad_Point &max, Pad_Bool leftBevel, int relief) = 0;
    virtual void        Draw_box(float xmin, float ymin, float xmax, float ymax) = 0;
    virtual void        Draw_box(Pad_Point *pt) = 0;
    virtual void        Draw_clip_region(Pad_Restorer *restorer) = 0;
    virtual void        Draw_filled_3d_polygon(Pad_PList &points, float polyBorderWidth, int leftRelief) = 0;
    virtual void        Draw_filled_3d_rectangle(Pad_Point &min, Pad_Point &max, float borderWidth, int relief) = 0;
    virtual void        Draw_filled_box(float xmin, float ymin, float xmax, float ymax) = 0;
    virtual Pad_Bool    Draw_image(Pad_ImageData *image,
				   float dst_left, float dst_bot, float dst_right, float dst_top, 
				   Pad_Bool dither) = 0;
    // JM's experimental tiling facility
    virtual void Set_tile(Pad_ImageData *src, Pad_ImageData *dst, long *tmp,
			  float a, int tx, int ty) = 0;
    virtual void           Draw_line(int npts, Pad_Point *pts, Pad_Bool closed) = 0;
    virtual void           Draw_line(Pad_PList &pts, Pad_Bool closed) = 0;
    virtual void           Draw_polygon(int npts, Pad_Point *pts) = 0;
    virtual void           Draw_polygon(Pad_PList &pts) = 0;
    virtual Pad_Bool       Draw_string(char *string, int level, float xoff, float yoff, float theta=0.0) = 0;
    virtual void           End_line(void) = 0;
    virtual void           End_oval(Pad_Bool fill) = 0;
    virtual void           End_polygon(void) = 0;
    virtual void           End_rectangle(void) = 0;
   
    virtual Pad_Bool       Free_image(char *token) = 0;
    virtual Pad_Bool       Free_image(Pad_ImageData *image) = 0;
   
    virtual void           Get_border(Pad_String &bordername) = 0;
    virtual float          Get_char_width(char c) = 0;
    virtual float          Get_char_height(char c) = 0;
    virtual unsigned char  Get_cap_style(void) = 0;
    virtual const char *   Get_cap_style_name(void) = 0;
    virtual void           Get_color(Pad_String &colorName) = 0;
    virtual void           Get_font(Pad_String &fontName) = 0;
    virtual float          Get_font_descender(void) = 0;
    virtual float          Get_font_height(void) = 0;
    virtual Pad_ImageData* Get_image(char *token) = 0;
    virtual unsigned char  Get_join_style(void) = 0;
    virtual const char *   Get_join_style_name(void) = 0;
    virtual float          Get_line_width(void) = 0;
    virtual void           Get_string_bbox(const char *string, Pad_BBox *bbox) = 0;
    virtual float          Get_mag(void) = 0;
    virtual void           Get_transform(Pad_Transform &transform) = 0;
    virtual float          Get_transparency(void) = 0;
    virtual void           Init_transform_stack(void) = 0;
    virtual Pad_Bool       Is_interrupted(Pad_Win *win) = 0;
    virtual void           Local_to_screen(Pad_Point &point) = 0;
    virtual float          Nearest_pixel_x(float x) = 0;
    virtual float          Nearest_pixel_y(float y) = 0;
    virtual void           Pop_object(void) = 0;
    virtual void           Pop_transform(void) = 0;
    virtual void           Pop_view(void) = 0;
    virtual void           Push_object(Pad_Object *obj) = 0;
    virtual void           Push_transform(Pad_Transform &transform) = 0;
    virtual void           Push_view(Pad_View *view) = 0;
    virtual void           Scale(float) = 0;
    virtual void           Set_abs_line_width(float width) = 0;
    virtual void           Set_abs_line_style(float w, unsigned char capStyle, unsigned char joinStyle) = 0;
    virtual void           Set_border(Pad_Border *border) = 0;
    void                   Set_border(Pad_BorderRef &borderref) { Set_border(borderref.border); }
    virtual void           Set_cap_style(unsigned char capStyle) = 0;
    virtual void           Set_cap_style(const char *capStyle) = 0;
    virtual void           Set_color(Pad_Color *color) = 0;
    void                   Set_color(Pad_ColorRef &colorref) { Set_color(colorref.color); }
    virtual void           Set_concave(Pad_Bool concave) = 0;
    virtual void           Set_device(Pad_RenderContext *context) = 0;
    virtual void           Set_font(Pad_Font *font) = 0;
    void                   Set_font(Pad_FontRef &fontref) { Set_font(fontref.font); }

    virtual void        Set_font_height(float size) = 0;	    // Height of font in pixels
    virtual void        Set_join_style(unsigned char joinStyle) = 0;
    virtual void        Set_join_style(char *joinStyle) = 0;
    virtual void        Set_line_width(float width) = 0;
    virtual void        Set_line_style(float w, unsigned char capStyle, unsigned char joinStyle, Pad_Bool absLineStyle = FALSE) = 0;
    virtual void        Set_transparency(float transparency) = 0;
    virtual void        Swap_buffers(int speed) = 0;
    virtual void        Translate(float x, float y) = 0;
    virtual void        B8f(Pad_Point &p0, Pad_Point &p1, Pad_Point &p2, Pad_Point &p3) = 0;
    virtual void        B8f(Pad_Point *p0, Pad_Point *p1, Pad_Point *p2, Pad_Point *p3) = 0;
    virtual void        V2f(const float x, const float y) = 0;
    virtual void        V2f(const float x, const float y, float theta) = 0;
    virtual void        V2f(const float v[2]) = 0;
    virtual void        V2f(const Pad_Point &p) = 0;
    virtual ~Pad_Renderer();
};

//
// Pad_XRenderer
//
//   X Renderer
//
class Pad_XRenderer : public Pad_Renderer
{
  public:
    virtual Pad_ImageData *Alloc_image(char *name,Pad_Bool *newflag);
    virtual Pad_ImageData *Alloc_image(unsigned char *data, int len);
    virtual Pad_ImageData *Alloc_image(void);
    virtual void        Begin_line(void);
    virtual void        Begin_oval(void);
    virtual void        Begin_polygon(void);
    virtual void        Begin_rectangle(void);
    virtual void        Draw_3d_horizontal_bevel(Pad_Point &min, Pad_Point &max, Pad_Bool leftIn, Pad_Bool rightIn, 
						 Pad_Bool topBevel, int relief);
    virtual void        Draw_3d_polygon(Pad_PList &points, float polyBorderWidth, int leftRelief);
    virtual void        Draw_3d_rectangle(Pad_Point &min, Pad_Point &max, float borderWidth, int relief);
    virtual void        Draw_3d_vertical_bevel(Pad_Point &min, Pad_Point &max, Pad_Bool leftBevel, int relief);
    virtual void        Draw_box(float xmin, float ymin, float xmax, float ymax);
    virtual void        Draw_box(Pad_Point *pt);
    virtual void        Draw_clip_region(Pad_Restorer *restorer);
    virtual void        Draw_filled_3d_rectangle(Pad_Point &min, Pad_Point &max, float borderWidth, int relief);
    virtual void        Draw_filled_3d_polygon(Pad_PList &points, float polyBorderWidth, int leftRelief);
    virtual void        Draw_filled_box(float xmin, float ymin, float xmax, float ymax);
    virtual Pad_Bool    Draw_image(Pad_ImageData *image,
				   float dst_left, float dst_bot, float dst_right, float dst_top, 
				   Pad_Bool dither);
    // JM's experimental tiling facility
    virtual void Set_tile(Pad_ImageData *src, Pad_ImageData *dst, long *tmp, 
			  float a, int tx, int ty);
    virtual void           Draw_line(int npts, Pad_Point *pts, Pad_Bool closed);
    virtual void           Draw_line(Pad_PList &pts, Pad_Bool closed);
    virtual void           Draw_polygon(int npts, Pad_Point *pts);
    virtual void           Draw_polygon(Pad_PList &pts);
    virtual Pad_Bool       Draw_string(char *string, int level, float xoff, float yoff, float theta=0.0);
    virtual void           End_line(void);
    virtual void           End_oval(Pad_Bool fill=FALSE);
    virtual void           End_polygon(void);
    virtual void           End_rectangle(void);
    virtual Pad_Bool       Free_image(char *token);
    virtual Pad_Bool       Free_image(Pad_ImageData *image);
    virtual void           Get_border(Pad_String &borderName);
    virtual unsigned char  Get_cap_style(void);
    virtual const char *   Get_cap_style_name(void);
    virtual float          Get_char_height(char c);
    virtual float          Get_char_width(char c);
    virtual void           Get_color(Pad_String &colorName);
    virtual void           Get_font(Pad_String &fontName);
    virtual float          Get_font_descender(void);
    virtual float          Get_font_height(void);
    virtual Pad_ImageData* Get_image(char *token);
    virtual unsigned char  Get_join_style(void);
    virtual const char *   Get_join_style_name(void);
    virtual float          Get_line_width(void);
    virtual void           Get_string_bbox(const char *string, Pad_BBox *bbox);
    virtual float          Get_transparency(void);
    virtual float          Get_mag(void);
    virtual void           Get_transform(Pad_Transform &transform);
    virtual void           Init_transform_stack(void);
    virtual Pad_Bool       Is_interrupted(Pad_Win *win);
    virtual void           Local_to_screen(Pad_Point &point);
    virtual float          Nearest_pixel_x(float x);
    virtual float          Nearest_pixel_y(float y);
    virtual void           Pop_object(void);
    virtual void           Pop_transform(void);
    virtual void           Pop_view(void);
    virtual void           Push_object(Pad_Object *obj);
    virtual void           Push_transform(Pad_Transform &transform);
    virtual void           Push_view(Pad_View *view);
    virtual void           Scale(float s);
    virtual void           Set_abs_line_width(float width);
    virtual void           Set_abs_line_style(float w, unsigned char capStyle, unsigned char joinStyle);
    virtual void           Set_border(Pad_Border *border);
    virtual void           Set_cap_style(unsigned char capStyle);
    virtual void           Set_cap_style(const char *capStyle);
    virtual void           Set_color(Pad_Color *color);
    virtual void           Set_concave(Pad_Bool concave);
    virtual void           Set_device(Pad_RenderContext *context);
    virtual void           Set_font(Pad_Font *font);
    virtual void           Set_font_height(float size);	    // Height of font in pixels
    virtual void           Set_join_style(unsigned char joinStyle);
    virtual void           Set_join_style(char *joinStyle);
    virtual void           Set_line_width(float width);
    virtual void           Set_line_style(float w, unsigned char capStyle, unsigned char joinStyle, Pad_Bool absLineStyle = FALSE);
    virtual void           Set_transparency(float transparency);
    virtual void           Swap_buffers(int speed);
    virtual void           Translate(float x, float y);
    virtual void           B8f(Pad_Point &p0, Pad_Point &p1, Pad_Point &p2, Pad_Point &p3);
    virtual void           B8f(Pad_Point *p0, Pad_Point *p1, Pad_Point *p2, Pad_Point *p3);
    virtual void           V2f(const float x, const float y);
    virtual void           V2f(const float x, const float y, float theta);
    virtual void           V2f(const float v[2]);
    virtual void           V2f(const Pad_Point &p);

    Pad_XRenderer(Pad_Display *dpy, Pad_Win *win);
    virtual ~Pad_XRenderer();

    // Used in fontcache.C
    XPoint *Get_device_points(int &numPoints);

  private:
				// Resources
    Pad_Display * _dpy;
    Pad_Win *     _win;
    unsigned char _flags;
				// Render state
    Display *     _display;
    Drawable      _drawable;
    XftDraw      *_xftdraw;
    int           _width;
    int           _height;
    GC            _fgGC;
				// Graphics state
    Pad_Font   * _font;             // generic font description
    Pad_FontData * _fontData;       // Device-specific data for this font
    float        _fontHeightMult;   // Multiplier for this font

    Pad_Color *  _color;	      // Current color (NULL if none)
    Pad_Border * _border;	      // Current border (NULL if none)
    Pad_3DBorder _3dborder;           // X resources for border (NULL if none)
    int          _lineWidth;	      // Line width
    unsigned char _capStyle;	      // X Cap style
    unsigned char _joinStyle;	      // X Join style
    int          _concave;	      // X Concave mode
    float        _transparency;	      // Current Transparency (0 - transparent, 1 - opaque)
    int          _ptIndex;	      // Index into array of points
    int          _ptNum;	      // Number of points allocated for _point
    XPoint       *_point;             // Array of points for building up object
    Pad_Bool      _viewAppliedToTransform;
    Pad_List     _transform;          // Stack of transformations (Pad_Transform)
    Pad_List     _views;              // Stack of views (Pad_View)
    Pad_List     _objects;            // Stack of objects (Pad_Object)

    void     _Bezier(const float, const float, const float, const float, 
		     const float, const float, const float, const float);
    void     _Dissolve_buffers(int speed);
    void     _Draw_3d_polygon(Pad_PList &points, float polyBorderWidth, int leftRelief, Pad_Bool filled);
    void     _Draw_3d_rectangle(Pad_Point &min, Pad_Point &max, float borderWidth, int relief, Pad_Bool filled);
    Pixmap   _Get_transparency_stipple(float transparency);
    void     _Reallocate_points(void);
    void     _Restore_border_gc(void);
    void     _Update_border_gc(void);
    void     _Transform_point(float x, float y, int &ix, int &iy);
    void     _Transform_point(Pad_Point &point, int &ix, int &iy);
    void     _Transform_point(Pad_Transform *transform, float x, float y, int &ix, int &iy);
    void     _Transform_point(Pad_Transform *transform, Pad_Point &point, int &ix, int &iy);
};

#endif
