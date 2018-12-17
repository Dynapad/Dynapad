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

#ifndef PAD_TK_BORDER_H
#define PAD_TK_BORDER_H 1

/*
 * Dummy types that are used by clients:
 */
typedef struct Pad_3DBorder_ *Pad_3DBorder;


Pad_3DBorder Pad_Alloc3DBorder(Pad_Display *dpy, int r, int g, int b);
void         Pad_3DVerticalBevel(Pad_Display *dpy, Drawable drawable, Pad_3DBorder border, 
				 int x, int y, int width, int height, int leftBevel, int relief);
void         Pad_3DHorizontalBevel(Pad_Display *dpy, Drawable drawable, Pad_3DBorder border, 
				   int x, int y, int width, int height,
				   int leftIn, int rightIn, int topBevel, int relief);
void         Pad_Draw3DRectangle(Pad_Display *dpy, Drawable drawable, Pad_3DBorder border, 
				 int x, int y, int width, int height,
				 int borderWidth, int relief);
GC           Pad_3DBorderGC(Pad_Display *dpy, Pad_3DBorder border, int which);
void         Pad_Free3DBorder(Pad_Display *dpy, Pad_3DBorder border);
void         Pad_Draw3DPolygon(Pad_Display *dpy, Drawable drawable, Pad_3DBorder border, XPoint *pointPtr, 
			       int numPoints, int borderWidth, int leftRelief);
void         Pad_Fill3DRectangle(Pad_Display *dpy, Drawable drawable, Pad_3DBorder border, 
				 int x, int y, int width, int height, int borderWidth, int relief);
void         Pad_Fill3DPolygon(Pad_Display *dpy, Drawable drawable, Pad_3DBorder border, XPoint *pointPtr, 
			       int numPoints, int borderWidth, int leftRelief);


#endif
