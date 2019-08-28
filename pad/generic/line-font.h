/*
"(c) Copyright 1993-1995 Pad++ Consortium {University of New Mexico (UNM),
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

#ifndef LINE_FONT_H
#define LINE_FONT_H 1

#include "point.h"

class Pad_LineFontChar {
public:
    int index;
    int segments;		// Number of segments
    int *segment_length;	// Length of each segment
    Pad_Point *point;		// Array of points with successive segments following each other
    float width;

    void Init(int new_segments, ...);
    void Add_point(float x, float y);

    Pad_LineFontChar();
};

class Pad_LineFont {
public:  
  static  float CharHeight(void);
  static  float CharWidth(unsigned char c);
  static  float DescenderHeight(void);
  static  void  DrawChar_1(float xoff, float yoff);
  static  void  DrawChar_1(float xoff, float yoff, float theta);
  static  void  DrawChar_2(char c, float xoff, float yoff);
  static  void  DrawChar_2(char c, float xoff, float yoff, float theta);
  static  void  DrawString(char *string);
  static  void  DrawString_0(char *string, float xoff, float yoff);
  static  void  DrawString_1(char *string, float xoff, float yoff);
  static  void  DrawString_1(char *string, float xoff, float yoff, float theta);
  static  void  DrawString_2(char *string, float xoff, float yoff);
  static  void  DrawString_2(char *string, float xoff, float yoff, float theta);
  static  void  InitLineFonts(void);
  static  float LineHeight(void);
  static  float StringWidth(const char *string);

  static Pad_LineFontChar *letters[256];
};

#endif
