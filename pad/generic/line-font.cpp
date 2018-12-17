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

#include "defs.h"
#include "line-font.h"
#include "renderer.h"
#include "global.h"

//
// Draw a character at full resolution (level 2)
//
void
Pad_LineFont::DrawChar_2(char c, float xoff, float yoff)
{
    int i, j;
    int segments;
    int *segment_length;
    Pad_Point *point;
    Pad_LineFontChar *data;

    data = Pad_LineFont::letters[c];
    segments = data->segments;
    segment_length = data->segment_length;
    point = data->point;
    for (i=0; i<segments; i++) {
	Pad_renderer->Begin_line();
	for (j=0; j<segment_length[i]; j++) {
	    Pad_renderer->V2f(point[0].x + xoff, point[0].y + yoff);
	    point++;
	}
	Pad_renderer->End_line();
    }
}

void
Pad_LineFont::DrawChar_2(char c, float xoff, float yoff, float theta)
{
    int i, j;
    int segments;
    int *segment_length;
    Pad_Point *point;
    Pad_LineFontChar *data;

    data = Pad_LineFont::letters[c];
    segments = data->segments;
    segment_length = data->segment_length;
    point = data->point;
    for (i=0; i<segments; i++) {
	Pad_renderer->Begin_line();
	for (j=0; j<segment_length[i]; j++) {
	    Pad_renderer->V2f(point[0].x + xoff, point[0].y + yoff, theta);
	    point++;
	}
	Pad_renderer->End_line();
    }
}

//
// Draw a character at medium resolution (level 1)
//
void
Pad_LineFont::DrawChar_1(float xoff, float yoff)
{
    Pad_renderer->Begin_line();
    Pad_renderer->V2f(xoff, yoff);
    Pad_renderer->V2f(xoff, 0.7 + yoff);
    Pad_renderer->End_line();
}

void
Pad_LineFont::DrawChar_1(float xoff, float yoff, float theta)
{
    Pad_renderer->Begin_line();
    Pad_renderer->V2f(xoff, yoff, theta);
    Pad_renderer->V2f(xoff, 0.7 + yoff, theta);
    Pad_renderer->End_line();
}

void
Pad_LineFont::DrawString(char *string)
{
    DrawString_2(string, 0.0, 0.0);
}

//
// Render text at maximum resolution
//
void
Pad_LineFont::DrawString_2(char *string, float xoff, float yoff)
{
    int i, j, index;
    int segments;
    int *segment_length;
    Pad_Point *point;
    Pad_LineFontChar *data;
    unsigned char c;
    float rxoff, ryoff;
    int tabpos = 0;
    float space_width;

    if (!string)
      return;

    extern float nearest_pixel_x(float x);
    extern float nearest_pixel_y(float y);

				// Round offsets to nearest pixel
    rxoff = Pad_renderer->Nearest_pixel_x(xoff);
    ryoff = Pad_renderer->Nearest_pixel_y(yoff);

    space_width = Pad_LineFont::letters[' ']->width;

    for (c=*string; *string != 0; c=*++string) {
	data = Pad_LineFont::letters[c];
	segments = data->segments;
	segment_length = data->segment_length;
	point = data->point;
	if (c == '\t') {
	    xoff += (8 - (tabpos % 8)) * space_width;
	    rxoff = Pad_renderer->Nearest_pixel_x(xoff);
	    tabpos = 0;
	    continue;
	}
	tabpos++;
	if ((c != ' ') &&
	    (c != '\t')) {
	    index = 0;
	    for (i=0; i<segments; i++) {
		Pad_renderer->Begin_line();
		for (j=0; j<segment_length[i]; j++) {
		    Pad_renderer->V2f(point[index].x + xoff, point[index].y + yoff);
		    index++;
		}
		Pad_renderer->End_line();
	    }
	}
	xoff += data->width;
	rxoff = Pad_renderer->Nearest_pixel_x(xoff);
    }
}

//
// Rotate text by angle theta and
// Render text at maximum resolution
//
void
Pad_LineFont::DrawString_2(char *string, float xoff, float yoff, float theta)
{
    int i, j, index;
    int segments;
    int *segment_length;
    Pad_Point *point;
    Pad_LineFontChar *data;
    unsigned char c;
    float rxoff, ryoff;
    int tabpos = 0;
    float space_width;
    Pad_Point tmppt;
    Pad_Point center; // center of rotation by theta
    float thetaRad;

    if (!string)
      return;

    thetaRad = DEG2RAD(theta);
    center.x = 0.0;
    center.y = 0.0;

    extern float nearest_pixel_x(float x);
    extern float nearest_pixel_y(float y);

				// Round offsets to nearest pixel
    rxoff = Pad_renderer->Nearest_pixel_x(xoff);
    ryoff = Pad_renderer->Nearest_pixel_y(yoff);

    space_width = Pad_LineFont::letters[' ']->width;


    for (c=*string; *string != 0; c=*++string) {
	data = Pad_LineFont::letters[c];
	segments = data->segments;
	segment_length = data->segment_length;
	point = data->point;
	if (c == '\t') {
	    xoff += (8 - (tabpos % 8)) * space_width;
	    rxoff = Pad_renderer->Nearest_pixel_x(xoff);
	    tabpos = 0;
	    continue;
	}
	tabpos++;
	if ((c != ' ') &&
	    (c != '\t')) {
	    index = 0;
	    for (i=0; i<segments; i++) {
		Pad_renderer->Begin_line();
		for (j=0; j<segment_length[i]; j++) { // Rotate each point defining the		   		                   
		    tmppt.x = point[index].x + xoff;  // character by theta  i.e. rotate
		    tmppt.y = point[index].y + yoff;  // each character by theta	    
		    tmppt.Rotate(theta, center);
		    Pad_renderer->V2f(tmppt.x, tmppt.y);
		    index++;
		}
		Pad_renderer->End_line();
	    }
	}
	xoff += data->width;
	rxoff = Pad_renderer->Nearest_pixel_x(xoff);
    }
}

//
// Render text at medium resolution
// (i.e., draw a short vertical line for each character
//
void
Pad_LineFont::DrawString_1(char *string, float xoff, float yoff)
{
    int tabpos = 0;
    unsigned char c;
    float vec1[3], vec2[3];
    float space_width, char_width;

    if (!string)
      return;

    space_width = Pad_LineFont::letters[' ']->width;

    vec1[1] = 0.0 + yoff;
    vec2[1] = 0.5 + yoff;
    vec1[2] = 0.0;
    vec2[2] = 0.0;
    for (c=*string; *string != 0; c=*++string) {
	if (c == '\t') {
	    xoff += (8 - (tabpos % 8)) * space_width;
	    tabpos = 0;
	    continue;
	}
	tabpos++;
	char_width = Pad_LineFont::letters[c]->width;
	if ((c != ' ') &&
	    (c != '\t')) {
	    Pad_renderer->Begin_line();
	    Pad_renderer->V2f(xoff + 0.5*char_width, yoff);
	    Pad_renderer->V2f(xoff + 0.5*char_width, 0.5 + yoff);
	    Pad_renderer->End_line();
	}
	xoff += char_width;
    }
}

//
// Render rotated text at medium resolution
// (i.e., draw a short vertical line for each character
//
void
Pad_LineFont::DrawString_1 (char *string, float xoff, float yoff, float theta)
{
    int tabpos = 0;
    unsigned char c;
    float vec1[3], vec2[3];
    float space_width, char_width;
    Pad_Point tmppt1, tmppt2;
    Pad_Point center;
    float thetaRad;

    if (!string)
      return;

    thetaRad = DEG2RAD(theta);
    center.x = 0.0;
    center.y = 0.0;

    space_width = Pad_LineFont::letters[' ']->width;

    vec1[1] = 0.0 + yoff;
    vec2[1] = 0.5 + yoff;
    vec1[2] = 0.0;
    vec2[2] = 0.0;
    for (c=*string; *string != 0; c=*++string) {
	if (c == '\t') {
	    xoff += (8 - (tabpos % 8)) * space_width;
	    tabpos = 0;
	    continue;
	}
	tabpos++;
	char_width = Pad_LineFont::letters[c]->width;
	if ((c != ' ') &&
	    (c != '\t')) {
	    Pad_renderer->Begin_line();

	    tmppt1.x = xoff + 0.5*char_width;  
	    tmppt1.y = yoff;
  	    tmppt2.x = xoff + 0.5*char_width;
	    tmppt2.y = 0.5 + yoff;
	    tmppt1.Rotate(theta, center);
	    tmppt2.Rotate(theta, center);

	    Pad_renderer->V2f(tmppt1.x, tmppt1.y);
	    Pad_renderer->V2f(tmppt2.x, tmppt2.y);

	    Pad_renderer->End_line();
	}
	xoff += char_width;
    }
}

//
// Render text at low resolution
// (i.e., draw a horizontal line for the string)
//
void
Pad_LineFont::DrawString_0(char *string, float xoff, float yoff)
{
    int tabpos = 0;
    int nws_flag = 0;
    unsigned char c;
    float vec1[3], vec2[3];
    float space_width, char_width;

    if (!string)
      return;

    space_width = Pad_LineFont::letters[' ']->width;

    vec1[1] = 0.25 + yoff;
    vec2[1] = 0.25 + yoff;
    vec1[2] = 0.0;
    vec2[2] = 0.0;
    for (c=*string; *string != 0; c=*++string) {
	if (c == '\t') {
	    xoff += (8 - (tabpos % 8)) * space_width;
	    tabpos = 0;
	    continue;
	}
	tabpos++;
	char_width = Pad_LineFont::letters[c]->width;
	vec1[0] = xoff + 0.5*char_width;
	if ((c != ' ') &&
	    (c != '\t')) {
	    nws_flag = 1;
	    break;
	}
	xoff += char_width;
    }
    for (c=*string; *string != 0; c=*++string) {
	char_width = Pad_LineFont::letters[c]->width;
	if ((c != ' ') &&
	    (c != '\t')) {
	    vec2[0] = xoff + 0.5*char_width;
	}
	xoff += char_width;
    }
    if (nws_flag) {
	Pad_renderer->Begin_line();
	Pad_renderer->V2f(vec1);
	Pad_renderer->V2f(vec2);
	Pad_renderer->End_line();
    }
}

float
Pad_LineFont::LineHeight(void) 
{
    return(1.75);
}
float 
Pad_LineFont::CharHeight(void)
{
    return(1.1);
}
float 
Pad_LineFont::DescenderHeight(void) 
{
    return(0.35);
}
float
Pad_LineFont::CharWidth(unsigned char c)
{
    return(Pad_LineFont::letters[c]->width);
}

//
// Find the length of this string, expanding tabs.
//
float
Pad_LineFont::StringWidth(char *string)
{
    int tabpos = 0;
    unsigned char c;
    float w;
    float space_width, char_width;
    
    w = 0.0;
    if (string) {
	space_width = Pad_LineFont::letters[' ']->width;
	for (c=*string; *string != '\0'; c=*++string) {
	    char_width = Pad_LineFont::letters[c]->width;
	    if (c == '\t') {
		w += (8 - (tabpos % 8)) * space_width;
		tabpos = 0;
	    } else {
		w += char_width;
		tabpos++;
	    }
	}
    }
    return(w);
}

