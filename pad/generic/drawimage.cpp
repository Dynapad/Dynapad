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


/* ---- X Image Display Routine -------------------------------------------

N.B. It is the callers responsibility to ensure that the coordinates passed to
this routine are 'sensible':

    0 <= src_left < src_right <= img_width
    0 <= src_top < src_bottom <= img_width
    dst_left < dst_right,
    dst_top < dst_bottom
*/

//
// This code inspired by David Fox's image "Zoom" library.
// By combining shared memory and an optimized inner loop,
// surprisingly fast image rendering is possible.
//
//   - Ben Bederson (7/9/95)
//


#include "defs.h"
#include "win.h"
#include "point.h"
#include "pad-string.h"
#include "display.h"
#include "renderer.h"
#include "object.h"
#include "image.h"
#include "global.h"
#include "colorcube.h"

#  include "../unix/shm.h"


// set by Init_dither to the pixels for the current color cube
static unsigned long *pixel_map;


// Drawing modes

                     // bit masks for drawing modes:
#define B8 0         // 8-bit
#define B32 1        // 32-bit
#define BANY 2       // other bit sizes
#define BMASK 4      // drawing with a mask
#define BSTIP 8      // drawing with a stipple
#define BDITH 16     // drawing using color dithering


#define NORMAL_8          B8
#define NORMAL_32         B32
#define NORMAL_ANY        BANY

#define MASK_8            BMASK | B8
#define MASK_32           BMASK | B32
#define MASK_ANY          BMASK | BANY

#define STIPPLE_8         BSTIP | B8
#define STIPPLE_32        BSTIP | B32
#define STIPPLE_ANY       BSTIP | BANY

#define MASK_STIPPLE_8    BSTIP | BMASK | B8
#define MASK_STIPPLE_32   BSTIP | BMASK | B32
#define MASK_STIPPLE_ANY  BSTIP | BMASK | BANY

// dithering only available in 8-bit mode
#define DITHER_8              BDITH | B8
#define DITHER_MASK_8         BDITH | BMASK | B8
#define DITHER_STIPPLE_8      BDITH | BSTIP | B8
#define DITHER_MASK_STIPPLE_8 BDITH | BMASK | BSTIP | B8


#define DITHER_LENGTH  34                // Number of elements in dither pattern
#define DITHER_SHIFT   13                // 3 over 5 tilt prevents flicker

#define DITHER_LOOP(x, p, b, tab, tabstart, tabend) \
          p = (Pixel)srclineptr[tmp_offsets[x]]; \
          b = (unsigned char)(tab[((p & 0xff0000) >> 16)] \
                              + tab[((p & 0x00ff00) >> 8) + 256] \
                              + tab[((p & 0x0000ff)) + 512]); \
          dstptr[x] = (char)pixel_map[b]; \
          tab += 768; \
          if (tab == tabend) { \
              tab = tabstart;  \
          } \

#define STIP_DITHER_LOOP(x, p, b, tab, tabstart, tabend) \
          if (pat[x]) { \
              p = (Pixel)srclineptr[tmp_offsets[x]]; \
              b = (unsigned char)(tab[((p & 0xff0000) >> 16)] \
                              + tab[((p & 0x00ff00) >> 8) + 256] \
                              + tab[((p & 0x0000ff)) + 512]); \
              dstptr[x] = (char)pixel_map[b]; \
            } else { \
                dstptr[x] = 0; \
            } \
          tab += 768; \
          if (tab == tabend) { \
              tab = tabstart;  \
          } \

#define MASK_DITHER_LOOP(x, p, b, tab, tabstart, tabend) \
          p = (Pixel)srclineptr[tmp_offsets[x]]; \
          if (p & 0xff000000) { \
              dstptr[x] = 0; \
          } else { \
              b = (unsigned char)(tab[((p & 0xff0000) >> 16)] \
                              + tab[((p & 0x00ff00) >> 8) + 256] \
                              + tab[((p & 0x0000ff)) + 512]); \
              dstptr[x] = (char)pixel_map[b]; \
          } \
          tab += 768; \
          if (tab == tabend) { \
              tab = tabstart;  \
          } \

#define MASK_STIP_DITHER_LOOP(x, p, b, tab, tabstart, tabend) \
          if (pat[x]) { \
              p = (Pixel)srclineptr[tmp_offsets[x]]; \
              if (p & 0xff000000) { \
                  dstptr[x] = 0; \
              } else { \
                  b = (unsigned char)(tab[((p & 0xff0000) >> 16)] \
                              + tab[((p & 0x00ff00) >> 8) + 256] \
                              + tab[((p & 0x0000ff)) + 512]); \
                  dstptr[x] = (char)pixel_map[b]; \
              } \
            } else { \
                dstptr[x] = 0; \
            } \
          tab += 768; \
          if (tab == tabend) { \
              tab = tabstart;  \
          } \


        // win32 version, don't keep the pixel_map[b], which in win32, it is 
        // 32 bit RGBA value, but instead, keep the index which is b.




int dither[DITHER_LENGTH];
unsigned char lookup[DITHER_LENGTH][3][256];
int dither_pattern[DITHER_LENGTH] = {
                         12,
                29, 13, 30,  1,
            18,  5, 22, 15, 32,
         8, 25, 16, 33,  3, 20,  4,
            21, 14, 31,  6, 23,  2, 19,
             9, 26, 11, 28, 10, 27,
                 7, 24,  0,
                    17
};

//
// Init_dither
//
//   Initialize dither matrix for colorcube of specified
//   number of reds, greens, and blues.
//
static void
Init_dither(Pad_ColorCube *colorcube)
{
    int i;
    int reds, greens, blues;
    static unsigned long *old_pixel_map = NULL;
    static int old_reds = 0;

    pixel_map = colorcube->Get_device_pixel_map(reds, greens, blues);

    if (pixel_map == old_pixel_map && reds == old_reds)
      return;

    if (!pixel_map) {      // Sanity check
        cerr << "Init_dither: Attempt to use dithering on TrueColor visual" << endl;
        exit(1);
    }
                                // Scale the values in DITHER up to range from 0 to 255.
    for (i = 0; i < DITHER_LENGTH; i++) {
        dither[i] = dither_pattern[i] * 255 / (DITHER_LENGTH - 1);
    }

    for (i = 0; i < 256; i++) {
        for (int j = 0; j < DITHER_LENGTH; j++) {
            lookup[j][2][i] = blues * greens * ((i * (reds - 1) + dither[j]) >> 8);
                                // Anti correlate green to smooth luminance
            lookup[j][1][i] = blues * ((i * (greens - 1) + dither[DITHER_LENGTH - j - 1]) >> 8);
            lookup[j][0][i] = ((i * (blues - 1) + dither[j]) >> 8);
        }
    }

    old_pixel_map = pixel_map;
    old_reds = reds;
}


//
// Pad_DrawImage
//
//   Render an image onto the specified drawable with arbitrary scale.
//   The input image region is specified, and the output region is specified.
//   The renderer magnfies or shrinks the image as necessary.
//
//   If dithering is specified, then img_data must be an array of rgb.
//   If no dithering is specified, then img_data must be an array of X Pixels.
//
//   The pad window is used to hold a shared memory XImage if available.
//   If available, the rendering will be *much* (approximately 6 times) faster.
//
//   This code is complicated by the fact that there are several kinds of
//   image render - with and without dithering, with and without a mask,
//   with and without a stipple
//
int
Pad_DrawImage(Display *display, Pad_Win *win, Drawable dbl, GC gc,
              void *img_data, unsigned char *mask, 
              int img_width, int img_height,
              int src_left, int src_top, int src_right, int src_bot,
              int dst_left, int dst_top, int dst_right, int dst_bot, 
              Pad_Bool dither, float transparency)
{
    int x, y;
    int rc = 1;
    int src_width, src_height;
    int dst_width, dst_height;
    int lmt_dst_left, lmt_dst_top;
    int lmt_dst_right, lmt_dst_bot;
    int lmt_dst_width, lmt_dst_height;
    int img_widthm1 = img_width - 1;
    int *tmp_offsets;
    int *offsets;
    char *dstptr;
    float xfac, yfac;
    Pad_Stipple *stipple = NULL;
    Pixmap x_stipple;
    XImage *xim = win->ximage;    
    int depth = win->dpy->depth;

    unsigned long *xpixelmap;
    int padncolors;
        
    xpixelmap = NULL;
    padncolors = 0;

                                // Compute source and destination image dimensions
    src_width = src_right - src_left + 1;
    src_height = src_bot - src_top + 1;
    dst_width = dst_right - dst_left;
    dst_height = dst_bot - dst_top;
                                // Compute image's parameters limited by destination's region
                                // I.e., don't render outside of region, but allow partial
                                // rendering of large pixels
    lmt_dst_left  = MAX(dst_left, 0);
    lmt_dst_top   = MAX(dst_top, 0);
    lmt_dst_right = MIN(dst_right, win->width);
    lmt_dst_bot   = MIN(dst_bot, win->height);
    lmt_dst_width = MAX(MIN(lmt_dst_right - lmt_dst_left, win->width), 0);
    lmt_dst_height = MAX(MIN(lmt_dst_bot - lmt_dst_top, win->height), 0);

                                // Used to read input image
    unsigned char *srclineptr8;
    unsigned char *srcstartptr8;
    Pixel *srclineptr;
    Pixel *srcstartptr;

    if ((depth == 8) && !dither) {
        srcstartptr8 = (unsigned char *)img_data + (src_top * img_width);
    } else {
        srcstartptr = (Pixel *)img_data + (src_top * img_width);
    }

                                // Check for empty destination region
    if (dst_width <= 0 || dst_height <= 0) {
        return(1);
    }
                                // Check for illegal image src values
    if ((src_right - src_left + 1 > img_width) ||
        (src_bot - src_top + 1 > img_height)) {
        return(1);
    }

                                // Compute magnification factors
    xfac = (float)src_width / dst_width;
    yfac = (float)src_height / dst_height;

                                // Data for x-coord of src image indices
    offsets = new int[lmt_dst_width];

                                // Pre-compute x-coord of src image indices
    int dx = lmt_dst_left - dst_left;
    for (x = 0; x < lmt_dst_width; x++) {
        offsets[x] = ((int)((x + dx) * xfac) + src_left);
        if (offsets[x] > img_widthm1) {
            offsets[x] = img_widthm1;
        }
    }

                                // Fill in image.
                                // For each destination pixel, find the source pixel
                                // that matches most closely.
    int rolled_dst_width = MAX(0, lmt_dst_width - 7);
    int dy = lmt_dst_top - dst_top;
    Pixel p;
    unsigned char b;
    unsigned char *tabstart;
    unsigned char *tabend;
    unsigned char *tab;
    char *orig_dstptr;
    int stippat[STIP_SIZE][STIP_SIZE];

    if (transparency != 1.0) {
        int index = (int)(transparency * 16);
        stipple = &pad_stipples[index];
        x_stipple = Pad_prc->dpy->ditherStipples[index];
    }

    //
    // Set -mode- to the kind of image rendering being performed
    //
    int mode = 0;

    switch (xim->bits_per_pixel) {
      case 8:
        mode |= B8;
        break;
      case 32:
        mode |= B32;
        break;
      default:
        mode |= BANY; // unknown depth
        break;
    }        

    if (dither) {
        mode |= BDITH;
        Init_dither(Pad_prc->dpy->Get_colorcube());
    }
    if (stipple) {
        mode |= BSTIP;
    }
    if (mask) {
        mode |= BMASK;
    }

    //
    // SCALE AND RENDER STIPPLE (if there is no mask)
    //
    if (stipple) {
        // Store the stipple in stippat rotated so that the rows are
        // aligned correctly
        for (y = 0 ; y < STIP_SIZE; y++) {
            char *srcrow = stipple->pattern[y];
            int *dstrow = stippat[y];
            for (x = 0; x < STIP_SIZE; x++) {
                dstrow[x] = srcrow[(lmt_dst_left + x) & STIP_MASK]; 
            }
        }
        if (!mask) {
            XSetFillStyle(display, gc, FillStippled);
            XSetStipple(display, gc, x_stipple);
            XSetForeground(display, gc, 0);
            XFillRectangle(display, dbl, gc, lmt_dst_left, lmt_dst_top,
                       lmt_dst_width, lmt_dst_height);
            XSetFunction(display, gc, GXor);
        }
    }

    //
    // SCALE AND RENDER MASK (with stipple, if there is one)
    // 

    if (mask) {                        // Scale and render the mask
        unsigned char *maskstartptr = mask + (src_top * img_width);
        unsigned char *masklineptr;
        for (y = 0; y < lmt_dst_height; y++) {
            int *stiprow, *pat;
            dstptr = xim->data + (y * xim->bytes_per_line);
            masklineptr = maskstartptr + ((int)((y + dy) * yfac)) * img_width;
            tmp_offsets = offsets;
            if (stipple) {
                stiprow = stippat[(y + lmt_dst_top) & STIP_MASK];
            }
            switch (mode) {

              //
              // Scaling a mask (no stippling)
              //

              case MASK_ANY:                // generic scaling
                for (x = 0; x < lmt_dst_width; x++) {
                    XPutPixel(xim, x, y, masklineptr[offsets[x]] ? 0xffffffff : 0);
                }
                break;
                
                
              case MASK_8:
              case DITHER_MASK_8:       // scaling an 8-bit image
                // Unroll loop
                for (x = 0; x < rolled_dst_width; x+=8) {
                    dstptr[0] = (char)masklineptr[tmp_offsets[0]];
                    dstptr[1] = (char)masklineptr[tmp_offsets[1]];
                    dstptr[2] = (char)masklineptr[tmp_offsets[2]];
                    dstptr[3] = (char)masklineptr[tmp_offsets[3]];
                    dstptr[4] = (char)masklineptr[tmp_offsets[4]];
                    dstptr[5] = (char)masklineptr[tmp_offsets[5]];
                    dstptr[6] = (char)masklineptr[tmp_offsets[6]];
                    dstptr[7] = (char)masklineptr[tmp_offsets[7]];
                    dstptr += 8;
                    tmp_offsets += 8;
                }
                
                // Do bits of unrolled loop that didn't fit
                for (; x < lmt_dst_width; x++) {
                    *dstptr++ = (char)masklineptr[offsets[x]];
                }
                break;
                
              case MASK_32:             // scaling a mask on a 32-bit visual
                {
                    Pixel *dlongptr = (Pixel*)dstptr;
                    // Unroll loop
                    for (x = 0; x < rolled_dst_width; x+=8) {
                        dlongptr[0] = masklineptr[tmp_offsets[0]] ? 0xffffffff : 0;
                        dlongptr[1] = masklineptr[tmp_offsets[1]] ? 0xffffffff : 0;
                        dlongptr[2] = masklineptr[tmp_offsets[2]] ? 0xffffffff : 0;
                        dlongptr[3] = masklineptr[tmp_offsets[3]] ? 0xffffffff : 0;
                        dlongptr[4] = masklineptr[tmp_offsets[4]] ? 0xffffffff : 0;
                        dlongptr[5] = masklineptr[tmp_offsets[5]] ? 0xffffffff : 0;
                        dlongptr[6] = masklineptr[tmp_offsets[6]] ? 0xffffffff : 0;
                        dlongptr[7] = masklineptr[tmp_offsets[7]] ? 0xffffffff : 0;
                        dlongptr += 8;
                        tmp_offsets += 8;
                    }
                    
                    // Do bits of unrolled loop that didn't fit
                    for (; x < lmt_dst_width; x++) {
                        *dlongptr++ = masklineptr[offsets[x]] ? 0xffffffff : 0;
                    }
                    break;
                }

              //
              // Scaling a mask (with stippling)
              //

              case MASK_STIPPLE_ANY:         // generic case with stippling
                pat = stiprow;
                for (x = 0; x < lmt_dst_width; x++) {
                    XPutPixel(xim, x, y, (pat[x&7] ? 
                                          (masklineptr[offsets[x]] ? 0xffffffff : 0) 
                                          : 0xffffffff));
                }
                break;
                
                
              case MASK_STIPPLE_8:
              case DITHER_MASK_STIPPLE_8:      // scaling an 8-bit mask with stippling

#define DO_PIX(i) dstptr[i] = (pat[i] ? masklineptr[tmp_offsets[i]] : 0xff);

                pat = stiprow;
                for (x = 0; x < rolled_dst_width; x+=8) {
                    DO_PIX(0);
                    DO_PIX(1);
                    DO_PIX(2);
                    DO_PIX(3);
                    DO_PIX(4);
                    DO_PIX(5);
                    DO_PIX(6);
                    DO_PIX(7);
                    dstptr += 8;
                    tmp_offsets += 8;
                }
                // Do bits of unrolled loop that didn't fit
                pat = stiprow;
                for (; x < lmt_dst_width; x++) {
                    *dstptr++ = (*pat++ ? masklineptr[offsets[x]] : 0xff);
                }
                break;

#undef DO_PIX

              case MASK_STIPPLE_32:    // Scaling a 32 bit mask with stippling
                {

#define DO_PIX(i) dlongptr[i] = (!pat[i] || masklineptr[tmp_offsets[i]] ? 0xffffffff : 0);

                    Pixel *dlongptr = (Pixel*)dstptr;
                    pat = stiprow;

                    for (x = 0; x < rolled_dst_width; x+=8) {
                        DO_PIX(0);
                        DO_PIX(1);
                        DO_PIX(2);
                        DO_PIX(3);
                        DO_PIX(4);
                        DO_PIX(5);
                        DO_PIX(6);
                        DO_PIX(7);
                        dlongptr += 8;
                        tmp_offsets += 8;
                    }
                    
                    // Do bits of unrolled loop that didn't fit
                    pat = stiprow;
                    for (; x < lmt_dst_width; x++) {
                        *dlongptr++ = (!(*pat++) || masklineptr[offsets[x]] ? 
                                       0xffffffff : 0);
                    }
                    break;                    
#undef DO_PIX
                }
                
            }
        }

            // Now render the mask        
        XSetFunction(display, gc, GXand);

        if (win->sharedMemory) {
#ifdef XSHM
            ShmPutImage(display, dbl, gc, xim,
                        0, 0, lmt_dst_left, lmt_dst_top,
                        lmt_dst_width, lmt_dst_height,
                        SHM_PUT_COMPLETION_WAIT);
#endif
        } else {
            XPutImage(display, dbl, gc, xim,
                      0, 0, lmt_dst_left, lmt_dst_top, 
                      lmt_dst_width, lmt_dst_height);
        }        

        XSetFunction(display, gc, GXor);
    }

    // 
    // RENDER IMAGE
    // 
    for (y = 0; y < lmt_dst_height; y++) {
        int *stiprow, *pat;
        dstptr = xim->data + (y * xim->bytes_per_line);
        Pixel *dlongptr = (Pixel*)dstptr;
        if ((depth == 8) && !dither) {
            srclineptr8 = srcstartptr8 + ((int)((y + dy) * yfac)) * img_width;
        } else {
            srclineptr = srcstartptr + ((int)((y + dy) * yfac)) * img_width;
        }
        tmp_offsets = offsets;
        if (stipple) {
            stiprow = stippat[(y + lmt_dst_top) & STIP_MASK];
        }

        switch (mode) {
          case NORMAL_ANY:
          case MASK_ANY:
            // scale using generic XPutPixel approach
            for (x = 0; x < lmt_dst_width; x++) {
              XPutPixel(xim, x, y, srclineptr[offsets[x]]);
            }
            break;

          case NORMAL_8:
          case MASK_8:
            // fast version for 8-bit images 

            // Unroll loop
            for (x = 0; x < rolled_dst_width; x+=8) {
              dstptr[0] = (char)srclineptr8[tmp_offsets[0]];
              dstptr[1] = (char)srclineptr8[tmp_offsets[1]];
              dstptr[2] = (char)srclineptr8[tmp_offsets[2]];
              dstptr[3] = (char)srclineptr8[tmp_offsets[3]];
              dstptr[4] = (char)srclineptr8[tmp_offsets[4]];
              dstptr[5] = (char)srclineptr8[tmp_offsets[5]];
              dstptr[6] = (char)srclineptr8[tmp_offsets[6]];
              dstptr[7] = (char)srclineptr8[tmp_offsets[7]];
              dstptr += 8;
              tmp_offsets += 8;
            }
            // Do bits of unrolled loop that didn't fit
            for (; x < lmt_dst_width; x++) {
              *dstptr++ = (char)srclineptr8[offsets[x]];
            }
            break;

          case NORMAL_32:
          case MASK_32:
            // fast version for 32-bit images 
            for (x = 0; x < rolled_dst_width; x+=8) {
                dlongptr[0] = srclineptr[tmp_offsets[0]]; 
                dlongptr[1] = srclineptr[tmp_offsets[1]]; 
                dlongptr[2] = srclineptr[tmp_offsets[2]]; 
                dlongptr[3] = srclineptr[tmp_offsets[3]]; 
                dlongptr[4] = srclineptr[tmp_offsets[4]]; 
                dlongptr[5] = srclineptr[tmp_offsets[5]]; 
                dlongptr[6] = srclineptr[tmp_offsets[6]]; 
                dlongptr[7] = srclineptr[tmp_offsets[7]]; 
                dlongptr += 8;
                tmp_offsets += 8;
            }
              
            // Do bits of unrolled loop that didn't fit
            for (; x < lmt_dst_width; x++) {
                *dlongptr++ = srclineptr[offsets[x]];
            }
            break;

          case STIPPLE_ANY:
          case MASK_STIPPLE_ANY:            
            // Do bits of unrolled loop that didn't fit
            pat = stiprow;
            for (x = 0; x < lmt_dst_width; x++) {
              XPutPixel(xim, x, y, pat[x&7] ? srclineptr[offsets[x]] : 0);
            }
            break;


          case STIPPLE_8:
          case MASK_STIPPLE_8:            
            // Render stippled with (precomputed) closest available color
            // Unroll loop
            pat = stiprow;
            for (x = 0; x < rolled_dst_width; x+=8) {                      
              dstptr[0] = (pat[0] ? (char)srclineptr8[tmp_offsets[0]] : 0);
              dstptr[1] = (pat[1] ? (char)srclineptr8[tmp_offsets[1]] : 0);
              dstptr[2] = (pat[2] ? (char)srclineptr8[tmp_offsets[2]] : 0);
              dstptr[3] = (pat[3] ? (char)srclineptr8[tmp_offsets[3]] : 0);
              dstptr[4] = (pat[4] ? (char)srclineptr8[tmp_offsets[4]] : 0);
              dstptr[5] = (pat[5] ? (char)srclineptr8[tmp_offsets[5]] : 0);
              dstptr[6] = (pat[6] ? (char)srclineptr8[tmp_offsets[6]] : 0);
              dstptr[7] = (pat[7] ? (char)srclineptr8[tmp_offsets[7]] : 0);
              dstptr += 8;
              tmp_offsets += 8;
            }
            // Do bits of unrolled loop that didn't fit
            pat = stiprow;
            for (; x < lmt_dst_width; x++) {
              *dstptr++ = (*pat++ ? (char)srclineptr8[offsets[x]] : 0);
            }
            break;

          case STIPPLE_32:
          case MASK_STIPPLE_32:            
            // Unroll loop
            pat = stiprow;
            for (x = 0; x < rolled_dst_width; x+=8) {                      
                dlongptr[0] = (pat[0] ? srclineptr[tmp_offsets[0]] : 0);
                dlongptr[1] = (pat[1] ? srclineptr[tmp_offsets[1]] : 0);
                dlongptr[2] = (pat[2] ? srclineptr[tmp_offsets[2]] : 0);
                dlongptr[3] = (pat[3] ? srclineptr[tmp_offsets[3]] : 0);
                dlongptr[4] = (pat[4] ? srclineptr[tmp_offsets[4]] : 0);
                dlongptr[5] = (pat[5] ? srclineptr[tmp_offsets[5]] : 0);
                dlongptr[6] = (pat[6] ? srclineptr[tmp_offsets[6]] : 0);
                dlongptr[7] = (pat[7] ? srclineptr[tmp_offsets[7]] : 0);
                dlongptr += 8;
                tmp_offsets += 8;
            }
            // Do bits of unrolled loop that didn't fit
            pat = stiprow;
            for (; x < lmt_dst_width; x++) {
                *dlongptr++ = (*pat++ ? srclineptr[offsets[x]] : 0);
            }
            break;

          case DITHER_8:
            // Render dithered - no mask or stipple
            tabstart = lookup[0][0];
            tabend = lookup[DITHER_LENGTH][0];
            tab = lookup[(y * DITHER_SHIFT) % DITHER_LENGTH][0];
            orig_dstptr = dstptr;
            
            // Unroll loop
            for (x = 0; x < rolled_dst_width; x+=8) {
                DITHER_LOOP(0, p, b, tab, tabstart, tabend);
                DITHER_LOOP(1, p, b, tab, tabstart, tabend);
                DITHER_LOOP(2, p, b, tab, tabstart, tabend);
                DITHER_LOOP(3, p, b, tab, tabstart, tabend);
                DITHER_LOOP(4, p, b, tab, tabstart, tabend);
                DITHER_LOOP(5, p, b, tab, tabstart, tabend);
                DITHER_LOOP(6, p, b, tab, tabstart, tabend);
                DITHER_LOOP(7, p, b, tab, tabstart, tabend);
                dstptr += 8;
                tmp_offsets += 8;
            }
            // Do bits of unrolled loop that didn't fit
            dx = dstptr - orig_dstptr;
            for (; x < lmt_dst_width; x++) {
                DITHER_LOOP((x - dx), p, b, tab, tabstart, tabend);
            }
            break;

          case DITHER_STIPPLE_8:
            // Render dithered through a stipple
            tabstart = lookup[0][0];
            tabend = lookup[DITHER_LENGTH][0];
            tab = lookup[(y * DITHER_SHIFT) % DITHER_LENGTH][0];
            orig_dstptr = dstptr;
            
            // Unroll loop
            pat = stiprow;
            for (x = 0; x < rolled_dst_width; x+=8) {
                STIP_DITHER_LOOP(0, p, b, tab, tabstart, tabend);
                STIP_DITHER_LOOP(1, p, b, tab, tabstart, tabend);
                STIP_DITHER_LOOP(2, p, b, tab, tabstart, tabend);
                STIP_DITHER_LOOP(3, p, b, tab, tabstart, tabend);
                STIP_DITHER_LOOP(4, p, b, tab, tabstart, tabend);
                STIP_DITHER_LOOP(5, p, b, tab, tabstart, tabend);
                STIP_DITHER_LOOP(6, p, b, tab, tabstart, tabend);
                STIP_DITHER_LOOP(7, p, b, tab, tabstart, tabend);
                dstptr += 8;
                tmp_offsets += 8;
            }
            // Do bits of unrolled loop that didn't fit
            dx = dstptr - orig_dstptr;
            for (; x < lmt_dst_width; x++) {
                STIP_DITHER_LOOP((x - dx), p, b, tab, tabstart, tabend);
            }
            break;
            
          case DITHER_MASK_8:
            // Render dithered through a mask
            tabstart = lookup[0][0];
            tabend = lookup[DITHER_LENGTH][0];
            tab = lookup[(y * DITHER_SHIFT) % DITHER_LENGTH][0];
            orig_dstptr = dstptr;
            
            // Unroll loop
            for (x = 0; x < rolled_dst_width; x+=8) {
                MASK_DITHER_LOOP(0, p, b, tab, tabstart, tabend);
                MASK_DITHER_LOOP(1, p, b, tab, tabstart, tabend);
                MASK_DITHER_LOOP(2, p, b, tab, tabstart, tabend);
                MASK_DITHER_LOOP(3, p, b, tab, tabstart, tabend);
                MASK_DITHER_LOOP(4, p, b, tab, tabstart, tabend);
                MASK_DITHER_LOOP(5, p, b, tab, tabstart, tabend);
                MASK_DITHER_LOOP(6, p, b, tab, tabstart, tabend);
                MASK_DITHER_LOOP(7, p, b, tab, tabstart, tabend);
                dstptr += 8;
                tmp_offsets += 8;
            }
            // Do bits of unrolled loop that didn't fit
            dx = dstptr - orig_dstptr;
            for (; x < lmt_dst_width; x++) {
                MASK_DITHER_LOOP((x - dx), p, b, tab, tabstart, tabend);
            }
            break;

          case DITHER_MASK_STIPPLE_8:
            // Render dithered through a mask and a stipple
            tabstart = lookup[0][0];
            tabend = lookup[DITHER_LENGTH][0];
            tab = lookup[(y * DITHER_SHIFT) % DITHER_LENGTH][0];
            orig_dstptr = dstptr;
            
            // Unroll loop
            pat = stiprow;
            for (x = 0; x < rolled_dst_width; x+=8) {
                MASK_STIP_DITHER_LOOP(0, p, b, tab, tabstart, tabend);
                MASK_STIP_DITHER_LOOP(1, p, b, tab, tabstart, tabend);
                MASK_STIP_DITHER_LOOP(2, p, b, tab, tabstart, tabend);
                MASK_STIP_DITHER_LOOP(3, p, b, tab, tabstart, tabend);
                MASK_STIP_DITHER_LOOP(4, p, b, tab, tabstart, tabend);
                MASK_STIP_DITHER_LOOP(5, p, b, tab, tabstart, tabend);
                MASK_STIP_DITHER_LOOP(6, p, b, tab, tabstart, tabend);
                MASK_STIP_DITHER_LOOP(7, p, b, tab, tabstart, tabend);
                dstptr += 8;
                tmp_offsets += 8;
            }
            // Do bits of unrolled loop that didn't fit
            dx = dstptr - orig_dstptr;
            for (; x < lmt_dst_width; x++) {
                MASK_STIP_DITHER_LOOP((x - dx), p, b, tab, tabstart, tabend);
            }
            break;
        }
    }

    if (win->sharedMemory) {
#ifdef XSHM
        ShmPutImage(display, dbl, gc, xim,
                    0, 0, lmt_dst_left, lmt_dst_top,
                    lmt_dst_width, lmt_dst_height,
                    SHM_PUT_COMPLETION_WAIT);
#endif
    } else {
        XPutImage(display, dbl, gc, xim,
                  0, 0, lmt_dst_left, lmt_dst_top, lmt_dst_width, lmt_dst_height);
   }

    if (stipple || mask) {
        XSetFunction(display, gc, GXcopy);
        XSetFillStyle(display, gc, FillSolid);
    }

    delete [] offsets;
    
    return(rc);
}
