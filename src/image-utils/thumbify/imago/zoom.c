#include <stdio.h>
#include <string.h>
#include <malloc.h>
#include <math.h>

typedef	unsigned char Pixel;

typedef struct {
	int	xsize;		/* horizontal size of the image in Pixels */
	int	ysize;		/* vertical size of the image in Pixels */
	Pixel *	data;		/* pointer to first scanline of image */
	int	span;		/* byte offset between two scanlines */
} Image;

#define	WHITE_PIXEL	(255)
#define	BLACK_PIXEL	(0)

#define	Mitchell_support	(2.0)

#define	B	(1.0 / 3.0)
#define	C	(1.0 / 3.0)

double
Mitchell_filter(t)
double t;
{
	double tt;

	tt = t * t;
	if(t < 0) t = -t;
	if(t < 1.0) {
		t = (((12.0 - 9.0 * B - 6.0 * C) * (t * tt))
		   + ((-18.0 + 12.0 * B + 6.0 * C) * tt)
		   + (6.0 - 2 * B));
		return(t / 6.0);
	} else if(t < 2.0) {
		t = (((-1.0 * B - 6.0 * C) * (t * tt))
		   + ((6.0 * B + 30.0 * C) * tt)
		   + ((-12.0 * B - 48.0 * C) * t)
		   + (8.0 * B + 24 * C));
		return(t / 6.0);
	}
	return(0.0);
}

double
sinc(x)
double x;
{
	x *= M_PI;
	if(x != 0) return(sin(x) / x);
	return(1.0);
}

double
Lanczos3_filter(t)
double t;
{
	if(t < 0) t = -t;
	if(t < 3.0) return(sinc(t) * sinc(t/3.0));
	return(0.0);
}

typedef struct {
	int	pixel;
	double	weight;
} CONTRIB;

typedef struct {
	int	n;		/* number of contributors */
	CONTRIB	*p;		/* pointer to list of contributions */
} CLIST;

CLIST	*contrib;		/* array of contribution lists */

#define CLAMP(v,l,h)    ((v)<(l) ? (l) : (v) > (h) ? (h) : v)

void
zoom (src, dst, src_w, src_h, dst_w, dst_h, filter, fwidth, num_components)
float *src;
unsigned char *dst;
int src_w, src_h, dst_w, dst_h, num_components;
double (*filter)();			/* filter function */
double fwidth;				/* filter width (support) */
{
	float *tmp;			/* intermediate data */
	double xscale, yscale;		/* zoom scale factors */
	int i, j, k, c;			/* loop variables */
	int n;				/* pixel number */
	double center, left, right;	/* filter calculation variables */
	double width, fscale, weight;	/* filter calculation variables */
	/*float *raster;		/* a row or column of pixels */
	float *src_p, *tmp_p;
	unsigned char *dst_p;
	int floats_per_src_line = src_w * num_components;
	int floats_per_tmp_line = dst_w * num_components;
	int chars_per_dst_line = dst_w * num_components;

	/* create intermediate buffer to hold horizontal zoom */
	tmp = (float *)malloc(src_h * floats_per_tmp_line * sizeof(float));
	if(!tmp){
	  fprintf(stderr, "zoom: can't malloc buffer\n");
	  return;
	}
	xscale = (double) dst_w / (double) src_w;
	yscale = (double) dst_w / (double) src_w;

	fprintf(stderr, "zoom: calculating filter contributions for a row\n");
	/* pre-calculate filter contributions for a row */
	contrib = (CLIST *)calloc(dst_w, sizeof(CLIST));
	if(xscale < 1.0) {
		width = fwidth / xscale;
		fscale = 1.0 / xscale;
		for(i = 0; i < dst_w; ++i) {
			contrib[i].n = 0;
			contrib[i].p = (CONTRIB *)calloc((int) (width * 2 + 1),
					sizeof(CONTRIB));
			center = (double) i / xscale;
			left = ceil(center - width);
			right = floor(center + width);
			for(j = left; j <= right; ++j) {
				weight = center - (double) j;
				weight = (*filter)(weight / fscale) / fscale;
				if(j < 0) {
				  n = -j;
				  if(n >= src_w)
				    n = src_w - 1;
				} 
				else if(j >= src_w) {
				  n = (src_w - j) + src_w - 1;
				  if(n < 0)
				    n = 0;
				} else {
				  n = j;
				}
				k = contrib[i].n++;
				contrib[i].p[k].pixel = n * num_components;
				contrib[i].p[k].weight = weight;
			}
		}
	} else {
		for(i = 0; i < dst_w; ++i) {
			contrib[i].n = 0;
			contrib[i].p = (CONTRIB *)calloc((int) (fwidth * 2 + 1),
					sizeof(CONTRIB));
			center = (double) i / xscale;
			left = ceil(center - fwidth);
			right = floor(center + fwidth);
			for(j = left; j <= right; ++j) {
				weight = center - (double) j;
				weight = (*filter)(weight);
				if(j < 0) {
				  n = -j;
				  if(n >= src_w)
				    n = src_w - 1;
				} 
				else if(j >= src_w) {
				  n = (src_w - j) + src_w - 1;
				  if(n < 0)
				    n = 0;
				} else {
				  n = j;
				}
				k = contrib[i].n++;
				contrib[i].p[k].pixel = n * num_components;
				contrib[i].p[k].weight = weight;
			}
		}
	}
	fprintf(stderr, "zoom: applying filter to zoom horizontally from src to tmp\n");
	src_p = src;
	tmp_p = tmp;
	for(k = 0; k < src_h; ++k) {
	  for(i = 0; i < dst_w; ++i) {
	    for(c = 0; c < num_components; c++){
	      weight = 0.0;
	      for(j = 0; j < contrib[i].n; ++j)
		weight +=
		  src_p[contrib[i].p[j].pixel + c] * contrib[i].p[j].weight;
	      *tmp_p++ = CLAMP(weight, BLACK_PIXEL, WHITE_PIXEL);
	    }
	  }
	  src_p += floats_per_src_line;
	}

	/* free the memory allocated for horizontal filter weights */
	for(i = 0; i < dst_w; ++i)
	  free(contrib[i].p);
	free(contrib);

	fprintf(stderr, "zoom: calculating filter contributions for a column\n");
	/* pre-calculate filter contributions for a column */
	contrib = (CLIST *)calloc(dst_h, sizeof(CLIST));
	if(yscale < 1.0) {
		width = fwidth / yscale;
		fscale = 1.0 / yscale;
		for(i = 0; i < dst_h; ++i) {
			contrib[i].n = 0;
			contrib[i].p = (CONTRIB *)calloc((int) (width * 2 + 1),
					sizeof(CONTRIB));
			center = (double) i / yscale;
			left = ceil(center - width);
			right = floor(center + width);
			for(j = left; j <= right; ++j) {
				weight = center - (double) j;
				weight = (*filter)(weight / fscale) / fscale;
				if(j < 0) {
				  n = -j;
				  if(n >= src_h)
				    n = src_h - 1;
				} 
				else if(j >= src_h) {
				  n = (src_h - j) + src_h - 1;
				  if(n < 0)
				    n = 0;
				} 
				else {
				  n = j;
				}
				k = contrib[i].n++;
				contrib[i].p[k].pixel = n * floats_per_tmp_line;
				contrib[i].p[k].weight = weight;
			}
		}
	} else {
		for(i = 0; i < dst_h; ++i) {
			contrib[i].n = 0;
			contrib[i].p = (CONTRIB *)calloc((int) (fwidth * 2 + 1),
					sizeof(CONTRIB));
			center = (double) i / yscale;
			left = ceil(center - fwidth);
			right = floor(center + fwidth);
			for(j = left; j <= right; ++j) {
				weight = center - (double) j;
				weight = (*filter)(weight);
				if(j < 0) {
				  n = -j;
				  if(n >= src_h)
				    n = src_h - 1;
				} 
				else if(j >= src_h) {
				  n = (src_h - j) + src_h - 1;
				  if(n < 0)
				    n = 0;
				} else {
				  n = j;
				}
				k = contrib[i].n++;
				contrib[i].p[k].pixel = n * floats_per_tmp_line;
				contrib[i].p[k].weight = weight;
			}
		}
	}
	fprintf(stderr, "zoom: applying filter to zoom vertically from tmp to dst\n");
	tmp_p = tmp;
	for(k = 0; k < dst_w; ++k) {
	  for(i = 0; i < dst_h; ++i) {
	    dst_p = dst + i * chars_per_dst_line + k * num_components;
	    for(c = 0; c < num_components; c++){
	      weight = 0.0;
	      for(j = 0; j < contrib[i].n; ++j)
		weight +=
		  tmp_p[contrib[i].p[j].pixel + c] *
		  contrib[i].p[j].weight;
	      *dst_p++ = CLAMP(weight + 0.5, BLACK_PIXEL, WHITE_PIXEL);
	    }
	  }
	  tmp_p += num_components;
	}

	/* free the memory allocated for vertical filter weights */
	for(i = 0; i < dst_h; ++i) {
		free(contrib[i].p);
	}
	free(contrib);
	free(tmp);
}
