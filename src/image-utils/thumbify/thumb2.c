#include "server.h"
#include <time.h>
#include <netdb.h>
#include <math.h>

unsigned char *
new_jpg_thumb2 (image, thumb)
Image *image;
Thumb *thumb;
{
  unsigned char *jpg_thumb_data
    = (unsigned char *)malloc(thumb->w * thumb->h * image->total_components);
  if(!jpg_thumb_data)
    return NULL;
  switch(thumb->filter_type){
  case FILTER_NONE:
    fprintf(stderr, "no filter (uniform sampling)\n");
    jpg_copy_resized(jpg_thumb_data, image->jpg_image_data,
		     0, 0, 0, 0, thumb->w, thumb->h,
		     image->width, image->height, image->total_components);
    break;
  case FILTER_GAUSSIAN:
    fprintf(stderr, "Gaussian filter\n");
    if(image->width/2 < thumb->w || image->height/2 < thumb->h)
      jpg_copy_lanczos(jpg_thumb_data, image->jpg_image_data,
		       0, 0, 0, 0, thumb->w, thumb->h,
		       image->width, image->height, image->total_components);
    else
      jpg_copy_gaussian(jpg_thumb_data, image->jpg_image_data,
			0, 0, 0, 0, thumb->w, thumb->h,
			image->width, image->height, image->total_components);
    break;
  case FILTER_LANCZOS:
    fprintf(stderr, "Lanczos filter\n");
    jpg_copy_lanczos(jpg_thumb_data, image->jpg_image_data,
		     0, 0, 0, 0, thumb->w, thumb->h,
		     image->width, image->height, image->total_components);
    break;
  }
  return jpg_thumb_data;
}

gdImagePtr
new_gif_thumb (gdp, w, h, new_w, new_h)
gdImagePtr gdp;
int w, h, new_w, new_h;
{
  gdImagePtr new;
  int trans, new_trans;

  new = gdImageCreate(new_w, new_h);
  if(new == NULL)
    return NULL;

  /* copy transparent color if any */
  trans = gdImageGetTransparent(gdp);
  if(trans >= 0){
    new_trans = gdImageColorAllocate(new, 
      gdImageRed(gdp, trans),
      gdImageGreen(gdp, trans),
      gdImageBlue(gdp, trans));
    gdImageColorTransparent(new, new_trans);
  }

  /* copy interlace flag if set */
  if(gdImageGetInterlaced(gdp))
    gdImageInterlace(new, gdImageGetInterlaced(gdp)); 

  gdImageCopyResized(new, gdp, 0, 0, 0, 0, 
    new_w + 1, new_h + 1, w, h);

  return new;
}

int
compute_thumb2 (image, thumb)
Image *image;
Thumb *thumb;
{
  gdImagePtr gif_thumb;
  unsigned char *jpg_thumb_data = NULL;

  if(thumb->filter_type == FILTER_NONE && image->gdp){
    gif_thumb = new_gif_thumb(image->gdp, image->width, image->height,
			      thumb->w, thumb->h);
    gdImageDestroy(image->gdp);
    image->gdp = gif_thumb;
    if(!gif_thumb)
      return 0;
  }
  else{
    if(image->gdp) /* convert gif to jpg */
      gif_to_jpg(image);
    jpg_thumb_data = new_jpg_thumb2(image, thumb);
    free(image->jpg_image_data);
    image->jpg_image_data = jpg_thumb_data;
    if(!jpg_thumb_data)
      return 0;
  }
  image->width = thumb->w;
  image->height = thumb->h;
  image->content_length = -1;
  return 1;
}

make_thumb (url, thumb_name, image, thumb)
char *url, *thumb_name;
Image *image;
Thumb *thumb;
{
  int rc, free_image = 0;
  char buf[BUFSIZE];

  if(!thumb->max_size)
    thumb->max_size = DEFAULT_MAX_THUMB_SIZE;
  fprintf(stderr, "make_thumb: thumb_name: %s\n", thumb_name);
  /* try to get full-sized image */
  if(!get_image(url, image)){
    return 0;
  }
  get_thumb_dimensions(image->width, image->height, &thumb->w, &thumb->h,
		       thumb->max_size);
  if(image->width < thumb->src_min_width ||
     image->height < thumb->src_min_height ||
     image->total_colors < thumb->src_min_colors ||
     thumb->w < thumb->min_width ||
     thumb->h < thumb->min_height){
    fprintf(stderr,
	    "make_thumb: image filtered out: w: %d, h: %d, colors: %d, thumb_w: %d, thumb_h: %d\n",
	    image->width, image->height, image->total_colors, thumb->w, thumb->h);    
    if(free_image){
      free_image_structures(image);
      free(image);
    }
    return 0;
  }
  if(!compute_thumb2(image, thumb)){
    if(free_image){
      free_image_structures(image);
      free(image);
    }
    return 0;
  }
  write_image_to_disk(image, thumb_name);
  if(free_image){
    free_image_structures(image);
    free(image);
  }
  return 1;
}

get_thumb_dimensions2 (w, h, new_w, new_h, max_size)
int w, h, *new_w, *new_h, max_size;
{
  *new_w = w/2;
  *new_h = h/2;
  fprintf(stderr, "get_thumb_dimensions2: %d x %d\n", *new_w, *new_h);
}

stats (src, size)
unsigned char *src;
int size;
{
  int i;
  int max = *src;
  int min = *src;
  float mean = 0;
  double sum = *src++;

  for(i = 1; i < size; i++){
    max = MAX(max, *src);
    min = MIN(min, *src);
    sum += *src;
    src++;
  }
  mean = sum / size;
  fprintf(stderr, "stats: max: %d, min: %d, mean: %g\n", max, min, mean);
}

int
jpg_copy_lanczos (dst, src, dst_x, dst_y, src_x, src_y, dst_w, dst_h, src_w, src_h, num_components)
unsigned char *dst, *src;
int dst_x, dst_y, src_x, src_y, dst_w, dst_h, src_w, src_h, num_components;
{
  float *buffer = 0;
  float *buf_p;
  int i;
  unsigned char *src_p;

  /*stats(src, src_w * src_h * num_components);*/

  if(!buffer)
    buffer = (float *)malloc
      (sizeof(float) * src_w * src_h * num_components);
  if(!buffer)
    return 0;
  buf_p = buffer;
  src_p = src;
  for(i = 0; i < src_w * src_h * num_components; i++)
    *buf_p++ = *src_p++;
  zoom(buffer, dst, src_w, src_h, dst_w, dst_h,
       Lanczos3_filter, Lanczos3_support, num_components);

  /*stats(dst, dst_w * dst_h * num_components);*/
  if(buffer)
    free(buffer);
  return 0;
}

int
jpg_copy_gaussian (dst, src, dst_x, dst_y, src_x, src_y, dst_w, dst_h, src_w, src_h, num_components)
unsigned char *dst, *src;
int dst_x, dst_y, src_x, src_y, dst_w, dst_h, src_w, src_h, num_components;
{
  float *buffer = 0;
  float *buffer2 = 0;
  float *buf_p;
  int i = 0;
  int prev_w = src_w;
  int prev_h = src_h;
  int next_w = src_w;
  int next_h = src_h;
  int buffer_w, buffer_h;
  unsigned char *dst_p;
  int first_time = 1;

  /*stats(src, src_w * src_h * num_components);*/

  while(prev_w/2 >= dst_w && prev_h/2 >= dst_h){
    buffer_h = prev_h;
    buffer_w = prev_w/2;
    next_w = prev_w/2;
    next_h = prev_h/2;
    if(!buffer)
      buffer = (float *)malloc
	(sizeof(float) * buffer_w * buffer_h * num_components);
    if(!buffer)
      return 0;
    if(!buffer2)
      buffer2 = (float *)malloc
	(sizeof(float) * next_w * next_h * num_components);
    if(!buffer2)
      return 0;

    fprintf(stderr, 
	    "calling horizontal: prev_w: %d, prev_h: %d, num_components: %d, buffer_w: %d, buffer_h: %d\n", 
	    prev_w, prev_h, num_components, buffer_w, buffer_h);

    if(first_time){
      first_time = 0;
      horizontal1(src, buffer, prev_w, prev_h, num_components,
		  buffer_w, buffer_h);
    }
    else
      horizontal(buffer2, buffer, prev_w, prev_h, num_components,
		  buffer_w, buffer_h);
    fprintf(stderr, 
	    "calling vertical: buffer_w: %d, buffer_h: %d, num_components: %d, next_w: %d, next_h: %d\n", 
	    buffer_w, buffer_h, num_components, next_w, next_h);

    vertical(buffer, buffer2, buffer_w, buffer_h, num_components,
	     next_w, next_h);
    prev_w = next_w;
    prev_h = next_h;
  }
  if(next_w == dst_w && next_h == dst_h){
    buf_p = buffer2;
    dst_p = dst;
    for(i = 0; i < dst_w * dst_h * num_components; i++){
      *dst_p++ = *buf_p + 0.5;
      buf_p++;
    }
  }
  else{
    zoom(buffer2, dst, next_w, next_h, dst_w, dst_h,
	 Lanczos3_filter, Lanczos3_support, num_components);
  }

 /*
  stats(dst, dst_w * dst_h * num_components);
  */
  if(buffer)
    free(buffer);
  if(buffer2)
    free(buffer2);
  return 0;
}

vertical (src, dst, src_w, src_h, num_components, dst_w, dst_h)
float *dst, *src;
int src_w, src_h, num_components;
int dst_w, dst_h;
{
  int src_floats_per_line = src_w * num_components;
  int dst_floats_per_line = dst_w * num_components;
  int twice_src_floats_per_line = src_floats_per_line * 2;
  register float *src_p;
  register float *dst_p;
  int x, y, c;

  /* first row    */
  dst_p = dst;
  src_p = src;
  for(x = 0; x < src_w; x++){
    for(c = 0; c < num_components; c++){
      *dst_p++ = *src_p * 0.5 + src_p[src_floats_per_line] * 0.5;
      src_p++;
    }
  }
  /* normal case */
  dst_p = dst + dst_floats_per_line;
  for(y = 1; y <= src_h - 3; y+=2){
    src_p = src + y * src_floats_per_line;
    for(x = 0; x < src_w; x++){
      for(c = 0; c < num_components; c++){
	*dst_p++ = *src_p * 0.25 +
	  src_p[src_floats_per_line] * 0.5 +
	  src_p[twice_src_floats_per_line] * 0.25;
	src_p++;
      }
    }
  }
  if(!IS_EVEN(src_h)){
    /* last row */
    src_p = src + (src_h - 2) * src_floats_per_line;
    dst_p = dst + (dst_h - 1) * dst_floats_per_line;
    for(x = 0; x < src_w; x++){
      for(c = 0; c < num_components; c++){
	*dst_p++ = *src_p * 0.5 + src_p[src_floats_per_line] * 0.5;
	src_p++;
      }
    }
  }
}

horizontal (src, dst, src_w, src_h, num_components, dst_w, dst_h)
float *src, *dst;
int src_w, src_h, num_components, dst_w, dst_h;
{
  int src_floats_per_line = src_w * num_components;
  int dst_floats_per_line = dst_w * num_components;
  int twice_num_components = 2 * num_components;
  register float *src_p, *dst_p;
  register int x, y, c;

  /* 1st column      */
  for(y = 0; y < src_h; y++){
    src_p = src + y * src_floats_per_line;
    dst_p = dst + y * dst_floats_per_line;
    for(c = 0; c < num_components; c++){
      *dst_p++ = *src_p * 0.5 + src_p[num_components] * 0.5;
      src_p++;
    }
  }
  /* normal */
  for(y = 0; y < src_h; y++){
    /* the second src pixel */
    src_p = src + y * src_floats_per_line + num_components;
    /* the second dst pixel */
    dst_p = dst + y * dst_floats_per_line + num_components;
    for(x = 1; x <= src_w - 3; x+=2){
      for(c = 0; c < num_components; c++){
	*dst_p++ = *src_p * 0.25 +
	  src_p[num_components] * 0.5 +
	  src_p[twice_num_components] * 0.25;
	src_p++;
      }
      src_p += num_components;
    }
  }
  if(!IS_EVEN(src_w)){
    /* last column */
    for(y = 0; y < src_h; y++){
      src_p = src + (y + 1) * src_floats_per_line - twice_num_components;
      dst_p = dst + (y + 1) * dst_floats_per_line - num_components;
      for(c = 0; c < num_components; c++){
	*dst_p++ = *src_p * 0.5 + src_p[num_components] * 0.5;
	src_p++;
      }
    }
  }
}

horizontal1 (src, dst, src_w, src_h, num_components, dst_w, dst_h)
unsigned char *src;
float *dst;
int src_w, src_h, num_components;
int dst_w, dst_h;
{
  int src_bytes_per_line = src_w * num_components;
  int dst_floats_per_line = dst_w * num_components;
  int twice_num_components = 2 * num_components;
  register unsigned char *src_p;
  register float *dst_p;
  register int x, y, c;

  /* 1st column      */
  for(y = 0; y < src_h; y++){
    src_p = src + y * src_bytes_per_line;
    dst_p = dst + y * dst_floats_per_line;
    for(c = 0; c < num_components; c++){
      *dst_p++ = *src_p * 0.5 + src_p[num_components] * 0.5;
      src_p++;
    }
  }
  /* normal */
  for(y = 0; y < src_h; y++){
    /* the second src pixel */
    src_p = src + y * src_bytes_per_line + num_components;
    /* the second dst pixel */
    dst_p = dst + y * dst_floats_per_line + num_components;
    for(x = 1; x <= src_w - 3; x+=2){
      for(c = 0; c < num_components; c++){
	*dst_p++ = *src_p * 0.25 +
	  src_p[num_components] * 0.5 +
	  src_p[twice_num_components] * 0.25;
	src_p++;
      }
      src_p += num_components;
    }
  }
  if(!IS_EVEN(src_w)){
    /* last column */
    for(y = 0; y < src_h; y++){
      src_p = src + (y + 1) * src_bytes_per_line - twice_num_components;
      dst_p = dst + (y + 1) * dst_floats_per_line - num_components;
      for(c = 0; c < num_components; c++){
	*dst_p++ = *src_p * 0.5 + src_p[num_components] * 0.5;
	src_p++;
      }
    }
  }
}

get_mins (thumb, buf)
Thumb *thumb;
char *buf;
{
  if(STARTS_WITH(buf, "thumb_min_width=")){
    int rc = sscanf(buf, "%*[^=]=%d\n", &thumb->min_width);
    if(rc != 1){
      fprintf(stderr, "get_thumb_mins: can't parse line: %s\n", buf);
      return;
    }
    fprintf(stderr, "thumb_min_width: %d\n", thumb->min_width);
  }
  else if(STARTS_WITH(buf, "thumb_min_height=")){
    int rc = sscanf(buf, "%*[^=]=%d\n", &thumb->min_height);
    if(rc != 1){
      fprintf(stderr, "get_thumb_mins: can't parse line: %s\n", buf);
      return;
    }
    fprintf(stderr, "thumb_min_height: %d\n", thumb->min_height);
  }
  else if(STARTS_WITH(buf, "thumb_src_min_width=")){
    int rc = sscanf(buf, "%*[^=]=%d\n", &thumb->src_min_width);
    if(rc != 1){
      fprintf(stderr, "get_thumb_mins: can't parse line: %s\n", buf);
      return;
    }
    fprintf(stderr, "thumb_src_min_width: %d\n", thumb->src_min_width);
  }
  else if(STARTS_WITH(buf, "thumb_src_min_height=")){
    int rc = sscanf(buf, "%*[^=]=%d\n", &thumb->src_min_height);
    if(rc != 1){
      fprintf(stderr, "get_thumb_mins: can't parse line: %s\n", buf);
      return;
    }
    fprintf(stderr, "thumb_src_min_height: %d\n", thumb->src_min_height);
  }
  else if(STARTS_WITH(buf, "thumb_src_min_colors=")){
    int rc = sscanf(buf, "%*[^=]=%d\n", &thumb->src_min_colors);
    if(rc != 1){
      fprintf(stderr, "get_thumb_mins: can't parse line: %s\n", buf);
      return;
    }
    fprintf(stderr, "thumb_src_min_colors: %d\n", thumb->src_min_colors);
  }
}

parse_thumb_request (argv, thumb, image)
char **argv;
Thumb *thumb;
Image *image;
{
  char **pp;
  int cnt;
  char buf[BUFSIZE];
  char value[MEDIUMBUFSIZE];
  int rc, i;

  thumb->min_width = DEFAULT_THUMB_MIN_WIDTH;
  thumb->min_height = DEFAULT_THUMB_MIN_HEIGHT;
  thumb->src_min_width = DEFAULT_THUMB_MIN_WIDTH;
  thumb->src_min_height = DEFAULT_THUMB_MIN_HEIGHT;
  thumb->src_min_colors = DEFAULT_THUMB_MIN_COLORS;
  for (pp = argv, cnt = 0; *pp; pp++, cnt++) {
    strcpy(buf, *pp);
    if(STARTS_WITH(buf, "filter="))
      get_filter_type(thumb, buf);
    else if(STARTS_WITH(buf, "image_mime_"))
      get_image_mime_type(image, buf);
    else if(STARTS_WITH(buf, "thumb_min_") || STARTS_WITH(buf, "thumb_src_min"))
      get_mins(thumb, buf);
    else if(STARTS_WITH(buf, "thumb_max_size=")){
      rc = sscanf(buf, "%*[^=]=%d\n", &i);
      if(rc != 1){
	fprintf(stderr, "parse_thumb_request: can't parse line: %s\n", buf);
	fprintf(stderr, "parse_thumb_request: rc: %d\n", rc);
	return;
      }
      thumb->max_size = i;
    }
    /*
    else if(STARTS_WITH(buf, "x_src_offset:") ||
	    STARTS_WITH(buf, "y_src_offset:") ||
	    STARTS_WITH(buf, "dst_width:") ||
	    STARTS_WITH(buf, "dst_height:") ||
	    STARTS_WITH(buf, "crop:"))
    */
    else
      break;
  }
  return cnt;
}

get_filter_type (thumb, buf)
Thumb *thumb;
char *buf;
{
  char value[MEDIUMBUFSIZE];

  int rc = sscanf(buf, "%*[^=]=%s\n", value);
  if(rc != 1){
    fprintf(stderr, "get_filter_type: can't parse line: %s\n", buf);
    return;
  }
  /* sunOS sscanf leaves a trailing carriage-return! */
  if(isspace(value[strlen(value) - 1]))
    value[strlen(value) - 1] = '\0';
  fprintf(stderr, "get_filter_type: value: %s\n", value);
  
  if(!strcmp(value, "none"))
    thumb->filter_type = FILTER_NONE;
  else if(!strcmp(value, "gaussian"))
    thumb->filter_type = FILTER_GAUSSIAN;
  else if(!strcmp(value, "lanczos"))
    thumb->filter_type = FILTER_LANCZOS;
  else{
    thumb->filter_type = FILTER_NONE;
    fprintf(stderr, "get_filter_type: illegal filter: %s\n", value);
    fprintf(stderr, "  using default filter: none\n");
  }
}

get_image_mime_type (image, buf)
Image *image;
char *buf;
{
  char value[MEDIUMBUFSIZE];

  int rc = sscanf(buf, "%*[^=]=%s\n", value);
  if(rc != 1){
    fprintf(stderr, "get_image_mime_type: can't parse line: %s\n", buf);
    return;
  }
  /* sunOS sscanf leaves a trailing carriage-return! */
  if(isspace(value[strlen(value) - 1]))
    value[strlen(value) - 1] = '\0';
  fprintf(stderr, "get_image_mime_type: value: %s\n", value);
  
  if(!strcmp(value, "gif"))
    strcpy(image->mime_type, "image/gif");
  else if(!strcmp(value, "jpeg"))
    strcpy(image->mime_type, "image/jpeg");
  else{
    fprintf(stderr, "get_image_mime_type: illegal type: %s\n", value);
  }
}

