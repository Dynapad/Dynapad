#include "server.h"
#include <time.h>
#include <netdb.h>

int
get_image (url, image)
char *url;
Image *image;         /* non-NULL to store uncompressed image   */
{
  int proxysocket, rc;
  static char buf[BUFSIZE];
  FILE *proxy_stream;
  
  if (!strcmp(url, "-"))
    proxy_stream = stdin;
  else if ((proxy_stream = fopen(url, "r")) == NULL) {
    fprintf(stderr, "%s: %s\n", strerror(errno), url);
    return 0;
  }
  fprintf(stderr, "get_image: %s\n", url);
  if(image){  /* read image into structure */
    if(!check_and_set_image_type(image, url)){
      while(fread(buf, sizeof(char), BUFSIZE, proxy_stream));    /* read off data */
      fprintf(stderr, "get_image: unsupported image format\n");
      fclose(proxy_stream);
      close(proxysocket);
      return 0;
    }
    if(!read_and_decompress_image(image, proxy_stream)){
      fprintf(stderr, "get_image: can't decompress image\n");
      fclose(proxy_stream);
      close(proxysocket);
      return 0;
    }
    fill_image_structure(image);
  }
  fclose(proxy_stream);
  close(proxysocket);
  return 1;
}

int
check_and_set_image_type (image, url)
Image *image;
char *url;
{
  if(image->mime_type[0] == '\0'){
    if(ENDS_WITH(url, ".gif") || ENDS_WITH(url, ".GIF")){
      image->type = IMAGE_FORMAT_GIF;
      fprintf(stderr, "content type of image/gif is supported\n");
      return 1;
    }
    else if(ENDS_WITH(url, ".jpg") || ENDS_WITH(url, ".JPG") ||
	    ENDS_WITH(url, ".jpeg") || ENDS_WITH(url, ".JPEG")){
      image->type = IMAGE_FORMAT_JPG;
      fprintf(stderr, "content type of image/jpeg is supported\n");
      return 1;
    }
  }
  if(STARTS_WITH(image->mime_type, "image/gif")){
    image->type = IMAGE_FORMAT_GIF;
    fprintf(stderr, "content type of image/gif is supported\n");
    return 1;
  }
  if(STARTS_WITH(image->mime_type, "image/jpeg")){
    image->type = IMAGE_FORMAT_JPG;
    fprintf(stderr, "content type of image/jpeg is supported\n");
    return 1;
  }
  return 0;
}

Image *
read_and_decompress_image_file (file_name)
char *file_name;
{
  Image *image;
  FILE *fd = fopen(file_name, "r");
  if(fd == NULL)
    return NULL;
  image = (Image *)malloc(sizeof(Image));
  if(!image){
    fprintf(stderr, "read_and_decompress_image_file: can't malloc Image\n");
    fclose(fd);
    return NULL;
  }
  bzero(image, sizeof(Image));
  image->content_length = -1;
  strncpy(image->mime_type, "image/jpeg", MIME_TYPE_LENGTH);
  image->type = IMAGE_FORMAT_JPG;
  if(read_and_decompress_image(image, fd)){
    fclose(fd);
    return image;
  }
  fprintf(stderr, "read_and_decompress_image_file: can't read image\n");
  fclose(fd);
  free(image);
  return NULL;
}

int
read_and_decompress_image (image, proxy_stream)
Image *image;
FILE *proxy_stream;
{
  int rc;

  if(image->type == IMAGE_FORMAT_GIF){
    image->gdp = gdImageCreateFromGif(proxy_stream);
    if(!image->gdp){
      fprintf(stderr, "can't create GIF from data\n");
      return 0;
    }
  }
  else if(image->type == IMAGE_FORMAT_JPG){
    image->jpg_in = read_jpg_header(proxy_stream);
    if(!image->jpg_in){
      fprintf(stderr, "can't malloc jpg decompression structure\n");
      return 0;
    }
    image->bytes_per_line = image->jpg_in->num_components * image->jpg_in->image_width;
    image->jpg_image_data = (unsigned char *)malloc(image->bytes_per_line * image->jpg_in->image_height);
    if(!image->jpg_image_data){
      fprintf(stderr, "can't malloc jpg image data\n");
      jpeg_destroy_decompress(image->jpg_in);
      free(image->jpg_in);
      image->jpg_in = 0;
      return 0;
    }
    if(!read_jpg_image(proxy_stream, image->jpg_in, image->jpg_image_data,
		       image->bytes_per_line)){
      fprintf(stderr, "can't read jpg image!\n");
      jpeg_destroy_decompress(image->jpg_in);
      free(image->jpg_in);
      image->jpg_in = 0;
      return 0;
    }
  }
  fill_image_structure(image);
  return 1;
}

/*
  read off and ignore image data without decompressing it
*/
int
get_image_header_read_off_data (image, proxy_stream)
Image *image;
FILE *proxy_stream;
{
  char buf[BUFSIZE];

  if(image->type == IMAGE_FORMAT_GIF){
    image->gdp = gdImageHeaderFromGif(proxy_stream);
    while(fread(buf, sizeof(char), BUFSIZE, proxy_stream));
    if(!image->gdp){
      fprintf(stderr, "error reading gif image header\n");
      return 0;
    }
  }
  else if(image->type == IMAGE_FORMAT_JPG){
    image->jpg_in = read_jpg_header(proxy_stream);
    while(fread(buf, sizeof(char), BUFSIZE, proxy_stream));
    if(!image->jpg_in){
      fprintf(stderr, "error reading jpg image header\n");
      return 0;
    }
  }
  fill_image_structure(image);
  return 1;
}

fill_image_structure (image)
Image *image;
{
  if(image->type == IMAGE_FORMAT_GIF){
    image->width = image->gdp->sx;
    image->height = image->gdp->sy;
    image->total_components = 1;
    image->in_color_space = GIF_INDEXED;
    image->out_color_space = GIF_INDEXED;
    image->total_colors = image->gdp->colorsTotal;
    image->transparent_color = image->gdp->transparent;
    image->interlace = image->gdp->interlace;
  }
  else if(image->type == IMAGE_FORMAT_JPG){
    if(image->jpg_in){
      image->width = image->jpg_in->image_width;
      image->height = image->jpg_in->image_height;
      image->total_components = image->jpg_in->num_components;
      image->in_color_space = image->jpg_in->jpeg_color_space;
      image->out_color_space = image->jpg_in->out_color_space;
      image->interlace = image->jpg_in->buffered_image;
    }
    else{
      /* be sure to set width and height in caller */
      image->total_components = 3;
      image->in_color_space = JCS_RGB;
      image->out_color_space = JCS_RGB;
      image->interlace = -1;
    }
    switch(image->in_color_space){
    case JCS_UNKNOWN:
      image->total_colors = -1;
      break;
    case JCS_GRAYSCALE:
      image->total_colors = MAX_GREYS;
      break;
    case JCS_RGB:
    case JCS_YCbCr:
    case JCS_CMYK:
    case JCS_YCCK:
      image->total_colors = MAX_COLORS;
      break;
    }
    image->transparent_color = -1;
  }
}

/*
 ctime returns a string that already ends in a newline char
*/
char *
time_string ()
{
  time_t clock;

  time(&clock);
  return ctime(&clock);
}

/*
  sample header:

  HTTP/1.0 200 OK 
  Date: Tue, 23 Sep 1997 18:47:51 GMT
  Server: Apache/1.1.3
  Content-type: image/gif
  Content-length: 12905
  Last-modified: Thu, 05 Sep 1996 19:29:37 GMT
*/
send_image_header (fp, length, type, url)
FILE *fp;
int length;
char *type, *url;
{
  char* time = time_string();

  fprintf(fp, "HTTP/1.0 200 OK\n");
  fprintf(fp, "Date: %s", time);
  fprintf(fp, "Server: %s\n", IMAGE_SERVER_VERSION);
  fprintf(fp, "Content-Type: %s\n", type);
  fprintf(fp, "Location: %s\n", url);
  if(length != -1)
    fprintf(fp, "Content-length: %d\n", length);
  fprintf(fp, "Last-modified: %s\n", time); /* this prints two newline chars */
}

/*
  return 1 on success, 0 on failure
*/
verify_directory (dir_name)
char *dir_name;
{
  struct stat buf;
  int rc;

  rc = stat(dir_name, &buf);
  if(rc == 0){
    fprintf(stderr, "verify_directory: dir exists: %s\n", dir_name);
    return 1;
  }
  fprintf(stderr, "verify_directory: can't stat dir: %s\n", dir_name);
  rc = mkdir(dir_name, 0755);
  if(rc == -1){
    fprintf(stderr, "verify_directory: can't mkdir: %s\n", dir_name);
    return 0;
  }
  return 1;
}

get_thumb_dimensions (w, h, new_w, new_h, max_size)
int w, h, *new_w, *new_h, max_size;
{
  if(w < h){    /* h/max_size == w/new_w; */
    *new_h = max_size;
    *new_w = max_size * w / h;
  }
  else{    /* w/max_size == h/new_h; */
    *new_w = max_size;
    *new_h = max_size * h / w;
  }
  if(*new_w == 0)
    *new_w = 1;
  if(*new_h == 0)
    *new_h = 1;
  fprintf(stderr, "get_thumb_dimensions: %d x %d\n", *new_w, *new_h);
}

int
write_image_to_disk (image, file_name)
Image *image;
char *file_name;
{
  FILE *diskfile_stream;

  if (!strcmp(file_name, "-"))
    diskfile_stream = stdout;
  else if((diskfile_stream = fopen(file_name, "w")) == NULL){
    fprintf(stderr, "write_image_to_disk: can't fopen disk file: %s\n", file_name);
    return 0;
  }
  if(image->gdp)
    gdImageGif(image->gdp, diskfile_stream);
  else if(image->type == IMAGE_FORMAT_JPG)
    write_jpg(image->jpg_image_data, image->width, image->height,
	      image->total_components, image->out_color_space,
	      image->total_components * image->width, diskfile_stream);
  fclose(diskfile_stream);
  fprintf(stderr, "image successfully written to disk.\n");
  return 1;
}

int
gif_to_jpg (image)
Image *image;
{
  int trans;
  unsigned char *cp;
  int x, y, c;

  cp = image->jpg_image_data = (unsigned char *)malloc(image->width * image->height * 3);
  if(!cp){
    fprintf(stderr, "gif_to_jpg: can't malloc jpg image data\n");
    gdImageDestroy(image->gdp);
    image->gdp = 0;
    return 0;
  }
  bzero(cp, image->width * image->height * 3);
  image->type = IMAGE_FORMAT_JPG;
  image->in_color_space = JCS_RGB;
  image->out_color_space = JCS_RGB;
  image->total_components = 3;
  trans = gdImageGetTransparent(image->gdp);
  if(trans >= 0){
    for(y = 0; y < image->height; y++){
      for(x = 0; x < image->width; x++){
	c = gdImageGetPixel(image->gdp, x, y);
	if(c == trans)
	  cp += 3;
	else{
	  *cp++ = (unsigned char)gdImageRed(image->gdp, c);
	  *cp++ = (unsigned char)gdImageGreen(image->gdp, c);
	  *cp++ = (unsigned char)gdImageBlue(image->gdp, c);
	}
      }
    }
  }
  else{
    for(y = 0; y < image->height; y++){
      for(x = 0; x < image->width; x++){
	c = gdImageGetPixel(image->gdp, x, y);
	*cp++ = (unsigned char)gdImageRed(image->gdp, c);
	*cp++ = (unsigned char)gdImageGreen(image->gdp, c);
	*cp++ = (unsigned char)gdImageBlue(image->gdp, c);
      }
    }
  }
  gdImageDestroy(image->gdp);
  image->gdp = 0;
  return 1;
}

jpg_tile (background, thumb)
Image *background, *thumb;
{
  int i, j;

  if(background->width < thumb->width ||
     background->height < thumb->height)
    return;
  for(j = 0; j < background->height; j += thumb->height)
    for(i = 0; i < background->width; i += thumb->width)
      jpg_blt(thumb, background, 0, 0, thumb->width, thumb->height, i, j);
}

/*
  just works for color jpg dst. 
  clips height and width on the fly.
*/
jpg_blt (src, dst, src_x, src_y, src_w, src_h, dst_x, dst_y)
Image *src, *dst;
int src_x, src_y, src_w, src_h, dst_x, dst_y;
{
  int i, j, k;
  int new_x, new_y;
  unsigned char *src_p, *dst_p;

  fprintf(stderr, "jpg_blt src=%d,%d,%d,%d dst=%d,%d\n",
	  src_x, src_y, src_w, src_h, dst_x, dst_y);
  if(dst->total_components == 1)
    return;
  /*
  for(j = src_y, new_y = dst_y; j < src_y + src_h; j++, new_y++){
  */
  for(j = 0; j < src_h; j++){
    if(dst_y + j >= dst->height)
      break;
    src_p = src->jpg_image_data + src->total_components *
      ((src_y + j) * src->width + src_x);
    dst_p = dst->jpg_image_data + dst->total_components *
      ((dst_y + j) * dst->width + dst_x);
    /*
    for(i = src_x, new_x = dst_x; i < src_x + src_w; i++, new_x++){
    */
    for(i = 0; i < src_w; i++){
      if(dst_x + i >= dst->width)
	break;
      if(src->total_components == 3)
	for(k = 0; k < dst->total_components; k++)
	  *dst_p++ = *src_p++;
      else if(src->total_components == 1){
	for(k = 0; k < dst->total_components; k++)
	  *dst_p++ = *src_p;
	src_p++;
      }
    }
  }
}

/*
  just works for color jpg backgrounds. 
  clips height and width on the fly.
*/
jpg_blt0 (background, thumb, src_x, src_y, x, y)
Image *background, *thumb;
int src_x, src_y, x, y;
{
  int i, j, k;
  int new_x, new_y;
  unsigned char *dst, *src;

  src = thumb->jpg_image_data;
  for(j = src_y, new_y = y; j < thumb->height; j++, new_y++){
    if(new_y >= background->height)
      break;
    src = thumb->jpg_image_data + j * thumb->width * thumb->total_components;
    dst = background->jpg_image_data +
      (y + j) * background->width * background->total_components +
      x * background->total_components;
    for(i = src_x, new_x = x; i < thumb->width; i++, new_x++){
      if(new_x >= background->width)
	break;
      if(thumb->total_components == 3)
	for(k = 0; k < background->total_components; k++)
	  *dst++ = *src++;
      else if(thumb->total_components == 1){
	for(k = 0; k < background->total_components; k++)
	  *dst++ = *src;
	src++;
      }
    }
  }
}

Image *
new_jpg_image (w, h)
int w, h;
{
  unsigned char *cp;
  Image *image = (Image *)malloc(sizeof(Image));
  if(!image)
    return 0;

  image->content_length = -1;
  strncpy(image->mime_type, "image/jpeg", MIME_TYPE_LENGTH);
  image->jpg_in = 0;
  image->gdp = 0;
  image->type = IMAGE_FORMAT_JPG;
  image->in_color_space = JCS_RGB;
  image->out_color_space = JCS_RGB;
  image->total_components = 3;
  image->width = w;
  image->height = h;
  image->total_colors = MAX_COLORS;
  cp = image->jpg_image_data = (unsigned char *)malloc(w * h * 3);
  if(!cp){
    fprintf(stderr, "jpg_cut: can't malloc jpg image data\n");
    free(image);
    return 0;
  }
  bzero(cp, w * h * 3);
  return image;
}

Image *
jpg_cut (background, x, y, w, h)
Image *background;
int x, y, w, h;
{
  unsigned char *cp;
  int c;

  Image *image = new_jpg_image(w, h);
  if(!image)
    return 0;
  jpg_blt(background, image, x, y, w, h, 0, 0);
  return image;
}

int
trim_tail (s)
char *s;
{
  char lastc = s[strlen(s) - 1];
  switch (lastc){
  case 10:
  case 12:
  case 13:
    s[strlen(s) - 1] = '\0';
    return 1;
  default:
    return 0;
  }
}

print_color_space (request_stream, prefix, color_space)
FILE *request_stream;
char *prefix;
int color_space;
{
  switch(color_space){
  case GIF_INDEXED:
    fprintf(request_stream, "%s: indexed\n", prefix);
    break;
  case JCS_UNKNOWN:
    fprintf(request_stream, "%s: unknown\n", prefix);
    break;
  case JCS_GRAYSCALE:
    fprintf(request_stream, "%s: greyscale\n", prefix);
    break;
  case JCS_RGB:
    fprintf(request_stream, "%s: rgb\n", prefix);
    break;
  case JCS_YCbCr:
    fprintf(request_stream, "%s: Y/Cb/Cr (yuv)\n", prefix);
    break;
  case JCS_CMYK:
    fprintf(request_stream, "%s: cmyk\n", prefix);
    break;
  case JCS_YCCK:
    fprintf(request_stream, "%s: Y/Cb/Cr/K\n", prefix);
    break;
  }
}

print_image_info (request_stream, image)
FILE *request_stream;
Image *image;
{
  fprintf(stderr, "print_image_info\n");
  fprintf(request_stream, "Content type: %s\n", image->mime_type);
  fprintf(request_stream, "Content length: %d\n", image->content_length);
  fprintf(request_stream, "width: %d\n", image->width);
  fprintf(request_stream, "height: %d\n", image->height);
  fprintf(request_stream, "total components: %d\n", image->total_components);
  print_color_space(request_stream, "input color space", image->in_color_space);
  print_color_space(request_stream, "output color space", image->out_color_space);
  fprintf(request_stream, "total colors: %d\n", image->total_colors);
  fprintf(request_stream, "transparent color: %d\n", image->transparent_color);
  if(image->interlace)
    fprintf(request_stream, "interlace: true\n");
  else
    fprintf(request_stream, "interlace: false\n");
}

trim_url (url)
char *url;
{
  char *cp;
  if(ENDS_WITH(url, ".html") ||
     ENDS_WITH(url, ".gif") ||
     ENDS_WITH(url, ".jpg") ||
     ENDS_WITH(url, ".jpeg") ||
     ENDS_WITH(url, ".GIF") ||
     ENDS_WITH(url, ".JPG") ||
     ENDS_WITH(url, ".JPEG"))
    return;
  cp = strchr(url, '?');
  if(cp != NULL)
    *cp = '\0';
  cp = strchr(url, ';');
  if(cp != NULL)
    *cp = '\0';
  cp = strchr(url, '#');
  if(cp != NULL)
    *cp = '\0';
}

/*
  read the data and copy it back
*/
copy_data (s, proxy)
int s, proxy;
{
  int rc, count = 0;
  char buffer[BIGBUFSIZE];

  for(;;){                           
    rc = read(proxy, buffer, BIGBUFSIZE);
    if(rc == -1){
      perror("copy_data: read");
      break;
    }
    if(rc == 0){
      fprintf(stderr, "end of file\n");
      break;
    }
    count += rc;
    if(write(s, buffer, rc) == -1){
      perror("copy_data: write");
      break;
    }
    fprintf(stderr, "%d bytes copied\n", rc);
  }
  fprintf(stderr, "total of %d bytes copied\n", count);
  close(proxy);
}

free_image_structures (image)
Image *image;
{
  fprintf(stderr, "free_image_structures\n");
  if(image->gdp){
    fprintf(stderr, "free_image_structures: freeing gdp\n");
    gdImageDestroy(image->gdp);
    image->gdp = 0;
  }
  if(image->jpg_in){
    fprintf(stderr, "free_image_structures: freeing jpg_in\n");
    if(image->jpg_in->mem == NULL)
      fprintf(stderr, "free_image_structures: jpg_in->mem already NULL!\n");
    jpeg_destroy_decompress(image->jpg_in);
    free(image->jpg_in);
    image->jpg_in = 0;
  }
  if(image->jpg_image_data){
    fprintf(stderr, "free_image_structures: freeing jpg_image_data\n");
    free(image->jpg_image_data);
    image->jpg_image_data = 0;
  }
}
