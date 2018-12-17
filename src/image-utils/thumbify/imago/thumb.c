#include "server.h"
#include <time.h>
#include <netdb.h>

/*
  get the image from the proxy server
  copy back the uncompressed bits
*/
void
copy_full_sized_image (request_fd, request_stream, url, no_cache)
int request_fd;
FILE *request_stream;
char *url;
int no_cache;
{
  int proxysocket;
  char buffer[BUFSIZE];

  if(no_cache)
    sprintf(buffer, "GET %s HTTP/1.0\nUser-Agent: Imago/0.0\nPragma: no-cache\n\n", url);
  else
    sprintf(buffer, "GET %s HTTP/1.0\nUser-Agent: Imago/0.0\n\n", url);
  proxysocket = make_request(buffer);
  if(proxysocket == -1){
    if(request_stream)
      http_error(request_stream, 500, url, NULL, "could not connect to proxy\n");
    return;
  }
  copy_data(request_fd, proxysocket); /* copy_data calls close(proxysocket);*/
}

/*
  parse version and return codes
  returns server response code, or 0 on error
*/
int
get_first_line_of_response_header (fp, first_line)
FILE *fp;
char **first_line;
{
  int rc;
  static char buf[BUFSIZE];
  char version[SMALLBUFSIZE], *tok;

  if(fgets(buf, BUFSIZE, fp) == NULL)
    return 0;
  tok = strtok(buf, " ");     /* get version   */
  strncpy(version, tok, SMALLBUFSIZE);
  
  tok = strtok(NULL, " ");    /* get rc code   */
  rc = atoi(tok);

  tok = strtok(NULL, "\n");   /* get rc string */
  while(trim_tail(tok));

  sprintf(buf, "%s %d %s\n", version, rc, tok);
  *first_line = buf;
  return rc;  
}

/*
  check for mime type and content length
  returns 1 on success, 0 on error
*/
int
get_rest_of_response_header (fp, image)
FILE *fp;
Image *image;
{
  char buf[BUFSIZE], *tok;

  for(;;){
    if(fgets(buf, BUFSIZE, fp) == NULL)
      return 0;
    if(buf[0] == 10 || buf[0] == 12 || buf[0] == 13)   /* end of response header */
      break;
    if((tok = strstr(buf, "Content-Type: ")) ||
       (tok = strstr(buf, "Content-type: "))){
      tok += 14;
      while(trim_tail(tok));
      strncpy(image->mime_type, tok, MIME_TYPE_LENGTH); /* copy mime type */
    }
    if((tok = strstr(buf, "Content-Length: ")) ||
       (tok = strstr(buf, "Content-length: "))){
      tok += 16;
      while(trim_tail(tok));
      image->content_length = atoi(tok);
      /*fprintf(stderr, "content_length = %d\n", image->content_length);*/
    }
  }
  return 1;
}

int
get_image_from_proxy (url, request_stream, error_stream, image)
char *url;
FILE *request_stream; /* non-NULL to send back compressed image */
FILE *error_stream;   /* non-NULL to send back errors           */
Image *image;         /* non-NULL to store uncompressed image   */
{
  int proxysocket, rc;
  static char buf[BUFSIZE];
  char *first_line;
  FILE *proxy_stream;
  
  sprintf(buf, "GET %s HTTP/1.0\nUser-Agent: Imago/0.0\n\n", url);

  if((proxysocket = make_request(buf)) == -1){
    if(error_stream)
      http_error(error_stream, 500, url, NULL, "could not connect to proxy server");
    return 0;
  }
  if((proxy_stream = fdopen(proxysocket, "r")) == NULL){
    if(error_stream)
      http_error(error_stream, 500, url, NULL, "could not fdopen proxy connection\n");
    close(proxysocket);
    return 0;
  }
  rc = get_first_line_of_response_header(proxy_stream, &first_line);
  if(!rc){
    if(error_stream)
      http_error(error_stream, 500, url, NULL,
		 "could not read first line of response header\n");
    fclose(proxy_stream);
    close(proxysocket);
    return 0;
  }
  if(rc != 200){
    if(error_stream){
      fwrite(first_line, sizeof(char), strlen(first_line), error_stream);
      buffered_copy_data(error_stream, proxy_stream);    /* copy error to error_stream */
    }
    fclose(proxy_stream);
    close(proxysocket);
    return 0;
  }
  fprintf(stderr, "get_image_from_proxy: image available from proxy: %s\n", url);
  if(request_stream){
    fwrite(first_line, sizeof(char), strlen(first_line), request_stream);
    /* add Location header */
    sprintf(buf, "Location: %s\n", url);
    fwrite(buf, sizeof(char), strlen(buf), request_stream);
    buffered_copy_data(request_stream, proxy_stream);    /* copy image to request_stream */
    fclose(proxy_stream);
    close(proxysocket);
    return 1;
  }
  if(image){  /* read image into structure */
    if(!get_rest_of_response_header(proxy_stream, image)){
      if(error_stream)
	http_error(error_stream, 500, url, NULL, "can't read rest of response header\n");
      else
	fprintf(stderr, "get_image_from_proxy: can't get rest of response header\n");
      fclose(proxy_stream);
      close(proxysocket);
      return 0;
    }
    if(!check_and_set_image_type(image, url)){
      while(fread(buf, sizeof(char), BUFSIZE, proxy_stream));    /* read off data */
      if(error_stream)
	http_error(error_stream, 500, url, NULL, "unsupported image format\n");
      else
	fprintf(stderr, "get_image_from_proxy: unsupported image format\n");
      fclose(proxy_stream);
      close(proxysocket);
      return 0;
    }
    if(!read_and_decompress_image(image, proxy_stream)){
      if(error_stream)
	http_error(error_stream, 500, url, NULL, "can't decompress image\n");
      else
	fprintf(stderr, "get_image_from_proxy: can't decompress image\n");
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

/*
  calling function should zero out the image structure:
    image->jpg_in = 0;
    image->gdp = 0;
  request_fd is closed in main
  proxy_stream is closed in caller
*/
int
get_image_header (request_stream, url, image, proxy_stream)
FILE *request_stream;
char *url;
Image *image;
FILE *proxy_stream;
{
  int rc;
  char *first_line;
  char buf[BUFSIZE];

  rc = get_first_line_of_response_header(proxy_stream, &first_line);
  if(!rc){
    if(request_stream)
      http_error(request_stream, 500, url, NULL,
		 "could not read first line of response header\n");
    return 0;
  }
  if(rc != 200){                /* response error */
    if(request_stream){
      /* copy response error message to requestor */
      fwrite(first_line, sizeof(char), strlen(first_line), request_stream);
      buffered_copy_data(request_stream, proxy_stream);
    }
    else{
      /* read off remainder of proxy error */
      while(fread(buf, sizeof(char), BUFSIZE, proxy_stream));
    }
    return 0;
  }
  if(!get_rest_of_response_header(proxy_stream, image)){
    if(request_stream)
      http_error(request_stream, 500, url, image->mime_type,
		 "could not read remainder of response header\n");
    return 0;
  }
  if(!check_and_set_image_type(image, url)){
    if(request_stream){
      /*
	unsupported mime type
	"HTTP/1.0 501 Not Implemented" written back to requestor of unsupported file types
      */
      http_error(request_stream, 501, url, image->mime_type, NULL);
    }
    while(fread(buf, sizeof(char), BUFSIZE, proxy_stream));    /* read off data */
    return 0;
  }
  return 1;
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

get_info (request_stream, url)
FILE *request_stream;
char *url;
{
  int proxysocket;
  FILE *proxy_stream;
  char buf[BUFSIZE];
  Image image;

  image.jpg_in = 0;
  image.jpg_image_data = 0;
  image.gdp = 0;

  fprintf(stderr, "get_info: GET_INFO %s\n", url);

  sprintf(buf, "GET %s HTTP/1.0\nUser-Agent: Imago/0.0\n\n", url);

  if((proxysocket = make_request(buf)) == -1){
    http_error(request_stream, 500, url, NULL, "could not connect to proxy server");
    return;
  }
  if((proxy_stream = fdopen(proxysocket, "r")) == NULL){
    http_error(request_stream, 500, url, NULL, "could not fdopen proxy connection\n");
    close(proxysocket);
    return;
  }
  if(!get_image_header(request_stream, url, &image, proxy_stream)){
    fprintf(stderr, "get_info: get_image_header returned 0!\n");
    fclose(proxy_stream);
    close(proxysocket);
    return;
  }
  if(!get_image_header_read_off_data(&image, proxy_stream)){
    fprintf(stderr, "get_info: error reading off image data\n");
    fclose(proxy_stream);
    close(proxysocket);
    return;
  }
  fprintf(request_stream, "INFO 200 %s\n\n", url);
  print_image_info(request_stream, &image);
  fclose(proxy_stream);
  close(proxysocket);
  free_image_structures(&image);
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


/*
  the name: stub.FILTER.MAX_SIZE.TYPE where FILTER is one of g, l, n
  (for Gaussian, Lanczos, none), MAX_SIZE is the thumb_max_size
  integer argument, and TYPE is gif or jpg (e.g. foo.g.100.gif).
*/
int
url_to_filename (url, thumb_url, thumb_name, filter_type, use_time, max_size)
char *url, *thumb_url, *thumb_name;
int filter_type, use_time, max_size;
{
  char buf[BUFSIZE], *cp, *ncp, *ucp, *file, *host, hex_buf[3], filter_c;
  int rc, result_size, total_non_alpha, max_size_length, dir_length,
    time_length, time_it_is;

  if(strlen(url) > BUFSIZE - 1){
    fprintf(stderr, "url_to_filename: url too long\n");
    return 0;
  }
  dir_length = MAX(strlen(server_url_dir), strlen(server_disk_dir));
  if(use_time){
    time_it_is = time(0);
    sprintf(buf, "%d", time_it_is);
    time_length = strlen(buf);
  }
  else
    time_length = 0;
  sprintf(buf, "%d", max_size);
  max_size_length = strlen(buf);
  strncpy(buf, url, BUFSIZE);
  fprintf(stderr, "url_to_filename: %s\n", buf);
  cp = strstr(buf, "://");
  if(!cp){
    fprintf(stderr, "url_to_filename: can't find host in url\n");
    return 0;
  }
  host = cp + 3;
  cp = strchr(host, '/');
  *cp = '\0';
  file = cp + 1;
  cp = file;

  for(total_non_alpha = 0; *cp != '\0'; cp++)
    if(*cp != '.' && !isalnum(*cp))
      total_non_alpha++;
  cp = file;

  /* name == server_url_dir/thumb/host/g.100.filtered_file */
  /* name == server_url_dir/thumb/host/g.100.filtered_file */
  result_size = dir_length + 7 + 
                strlen(host) + 1 +
                2 +                        /* filter_type (e.g. "g.") */
                max_size_length + 1 +      /* e.g. "100." */
                time_length + 1 +
                strlen(file) + total_non_alpha * 2;
  if(result_size > BUFSIZE - 1){
    fprintf(stderr, "url_to_filename: url too long\n");
    return 0;
  }
  switch(filter_type){
  case FILTER_NONE:
    filter_c = 'n';
    break;
  case FILTER_GAUSSIAN:
    filter_c = 'g';
    break;
  case FILTER_LANCZOS:
    filter_c = 'l';
    break;
  }
  /*
  sprintf on SunOS doesn't return the strlen of the result!
  */
  sprintf(thumb_name, "%s/thumb/%s", server_disk_dir, host);
  verify_directory(thumb_name);

  if(use_time){
    sprintf(thumb_name, "%s/%c.%d.%d.",
	    thumb_name, filter_c, max_size, time_it_is);
    sprintf(thumb_url, "%s/thumb/%s/%c.%d.%d.",
	    server_url_dir, host, filter_c, max_size, time_it_is);
  }
  else{
    sprintf(thumb_name, "%s/%c.%d.", thumb_name, filter_c, max_size);
    sprintf(thumb_url, "%s/thumb/%s/%c.%d.", server_url_dir, host,
	    filter_c, max_size);
  }
  ncp = thumb_name + strlen(thumb_name);
  ucp = thumb_url + strlen(thumb_url);
  for(;;){
    if(*cp == '\0'){
      *ucp = *ncp = '\0';
      break;
    }
    if(*cp == '.' || isalnum(*cp))
      *ucp++ = *ncp++ = *cp++;
    else{
      /* 
	 using percent signs breaks web servers and browsers, which
	 convert them and any following hex digits back into their
	 non-alpha-numeric ascii equivalents before requesting the URL!
	 so we use underscores.
      */
      /* *ucp++ = *ncp++ = '%';*/
      *ucp++ = *ncp++ = '_';
      sprintf(hex_buf, "%x", *cp++);
      *ucp++ = *ncp++ = hex_buf[0];
      *ucp++ = *ncp++ = hex_buf[1];
    }
  }
  if(filter_type == FILTER_NONE)
    return 1;
  if(ENDS_WITH(thumb_name, ".jpg") || ENDS_WITH(thumb_name, ".jpeg"))
    return 1;
  /* change names to end with ".jpg" */
  ucp = thumb_url + strlen(thumb_url) - 3;
  sprintf(ucp, "jpg");
  ncp = thumb_name + strlen(thumb_name) - 3;
  sprintf(ncp, "jpg");
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
write_image_to_stream (image, request_stream, url)
Image *image;
FILE *request_stream;
char *url;
{
  send_image_header(request_stream, image->content_length, image->mime_type, url);
  if(image->gdp){
    fprintf(stderr, "writing image with gif run-length compression\n");
    gdImageGif(image->gdp, request_stream);
    return 1;
  }
  else if(image->type == IMAGE_FORMAT_JPG){
    fprintf(stderr, "writing image with lossy jpg compression\n");
    return write_jpg(image->jpg_image_data, image->width, image->height,
	      image->total_components, image->out_color_space,
	      image->total_components * image->width, request_stream);
  }
  else{
    fprintf(stderr, "write_image_to_stream: no gif or jpeg to write!\n");
    return 0;
  }
}

int
write_image_to_disk (image, file_name)
Image *image;
char *file_name;
{
  FILE *diskfile_stream;

  if((diskfile_stream = fopen(file_name, "w")) == NULL){
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

char separators[] = { 012, 014, 015, 040, 000 };
/* 
  read http header
  switch on request type
  request_fd is closed in main()
*/
void
parse_request (request_fd)
int request_fd;
{
  static Thumb thumb;
  char buf[BUFSIZE], *tok, command[512], url[BUFSIZE];
  FILE *request_stream;
  int no_cache = 0;
  url[0] = '\0';

  if((request_stream = fdopen(request_fd, "r+")) == NULL){
    fprintf(stderr, "parse_request: can't fdopen request connection\n");
    return;
  }
  if(fgets(buf, BUFSIZE, request_stream) == NULL){
    fprintf(stderr, "parse_request: can't read request\n");
    fclose(request_stream);
    return;
  }
  fprintf(stderr, "parse_request: read line: %s\n", buf);
  
  tok = strtok(buf, separators);

  if(tok == NULL){
    fprintf(stderr, "parse_request: couldn't parse command\n");
    fprintf(stderr, "parse_request: usage: GET (or GET_THUMB or MAKE_MAP) url\n");
    fclose(request_stream);
    return;
  }
  strncpy(command, tok, 512);

  tok = strtok(NULL, separators);
  fprintf(stderr, "parse_request: tok: %s\n", tok);
  if(tok == NULL){
    fprintf(stderr, "parse_request: couldn't parse url\n");
    fprintf(stderr, "parse_request: usage: GET (or GET_THUMB or MAKE_MAP) url\n");
    fclose(request_stream);
    return;
  }
  trim_url(tok);
  strncpy(url, tok, BUFSIZE);
  fprintf(stderr, "parse_request: parsed command: %s, arg: %s\n", command, url);
  if(!strcmp(command, "GET_THUMB")){
    bzero(&thumb, sizeof(Thumb));  /*  set all defaults to zero  */
    parse_thumb_request(request_stream, &thumb);
    make_thumb(url, request_stream, NULL, &thumb);
  }
  else if(!strcmp(command, "MAKE_MAP"))
    make_map(request_stream, url);
  else{
    /* read off header */
    for(;;){
      if(fgets(buf, BUFSIZE, request_stream) == NULL){
	fprintf(stderr, "parse_request: can't read request\n");
	fclose(request_stream);
	return;
      }
      if(buf[0] == 13 || buf[0] == 12 || buf[0] == 10)
	break;
      if(!strcmp(buf, "Pragma: no-cache\n"))
	no_cache = 1;
      else
	fprintf(stderr, "parse_request: ignoring line: [%s]\n", buf);
    }
    /* without the rewind, no data will be written on request_stream! */
    rewind(request_stream);
    if(!strcmp(command, "GET"))
      copy_full_sized_image(request_fd, request_stream, url, no_cache);
    else if(!strcmp(command, "GET_INFO"))
      get_info(request_stream, url);
    else if(!strcmp(command, "GET_MAP_INFO"))
      get_map_info(request_stream, url);
    else if(!strcmp(command, "GET_RATED_MAPS"))
      get_rated_maps(request_stream, url);
    else{
      fprintf(stderr, "parse_request: unrecognized command: %s\n", command);
      fprintf(stderr, "parse_request: usage: COMMAND url\n");
      fprintf(stderr, "  where COMMAND is one of:\n");
      fprintf(stderr, "  GET, GET_THUMB, GET_INFO, GET_MAP_INFO, MAKE_MAP, or GET_RATED_MAPS\n");
      http_error(request_stream, 400, command, NULL, "unrecognized command");
    }
  }
  if(request_stream != NULL)
    fclose(request_stream);
}

http_error (error_stream, ec, url, mime_type, str)
FILE *error_stream;
int ec;
char *url, *mime_type, *str;
{
  switch(ec){
  case 400:
    fputs("HTTP/1.0 400 Bad Request\n", error_stream);
    fprintf(error_stream, "Date: %s", time_string());
    fprintf(error_stream, "Server: %s\n", IMAGE_SERVER_VERSION);
    fputs("Content-type: text/html\n\n", error_stream);
    fputs("<html><head><title>Bad Request</title></head>\n", error_stream);
    fputs("<body><h1>Bad Request</h1>\n", error_stream);
    fprintf(error_stream, "Bummer. The request, %s, ", url);
    fprintf(error_stream, "generated an error on this server: %s\n", str);
    fputs("</body></html>\n", error_stream);  
    break;
  case 406:
    fputs("HTTP/1.0 406 Not Acceptable\n", error_stream);
    fprintf(error_stream, "Date: %s", time_string());
    fprintf(error_stream, "Server: %s\n", IMAGE_SERVER_VERSION);
    fputs("Content-type: text/html\n\n", error_stream);
    fputs("<html><head><title>Not Acceptable</title></head>\n", error_stream);
    fputs("<body><h1>Not Acceptable</h1>\n", error_stream);
    fprintf(error_stream, "The request, %s,\n", url);
    fprintf(error_stream, "%s\n", str);
    fputs("</body></html>\n", error_stream);  
    break;
  case 414:
    fputs("HTTP/1.0 414 Request-URI Too Long\n", error_stream);
    fprintf(error_stream, "Date: %s", time_string());
    fprintf(error_stream, "Server: %s\n", IMAGE_SERVER_VERSION);
    fputs("Content-type: text/html\n\n", error_stream);
    fputs("<html><head><title>Request-URI Too Long</title></head>\n", error_stream);
    fputs("<body><h1>Request-URI Too Long</h1>\n", error_stream);
    fprintf(error_stream, "The request, %s,\n", url);
    fprintf(error_stream, "%s\n", str);
    fputs("</body></html>\n", error_stream);  
    break;
  case 500:
    fputs("HTTP/1.0 500 Internal Server Error\n", error_stream);
    fprintf(error_stream, "Date: %s", time_string());
    fprintf(error_stream, "Server: %s\n", IMAGE_SERVER_VERSION);
    fputs("Content-type: text/html\n\n", error_stream);
    fputs("<html><head><title>Internal Server Error</title></head>\n", error_stream);
    fputs("<body><h1>Internal Server Error</h1>\n", error_stream);
    fprintf(error_stream, "Bummer. The requested URL, %s, ", url);
    if(mime_type)
      fprintf(error_stream, "with mime_type of %s, ", mime_type);
    fprintf(error_stream, "generated an error on this server: %s\n", str);
    fputs("</body></html>\n", error_stream);  
    break;
  case 501:
    fputs("HTTP/1.0 501 Not Implemented\n", error_stream);
    fprintf(error_stream, "Date: %s", time_string());
    fprintf(error_stream, "Server: %s\n", IMAGE_SERVER_VERSION);
    fputs("Content-type: text/html\n\n", error_stream);
    fputs("<html><head><title>File Format Not Implemented</title></head>\n", error_stream);
    fputs("<body><h1>File Format Not Implemented</h1>\n", error_stream);
    fprintf(error_stream, "The requested URL, %s, has a file format of %s, ",
      url, mime_type);
    fputs("which is not implemented by this server.\n", error_stream);
    fputs("</body></html>\n", error_stream);  
    break;
  }
}

int
make_request (request)
char *request;
{
  int proxysocket;
  char buffer[BUFSIZE]; 
  static struct hostent *hp = NULL;
  static u_long address;

  fprintf(stderr, "%s", request);
  if(hp == NULL){
    hp = gethostbyname(proxy_host);
    if(hp == NULL){
      /*fprintf(stderr, "gethostbyname error: %s\n", hstrerror(h_errno));*/
      fprintf(stderr, "gethostbyname error\n");
      errno = ECONNREFUSED;
      fprintf(stderr, "make_request: connection refused. addressing error?\n");
      return -1;
    }
    address = *(u_long *)(hp->h_addr_list[0]);
  }
  proxysocket = call_socket3(address, proxy_port);
  if(proxysocket < 0){
    fprintf(stderr, "can't open socket to proxy\n");
    return -1;
  }
  /* write request */
  if(write(proxysocket, request, strlen(request)) == -1){
    close(proxysocket);
    fprintf(stderr, "couldn't write request\n");
    return -1;
  }
  return proxysocket;
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

/*
  fread the data and copy it back
*/
buffered_copy_data (request_stream, proxy_stream)
FILE *request_stream;
FILE *proxy_stream;
{
  int rc, count = 0;
  char buffer[BIGBUFSIZE];

  for(;;){                           
    rc = fread(buffer, sizeof(char), BIGBUFSIZE, proxy_stream);
    if(!rc){
      if(feof(proxy_stream))
	fprintf(stderr, "buffered_copy_data: EOF reading from proxy_stream\n");
      else if(ferror(proxy_stream))
	fprintf(stderr, "buffered_copy_data: error reading from proxy_stream\n");
      break;
    }
    count += rc;
    if(!fwrite(buffer, sizeof(char), rc, request_stream)){
      if(ferror(request_stream))
	fprintf(stderr, "buffered_copy_data: error writing to request_stream\n");
      break;
    }
    fprintf(stderr, "%d bytes copied\n", rc);
  }
  fprintf(stderr, "total of %d bytes copied\n", count);
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
