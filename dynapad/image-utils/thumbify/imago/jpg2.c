#include <setjmp.h>
#include <stdio.h>
#include <sys/stat.h>
#include "jpeglib.h"
#include "jerror.h"
#include "math.h"

struct my_error_mgr {
  struct jpeg_error_mgr pub;	/* "public" fields */
  jmp_buf setjmp_buffer;	/* for return to caller */
};

typedef struct my_error_mgr *my_error_ptr;

/*
routine that will replace the standard error_exit method
*/
METHODDEF(void)
my_error_exit (j_common_ptr cinfo)
{
  my_error_ptr myerr = (my_error_ptr) cinfo->err;
  fprintf(stderr, "my_error_exit\n");
  (*cinfo->err->output_message) (cinfo);
  longjmp(myerr->setjmp_buffer, 1);
}

j_decompress_ptr
read_jpg_header (fp)
FILE *fp;
{
  j_decompress_ptr jpg_in;
  static struct my_error_mgr jerr;

  jpg_in = (j_decompress_ptr)malloc(sizeof(struct jpeg_decompress_struct));
  if(!jpg_in)
    return 0;
  jpg_in->err = jpeg_std_error(&jerr.pub);
  jerr.pub.error_exit = my_error_exit;
  if(setjmp(jerr.setjmp_buffer)) {
    /* If we get here, the JPEG code has signaled an error.
     * We need to clean up the JPEG object and return.
     */
    jpeg_destroy_decompress(jpg_in);
    free(jpg_in);
    return 0;
  }

  jpeg_create_decompress(jpg_in);
  jpeg_stdio_src(jpg_in, fp);
  (void)jpeg_read_header(jpg_in, TRUE);
  return jpg_in;
}

int
read_jpg_image (fp, jpg_in, data, bytes_per_line)
FILE *fp;
j_decompress_ptr jpg_in;
unsigned char *data;
int bytes_per_line;
{
  int rc;
  unsigned char *buffer;	/* Output row buffer */

  struct my_error_mgr jerr;
  
  jpg_in->err = jpeg_std_error(&jerr.pub);
  jerr.pub.error_exit = my_error_exit;
  if (setjmp(jerr.setjmp_buffer)) {
    /* If we get here, the JPEG code has signaled an error.
     * We need to clean up the JPEG object and return.
     */
    jpeg_destroy_decompress(jpg_in);
    return 0;
  }

  if(jpg_in->jpeg_color_space == JCS_YCbCr) /* we want rgb out, for yuv in */
    jpg_in->out_color_space = JCS_RGB;

  jpeg_start_decompress(jpg_in);

  while(jpg_in->output_scanline < jpg_in->output_height){
    buffer = data + jpg_in->output_scanline * bytes_per_line;
    (void)jpeg_read_scanlines(jpg_in, &buffer, 1);
  }

  (void)jpeg_finish_decompress(jpg_in);

  /*
     remember to jpeg_destroy_decompress(jpg_in);
     At this point you may want to check to see whether any corrupt-data
     warnings occurred (test whether jerr.pub.num_warnings is nonzero).
   */
  if(jpg_in->err->num_warnings)
    fprintf(stderr, "warnings: %ld\n", jpg_in->err->num_warnings);
  return 1;
}

int
write_jpg (data, w, h, num_components, color_space, bytes_per_line, fp)
unsigned char *data;
int w, h, num_components, color_space, bytes_per_line;
FILE *fp;
{
  struct jpeg_compress_struct cinfo;
  struct jpeg_error_mgr jerr;
  unsigned char *buffer;	/* Output row buffer */

  if(!data){
    fprintf(stderr, "write_jpg: no data!\n");
    return 0;
  }
  cinfo.err = jpeg_std_error(&jerr);
  jpeg_create_compress(&cinfo);
  jpeg_stdio_dest(&cinfo, fp);
  cinfo.image_width = w; 
  cinfo.image_height = h;
  cinfo.input_components = num_components;
  cinfo.in_color_space = color_space;
  jpeg_set_defaults(&cinfo);
  jpeg_set_quality(&cinfo, 100, TRUE /* highest quality setting */);
  jpeg_start_compress(&cinfo, TRUE);

  while (cinfo.next_scanline < cinfo.image_height) {
    buffer = data + cinfo.next_scanline * bytes_per_line;
    (void)jpeg_write_scanlines(&cinfo, &buffer, 1);
  }
  jpeg_finish_compress(&cinfo);
  fclose(fp);
  jpeg_destroy_compress(&cinfo);
  return 1;
}

write_uncompressed (data, w, h, num_components, color_space, bytes_per_line, fp)
unsigned char *data;
int w, h, num_components, color_space, bytes_per_line;
FILE *fp;
{
  int i, j;
  unsigned char *buffer;	/* Output row buffer */

  if(num_components == 1)
    putc((unsigned char)2, fp); /* greyscale */
  else
    putc((unsigned char)3, fp); /* rgb */
  gdPutWord(w, fp);
  gdPutWord(h, fp);
  for(i = 0; i < h; i++){
    buffer = data + i * bytes_per_line;
    fwrite(buffer, sizeof(unsigned char), bytes_per_line, fp);
  }
}

jpg_copy_resized (dst, src, dstX, dstY, srcX, srcY, dstW, dstH, srcW, srcH, num_components)
unsigned char *dst, *src;
int dstX, dstY, srcX, srcY, dstW, dstH, srcW, srcH, num_components;
{
  unsigned char r, g, b;
  int x, y;
  int tox, toy;
  int ydest;
  int i;
  int src_bytes_per_line = srcW * num_components;
  int dst_bytes_per_line = dstW * num_components;
  int offset;
	/* Stretch vectors */
  int *stx;
  int *sty;
	/* We only need to use floating point to determine the correct
		stretch vector for one line's worth. */
  double accum = 0;
  stx = (int *) malloc(sizeof(int) * srcW);
  sty = (int *) malloc(sizeof(int) * srcH);

  for (i=0; (i < srcW); i++) {
    int got;
    accum += (double)dstW/(double)srcW;
    got = (int)floor(accum);
    stx[i] = got;
    accum -= got;
  }
  accum = 0;
  for (i=0; (i < srcH); i++) {
    int got;
    accum += (double)dstH/(double)srcH;
    got = floor(accum);
    sty[i] = got;
    accum -= got;
  }
  fprintf(stderr, "src: %d, %d, dst %d, %d\n", srcW, srcH, dstW, dstH);
  if(num_components == 1){
    toy = 0;
    for(y = 0; y < srcH; y++){
      if(y != srcH -1 && !sty[y])
	continue;
      tox = 0;
      for(x = 0; x < srcW; x++){
	if(x != srcW - 1 && !stx[x])
	  continue;
	offset = y * srcW + x;
	r = src[offset];
	offset = toy * dstW + tox;
	dst[offset] = r;
	tox++;
      }
      toy++;
    }
  }
  else if(num_components == 3){
    toy = 0;
    for(y = 0; y < srcH; y++){
      if(y != srcH -1 && !sty[y])
	continue;
      tox = 0;
      for(x = 0; x < srcW; x++){
	if(x != srcW - 1 && !stx[x])
	  continue;
	offset = y * src_bytes_per_line + x * 3;
	r = src[offset];
	g = src[offset + 1];
	b = src[offset + 2];
	offset = toy * dst_bytes_per_line + tox * 3;
	dst[offset] = r;
	dst[offset + 1] = g;
	dst[offset + 2] = b;
	tox++;
      }
      toy++;
    }
  }
  else
    return;
  free(stx);
  free(sty);
}

