#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <stdlib.h>
#include "gd.h"
#include "jpeglib.h"

#define BIGBUFSIZE 16384
#define BUFSIZE 1024
#define MEDIUMBUFSIZE 512
#define SMALLBUFSIZE 10
#define MIME_TYPE_LENGTH 100
#define IMAGE_SERVER_VERSION "Imago/0.0"
#define IMAGE_FORMAT_GIF 1
#define IMAGE_FORMAT_JPG 2
#define GIF_INDEXED -1
#define MAX_COLORS 16777216
#define MAX_GREYS 256

/* map and thumb options */
#define RETURN_URL 0
#define RETURN_BITS 1
#define LAYOUT_SPIRAL 0
#define LAYOUT_RANDOM 1
#define LAYOUT_GRID 2
#define LAYOUT_TILE 3
#define LAYOUT_PRESET 4
#define LAYOUT_NORMAL 5
#define LAYOUT_SPIRAL2 6
#define BACKGROUND_SOLID 0
#define BACKGROUND_TILE 1
#define BACKGROUND_CENTER 2
#define BACKGROUND_STRETCH 3
#define RATE_AREA_COLORS 0
#define REPLACE_CREATE 0
#define REPLACE_UNIQUE 1
#define REPLACE_OVERWRITE 2
#define REPLACE_AUGMENT 3
#define DIMENSION_MIN_SQUARE 0
#define DIMENSION_FIXED 1
#define FILTER_GAUSSIAN 0
#define FILTER_NONE 1
#define FILTER_LANCZOS 2

/* default values for options: */
#define DEFAULT_IMAGO_PORT 6666
#define DEFAULT_PROXY_PORT 80
#define DEFAULT_PROXY_HOST "hci.ucsd.edu"
#define DEFAULT_SERVER_URL_DIR "http://hci.ucsd.edu"
#define DEFAULT_SERVER_DISK_DIR "/home/httpd/html"

#define DEFAULT_MAX_THUMB_SIZE 100
#define DEFAULT_THUMB_MIN_WIDTH 10
#define DEFAULT_THUMB_MIN_HEIGHT 10
#define DEFAULT_THUMB_MIN_COLORS 1

#define IS_EVEN(A) (ceil((A) / 2.0) == floor((A) / 2.0))

/* true if string s1 starts with substring s2 */
#define STARTS_WITH(s1, s2) (strstr((s1),(s2)) == (s1))

/* true if string s1 ends with substring s2 */
#define ENDS_WITH(s1, s2) (strstr((s1),(s2)) == (s1) + strlen(s1) - strlen(s2))

#define MAX(a, b) (((a)>(b))?(a):(b))
#define MIN(a, b) (((a)<(b))?(a):(b))

extern int imago_port;
extern int proxy_port;
extern char *proxy_host;
extern char *server_url_dir;
extern char *server_disk_dir;

typedef struct meta_image {
  gdImagePtr gdp;
  int type, width, height, content_length;
  int total_colors, transparent_color, interlace;
  int total_components, in_color_space, out_color_space;
  int bytes_per_line;
  char mime_type[MIME_TYPE_LENGTH];
  j_decompress_ptr jpg_in;
  unsigned char *jpg_image_data;
} Image;

typedef struct map_node {
  char *image_url, *thumb_url, *other_url, *alt_text;
  int x, y, w, h, max_size;
  double rate;
  Image *thumb;
  struct map_node *next;
} MapNode;

typedef struct thumb {
  int x, y, w, h;
  int crop;
  int max_size;
  int filter_type;
  int min_width;
  int min_height;
  int src_min_width;
  int src_min_height;
  int src_min_colors;
  int return_type;
  int replace_type;
} Thumb;

typedef struct map {
  int w, h;
  int thumbs_per_edge, max_thumb_width, max_thumb_height, thumb_total;
  int background_red, background_green, background_blue;
  int return_type;
  int layout_type;
  int background_type;
  int dimension_type;
  int rate_type;
  int replace_type;
  int update_html, update_image;
  double rate;
  char *host;
  char *default_url;
  char *background_thumb_url;
  Image *background;
  MapNode *thumb_list, *stored_thumb_list;
  MapNode **map_node_array;
  Thumb thumb;
  struct map *next;
} Map;

typedef struct rectangle {
  int x, y, w, h;
} Rectangle;

j_decompress_ptr read_jpg_header();
void fireman();
MapNode *new_map_node();
void print_map_node_list();
void free_map_node_list();
double Lanczos3_filter();
#define	Lanczos3_support	(3.0)
Image *new_jpg_image(), *read_and_decompress_image_file();
