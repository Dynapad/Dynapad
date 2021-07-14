#include "server.h"
#ifdef SOLARIS
#include <dirent.h>
#else
#include <sys/dir.h>
#endif
#include <math.h>

/* 
   sort in decreasing order
*/
int
compar_rates (arg1, arg2)
MapNode **arg1, **arg2;
{
  if((*arg1)->rate > (*arg2)->rate)
    return -1;
  if((*arg1)->rate < (*arg2)->rate)
    return 1;
  return 0;
}

int
compar_heights (arg1, arg2)
MapNode **arg1, **arg2;
{
  if((*arg1)->h > (*arg2)->h)
    return -1;
  if((*arg1)->h < (*arg2)->h)
    return 1;
  return 0;
}

int
compar_thumbs (arg1, arg2)
MapNode **arg1, **arg2;
{
  if((*arg1)->max_size > (*arg2)->max_size)
    return -1;
  if((*arg1)->max_size < (*arg2)->max_size)
    return 1;
  return 0;
}

void
free_map_node (map_node)
MapNode *map_node;
{
  if(!map_node)
    return;
  if(map_node->image_url){
    free(map_node->image_url);
    map_node->image_url = 0;
  }
  if(map_node->other_url){
    free(map_node->other_url);
    map_node->other_url = 0;
  }
  if(map_node->thumb_url){
    free(map_node->thumb_url);
    map_node->thumb_url = 0;
  }
  if(map_node->alt_text){
    free(map_node->alt_text);
    map_node->alt_text = 0;
  }
  if(map_node->thumb){
    free_image_structures(map_node->thumb);
    free(map_node->thumb);
  }
  free(map_node);
  map_node = 0;
}

void
free_map_node_list (map_node)
MapNode *map_node;
{
  MapNode *tmp;

  for(;;){
    if(!map_node)
      break;
    tmp = map_node;
    map_node = map_node->next;
    free_map_node(tmp);
  }
}

MapNode *
new_map_node (image_url, thumb_url, other_url, alt_text, x, y, w, h, max_size, rate)
char *image_url, *other_url, *thumb_url, *alt_text;
int x, y, w, h, max_size;
double rate;
{
  MapNode *map_node = (MapNode *)malloc(sizeof(MapNode));
  if(!map_node){
    fprintf(stderr, "new_map_node: can't malloc new MapNode\n");
    return NULL;
  }
  bzero(map_node, sizeof(MapNode));
  map_node->rate = -1;
  if(image_url && image_url[0] != '\0'){
    map_node->image_url = (char *)malloc(strlen(image_url) + 1);
    if(!map_node->image_url){
      fprintf(stderr, "new_map_node: can't malloc image_url\n");
      free(map_node);
      return NULL;
    }
    strncpy(map_node->image_url, image_url, strlen(image_url) + 1);
  }
  if(thumb_url && thumb_url[0] != '\0'){
    map_node->thumb_url = (char *)malloc(strlen(thumb_url) + 1);
    if(!map_node->thumb_url){
      fprintf(stderr, "new_map_node: can't malloc thumb url\n");
      if(map_node->image_url)
	free(map_node->image_url);
      free(map_node);
      return NULL;
    }
    strncpy(map_node->thumb_url, thumb_url, strlen(thumb_url) + 1);
  }
  if(other_url && other_url[0] != '\0'){
    map_node->other_url = (char *)malloc(strlen(other_url) + 1);
    if(!map_node->other_url){
      fprintf(stderr, "new_map_node: can't malloc other url\n");
      if(map_node->image_url)
	free(map_node->image_url);
      if(map_node->thumb_url)
	free(map_node->thumb_url);
      free(map_node);
      return NULL;
    }
    strncpy(map_node->other_url, other_url, strlen(other_url) + 1);
  }
  if(alt_text && alt_text[0] != '\0'){
    map_node->alt_text = (char *)malloc(strlen(alt_text) + 1);
    if(!map_node->alt_text){
      fprintf(stderr, "new_map_node: can't malloc alt text\n");
      if(map_node->image_url)
	free(map_node->image_url);
      if(map_node->thumb_url)
	free(map_node->thumb_url);
      if(map_node->other_url)
	free(map_node->other_url);
      free(map_node);
      return NULL;
    }
    strncpy(map_node->alt_text, alt_text, strlen(alt_text) + 1);
  }
  map_node->x = x;
  map_node->y = y;
  map_node->w = w;
  map_node->h = h;
  map_node->max_size = max_size;
  map_node->rate = rate;
  return map_node;
}

void
print_map_node (map_node)
MapNode *map_node;
{
  if(map_node->image_url)
    fprintf(stderr, " image_url: %s", map_node->image_url);
  if(map_node->other_url)
    fprintf(stderr, " other_url: %s", map_node->other_url);
  fprintf(stderr, " x: %d", map_node->x);
  fprintf(stderr, " y: %d", map_node->y);
  fprintf(stderr, " w: %d", map_node->w);
  fprintf(stderr, " h: %d", map_node->h);
}

void
print_map_node_list (map_node)
MapNode *map_node;
{
  while(map_node != NULL){
    print_map_node(map_node);
    fprintf(stderr, "\n");
    map_node = map_node->next;
  }
}

Rectangle *
new_rectangle (x, y, w, h)
int x, y, w, h;
{  
  Rectangle *r = (Rectangle *)malloc(sizeof(Rectangle));
  if(r){
    r->x = x;
    r->y = y;
    r->w = w;
    r->h = h;
  }
  return r;
}

Map *
new_map () {
  Map *map = (Map *)malloc(sizeof(Map));
  if(!map)
    return NULL;
  /* 
    set all defaults to zero
    also set background color to black
  */
  bzero(map, sizeof(Map));
  map->thumb.return_type = RETURN_BITS;
  map->thumb.max_size = DEFAULT_MAX_THUMB_SIZE;
  map->thumb.min_width = DEFAULT_THUMB_MIN_WIDTH;
  map->thumb.min_height = DEFAULT_THUMB_MIN_HEIGHT;
  map->thumb.src_min_width = DEFAULT_THUMB_MIN_WIDTH;
  map->thumb.src_min_height = DEFAULT_THUMB_MIN_HEIGHT;
  map->thumb.src_min_colors = DEFAULT_THUMB_MIN_COLORS;
  return map;
}

get_map_replace (map, buf)
Map *map;
char *buf;
{
  char value[MEDIUMBUFSIZE];

  int rc = sscanf(buf, "%*s %s\n", value);
  if(rc != 1){
    fprintf(stderr, "get_map_replace: can't parse line: %s\n", buf);
    return;
  }
  /* sunOS sscanf leaves a trailing carriage-return! */
  if(isspace(value[strlen(value) - 1]))
    value[strlen(value) - 1] = '\0';
  fprintf(stderr, "get_map_replace: value: %s\n", value);
  
  if(!strcmp(value, "create"))
    map->replace_type = REPLACE_CREATE;
  else if(!strcmp(value, "unique"))
    map->replace_type = REPLACE_UNIQUE;
  else if(!strcmp(value, "overwrite"))
    map->replace_type = REPLACE_OVERWRITE;
  else if(!strcmp(value, "augment"))
    map->replace_type = REPLACE_AUGMENT;
  else{
    map->replace_type = REPLACE_CREATE;
    fprintf(stderr, "get_map_replace: illegal replacement policy: %s\n", value);
    fprintf(stderr, "  using default replacement policy: create\n");
  }
}

get_thumb_mins (map, buf)
Map *map;
char *buf;
{
  if(STARTS_WITH(buf, "thumb_min_width:")){
    int rc = sscanf(buf, "%*s %d\n", &map->thumb.min_width);
    if(rc != 1){
      fprintf(stderr, "get_thumb_mins: can't parse line: %s\n", buf);
      return;
    }
    fprintf(stderr, "thumb_min_width: %d\n", map->thumb.min_width);
  }
  else if(STARTS_WITH(buf, "thumb_min_height:")){
    int rc = sscanf(buf, "%*s %d\n", &map->thumb.min_height);
    if(rc != 1){
      fprintf(stderr, "get_thumb_mins: can't parse line: %s\n", buf);
      return;
    }
    fprintf(stderr, "thumb_min_height: %d\n", map->thumb.min_height);
  }
  else if(STARTS_WITH(buf, "thumb_src_min_width:")){
    int rc = sscanf(buf, "%*s %d\n", &map->thumb.src_min_width);
    if(rc != 1){
      fprintf(stderr, "get_thumb_mins: can't parse line: %s\n", buf);
      return;
    }
    fprintf(stderr, "thumb_src_min_width: %d\n", map->thumb.src_min_width);
  }
  else if(STARTS_WITH(buf, "thumb_src_min_height:")){
    int rc = sscanf(buf, "%*s %d\n", &map->thumb.src_min_height);
    if(rc != 1){
      fprintf(stderr, "get_thumb_mins: can't parse line: %s\n", buf);
      return;
    }
    fprintf(stderr, "thumb_src_min_height: %d\n", map->thumb.src_min_height);
  }
  else if(STARTS_WITH(buf, "thumb_src_min_colors:")){
    int rc = sscanf(buf, "%*s %d\n", &map->thumb.src_min_colors);
    if(rc != 1){
      fprintf(stderr, "get_thumb_mins: can't parse line: %s\n", buf);
      return;
    }
    fprintf(stderr, "thumb_src_min_colors: %d\n", map->thumb.src_min_colors);
  }
}

get_thumb_max_size (map, buf)
Map *map;
char *buf;
{
  int rc;

  rc = sscanf(buf, "%*s %d\n", &map->thumb.max_size);
  if(rc != 1){
    fprintf(stderr, "get_thumb_max_size: can't parse line: %s\n", buf);
    fprintf(stderr, "get_thumb_max_size: rc: %d\n", rc);
    return;
  }
}

get_map_filter_type (map, buf)
Map *map;
char *buf;
{
  char value[MEDIUMBUFSIZE];

  int rc = sscanf(buf, "%*s %s\n", value);
  if(rc != 1){
    fprintf(stderr, "get_map_filter_type: can't parse line: %s\n", buf);
    return;
  }
  /* sunOS sscanf leaves a trailing carriage-return! */
  if(isspace(value[strlen(value) - 1]))
    value[strlen(value) - 1] = '\0';
  fprintf(stderr, "get_map_filter_type: value: %s\n", value);
  
  if(!strcmp(value, "none"))
    map->thumb.filter_type = FILTER_NONE;
  else if(!strcmp(value, "gaussian"))
    map->thumb.filter_type = FILTER_GAUSSIAN;
  else if(!strcmp(value, "lanczos"))
    map->thumb.filter_type = FILTER_LANCZOS;
  else{
    map->thumb.filter_type = FILTER_NONE;
    fprintf(stderr, "get_map_filter_type: illegal filter: %s\n", value);
    fprintf(stderr, "  using default filter: none\n");
  }
}

get_background_type (map, buf)
Map *map;
char *buf;
{
  char value[MEDIUMBUFSIZE];

  int rc = sscanf(buf, "%*s %s\n", value);
  if(rc != 1){
    fprintf(stderr, "get_background_type: can't parse line: %s\n", buf);
    return;
  }
  /* sunOS sscanf leaves a trailing carriage-return! */
  if(isspace(value[strlen(value) - 1]))
    value[strlen(value) - 1] = '\0';
  fprintf(stderr, "get_background_type: value: %s\n", value);
  if(!strcmp(value, "solid"))
    map->background_type = BACKGROUND_SOLID;
  else if(!strcmp(value, "tile"))
    map->background_type = BACKGROUND_TILE;
  else if(!strcmp(value, "center"))
    map->background_type = BACKGROUND_CENTER;
  else if(!strcmp(value, "stretch"))
    map->background_type = BACKGROUND_STRETCH;
  else{
    map->background_type = BACKGROUND_SOLID;
    fprintf(stderr, "get_background_type: illegal background style: %s\n", value);
    fprintf(stderr, "  using default background_type: solid\n");
  }
}

get_background_color (map, buf)
Map *map;
char *buf;
{
  int rc;
  unsigned int r, g, b;

  rc = sscanf(buf, "%*s 0x%2x%2x%2x", &r, &g, &b);
  if(rc != 3){
    fprintf(stderr, "get_background_color: can't parse line: %s\n", buf);
    fprintf(stderr, "get_background_color: rc: %d\n", rc);
    return;
  }
  map->background_red = r;
  map->background_green = g;
  map->background_blue = b;
  fprintf(stderr, "get_background_color: %d %d %d\n", r, g, b);
}

get_dimension_type (map, buf)
Map *map;
char *buf;
{
  char value[MEDIUMBUFSIZE];

  int rc = sscanf(buf, "%*s %s\n", value);
  if(rc != 1){
    fprintf(stderr, "get_dimension_type: can't parse line: %s\n", buf);
    return;
  }
  /* sunOS sscanf leaves a trailing carriage-return! */
  if(isspace(value[strlen(value) - 1]))
    value[strlen(value) - 1] = '\0';
  fprintf(stderr, "get_dimension_type: value: %s\n", value);
  if(!strcmp(value, "fixed"))
    map->dimension_type = DIMENSION_FIXED;
  else if(!strcmp(value, "min-square"))
    map->dimension_type = DIMENSION_MIN_SQUARE;
  else{
    map->dimension_type = DIMENSION_FIXED;
    fprintf(stderr, "get_dimension_type: illegal dimension policy: [[%s]]\n", value);
    fprintf(stderr, "  using default dimension policy: fixed\n");
  }
}

get_dimensions (map, buf)
Map *map;
char *buf;
{
  char value[MEDIUMBUFSIZE];
  int rc, total_w, total_h;

  rc = sscanf(buf, "%*s %d %d\n", &total_w, &total_h);
  if(rc != 2){
    fprintf(stderr, "get_dimensions: can't parse line: %s\n", buf);
    fprintf(stderr, "get_dimensions: rc: %d\n", rc);
    return;
  }
  fprintf(stderr, "get_dimensions: %d %d\n", total_w, total_h);
  map->w = total_w;
  map->h = total_h;
}

get_background_thumb_url (map, buf)
Map *map;
char *buf;
{
  char value[MEDIUMBUFSIZE];

  int rc = sscanf(buf, "%*s %s\n", value);
  if(rc != 1){
    fprintf(stderr, "get_background_thumb_url: can't parse line: %s\n", buf);
    return;
  }
  /* sunOS sscanf leaves a trailing carriage-return! */
  if(isspace(value[strlen(value) - 1]))
    value[strlen(value) - 1] = '\0';
  fprintf(stderr, "get_background_thumb_url: value: %s\n", value);
  if(!STARTS_WITH(value, "http://")){
    fprintf(stderr, "get_background_thumb_url: illegal image url: %s\n", value);
    return;
  }
  map->background_thumb_url = (char *)malloc(strlen(value) + 1);
  if(!map->background_thumb_url){
    fprintf(stderr, "get_background_thumb_url: can't malloc image url: %s\n", value);
    return;
  }
  strncpy(map->background_thumb_url, value, strlen(value) + 1);
}

get_default_url (map, buf)
Map *map;
char *buf;
{
  char value[MEDIUMBUFSIZE];

  int rc = sscanf(buf, "%*s %s\n", value);
  if(rc != 1){
    fprintf(stderr, "get_default_url: can't parse line: %s\n", buf);
    return;
  }
  /* sunOS sscanf leaves a trailing carriage-return! */
  if(isspace(value[strlen(value) - 1]))
    value[strlen(value) - 1] = '\0';
  fprintf(stderr, "get_default_url: value: %s\n", value);
  if(!STARTS_WITH(value, "http://")){
    fprintf(stderr, "get_default_url: illegal url: %s\n", value);
    return;
  }
  map->default_url = (char *)malloc(strlen(value) + 1);
  if(!map->default_url){
    fprintf(stderr, "get_default_url: can't malloc image url: %s\n", value);
    return;
  }
  strncpy(map->default_url, value, strlen(value) + 1);
}

get_map_return (map, buf)
Map *map;
char *buf;
{
  char value[MEDIUMBUFSIZE];

  int rc = sscanf(buf, "%*s %s\n", value);
  if(rc != 1){
    fprintf(stderr, "get_map_return: can't parse line: %s\n", buf);
    return;
  }
  /* sunOS sscanf leaves a trailing carriage-return! */
  if(isspace(value[strlen(value) - 1]))
    value[strlen(value) - 1] = '\0';
  fprintf(stderr, "get_map_return: value: %s\n", value);
  if(!strcmp(value, "url"))
    map->return_type = RETURN_URL;
  else if(!strcmp(value, "bits"))
    map->return_type = RETURN_BITS;
  else{
    map->return_type = RETURN_URL;
    fprintf(stderr, "get_map_return: illegal return type: %s\n", value);
    fprintf(stderr, "  using default return: url\n");
  }
}

get_rate (map, buf)
Map *map;
char *buf;
{
  char value[MEDIUMBUFSIZE];

  int rc = sscanf(buf, "%*s %s\n", value);
  if(rc != 1){
    fprintf(stderr, "get_rate: can't parse line: %s\n", buf);
    return;
  }
  /* sunOS sscanf leaves a trailing carriage-return! */
  if(isspace(value[strlen(value) - 1]))
    value[strlen(value) - 1] = '\0';
  fprintf(stderr, "get_rate: value: %s\n", value);
  if(!strcmp(value, "area-colors"))
    map->rate_type = RATE_AREA_COLORS;
  else{
    map->rate_type = RATE_AREA_COLORS;
    fprintf(stderr, "get_rate: illegal rate type: %s\n", value);
    fprintf(stderr, "  using default rate scheme: area-colors\n");
  }
}

get_layout (map, buf)
Map *map;
char *buf;
{
  char value[MEDIUMBUFSIZE];

  int rc = sscanf(buf, "%*s %s\n", value);
  if(rc != 1){
    fprintf(stderr, "get_layout: can't parse line: %s\n", buf);
    return;
  }
  /* sunOS sscanf leaves a trailing carriage-return! */
  if(isspace(value[strlen(value) - 1]))
    value[strlen(value) - 1] = '\0';
  fprintf(stderr, "get_layout: value: %s\n", value);
  if(!strcmp(value, "grid"))
    map->layout_type = LAYOUT_GRID;
  else if(!strcmp(value, "tile"))
    map->layout_type = LAYOUT_TILE;
  else if(!strcmp(value, "spiral"))
    map->layout_type = LAYOUT_SPIRAL;
  else if(!strcmp(value, "spiral2"))
    map->layout_type = LAYOUT_SPIRAL2;
  else if(!strcmp(value, "random"))
    map->layout_type = LAYOUT_RANDOM;
  else if(!strcmp(value, "preset"))
    map->layout_type = LAYOUT_PRESET;
  else if(!strcmp(value, "normal"))
    map->layout_type = LAYOUT_NORMAL;
  else{
    fprintf(stderr, "get_layout: illegal layout type: %s\n", value);
    fprintf(stderr, "  using default layout type: grid\n");
  }
}

/*
  area: image="url" thumb="url" other="url" alt="text" rate=0.0 
        coords="x,y,w,h" max_size=0
  area: shape=default href="url"
 */
MapNode *
get_thumb_line (map, buf)
Map *map;
char *buf;
{
  char image_url[BUFSIZE];
  char thumb_url[BUFSIZE];
  char other_url[BUFSIZE];
  char alt_text[BUFSIZE];
  char *att, *att_end, *val, *val_end, *end = buf + strlen(buf);
  int rc, x = -1, y = -1, w = -1, h = -1, max_size = -1;
  double rate = -1.0;
  MapNode *map_node, *mp;
  image_url[0] = thumb_url[0] = other_url[0] = alt_text[0] = '\0';

  /* trim trailing white space, newlines, linefeeds, etc. */
  while(isspace(buf[strlen(buf) - 1]))
    buf[strlen(buf) - 1] = '\0';
  fprintf(stderr, "get_thumb_line: %s\n", buf);

  /* skip past "area: " or "area " */
  if(STARTS_WITH(buf, "area: "))
    att_end = val_end = buf + 5;
  else if(STARTS_WITH(buf, "area "))
    att_end = val_end = buf + 4;
  for(;att_end < end && val_end < end;){
    /* try to get an attribute */
    att = val_end + 1;
    for(att_end = att + 1; att_end < end; att_end++){
      if(*att_end == '='){
	*att_end = '\0';
	break;
      }
    }
    if(att_end >= end){
      /*fprintf(stderr, "get_thumb_line: reached the end\n");*/
      break;
    }
    /* try to get a value */
    val = att_end + 1;
    if(*val == '"'){  /* try to get a quoted value (34) */
      val++;
      for(val_end = val + 1; val_end < end; val_end++){
	if(*val_end == '"'){
	  *val_end++ = '\0';
	  break;
	}
      }
    }
    else{            /* try to get an unquoted value */
      for(val_end = val + 1; val_end < end; val_end++){
	if(isspace(*val_end)){
	  *val_end = '\0';
	  break;
	}
      }
    }
    fprintf(stderr, "get_thumb_line: att: [%s], value: [%s]\n", att, val);
    /* store the value */
    if(!strcmp(att, "image")){
      strncpy(image_url, val, BUFSIZE);
    }
    else if(!strcmp(att, "thumb")){
      strncpy(thumb_url, val, BUFSIZE);
    }
    else if(!strcmp(att, "other")){
      strncpy(other_url, val, BUFSIZE);
    }
    else if(!strcmp(att, "href")){
      strncpy(other_url, val, BUFSIZE);
    }
    else if(!strcmp(att, "alt")){
      strncpy(alt_text, val, BUFSIZE);
    }
    else if(!strcmp(att, "rate")){
      rc = sscanf(val, "%lf", &rate);
      if(rc != 1)
	rate = 0.0;
      fprintf(stderr, "get_thumb_line: read rate: %g\n", rate);
    }
    else if(!strcmp(att, "coords")){
      rc = sscanf(val, "%d,%d,%d,%d", &x, &y, &w, &h);
      if(rc != 4){
	h = -1;
	rc = sscanf(val, "%d,%d,%d", &x, &y, &w);
	if(rc != 3){
	  w = -1;
	  rc = sscanf(val, "%d,%d", &x, &y);
	  if(rc != 2){
	    y = -1;
	    rc = sscanf(val, "%d", &x);
	    if(rc != 1){
	      x = -1;
	    }
	  }
	}
      }
    }
    else if(!strcmp(att, "max_size")){
      max_size = atoi(val);
    }
    else if(!strcmp(att, "shape")){
      if(!strcmp(val, "default"))
	if(!map->default_url)
	  map->default_url = strdup(other_url);
      return NULL;
    }
  }
  if(image_url[0] != '\0'){
    trim_url(image_url);
    if(!STARTS_WITH(image_url, "http://")){
      fprintf(stderr, "get_thumb_line: illegal image url: %s\n", image_url);
      return NULL;
    }
  }
  if(thumb_url[0] != '\0'){
    trim_url(thumb_url);
    if(!STARTS_WITH(thumb_url, "http://")){
      fprintf(stderr, "get_thumb_line: illegal thumb url: %s\n", thumb_url);
      return NULL;
    }
  }
  if(other_url[0] != '\0'){
    trim_url(other_url);
    if(!STARTS_WITH(other_url, "http://")){
      fprintf(stderr, "get_thumb_line: illegal other url: %s\n", other_url);
      return NULL;
    }
  }
  if(w != -1)
    w -= x;
  if(h != -1)
    h -= y;
  fprintf(stderr, "get_thumb_line: image_url: [%s], thumb_url: [%s], other_url: [%s]\n",
	  image_url, thumb_url, other_url);
  fprintf(stderr, "get_thumb_line: alt_text: [%s], rate: %g\n", alt_text, rate);
  fprintf(stderr, "get_thumb_line: x: %d, y: %d, w: %d, h: %d, max_size: %d\n", 
	  x, y, w, h, max_size);
  return new_map_node(image_url, thumb_url, other_url, alt_text, x, y, w, h, max_size, rate);
}

MapNode *
thumb_line_exists (map, image_url)
Map *map;
char *image_url;
{
  MapNode *mp;
  for(mp = map->thumb_list; mp; mp = mp->next){
    if(!strcmp(image_url, mp->image_url))
      return mp;
  }
  return NULL;
}

/*
  legal attribute/values:

  dimensions: w, h
  area: image=url other=url coords="x,y,w,h"
  background_color: hex_color (e.g. 0x00ffcc)
  background_thumb_url: url (e.g. http://www.cs.unm.edu/~jon/skull3thumb.gif)
  default_url: url (e.g. http://www.cs.unm.edu/~jon/)

  dimension_type: "fixed" or "min-square"
  return: "url" or "bits"
  layout: "preset", "random", "spiral", "tile" or "grid"
  background: "solid", "tile", "center" or "stretch"
  rate: "area-colors"
  filter: "box", "gaussian", or "lanczos"

*/
parse_map_request (request_stream, map)
FILE *request_stream;
Map *map;
{
  char buf[BUFSIZE];
  MapNode *mp, *last;

  for(;;){
    if(fgets(buf, BUFSIZE, request_stream) == NULL){
      fprintf(stderr, "parse_map_request: can't read input\n");
      return NULL;
    }
    if(buf[0] == 13 || buf[0] == 12 || buf[0] == 10){
      /*fprintf(stderr, "parse_map_request: read a blank line\n");*/
      rewind(request_stream);
      break;
    }
    if(STARTS_WITH(buf, "dimensions:"))
      get_dimensions(map, buf);
    else if(STARTS_WITH(buf, "dimension_type:"))
      get_dimension_type(map, buf);
    else if(STARTS_WITH(buf, "return:"))
      get_map_return(map, buf);
    else if(STARTS_WITH(buf, "layout:"))
      get_layout(map, buf);
    else if(STARTS_WITH(buf, "background_color:"))
      get_background_color(map, buf);
    else if(STARTS_WITH(buf, "rate:"))
      get_rate(map, buf);
    else if(STARTS_WITH(buf, "filter:"))
      get_map_filter_type(map, buf);
    else if(STARTS_WITH(buf, "thumb_max_size:"))
      get_thumb_max_size(map, buf);
    else if(STARTS_WITH(buf, "thumb_min_"))
      get_thumb_mins(map, buf);
    else if(STARTS_WITH(buf, "background:"))
      get_background_type(map, buf);
    else if(STARTS_WITH(buf, "replace:"))
      get_map_replace(map, buf);
    else if(STARTS_WITH(buf, "background_thumb_url:"))
      get_background_thumb_url(map, buf);    /* be sure to free background_thumb_url */
    else if(STARTS_WITH(buf, "default_url:"))
      get_default_url(map, buf);             /* be sure to free default_url */
    else if(STARTS_WITH(buf, "area")){
      mp = get_thumb_line(map, buf);
      if(!mp)
	continue;
      if(map->thumb_list == NULL)
	map->thumb_list = mp;
      else
	last->next = mp;
      last = mp;
    }
  }
  print_map_node_list(map->thumb_list);
}

/* 
   should be called after count_thumbs(), which sets map->thumb_total
 */
compute_dimensions (map)
Map *map;
{
  MapNode *mn;
  int max_dim, side;
  double root;

  map->max_thumb_width = 0;
  map->max_thumb_height = 0;
  for(mn = map->thumb_list; mn; mn = mn->next){
    if(mn->thumb){
      map->max_thumb_width = MAX(map->max_thumb_width, mn->thumb->width);
      map->max_thumb_height = MAX(map->max_thumb_height, mn->thumb->height);
    }
    else{
      map->max_thumb_width = MAX(map->max_thumb_width, mn->w);
      map->max_thumb_height = MAX(map->max_thumb_height, mn->h);
    }
  }
  root = sqrt((double)map->thumb_total);
  map->thumbs_per_edge = (int)(root);
  if(root - (double)map->thumbs_per_edge > 0)
    map->thumbs_per_edge++;
  max_dim = MAX(map->max_thumb_width, map->max_thumb_height);

  switch(map->dimension_type){
  case DIMENSION_FIXED:
    if(map->w > 0 && map->h > 0)
      break;
    fprintf(stderr, "compute_dimensions: no fixed dimensions provided; using min-square\n");
    map->dimension_type = DIMENSION_MIN_SQUARE;
    /* no break */
  case DIMENSION_MIN_SQUARE:
    map->w = map->h = max_dim * map->thumbs_per_edge;
  }
  fprintf(stderr, "compute_dimensions: thumb_total: %d, root: %g, thumbs_per_edge: %d\n", 
	  map->thumb_total, root, map->thumbs_per_edge);
  fprintf(stderr, "compute_dimensions: %dx%d\n", map->w, map->h);
}

/*
  called after layout is computed, before background is created
*/
minimize_dimensions (map)
Map *map;
{
  MapNode *mn;
  int min_x = 10000, min_y = 10000;
  int max_x = 0, max_y = 0;
  for(mn = map->thumb_list; mn; mn = mn->next){
    min_x = MIN(min_x, mn->x);
    min_y = MIN(min_y, mn->y);
    if(mn->thumb){
      max_x = MAX(max_x, mn->x + mn->thumb->width);
      max_y = MAX(max_y, mn->y + mn->thumb->height);
    }
    else{
      max_x = MAX(max_x, mn->x + mn->w);
      max_y = MAX(max_y, mn->y + mn->h);
    }
  }
  for(mn = map->thumb_list; mn; mn = mn->next){
    if(min_x > 0)
      mn->x -= min_x;
    else
      mn->x += min_x;
    if(min_y > 0)      
      mn->y -= min_y;
    else
      mn->y += min_y;
  }
  if(min_x > 0)
    map->w = max_x - min_x;
  else
    map->w = max_x + min_x;
  if(min_y > 0)      
    map->h = max_y - min_y;
  else
    map->h = max_y + min_y;
}

draw_background (map)
Map *map;
{
  Image thumb;
  thumb.jpg_in = 0;
  thumb.gdp = 0;

  /* we might not always need to fill, but it makes error handling easier */
  fill_image(map->background->jpg_image_data, map->w, map->h,
	     map->background_red, map->background_green,
	     map->background_blue);
  switch(map->background_type){
  case BACKGROUND_SOLID:
    break;
  case BACKGROUND_TILE:
    if(!map->background_thumb_url)
      break;
    if(!make_thumb(map->background_thumb_url, NULL, &thumb, map->thumb))
      break;
    if(thumb.type == IMAGE_FORMAT_GIF)  /* if thumb is type gif, convert it to jpg */
      if(!gif_to_jpg(&thumb)){
	free_image_structures(&thumb);
	break;
      }
    jpg_tile(map->background, &thumb);
    free_image_structures(&thumb);
    break;
  case BACKGROUND_CENTER:
    if(!map->background_thumb_url)
      break;
    if(!make_thumb(map->background_thumb_url, NULL, &thumb, map->thumb))
      break;
    if(thumb.type == IMAGE_FORMAT_GIF)  /* if thumb is type gif, convert it to jpg */
      if(!gif_to_jpg(&thumb)){
	free_image_structures(&thumb);
	break;
      }
    jpg_blt(&thumb, map->background, 0, 0, thumb.width, thumb.height,
	    (map->background->width - thumb.width) / 2,
	    (map->background->height - thumb.height) / 2);
    free_image_structures(&thumb);
    break;
  case BACKGROUND_STRETCH:
    fprintf(stderr, "draw_background: BACKGROUND_STRETCH unsupported\n");
    break;
  }
}

/*
  sets map_node->x and map_node->y for entire thumb_list
  assumes thumbs are already fetched
*/
random_layout (map)
Map *map;
{
  MapNode *map_node;

  srand(time(0));
  for(map_node = map->thumb_list; map_node; map_node = map_node->next){
    map_node->x = nrand(map->w - map_node->thumb->width);
    map_node->y = nrand(map->h - map_node->thumb->height);
  }
}

/*
  be sure to free each host string!
*/
char *
url_to_host (buf)
char *buf;
{
  char *s, *host, *cp = strstr(buf, "://");
  if(!cp){
    fprintf(stderr, "url_to_host: can't find host in url\n");
    return NULL;
  }
  host = cp + 3;
  cp = strchr(host, '/');
  *cp = '\0';
  s = strdup(host);
  *cp = '/';
  return s;
}

int
spiral2_layout (map, grid_size)
Map *map;
int grid_size;
{
  /*
    split into multiple maps, one per host
    run spiral layout on each map
    build new thumb_list of map images
    run spiral layout again
    rebuild master thumb_list with right coords
    free each small map
   */
  Map *map_list = NULL, *map_p, *last_map, *next_map;
  MapNode *map_node, *mnp;
  char *host;

  /* split into multiple maps, one per host */
  map_node = map->thumb_list;
  while(map_node){
    host = url_to_host(map_node->image_url);
    if(!host){
      fprintf(stderr, "null host for %s\n", map_node->image_url);
      return 0;
    }
    /*fprintf(stderr, "%s: %s\n", host, map_node->image_url);*/
    for(map_p = map_list; map_p; map_p = map_p->next){
      if(!strcmp(map_p->host, host)){
	/* add map_node to end of map_p's thumb_list */
	for(mnp = map_p->thumb_list; mnp->next; mnp = mnp->next);
	mnp->next = map_node;
	map_node = map_node->next;
	mnp->next->next = NULL;
	free(host);
	break;
      }
    }
    if(!map_p){ /* make a new map at end of map_list for map_node */
      next_map = new_map();
      if(!next_map){
	fprintf(stderr, "null map for %s\n", host);
	return 0;
      }
      next_map->host = host;
      if(!map_list){
	map_list = next_map;
	last_map = map_list;
      }
      else{
	last_map->next = next_map;
	last_map = last_map->next;
      }
      next_map->thumb_list = map_node;
      map_node = map_node->next;
      next_map->thumb_list->next = NULL;
    }
  }

  for(map_p = map_list; map_p; map_p = map_p->next){
    for(mnp = map_p->thumb_list; mnp; mnp = mnp->next){
      fprintf(stderr, "%s: %s %s\n", map_p->host, mnp->image_url,
	      mnp->other_url);
    }
  }

  /* run spiral layout on each map */
  for(map_p = map_list; map_p; map_p = map_p->next){
    count_thumbs(map_p);
    calculate_rate(map_p);
    sort_thumbs(map_p, compar_rates);
    map_p->dimension_type = DIMENSION_MIN_SQUARE;
    compute_dimensions(map_p);
    spiral3_layout(map_p, 
		   MAX(map_p->max_thumb_width, map_p->max_thumb_height), 3);
    minimize_dimensions(map_p);
  }

  for(map_p = map_list; map_p; map_p = map_p->next){
    for(mnp = map_p->thumb_list; mnp; mnp = mnp->next){
      fprintf(stderr, "+++%s: %s %s\n", map_p->host, mnp->image_url,
	      mnp->other_url);
    }
  }
  /*  build new thumb_list of map images */
  map_node = NULL;
  for(map_p = map_list; map_p; map_p = map_p->next){
    mnp = new_map_node(NULL, NULL, NULL, NULL, -1, -1, map_p->w, map_p->h,
		       map_p->thumb_total,
		    /*MAX(map_p->max_thumb_width, map_p->max_thumb_height),*/
		       map_p->rate);
    if(!mnp){
      fprintf(stderr, "can't get new map_node\n");
      return 0;
    }
    if(!map_node)
      map->thumb_list = mnp;
    else
      map_node->next = mnp;
    map_node = mnp;
  }
  for(mnp = map->thumb_list; mnp; mnp = mnp->next){
    fprintf(stderr, "--> %dx%d %f\n", mnp->w, mnp->h, mnp->rate);
  }

  /*  run spiral layout again */
  count_thumbs(map);
  calculate_rate(map);
  sort_thumbs(map, compar_thumbs);
  map->dimension_type = DIMENSION_MIN_SQUARE;
  compute_dimensions(map);
  spiral3_layout(map, MAX(map->max_thumb_width, map->max_thumb_height), 20);
  /*normal_layout(map);*/
  minimize_dimensions(map);

  /*  rebuild master thumb_list with right coords
      apply offsets from each "host" thumb
      free the map thumb_list
      rebuild it from the map_lists map's thumb_lists
  */
  
  for(mnp = map->thumb_list, map_p = map_list;
      mnp && map_p;
      mnp = mnp->next, map_p = map_p->next){
    for(map_node = map_p->thumb_list; map_node; map_node = map_node->next){
      map_node->x += mnp->x; 
      map_node->y += mnp->y;
    }
  }
  free_map_node_list(map->thumb_list);
  map_node = NULL;
  for(map_p = map_list; map_p; map_p = map_p->next){
    if(!map_node){
      map->thumb_list = map_p->thumb_list;
      for(map_node = map->thumb_list; map_node->next; map_node = map_node->next);
    }
    else{
      map_node->next = map_p->thumb_list;
      for( ; map_node->next; map_node = map_node->next);
    }
    map_p->thumb_list = NULL;
  }

  /*   free each small map in map_list */
  for(map_p = map_list; map_p; ){
    next_map = map_p;
    map_p = map_p->next;
    free_map_structures(next_map);
    free(next_map);
  }
  for(mnp = map->thumb_list; mnp; mnp = mnp->next){
    fprintf(stderr, "%s %s %d %d\n", mnp->image_url, mnp->other_url,
	    mnp->x, mnp->y);
  }
  return 1;
}

/*
  overwrites r1 with bounding box of r1 and r2
*/
bounding_box (r1, r2)
Rectangle *r1, *r2;
{
  Rectangle r3;
  r3.x = MIN(r1->x, r2->x);
  r3.y = MIN(r1->y, r2->y);
  r3.w = MAX(r1->x + r1->w, r2->x + r2->w) - r3.x;
  r3.h = MAX(r1->y + r1->h, r2->y + r2->h) - r3.y;
  r1->x = r3.x;
  r1->y = r3.y;
  r1->w = r3.w;
  r1->h = r3.h;
}

/*
  directional_dimension
*/
int dd (rectangle, direction)
Rectangle *rectangle;
int direction;
{
  int rc;
  switch(direction){
  case 0:
  case 2:
    rc = rectangle->h;
    break;
  case 1:
  case 3:
    rc = rectangle->w;
    break;
  }
  return rc;
}

spiral3_layout (map, grid_size, gap)
Map *map;
int grid_size, gap;
{
  Rectangle *A, *B, *C, *D;
  int offset, i, direction = 0, direction_c = 0;
  int x, y;   /* current point */
  MapNode *mnp;

  map->w = MAX(600, map->w);
  map->h = MAX(600, map->h);
  fprintf(stderr, "map: %d %d\n", map->w, map->h);
  A = B = C = D = NULL;
  if(!map->thumb_total)
    return;
  x = map->w / 2;
  y = map->h / 2;
  for(i = 0; i < map->thumb_total; i++){
    mnp = map->map_node_array[i];
    if(!A){
      /* position first thumb lower-right of center */
      mnp->x = x;
      mnp->y = y;
      A = new_rectangle(mnp->x, mnp->y, mnp->w, mnp->h);
      if(!A){
	fprintf(stderr, "can't malloc Rectangle!\n");
	return 0;
      }
      fprintf(stderr, "A: %d %d %d %d\n", A->x, A->y, A->w, A->h);
      continue;
    }
    if(!B){
      /* position new layer lower-left of direction */
      switch(direction){
      case 0:
	mnp->x = A->x - gap - mnp->w;
	mnp->y = A->y + A->h - mnp->h;
	break;
      case 1:
	mnp->x = A->x;
	mnp->y = A->y - gap - mnp->h;
	break;
      case 2:
	mnp->x = A->x + A->w + gap;
	mnp->y = A->y;
	break;
      case 3:
	mnp->x = A->x + A->w - mnp->w;
	mnp->y = A->y + A->h + gap;
	break;
      }
      B = new_rectangle(mnp->x, mnp->y, mnp->w, mnp->h);
      if(!B){
	fprintf(stderr, "can't malloc Rectangle!\n");
	free(A);
	return 0;
      }
      fprintf(stderr, "B: %d %d %d %d\n", B->x, B->y, B->w, B->h);
      continue;
    }
    if(!C){
      C = new_rectangle(-1, -1, -1, -1);
      if(!C){
	fprintf(stderr, "can't malloc Rectangle!\n");
	free(A);
	free(B);
	return 0;
      }
      D = new_rectangle(-1, -1, -1, -1);
      if(!D){
	fprintf(stderr, "can't malloc Rectangle!\n");
	free(A);
	free(B);
	free(C);
	return 0;
      }
    }
    C->w = mnp->w;
    C->h = mnp->h;
    if(mnp->next){
      D->w = mnp->next->w;
      D->h = mnp->next->h;
    }
    else{
      D->w = 0;
      D->h = 0;
    }
    /* position current thumb */
    switch(direction){
    case 0:
      mnp->x = B->x + B->w - mnp->w;
      mnp->y = B->y - gap - mnp->h;
      break;
    case 1:
      mnp->x = B->x + B->w + gap;
      mnp->y = B->y + B->h - mnp->h;
      break;
    case 2:
      mnp->x = B->x;
      mnp->y = B->y + B->h + gap;
      break;
    case 3:
      mnp->x = B->x - gap - mnp->w;
      mnp->y = B->y;
      break;
    }
    C->x = mnp->x;
    C->y = mnp->y;
    fprintf(stderr, "\n");
    fprintf(stderr, "direction: %d\n", direction);
    fprintf(stderr, "A: %d %d %d %d\n", A->x, A->y, A->w, A->h);
    fprintf(stderr, "B: %d %d %d %d\n", B->x, B->y, B->w, B->h);
    fprintf(stderr, "C: %d %d %d %d\n", C->x, C->y, C->w, C->h);
    fprintf(stderr, "D: %d %d %d %d\n", D->x, D->y, D->w, D->h);
    if(dd(C, direction) + dd(B, direction) > dd(A, direction)){
      bounding_box(A, B); /* A = A U B */
      /* make sure B does not overlap A */
      switch(direction){
      case 0:
	B->x = A->x;
	B->y = C->y;
	B->w = C->x + C->w - B->x;
	B->h = A->y - gap - B->y;
	A->w += B->x - C->x;
	A->x -= B->x - C->x;
	break;
      case 1:
	B->x = A->x + A->w + gap;
	B->y = A->y;
	B->w = C->x + C->w - (A->x + A->w) - gap;
	B->h = C->y + C->h - A->y;
	A->h += B->y - C->y;
	A->y -= B->y - C->y;
	break;
      case 2:
	B->x = C->x;
	B->y = A->y + A->h + gap;
	B->w = A->x + A->w - B->x;
	B->h = (C->y + C->h) - (A->y + A->h) - gap;
	A->w += C->x + C->w - (B->x + B->w);
	break;
      case 3:
	B->x = C->x;
	B->y = C->y;
	B->w = A->x - B->x - gap;
	B->h = A->y + A->h - B->y;
	A->h += C->y + C->h - (B->y + B->h);
	break;
      }
      direction = ++direction_c % 4;
    }
    else
      bounding_box(B, C); /* B = B U C */
  }
  free(A);
  if(B)
    free(B);
  if(C)
    free(C);
  if(D)
    free(D);
  return 1;
}

/*
  sets map_node->x and map_node->y for entire thumb_list
  assumes thumbs are already fetched and map->thumb_total is set
  assumes map->thumbs_per_edge is set
*/
spiral_layout (map, grid_size)
Map *map;
int grid_size;
{
  MapNode *map_node;

  int bb_x1, bb_x2, bb_y1, bb_y2; /* bounding box coords */
  int segment_length, i, j, k, x, y;
  
  if(IS_EVEN(map->thumbs_per_edge)){
    i = 0;
    segment_length = 1;
    bb_x1 = bb_x2 = bb_y1 = bb_y2 = map->thumbs_per_edge / 2 * grid_size;
  }
  else{
    i = 1;
    segment_length = 2;
    bb_x1 = bb_y1 = map->thumbs_per_edge / 2 * grid_size;
    bb_x2 = bb_y2 = bb_x1 + grid_size;
    map_node = map->map_node_array[0];
    if(map_node->thumb){
      map_node->x = bb_x1 + (grid_size - map_node->thumb->width) / 2;
      map_node->y = bb_y1 + (grid_size - map_node->thumb->height) / 2;
    }
    else{
      map_node->x = bb_x1 + (grid_size - map_node->w) / 2;
      map_node->y = bb_y1 + (grid_size - map_node->h) / 2;
    }
  }
  for(;;){
    x = bb_x2;
    y = bb_y2 - grid_size;
    for(j = 0; j < 4; j++){
      for(k = 0; k < segment_length; k++){
	if(i >= map->thumb_total)
	  return;
	map_node = map->map_node_array[i];
	if(map_node->thumb){
	  map_node->x = x + (grid_size - map_node->thumb->width) / 2;
	  map_node->y = y + (grid_size - map_node->thumb->height) / 2;
	}
	else{
	  map_node->x = x + (grid_size - map_node->w) / 2;
	  map_node->y = y + (grid_size - map_node->h) / 2;
	}
	switch(j){
	case 0:
	  y -= grid_size;
	  break;
	case 1:
	  x -= grid_size;
	  break;
	case 2:
	  y += grid_size;
	  break;
	case 3:
	  x += grid_size;
	  break;
	}
	i++;
      }
      switch(j){
      case 0:
	y = bb_y1 - grid_size;
	x = bb_x2 - grid_size;
	break;
      case 1:
	y = bb_y1;
	x = bb_x1 - grid_size;
	break;
      case 2:
	y = bb_y2;
	x = bb_x1;
	break;
      }    
    }
    /* grow bounding box */
    bb_x1 -= grid_size;
    bb_x2 += grid_size;
    bb_y1 -= grid_size;
    bb_y2 += grid_size;
    segment_length += 2;
  }
}

/*
  sets map_node->x and map_node->y for entire thumb_list
  assumes thumbs are already fetched and map->thumb_total is set
*/
grid_layout (map, grid_size)
Map *map;
int grid_size;
{
  int next_x = 0, next_y = 0;
  MapNode *map_node;

  for(map_node = map->thumb_list; map_node; map_node = map_node->next){
    if(next_x == -1 && next_y == -1){
      map_node->x = map_node->y = -1;
      continue;
    }
    map_node->x = next_x + (grid_size - map_node->thumb->width) / 2;
    map_node->y = next_y + (grid_size - map_node->thumb->height) / 2;
    next_x += grid_size;
    if(next_x >= map->w){
      next_x = 0;
      next_y += grid_size;
    }
    if(next_y >= map->h)
      next_x = next_y = -1;
  }
}

normal_layout (map)
Map *map;
{
  int next_x = 0, next_y = 0;
  int width, height;
  int max_height_in_row = 0;
  MapNode *map_node;

  for(map_node = map->thumb_list; map_node; map_node = map_node->next){
    if(next_x == -1 && next_y == -1){
      map_node->x = map_node->y = -1;
      continue;
    }
    if(map_node->thumb){
      width = map_node->thumb->width;
      height = map_node->thumb->height;
    }
    else{
      width = map_node->w;
      height = map_node->h;
    }
    if(next_x + width > map->w){
      next_x = 0;
      next_y += max_height_in_row;
      max_height_in_row = 0;
    }
    if(next_y + height > map->h){
      next_x = next_y = -1;
    }
    map_node->x = next_x;
    map_node->y = next_y;
    next_x += width;
    max_height_in_row = MAX(max_height_in_row, height);
  }
}

compute_layout (map)
Map *map;
{
  switch(map->layout_type){
  case LAYOUT_PRESET:
    break;
  case LAYOUT_GRID:
  case LAYOUT_TILE:
    grid_layout(map, map->thumb.max_size);
    break;
  case LAYOUT_SPIRAL:
    spiral3_layout(map, map->thumb.max_size, 3);
    break;
  case LAYOUT_SPIRAL2:
    spiral2_layout(map, map->thumb.max_size);
    break;
  case LAYOUT_RANDOM:
    random_layout(map);
    break;
  case LAYOUT_NORMAL:
    normal_layout(map);
    break;
  }
}

rate_thumbs (list)
MapNode *list;
{
  MapNode *map_node;
  int total_colors;

  for(map_node = list; map_node; map_node = map_node->next){
    /*
    fprintf(stderr, "\nrate_thumbs: %s\n", map_node->image_url);
    fprintf(stderr, "wxh: %d, %d\n", map_node->thumb->width, map_node->thumb->height);
    */
    if(!map_node->thumb){
      fprintf(stderr, "rate_thumbs: null thumb! %s\n", map_node->image_url);
      continue;
    }
    total_colors = map_node->thumb->total_colors;
    if(ENDS_WITH(map_node->image_url, ".gif"))
       total_colors = MIN(total_colors, 256);
    /*fprintf(stderr, "total colors: %d\n", total_colors);*/
    map_node->rate = map_node->thumb->width * map_node->thumb->height;
    map_node->rate *=  ((double)total_colors / MAX_COLORS) * 100;
    if(map_node->rate < 0){
      fprintf(stderr, "rate_thumbs: negative rate: %g\n", map_node->rate);
      fprintf(stderr, "wxh: %d, %d\n", map_node->thumb->width, map_node->thumb->height);
      fprintf(stderr, "total colors: %d\n", total_colors);
    }
  }
}

count_thumbs (map)
Map *map;
{
  MapNode *map_node;
  map->thumb_total = 0;
  for(map_node = map->thumb_list; map_node; map_node = map_node->next)
    map->thumb_total++;
}

sort_thumbs (map, f)
Map *map;
int (*f)();
{
  MapNode *map_node;
  int n = 0;

  map->map_node_array = (MapNode **)malloc(map->thumb_total * sizeof(MapNode *));
  if(!map->map_node_array){
    fprintf(stderr, "sort_thumbs: can't malloc space for %d rate_nodes\n",
	    map->thumb_total);
    return;
  }
  for(map_node = map->thumb_list; map_node; map_node = map_node->next){
    map->map_node_array[n] = map_node;
    n++;
  }
  fprintf(stderr, "sort_thumbs: sorting thumbs\n");
  qsort(map->map_node_array, map->thumb_total, sizeof(MapNode *), f);
  for(n = 0; n < map->thumb_total; n++)
    fprintf(stderr, "%d: %g %d %s\n", n, map->map_node_array[n]->rate,
	    map->map_node_array[n]->max_size,
	    map->map_node_array[n]->image_url);
}

/*
  assumes thumbs are already fetched and merged
*/
rate_map (map)
Map *map;
{
  MapNode *map_node;
  int n = 0;
  double accum = 0;

  for(map_node = map->thumb_list; map_node; map_node = map_node->next){
    n++;
    accum += map_node->rate;
  }
  map->rate = accum / n;
  fprintf(stderr, "rate_map: %g\n", map->rate);
}

calculate_rate (map)
Map *map;
{
  rate_thumbs(map->thumb_list);
  rate_map(map);
}

/*
  get all the thumbs into memory
*/
get_thumbs (map)
Map *map;
{
  MapNode *mp, *prev = NULL;

  for(mp = map->thumb_list; mp; mp = mp->next){
    if(!mp->image_url)
      continue;
    mp->thumb = (Image *)malloc(sizeof(Image));
    if(!mp->thumb){
      fprintf(stderr, "get_thumbs: problem mallocing data, freeing remaining thumb_list\n");
      free_map_node_list(mp);      /* free remaining thumb_list */
      if(!prev)
	map->thumb_list = NULL;
      else
	prev->next = NULL;
      return;
    }
    bzero(mp->thumb, sizeof(Image));
    if(!make_thumb(mp->image_url, NULL, mp->thumb, map->thumb)){
      fprintf(stderr, "get_thumbs: problem making thumb, freeing current map_node\n");
      if(!prev){
	map->thumb_list = mp->next;
	free_image_structures(mp->thumb);
	free_map_node(mp);
	get_thumbs(map);
	return;
      }
      prev->next = mp->next;
      free_image_structures(mp->thumb);
      free_map_node(mp);
      mp = prev;
      continue;
    }
    mp->w = mp->thumb->width;
    mp->h = mp->thumb->height;
    prev = mp;
    fprintf(stderr, "get_thumbs: success: %s %dx%d\n", mp->image_url,
	    mp->thumb->width, mp->thumb->height);
  }
  if(!map->background)
    return;
  for(mp = map->stored_thumb_list; mp; mp = mp->next){
    if(mp->w == -1 ||  mp->h == -1){
      fprintf(stderr, "get_thumbs: bad width or height\n");
      print_map_node(mp);
      continue;
    }
    mp->thumb = new_jpg_image(mp->w, mp->h);
    if(!mp->thumb){
      fprintf(stderr, "get_thumbs: can't get Image for thumb\n");
      print_map_node(mp);
      return;
    }
    jpg_blt(map->background, mp->thumb, mp->x, mp->y, mp->w, mp->h, 0, 0);
  }
}

/*
  suffixes: .jpg .html .info
*/
build_disk_file_name (buf, stub, suffix)
char *buf, *stub, *suffix;
{
  sprintf(buf, "%s/map/%s.%s", server_disk_dir, stub, suffix);
}

build_url_file_name (buf, stub, suffix)
char *buf, *stub, *suffix;
{
  sprintf(buf, "%s/map/%s.%s", server_url_dir, stub, suffix);
}

int
verify_subdir (name)
char *name;
{
  int sub_dir_name_length, i;
  char *cp, *cp2, buf[BUFSIZE];

  if((cp = strchr(name, '/')) == NULL)
    return 1;
  /* name includes a sub dir */
  sub_dir_name_length = cp - name;
  sprintf(buf, "%s/map/", server_disk_dir);
  cp2 = buf + strlen(buf);
  cp = name;
  for(i = 0; i < sub_dir_name_length; i++)
    *cp2++ = *cp++;
  *cp2 = '\0';
  if(!verify_directory(buf)){
    fprintf(stderr, "verify_subdir: can't create subdir: %s\n", buf);
    return 0;
  }
  return 1;
}

/*
  create: only create map if one of the same name does not exist
  unique: don't overwite an existing map, use a unique name instead
  overwrite: create or overwite map with simple name
  augment: overwrite coords and add new thumbs
*/
int
verify_map_replacement (map, name, buf)
Map *map;
char *name, *buf;
{
  struct stat stat_buf;
  int rc;

  sprintf(buf, "%s/map/%s.jpg", server_disk_dir, name);
  rc = stat(buf, &stat_buf);
  sprintf(buf, "%s", name);
  switch(map->replace_type){
  case REPLACE_CREATE:
    fprintf(stderr, "verify_map_replacement: type=create\n");
    if(!rc){            /* imagemap with name exists */
      fprintf(stderr, "imagemap %s already exists\n", name);
      return 0;
    }
    break;
  case REPLACE_UNIQUE:
    fprintf(stderr, "verify_map_replacement: type=unique\n");
    if(!rc)             /* imagemap with name exists */
      sprintf(buf, "%s%d", name, time(0));
    fprintf(stderr, "making new imagemap: %s\n", buf);
    break;
  case REPLACE_OVERWRITE:
    fprintf(stderr, "verify_map_replacement: type=overwrite; making new imagemap: %s\n", buf);
    break;
  case REPLACE_AUGMENT:
    fprintf(stderr, "verify_map_replacement: type=augment; augmenting imagemap: %s\n", buf);
    break;
  }
  return 1;
}

/*
  store image url as alt text
*/
int
write_client_side_image_map (map, stub)
Map *map;
char *stub;
{
  char buf[BUFSIZE];
  MapNode *map_node;
  FILE *fp;

  build_disk_file_name(buf, stub, "html");
  fp = fopen(buf, "w");
  if(!fp){
    fprintf(stderr, "write_client_side_image_map: can't open file %s\n", buf);
    return 0;
  }
  build_url_file_name(buf, stub, "jpg");
  
  fprintf(fp, "<html>\n<head>\n<title>%s</title>\n</head>\n", stub);
  fprintf(fp, "<body bgcolor=\"#000000\" link=\"#1f9d00\" text=\"#a67e53\" vlink=\"#4f9177\">\n");
  fprintf(fp, "<center>\n");
  fprintf(fp, "<img src=\"%s\" width=%d height=%d border=0 usemap=\"#%s\">\n", buf, map->background->width, map->background->height, stub);
  fprintf(fp, "<h1>%s</h1>\n", stub);
  fprintf(fp, "</center>\n");

  fprintf(fp, "<map name=\"%s\" ", stub);
  switch(map->layout_type){
  case LAYOUT_GRID:
    fprintf(fp, "layout=grid>\n");
    break;
  case LAYOUT_SPIRAL:
    fprintf(fp, "layout=spiral>\n");
    break;
  case LAYOUT_SPIRAL2:
    fprintf(fp, "layout=spiral2>\n");
    break;
  case LAYOUT_RANDOM:
    fprintf(fp, "layout=random>\n");
    break;
  case LAYOUT_PRESET:
    fprintf(fp, "layout=preset>\n");
    break;
  case LAYOUT_NORMAL:
    fprintf(fp, "layout=normal>\n");
    break;
  }
  for(map_node = map->thumb_list; map_node; map_node = map_node->next){
    fprintf(fp, "<area href=\"%s\" ", map_node->other_url);
    if(map_node->image_url && map_node->image_url[0] != '\0')
      fprintf(fp, "image=\"%s\" ", map_node->image_url);
    if(map_node->thumb_url && map_node->thumb_url[0] != '\0')
      fprintf(fp, "thumb=\"%s\" ", map_node->thumb_url);
    if(map_node->alt_text && map_node->alt_text[0] != '\0')
      fprintf(fp, "alt=\"%s\" ", map_node->alt_text);
    else{
      if(map_node->image_url && map_node->image_url[0] != '\0')
	fprintf(fp, "alt=\"%s\" ", map_node->image_url);
    }
    if(map_node->rate != 0.0)
      fprintf(fp, "rate=%g ", map_node->rate);
    if(map_node->max_size != -1)
      fprintf(fp, "max_size=%d ", map_node->max_size);
    if(map_node->thumb)
      fprintf(fp, "coords=\"%d,%d,%d,%d\">\n",
	      map_node->x, map_node->y, 
	      map_node->x + map_node->thumb->width,
	      map_node->y + map_node->thumb->height);
    else
      fprintf(fp, "coords=\"%d,%d,%d,%d\">\n",
	      map_node->x, map_node->y, 
	      map_node->x + map_node->w,
	      map_node->y + map_node->h);
  }
  if(map->default_url && map->default_url[0] != '\0')
    fprintf(fp, "<area href=\"%s\" shape=default>\n", map->default_url);
  fprintf(fp, "</map>\n");

  fprintf(fp, "</body>\n</html>\n");
  fclose(fp);
  return 1;
}

write_info_file (map, stub)
Map *map;
char *stub;
{
  char buf[BUFSIZE];
  MapNode *map_node;
  FILE *fp;

  build_disk_file_name(buf, stub, "info");
  
  fp = fopen(buf, "w");
  if(!fp){
    fprintf(stderr, "write_info_file: can't open file %s\n", buf);
    return;
  }
  fprintf(fp, "rating: %g\n", map->rate);
  fclose(fp);
}

write_map_image_file (map, stub)
Map *map;
char *stub;
{
  char buf[BUFSIZE];
  build_disk_file_name(buf, stub, "jpg");
  write_image_to_disk(map->background, buf);
}

read_map_image_file (map, stub)
Map *map;
char *stub;
{
  char buf[BUFSIZE];
  build_disk_file_name(buf, stub, "jpg");
  fprintf(stderr, "trying to open map image file: %s\n", buf);
  map->background = read_and_decompress_image_file(buf);
}

reply_to_requestor (map, request_stream, stub)
Map *map;
FILE *request_stream;
char *stub;
{
  char buf[BUFSIZE];
  switch(map->return_type){
  case RETURN_BITS:
    fprintf(stderr, "reply_to_requestor: writing image to stream\n");
    if(map->background){
      sprintf(buf, "%s/map/%s.jpg", server_url_dir, stub);
      if(!write_image_to_stream(map->background, request_stream, buf))
	fprintf(stderr, "reply_to_requestor: problem writing image to stream\n");
    }
    else{
      sprintf(buf, "%s/map/%s.jpg", server_url_dir, stub);
      copy_full_sized_image(fileno(request_stream), request_stream, buf, 0);
    }
    break;
  case RETURN_URL:
    fprintf(request_stream, "MAKE_MAP 200 %s/map/%s.html\n\n", server_url_dir, stub);
    break;
  }
}

/* 
   assumes map->stored_thumb_list == NULL
*/
int
read_map (map, stub)
Map *map;
char *stub;
{
  FILE *fd;
  char buf[BUFSIZE], *cp;
  MapNode *mp, *prev;

  sprintf(buf, "%s/map/%s.html", server_disk_dir, stub);
  fprintf(stderr, "read_map: %s\n", buf);
  fd = fopen(buf, "r");
  if(fd == NULL)
    return 0;
  while(fgets(buf, BUFSIZE, fd) != NULL){
    if(STARTS_WITH(buf, "<area")){
      /* remove angle brackets */
      cp = buf + 1;
      while(isspace(buf[strlen(buf) - 1]))
	buf[strlen(buf) - 1] = '\0';
      buf[strlen(buf) - 1] = '\0';
      mp = get_thumb_line(map, cp);
      if(mp == NULL)
	continue;
      if(map->stored_thumb_list == NULL)
	map->stored_thumb_list = mp;
      else
	prev->next = mp;
      prev = mp;
    }
  }
  fclose(fd);
  print_map_node_list(map->stored_thumb_list);
  return 1;
}

MapNode *
thumb_list_match (list, node)
MapNode *list, *node;
{
  MapNode *mp;

  for(mp = list; mp; mp = mp->next)
    if(!strcmp(mp->image_url, node->image_url))
      return mp;
  return NULL;
}

/* 
  merge MapNodes from thumb_list to stored_thumb_list that
  have the same image_url
  assume no coordinate changes are specified
*/
void
merge_thumb_lists (map)
Map *map;
{
  MapNode *mp, *smp, *mn, *prev;

  if(map->stored_thumb_list == NULL)
    return;
  for(mp = map->thumb_list; mp; ){
    if(smp = thumb_list_match(map->stored_thumb_list, mp)){
      fprintf(stderr, "merge_thumb_lists: merging %s\n", smp->image_url);
      update_map_node(map, smp, mp);
      /* remove mp */
      if(mp == map->thumb_list){
	map->thumb_list = mp->next;
	free_map_node(mp);
	mp = map->thumb_list;
      }
      else{
	prev->next = mp->next;
	free_map_node(mp);
	mp = prev->next;
      }
    }
    else{
      prev = mp;
      mp = mp->next;
    }
  }
}

/*
  append thumb_list to the end of stored_thumb_list
  move stored_thumb_list to thumb_list
*/
void
really_merge_thumb_lists (map)
Map *map;
{
  MapNode *end;
  for(end = map->stored_thumb_list; end->next; end = end->next);
  end->next = map->thumb_list;
  map->thumb_list = map->stored_thumb_list;
  map->stored_thumb_list = NULL;
}

/*
  caller should remove new_node
*/
update_map_node (map, old_node, new_node)
Map *map;
MapNode *old_node, *new_node;
{
  /* the two nodes already have the same image_url */
  if(new_node->other_url){
    if(old_node->other_url){
      if(strcmp(new_node->other_url, old_node->other_url)){
	/* other urls differ */
	if(new_node->image_url){
	  if(strcmp(new_node->other_url, new_node->image_url)){
	    /* the new other URL is not equal to the image URL */
	    free(old_node->other_url);
	    old_node->other_url = new_node->other_url;
	    new_node->other_url = NULL;
	    map->update_html = TRUE;
	  }
	}
	else{ /* no new image_url */
	  free(old_node->other_url);
	  old_node->other_url = new_node->other_url;
	  new_node->other_url = NULL;
	  map->update_html = TRUE;
	}
      }
      /* other urls are the same; do nothing */
    }
    else{
      old_node->other_url = new_node->other_url;
      new_node->other_url = NULL;
      map->update_html = TRUE;
    }
  }
  if(new_node->thumb_url){
    if(old_node->thumb_url){
      if(strcmp(new_node->thumb_url, old_node->thumb_url)){
	/* thumb urls differ */
	free(old_node->thumb_url);
	old_node->thumb_url = new_node->thumb_url;
	new_node->thumb_url = NULL;
	map->update_html = TRUE;
      }
      /* thumb urls are the same; do nothing */
    }
    else{
      old_node->thumb_url = new_node->thumb_url;
      new_node->thumb_url = NULL;
      map->update_html = TRUE;
    }
  }
  if(new_node->alt_text){
    if(old_node->alt_text){
      if(strcmp(new_node->alt_text, old_node->alt_text)){
	/* texts differ */
	free(old_node->alt_text);
	old_node->alt_text = new_node->alt_text;
	new_node->alt_text = NULL;
	map->update_html = TRUE;
      }
      /* texts are the same; do nothing */
    }
    else{
      old_node->alt_text = new_node->alt_text;
      new_node->alt_text = NULL;
      map->update_html = TRUE;
    }
  }
  if(new_node->max_size != -1){
    if(old_node->max_size != new_node->max_size){
      old_node->max_size = new_node->max_size;
      map->update_html = TRUE;
      map->update_image = TRUE;
    }
  }
  if(new_node->rate != -1){
    if(old_node->rate != new_node->rate){
      old_node->rate = new_node->rate;
      map->update_html = TRUE;
      map->update_image = TRUE;
    }
  }
}

/*
int
need_new_image (map)
Map *map;
{
  int thumbs_per_edge, stored_thumb_total = 0, new_thumb_total = 0;
  MapNode *mp;
  double root;

  for(mp = map->stored_thumb_list; mp; mp = mp->next)
    stored_thumb_total++;
  for(mp = map->thumb_list; mp; mp = mp->next)
    new_thumb_total++;
  root = sqrt((double)stored_thumb_total);
  thumbs_per_edge = (int)(root);
  if(root - (double)thumbs_per_edge > 0)
    thumbs_per_edge++;
  if(thumbs_per_edge * thumbs_per_edge < stored_thumb_total + new_thumb_total)
    return 1;
  return 0;
}
*/

make_map (request_stream, name)
FILE *request_stream;
char *name;  /* the stub name to use when saving the map */
{
  Map *map;
  char stub[BUFSIZE];

  map = new_map();
  if(!map){
    http_error(request_stream, 400, name, NULL, "can't malloc map");
    return;
  }
  parse_map_request(request_stream, map);      /* builds map.thumb_list */
  if(!verify_subdir(name)){
    free_map_structures(map);
    http_error(request_stream, 400, name, NULL, "can't create subdirectory");
    return;
  }
  if(!verify_map_replacement(map, name, stub)){
    free_map_structures(map);
    reply_to_requestor(map, request_stream, name);
    return;
  }
  if(map->replace_type == REPLACE_AUGMENT){
    read_map(map, stub);    /* get info from existing map, if possible */
    merge_thumb_lists(map); /* merge thumbs with duplicate image URLs */
    read_map_image_file(map, stub);    /* get stored image */
    if(!map->background){
      fprintf(stderr, "can't read map image file: %s\n", stub);
      map->update_image = TRUE;
      map->update_html = TRUE;
    }
  }
  else{
    map->update_image = TRUE;
    map->update_html = TRUE;
  }
  if(map->thumb_list)
    map->update_image = TRUE;
  if(map->stored_thumb_list)
    really_merge_thumb_lists(map);
  if(!map->thumb_list){
    free_map_structures(map);
    http_error(request_stream, 400, stub, NULL, "no thumb list specified");
    return;
  }
  if(map->update_image){
    get_thumbs(map); /* trims map.thumb_list for unavailable images */
    count_thumbs(map);
    if(!map->thumb_total){
      free_map_structures(map);
      http_error(request_stream, 400, stub, NULL, "no valid thumbs specified");
      return;
    }
    calculate_rate(map);
    if(map->layout_type == LAYOUT_SPIRAL)
      sort_thumbs(map, compar_rates);
    compute_dimensions(map);
    compute_layout(map);
    if(map->dimension_type == DIMENSION_MIN_SQUARE)
      minimize_dimensions(map);
    if(map->background){
      free_image_structures(map->background);
      free(map->background);
    }
    map->background = new_jpg_image(map->w, map->h);
    if(!map->background){
      free_map_structures(map);
      http_error(request_stream, 400, stub, NULL, "can't malloc background");
      return;
    }
    draw_background(map);
    draw_map(map);
    write_info_file(map, stub);
    write_map_image_file(map, stub);
  }
  if(map->update_html){
    if(!write_client_side_image_map(map, stub)){
      free_map_structures(map);
      http_error(request_stream, 400, stub, NULL,
		 "can't write client-side imagemap");
      return;
    }
  }
  reply_to_requestor(map, request_stream, stub);
  free_map_structures(map);
  free(map);
}

free_map_structures (map)
Map *map;
{
  if(map->thumb_list){
    free_map_node_list(map->thumb_list);
    map->thumb_list = 0;
  }
  if(map->stored_thumb_list){
    free_map_node_list(map->stored_thumb_list);
    map->stored_thumb_list = 0;
  }
  if(map->background_thumb_url){
    free(map->background_thumb_url);
    map->background_thumb_url = 0;
  }
  if(map->default_url){
    free(map->default_url);
    map->default_url = 0;
  }
  if(map->background){
    free_image_structures(map->background);
    free(map->background);
    map->background = 0;
  }
  if(map->map_node_array){
    free(map->map_node_array);
    map->map_node_array = 0;
  }
  if(map->host){
    free(map->host);
    map->host = 0;
  }
}

/*
  get background jpg
  for each node in list, 
    get thumb
    convert gif thumbs to jpg
    draw thumb
  write out final jpg
*/
draw_map (map)
Map *map;
{
  MapNode *map_node;

  if(!map)
    return;
  fprintf(stderr, "draw_map: dimensions: %dx%d\n", map->w, map->h);
  for(map_node = map->thumb_list; map_node; map_node = map_node->next){
    fprintf(stderr, "draw_map: %s %s %d %d %d %d\n",
	    map_node->image_url, map_node->other_url,
	    map_node->x, map_node->y,
	    map_node->thumb->width, map_node->thumb->height);
    if(map_node->x != -1 && map_node->y != -1){
      if(map_node->thumb->type == IMAGE_FORMAT_GIF)
	gif_to_jpg(map_node->thumb);
      /* draw thumb into map */
      jpg_blt(map_node->thumb, map->background,
	      0, 0, map_node->thumb->width, map_node->thumb->height,
	      map_node->x, map_node->y);
    }
    /* free thumbnail jpg image data */
    free_image_structures(map_node->thumb);
  }
}

fill_image (image_data, w, h, red, green, blue)
unsigned char *image_data;
int w, h, red, green, blue;
{
  int x, y;

  for(y = 0; y < h; y++)
    for(x = 0; x < w; x++){
      *image_data++ = (unsigned char)red;
      *image_data++ = (unsigned char)green;
      *image_data++ = (unsigned char)blue;
    }
}

typedef struct rated_map {
  double rate;
  char *name;
} RatedMap;

free_rated_map (rm)
RatedMap *rm;
{
  if(rm->name)
    free(rm->name);
  free(rm);
}

RatedMap *
new_rated_map (name)
char *name;   /* the map's stub_name */
{
  int rc;
  FILE *fp;
  char buf[BUFSIZE];
  double rate;
  RatedMap *rm = (RatedMap *)malloc(sizeof(RatedMap));

  /*fprintf(stderr, "new_rated_map: %s\n", name);*/
  if(!rm){
    fprintf(stderr, "new_rated_map: can't malloc space for RatedMap\n");
    return NULL;
  }
  rm->name = (char *)malloc(strlen(name) + 1);
  if(!rm->name){
    free(rm);
    fprintf(stderr, "new_rated_map: can't malloc space for RatedMap name\n");
    return NULL;
  }
  strncpy(rm->name, name, strlen(name) + 1);

  sprintf(buf, "%s/map/%s.info", server_disk_dir, name);
  fp = fopen(buf, "r");
  if(!fp){
    free(rm->name);
    free(rm);
    fprintf(stderr, "new_rated_map: can't open file %s\n", buf);
    return NULL;
  }
  while(fgets(buf, BUFSIZE, fp)){
    if(!STARTS_WITH(buf, "rating:"))
      continue;
    rc = sscanf(buf, "%*s %lg\n", &rate);
    if(rc != 1){
      free(rm->name);
      free(rm);
      fclose(fp);
      fprintf(stderr, "new_rated_map: can't parse rate in file %s\n", buf);
      return NULL;
    }
    fclose(fp);
    rm->rate = rate;
    return rm;
  }
  fprintf(stderr, "new_rated_map: can't read rate in file %s\n", buf);
  free(rm->name);
  free(rm);
  fclose(fp);
  return NULL;
}

/* 
   sort to decreasing order
*/
int
compar (arg1, arg2)
RatedMap **arg1, **arg2;
{
  if((*arg1)->rate > (*arg2)->rate)
    return -1;
  if((*arg1)->rate < (*arg2)->rate)
    return 1;
  return 0;
}

void
add_rated_map (dir_name, file_name, rated_maps, index, limit) 
char *dir_name, *file_name;
RatedMap **rated_maps;
int *index, limit;
{
  char buf[BUFSIZE], buf2[BUFSIZE], *cp;
  RatedMap *rm;
  int j;
  /*
  fprintf(stderr, "add_rated_map: dir_name: %s, file_name: %s\n",
	  dir_name, file_name);
  */
  strncpy(buf, file_name, BUFSIZE);
  cp = strrchr(buf, '.');
  *cp = '\0';

  sprintf(buf2, "%s/map", server_disk_dir);
  if(!(strcmp(dir_name, buf2)))
    rm = new_rated_map(buf);
  else{
    cp = dir_name + strlen(server_disk_dir) + 5; /* strlen("/map/") */
    sprintf(buf2, "%s/%s", cp, buf);
    rm = new_rated_map(buf2);
  }
  if(!rm)
    return;
  if(*index < limit){
    rated_maps[*index] = rm;
    *index = *index + 1;
    return;
  }
  for(j = 0; j < limit; j++){
    if(rm->rate > rated_maps[j]->rate){
      free_rated_map(rated_maps[j]);
      rated_maps[j] = rm;
      return;
    }
  }
  if(j == limit)
    free_rated_map(rm);
  return;
}

int
rated_map_dir (dir_name, rated_maps, index, limit) 
char *dir_name;
RatedMap **rated_maps;
int *index, limit;
{
  int rc;
  struct stat stat_buf;
  char buf[BUFSIZE], *cp;
  DIR *dirp;
#ifdef SOLARIS
  struct dirent *dp;
#else
  struct direct *dp;
#endif

  fprintf(stderr, "rated_map_dir: directory: %s\n", dir_name);
  dirp = opendir(dir_name);
  if(!dirp){
    fprintf(stderr, "rated_map_dir: can't open directory: %s\n", dir_name);
    return 0;
  }
  while(dp = readdir(dirp)){
    sprintf(buf, "%s/%s", dir_name, dp->d_name);
    rc = stat(buf, &stat_buf);
    if(rc == -1){
      fprintf(stderr, "rated_map_dir: can't stat %s\n", buf);
      continue;
    }
    if((stat_buf.st_mode & S_IFMT) == S_IFDIR){
      if(!strcmp(dp->d_name, "."))
	continue;
      if(!strcmp(dp->d_name, ".."))
	continue;
      fprintf(stderr, "rated_map_dir: dir: %s, dir_name: %s, recursing\n",
	      dp->d_name, buf);
      rated_map_dir(buf, rated_maps, index, limit);
    }
    else if(ENDS_WITH(dp->d_name, ".info"))
      add_rated_map(dir_name, dp->d_name, rated_maps, index, limit);
  }
  closedir(dirp);
  return 1;
}

get_rated_maps (request_stream, s)
FILE *request_stream;
char *s;
{
  int limit, j, index = 0;
  char buf[BUFSIZE], *cp;
  RatedMap **rated_maps = 0;

  fprintf(stderr, "get_rated_maps: %s\n", s);
  if(sscanf(s, "%d", &limit) != 1){
    fprintf(stderr, "get_rated_maps: can't parse arg as an int: %s\n", s);
    return;
  }
  rated_maps = (RatedMap **)malloc(limit * sizeof(RatedMap *));
  if(!rated_maps){
    fprintf(stderr, "get_rated_maps: can't malloc array for rated_maps\n");
    return;
  }
  sprintf(buf, "%s/map", server_disk_dir);
  if(!rated_map_dir(buf, rated_maps, &index, limit)){
    fprintf(stderr, "get_rated_maps: can't open directory\n");
    return;
  }
  sprintf(buf, "%s/map/", server_url_dir);
  cp = buf + strlen(buf);
  fprintf(stderr, "get_rated_maps: sorting maps, index: %d\n", index);
  qsort(rated_maps, index, sizeof(RatedMap *), compar);
  for(j = 0; j < index; j++){
    sprintf(cp, "%s.html", rated_maps[j]->name);
    fprintf(stderr, "%s %s %g\n", rated_maps[j]->name, buf, rated_maps[j]->rate);
    fprintf(request_stream, "%s %s %g\n", rated_maps[j]->name, buf, rated_maps[j]->rate);
    free_rated_map(rated_maps[j]);
  }
  free(rated_maps);  
}

get_map_info (request_stream, stub)
FILE *request_stream;
char *stub;
{
  FILE *fd;
  char buf[BUFSIZE], buf2[BUFSIZE], *image_url, *other_url, *cp, *coords, *end;
  Image image;
  struct stat stat_buf;
  int rc;

  image.jpg_in = 0;
  image.jpg_image_data = 0;
  image.gdp = 0;
  image.type = IMAGE_FORMAT_JPG;
  
  fprintf(stderr, "get_map_info: GET_MAP_INFO %s\n", stub);
  sprintf(buf, "%s/map/%s.jpg", server_disk_dir, stub);
  rc = stat(buf, &stat_buf);

  if(rc == -1){
    fprintf(stderr, "get_map_info: can't stat %s\n", buf);
    http_error(request_stream, 400, stub, NULL, "can't stat image file");
    return;
  }
  fd = fopen(buf, "r");
  if(fd == NULL){
    http_error(request_stream, 400, stub, NULL, "can't open image file");
    return;
  }
  if(!get_image_header_read_off_data(&image, fd)){
    http_error(request_stream, 400, stub, NULL, "can't parse file");
    fclose(fd);
    return;
  }
  fclose(fd);
  strncpy(image.mime_type, "image/jpeg", MIME_TYPE_LENGTH);
  image.content_length = stat_buf.st_size;

  sprintf(buf, "%s/map/%s.info", server_disk_dir, stub);
  fd = fopen(buf, "r");
  if(fd == NULL){
    http_error(request_stream, 400, stub, NULL, "can't open info file");
    free_image_structures(&image);
    return;
  }
  buf2[0] = '\0';
  while(fgets(buf2, BUFSIZE, fd) != NULL)
    if(STARTS_WITH(buf2, "rating:"))
      break;
  fclose(fd);

  sprintf(buf, "%s/map/%s.html", server_disk_dir, stub);
  fd = fopen(buf, "r");
  if(fd == NULL){
    http_error(request_stream, 400, stub, NULL, "can't open map file");
    free_image_structures(&image);
    return;
  }
  fprintf(request_stream, "GET_MAP_INFO 200 %s\n\n", stub);
  print_image_info(request_stream, &image);
  free_image_structures(&image);
  if(buf2[0] != '\0')
    fputs(buf2, request_stream);
  while(fgets(buf, BUFSIZE, fd) != NULL){
    if(STARTS_WITH(buf, "<area")){
      /* remove angle brackets, add the colon */
      strncpy(buf, "area:", 5);
      /*  cp = buf + 1; */
      while(isspace(buf[strlen(buf) - 1]))
	buf[strlen(buf) - 1] = '\0';
      buf[strlen(buf) - 1] = '\0';
      fprintf(request_stream, "%s\n", buf);
    }
  }
  fclose(fd);
}
