/* $Id: dtucsd.h,v 1.1.1.1 2005/10/09 04:28:49 stanonik Exp $ */

/* include dt.h first */

typedef struct {
  float beg;  /* beginning of interval */
  float end;  /* end of interval */
  float ctr;  /* center ("of mass") */
  unsigned char valid;  /* flag */
} dt_1dtouch;

typedef enum {ALIVE, DEAD, EOL} status_t;
typedef unsigned short int touch_id_t;

typedef struct {
  dt_1dtouch x;
  dt_1dtouch y;
  touch_id_t touch_id;
  dt_user_id user_id;
  status_t status;
} dt_2dtouch;

void d4_init(void);
void d4_uninit(void);
dt_1dtouch* d4_get_x_touches(dt_user_id user_id, dt_frame* fp);
dt_1dtouch* d4_get_y_touches(dt_user_id user_id, dt_frame* fp);
dt_2dtouch* d4_get_2dtouches(dt_user_id user_id, dt_frame* fp);
