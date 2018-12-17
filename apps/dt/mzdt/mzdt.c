/* $Id: mzdt.c,v 1.1.1.1 2005/10/09 04:28:49 stanonik Exp $ */
#include <stdio.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/un.h>

#include "dt.h"
#include "libdt.h"
#include "dtmouse.h"
#include "dtucsd.h"
#include "escheme.h"

#define USER_ID 0
#define SUN_DIR "/tmp/dt_sockets"
#define TOUCHD_SUN_PATH "/tmp/dt_sockets/touchd_socket"
#define MZDT_SUN_PATH "/tmp/dt_sockets/mzdt_socket"

static dt_table *tp;
static dt_device dtdev;
static dt_frame *fp[DT_MAX_USERS];
static dt_event *ep;
static dt_io_error ioerr;
static dt_mouse_action_list *malp;

static int sock;
static struct sockaddr_un my_addr, touchd_addr;
static dt_2dtouch touches[64];  /* frighteningly arbitrary */

static Scheme_Object* scheme_make_list(int n, ...);
static void print_mouseaction(dt_mouse_action_list *malp, dt_user_id user);

  static Scheme_Object *
sch_dt_touches(int argc, Scheme_Object **argv)
{
  int len;
  Scheme_Object *result = scheme_false;

  /* Send a zero-byte message (as a trivial query). */
  if (sendto(sock, "", 0, 0, (const struct sockaddr*)&touchd_addr,
        sizeof(struct sockaddr_un)) < 0)
    perror("sendto");

  /* Read the response and make the return value. */
  len = read(sock, &touches, sizeof(touches));
  if (len % sizeof(*touches) == 0)
  {
    int n_touches, i;
    n_touches = len / sizeof(*touches);
    result = scheme_null;
    for (i = n_touches-1; i >= 0; --i)
      result = scheme_make_pair(
          scheme_make_list(8,
            scheme_make_integer(touches[i].touch_id),
            scheme_make_integer(touches[i].user_id),
            scheme_make_float(touches[i].x.ctr),
            scheme_make_float(touches[i].y.ctr),
            scheme_make_float(touches[i].x.beg),
            scheme_make_float(touches[i].y.end),
            scheme_make_float(touches[i].x.end),
            scheme_make_float(touches[i].y.beg)),
          result);
  }
  else
    fprintf(stderr, "malformed response\n");

  return result;
}

/* Given n Scheme_Object*s, returns a (Scheme_Object) list of them. */
  Scheme_Object*
scheme_make_list(int n, ...)
{
  va_list ap;
  Scheme_Object** elts;
  Scheme_Object* result;
  int i;

  if ((elts = malloc(n * sizeof(*elts))) == NULL)
  {
    perror("malloc");
    return scheme_false;
  }

  /* Copy varargs into local array for random access. */
  va_start(ap, n);
  for (i = 0; i < n; ++i)
    elts[i] = va_arg(ap, Scheme_Object*);
  va_end(ap);

  /* Build a list from the end back. */
  result = scheme_null;
  for (i = n-1; i >= 0; --i)
    result = scheme_make_pair(elts[i], result);

  free(elts);

  return result;
}

  static Scheme_Object*
mouseaction(dt_mouse_action_list *malp, dt_user_id user)
{
  Scheme_Object *result = scheme_null;
  int i;

  if (malp->n == 0) return result;

  for (i = 0; i < malp->n; i++) {
    switch(malp->action[i].action) {
      case DT_MOUSE_ACTION_NONE:
        fprintf(stderr, " none");
        break;
      case DT_MOUSE_ACTION_MOVE_ABS:
        fprintf(stderr, " move_abs");
        break;
      case DT_MOUSE_ACTION_MOVE_REL:
        fprintf(stderr, " move_rel");
        break;
      case DT_MOUSE_ACTION_LEFT_DOWN:
        fprintf(stderr, " left_down");
        break;
      case DT_MOUSE_ACTION_LEFT_UP:
        fprintf(stderr, " left_up");
        break;
      case DT_MOUSE_ACTION_MIDDLE_DOWN:
        fprintf(stderr, " middle_down");
        break;
      case DT_MOUSE_ACTION_MIDDLE_UP:
        fprintf(stderr, " middle_up");
        break;
      case DT_MOUSE_ACTION_RIGHT_DOWN:
        fprintf(stderr, " right_down");
        break;
      case DT_MOUSE_ACTION_RIGHT_UP:
        fprintf(stderr, " right_up");
        break;
      case DT_MOUSE_ACTION_WHEEL:
        fprintf(stderr, " wheel");
        break;
    }
  }
  return result;
}

  static Scheme_Object *
sch_dtread(int argc, Scheme_Object **argv)
{
  Scheme_Object *result;
  int user;

  if (!dt_device_read(dtdev, tp, fp, &ioerr)) {
    scheme_warning("arf: dt_device_read failed");
  }

  result = scheme_null;

  for (user = 0; user < DT_MAX_USERS; ++user) {
    dt_fi_interpret_frame(ep, fp[user]);
    /*
       printf("%1d: [%4d,%4d] %d\n", user, (int)ep->max_point.x,
       (int)ep->max_point.y, (int)ep->touch);
     */
    if (ep->touch) {
      result = scheme_make_pair(
          scheme_make_list(7,
            scheme_make_integer(user),
            scheme_make_integer(ep->max_point.x),
            scheme_make_integer(ep->max_point.y),
            scheme_make_integer(ep->box_ul.x),
            scheme_make_integer(ep->box_ul.y),
            scheme_make_integer(ep->box_lr.x),
            scheme_make_integer(ep->box_lr.y)),
          result);
    }
    dt_me_update(malp, ep);
    /*
       if (malp->n > 0) {
       fprintf(stderr, "%d %d\n", user, malp->n);
       }
     */
  }

  return result;
}

  static Scheme_Object *
sch_dtmode(int argc, Scheme_Object **argv)
{
  Scheme_Object *result = scheme_false;
  int mode;

  if (argc == 0) {
    return scheme_make_integer(dt_me_get_mode());
  }

  if (!SCHEME_INTP(argv[0])) {
    scheme_wrong_type("sch_dtmode", "mouse emulation mode", 0, argc, argv);
  }
  mode = SCHEME_INT_VAL(argv[0]);
  if (mode < 0 || mode >= 7) {
    scheme_warning("sch_dtmode: mode must be 0 - 6, was given ", argv[0]);
  }
  dt_me_set_mode(mode);
  return scheme_true;
}

Scheme_Object *scheme_reload(Scheme_Env *env)
{
  scheme_add_global("dtread",
      scheme_make_prim_w_arity(sch_dtread, "dtread", 0, 0), env);
  scheme_add_global("dtmode",
      scheme_make_prim_w_arity(sch_dtmode, "dtmode", 0, 1), env);
  scheme_add_global("dt-touches",
      scheme_make_prim_w_arity(sch_dt_touches, "dt-touches", 0, 0), env);
  return scheme_make_string("Hello DiamondTouch!");
}

Scheme_Object *scheme_initialize(Scheme_Env *env)
{
  dt_user_id user_id = USER_ID;
  int i;

  tp = dt_get_table(user_id);

  if (tp->status != DT_TABLE_STATUS_OK) {
    scheme_warning("arf: can't find DiamondTouch device");
  }

  dtdev = dt_device_open(tp, &(tp->devname[0][0]));
  if (dtdev == DT_BAD_DEVICE) {
    scheme_warning("arf: can't open device \"%s\"",
        &(tp->devname[0][0]));
  }

  for (i = 0; i < DT_MAX_USERS; i++) {
    fp[i] = dt_frame_new(tp);         /* Make a dt_frame. */
    if (!fp[i]) {
      scheme_warning("arf: can't make dt_frame");
    }
  }

  for (i = 0; i < DT_MAX_USERS; i++) {
    dt_fi_user_initialize((dt_user_id)i);
    dt_me_initialize((dt_user_id)i);
  }

  ep = (dt_event *)calloc(sizeof(dt_event), 1);
  if (!ep) {
    scheme_warning("arf: can't make dt_event");
  }

  malp = (dt_mouse_action_list *)calloc(sizeof(dt_mouse_action_list), 1);
  if (!malp) {
    scheme_warning("arf: calloc malp failed");
  }

  sock = socket(AF_UNIX, SOCK_DGRAM, 0);
  if (sock < 0)
    perror("socket");

  /* socket stuff for touchd */
  my_addr.sun_family = AF_UNIX;
  strcpy(my_addr.sun_path, MZDT_SUN_PATH);

  unlink(MZDT_SUN_PATH);  /* hazard with multiple simultaneous invocations? */

  /* age-old trade-off: convenience vs. security */
  umask((mode_t)0000);
  mkdir(SUN_DIR, (mode_t)0777);

  if (bind(sock, (struct sockaddr *)&my_addr, sizeof(struct sockaddr_un)))
    perror("bind");

  touchd_addr.sun_family = AF_UNIX;
  strcpy(touchd_addr.sun_path, TOUCHD_SUN_PATH);

  return scheme_reload(env);
}

Scheme_Object *scheme_module_name()
{
  /* This extension doesn't define a module: */
  return scheme_false;
}
