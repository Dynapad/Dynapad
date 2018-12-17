#include "escheme.h"
#include <dtlib.h>
#include <dtmouse.h>
#include <stdio.h>

#define USER 0

static void print_mouseaction(dt_mouse_action_list *malp, dt_user_id user);

static dt_table *tp;
static dt_user_id user_id = USER;
static dt_device dtdev;
static dt_frame *fp[DT_MAX_USERS];
static dt_event *ep;
static dt_io_error ioerr;
static dt_mouse_action_list *malp;

static Scheme_Object *
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
    Scheme_Object *result = scheme_false;
    int i;

    if (!dt_device_read(dtdev, tp, fp, &ioerr)) {
        scheme_warning("arf: dt_device_read failed");
    }

    result = scheme_null;

    for (i = 0; i < DT_MAX_USERS; i++) {
        dt_fi_interpret_frame(ep, fp[i]);
        /*
        printf("%1d: [%4d,%4d] %d\n", i, (int)ep->max_point.x,
	       (int)ep->max_point.y, (int)ep->touch);
        */
	if (ep->touch) {
	    result = scheme_make_pair(
	      scheme_make_pair(scheme_make_integer(i),
	      scheme_make_pair(scheme_make_integer(ep->max_point.x),
	      scheme_make_pair(scheme_make_integer(ep->max_point.y),
	      scheme_make_pair(scheme_make_integer(ep->box_ul.x),
	      scheme_make_pair(scheme_make_integer(ep->box_ul.y),
	      scheme_make_pair(scheme_make_integer(ep->box_lr.x),
	      scheme_make_pair(scheme_make_integer(ep->box_lr.y),
		      scheme_null))))))),
	      result);
	}
        dt_me_update(malp, ep);
        /*
        if (malp->n > 0) {
            fprintf(stderr, "%d %d\n", i, malp->n);
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
  return scheme_make_string("Hello DiamondTouch!");
}

Scheme_Object *scheme_initialize(Scheme_Env *env)
{
    dt_user_id user_id = USER;
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

  return scheme_reload(env);
}

Scheme_Object *scheme_module_name()
{
  /* This extension doesn't define a module: */
  return scheme_false;
}
