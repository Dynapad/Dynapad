#include <dtlib.h>
#include <dtmouse.h>
#include <stdio.h>

#define USER 0

void print_mouseaction(dt_mouse_action_list *malp, dt_user_id user);

main(argc, argv)
    int argc;
    char **argv;
{
    dt_table *tp;
    dt_user_id user_id = USER;
    dt_device dtdev;
    dt_frame *fp[DT_MAX_USERS];
    dt_event *ep;
    dt_io_error ioerr;
    int i;
    dt_mouse_action_list *malp;
    int mode;

    if (argc == 2) {
	mode = atoi(argv[1]);
	if (mode < 0 || mode >= DT_NUM_MOUSE_MODES) {
	    fprintf(stderr, "mode must be 0 to %d, was %s\n",
	        DT_NUM_MOUSE_MODES-1, argv[1]);
	    exit(1);
	}
	dt_me_set_mode(mode);
    }

    tp = dt_get_table(user_id);

    if (tp->status != DT_TABLE_STATUS_OK) {
        fprintf(stderr, "arf: can't find DiamondTouch device\n");
        exit(1);
    }

    dtdev = dt_device_open(tp, &(tp->devname[0][0]));
    if (dtdev == DT_BAD_DEVICE) {
        fprintf(stderr, "arf: can't open device \"%s\"\n",
                &(tp->devname[0][0]));
        exit(1);
    }

    for (i = 0; i < DT_MAX_USERS; i++) {
      fp[i] = dt_frame_new(tp);         /* Make a dt_frame. */
      if (!fp[i]) {
          fprintf(stderr, "arf: can't make dt_frame\n");
  	exit(1);
      }
    }

    for (i = 0; i < DT_MAX_USERS; i++) {
        dt_fi_user_initialize((dt_user_id)i);
        dt_me_initialize((dt_user_id)i);
    }

    ep = (dt_event *)calloc(sizeof(dt_event), 1);
    if (!ep) {
        fprintf(stderr, "arf: can't make dt_event\n");
	exit(1);
    }

    malp = (dt_mouse_action_list *)calloc(sizeof(dt_mouse_action_list), 1);
    if (!malp) {
	fprintf(stderr, "arf: calloc malp failed\n");
	exit(1);
    }

    while(1) {
        if (!dt_device_read(dtdev, tp, fp, &ioerr)) {
            fprintf(stderr, "arf: dt_device_read failed\n");
            exit(1);
        }

        for (i = 0; i < DT_MAX_USERS; i++) {
            dt_fi_interpret_frame(ep, fp[i]);
            dt_me_update(malp, ep);
	    if (malp->n) {
	        printf("%1d: [%4d,%4d] %d  ", i, (int)ep->max_point.x,
	           (int)ep->max_point.y, (int)ep->touch);
	        print_mouseaction(malp, (dt_user_id)i);
	        printf("\n");
	     } else if (ep->touch) {
	        printf("%1d: [%4d,%4d] %d  ", i, (int)ep->max_point.x,
	           (int)ep->max_point.y, (int)ep->touch);
	        printf("\n");
	     }
	    /*
            if (malp->n > 0) {
	        fprintf(stderr, "%d %d\n", i, malp->n);
            }
	    */
        }
    }

    free(ep);
    for (i = 0; i < DT_MAX_USERS; i++) {
      dt_frame_free(fp[i]);
    }
    for (i = 0; i < DT_MAX_USERS; i++) {
        dt_fi_user_uninitialize((dt_user_id)i);
    }

    if (dt_device_close(tp, dtdev) < 0) {
        fprintf(stderr, "arf: dt_device_close failed\n");
        exit(1);
    }
}

void
print_mouseaction(dt_mouse_action_list *malp, dt_user_id user)
{
    int i;

    if (malp->n == 0) return;

    printf("%d:", user);
    for (i = 0; i < malp->n; i++) {
	switch(malp->action[i].action) {
	    case DT_MOUSE_ACTION_NONE:
		printf(" none");
		break;
	    case DT_MOUSE_ACTION_MOVE_ABS:
		printf(" move_abs");
		break;
	    case DT_MOUSE_ACTION_MOVE_REL:
		printf(" move_rel");
		break;
	    case DT_MOUSE_ACTION_LEFT_DOWN:
		printf(" left_down");
		break;
	    case DT_MOUSE_ACTION_LEFT_UP:
		printf(" left_up");
		break;
	    case DT_MOUSE_ACTION_MIDDLE_DOWN:
		printf(" middle_down");
		break;
	    case DT_MOUSE_ACTION_MIDDLE_UP:
		printf(" middle_up");
		break;
	    case DT_MOUSE_ACTION_RIGHT_DOWN:
		printf(" right_down");
		break;
	    case DT_MOUSE_ACTION_RIGHT_UP:
		printf(" right_up");
		break;
	    case DT_MOUSE_ACTION_WHEEL:
		printf(" wheel");
		break;
	}
    }
}
