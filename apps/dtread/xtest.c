#include <dtlib.h>
#include <dtmouse.h>
#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/extensions/XTest.h>

#define USER 0

int debug = 0;

int XMAX, YMAX;

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
    int i, j;
    dt_mouse_action_list *malp;
    int mode;
    Display *display;
    int screen_num;

    char *display_name = NULL;
    unsigned int display_width, display_height;
    int x, y; 	/* window position */
    int event_base, error_base, major, minor;
    
    /* connect to X server */
    if ((display=XOpenDisplay(display_name)) == NULL)
    {
    	fprintf(stderr, "arf: cannot connect to X server %s\n", 
    	    XDisplayName(display_name));
    	exit(-1);
    }
    if (XTestQueryExtension(display, &event_base, &error_base, &major, &minor) == False) {
	    fprintf(stderr, "arf: XTest extension not supported\n");
	    exit(-1);
	}
    
    /* get screen size from display structure macro */
    screen_num = DefaultScreen(display);
    display_width = DisplayWidth(display, screen_num);
    display_height = DisplayHeight(display, screen_num);

    if (argc == 2) {
	mode = atoi(argv[1]);
	if (mode < 0 || mode >= DT_NUM_MOUSE_MODES) {
	    fprintf(stderr, "mode must be 0 to %d, was %s\n",
	        DT_NUM_MOUSE_MODES-1, argv[1]);
	    exit(-1);
	}
	dt_me_set_mode(mode);
    }

    tp = dt_get_table(user_id);

    if (tp->status != DT_TABLE_STATUS_OK) {
        fprintf(stderr, "arf: can't find DiamondTouch device\n");
        exit(-1);
    }
    XMAX = tp->xn * DT_INTER;
    YMAX = tp->yn * DT_INTER;

    dtdev = dt_device_open(tp, &(tp->devname[0][0]));
    if (dtdev == DT_BAD_DEVICE) {
        fprintf(stderr, "arf: can't open device \"%s\"\n",
                &(tp->devname[0][0]));
        exit(-1);
    }

    for (i = 0; i < DT_MAX_USERS; i++) {
      fp[i] = dt_frame_new(tp);         /* Make a dt_frame. */
      if (!fp[i]) {
          fprintf(stderr, "arf: can't make dt_frame\n");
  	exit(-1);
      }
    }

    for (i = 0; i < DT_MAX_USERS; i++) {
        dt_fi_user_initialize((dt_user_id)i);
        dt_me_initialize((dt_user_id)i);
    }

    ep = (dt_event *)calloc(sizeof(dt_event), 1);
    if (!ep) {
        fprintf(stderr, "arf: can't make dt_event\n");
	exit(-1);
    }

    malp = (dt_mouse_action_list *)calloc(sizeof(dt_mouse_action_list), 1);
    if (!malp) {
	fprintf(stderr, "arf: calloc malp failed\n");
	exit(-1);
    }

    while(1) {
        if (!dt_device_read(dtdev, tp, fp, &ioerr)) {
            fprintf(stderr, "arf: dt_device_read failed\n");
            exit(-1);
        }

        for (i = 0; i < DT_MAX_USERS; i++) {
            dt_fi_interpret_frame(ep, fp[i]);
            dt_me_update(malp, ep);
	    if (malp->n) {
	        if (debug) printf("%1d: [%4d,%4d] %d  ", i, (int)ep->max_point.x,
	            (int)ep->max_point.y, (int)ep->touch);
	         if (debug) print_mouseaction(malp, (dt_user_id)i);
	         if (debug) printf("\n");
		 for (j = 0; j < malp->n; j++) {
		     switch (malp->action[j].action) {
		         case DT_MOUSE_ACTION_MOVE_ABS: {
		             x = malp->action[j].data.move.x;
		             y = YMAX - malp->action[j].data.move.y;
	                     x = display_width*((float)x/XMAX);
	                     y = display_height*((float)y/YMAX);
	                     if (debug) printf("fake motion %d %d\n", x, y);
	                     XTestFakeMotionEvent(display, screen_num, x, y, CurrentTime);
		             XFlush(display);
			     break;
		         }
		         case DT_MOUSE_ACTION_LEFT_DOWN: {
	                     if (debug) printf("fake left button down\n");
		             XTestFakeButtonEvent(display, 1, True, CurrentTime);
		             XFlush(display);
			     break;
		         }
		         case DT_MOUSE_ACTION_LEFT_UP: {
	                     if (debug) printf("fake left button up\n");
		             XTestFakeButtonEvent(display, 1, False, CurrentTime);
		             XFlush(display);
			     break;
		         }
		         case DT_MOUSE_ACTION_MIDDLE_DOWN: {
	                     if (debug) printf("fake middle button down\n");
		             XTestFakeButtonEvent(display, 2, True, CurrentTime);
		             XFlush(display);
			     break;
		         }
		         case DT_MOUSE_ACTION_MIDDLE_UP: {
	                     if (debug) printf("fake middle button up\n");
		             XTestFakeButtonEvent(display, 2, False, CurrentTime);
		             XFlush(display);
			     break;
		         }
		         case DT_MOUSE_ACTION_RIGHT_DOWN: {
	                     if (debug) printf("fake right button down\n");
		             XTestFakeButtonEvent(display, 3, True, CurrentTime);
		             XFlush(display);
			     break;
		         }
		         case DT_MOUSE_ACTION_RIGHT_UP: {
	                     if (debug) printf("fake right button up\n");
		             XTestFakeButtonEvent(display, 3, False, CurrentTime);
		             XFlush(display);
			     break;
		         }
		    }
		}
	     } else if (ep->touch) {
	         if (debug) printf("%1d: [%4d,%4d] %d  ", i, (int)ep->max_point.x,
	            (int)ep->max_point.y, (int)ep->touch);
	         if (debug) printf("\n");
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
        exit(-1);
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
