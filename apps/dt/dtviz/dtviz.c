/* $Id: dtviz.c,v 1.1.1.1 2005/10/09 04:28:49 stanonik Exp $
 *
 * Copyright (C) 1998 Hannu Mallat.
 * Copyright (C) 2003 Dana Dahlstrom.
 */

#include <X11/Xutil.h>
#include "libdt.h"
/* #include "dtmouse.h" */
#include "dtucsd.h"
#include "screenhack.h"

#ifdef HAVE_DOUBLE_BUFFER_EXTENSION
# include "xdbe.h"
#endif /* HAVE_DOUBLE_BUFFER_EXTENSION */


char *progclass = "DiamondTouch Viz";

char *defaults [] =
{
  ".background: black",
  ".foreground: white",
  "*shades: 256",  /* number of shades used for each axis */
  "*delay: 0",  /* or something */
  "*mono: false",  /* monochrome, not very much fun */
  "*rawsignals: true",  /* draw the raw signals */
  "*crosshairs: false",  /* draw crosshairs */
  "*touchlabels: false",  /* draw touch labels */
  "*bboxes: false",  /* draw bounding boxes around touches */
  "*doubleBuffer: True",

#ifdef HAVE_DOUBLE_BUFFER_EXTENSION
  "*useDBE: True",  /* use double buffering extension */
#endif /* HAVE_DOUBLE_BUFFER_EXTENSION */

  0
};

XrmOptionDescRec options [] =
{
  { "-shades", ".shades", XrmoptionSepArg, 0 }, 
  { "-delay", ".delay", XrmoptionSepArg, 0 },
  { "-mono", ".mono", XrmoptionNoArg, "True" },
  { "-rawsignals", ".rawsignals", XrmoptionNoArg, "True" },
  { "-norawsignals", ".rawsignals", XrmoptionNoArg, "False" },
  { "-crosshairs", ".crosshairs", XrmoptionNoArg, "True" },
  { "-nocrosshairs", ".crosshairs", XrmoptionNoArg, "False" },
  { "-touchlabels", ".touchlabels", XrmoptionNoArg, "True" },
  { "-notouchlabels", ".touchlabels", XrmoptionNoArg, "False" },
  { "-bboxes", ".bboxes", XrmoptionNoArg, "True" },
  { "-nobboxes", ".bboxes", XrmoptionNoArg, "False" },
  { "-db", ".doubleBuffer",XrmoptionNoArg, "True" },
  { "-nodb", ".doubleBuffer", XrmoptionNoArg, "False" },
  { 0, 0, 0, 0 }
};

int options_size = (sizeof(options) / sizeof(XrmOptionDescRec));

struct dtv_context
{
  /* display-related entries */
  Display* dpy;
  Window win;
  Window root;

#ifdef HAVE_DOUBLE_BUFFER_EXTENSION
  XdbeBackBuffer back_buf;
#endif /* HAVE_DOUBLE_BUFFER_EXTENSION */
  Pixmap pix_buf;

  GC copy_gc;

  /* xrm resources / command-line options */
  int shades;
  int delay;
  int mono;
  int rawsignals;
  int crosshairs;
  int touchlabels;
  int bboxes;

  /* drawing-related entries */
  int w; int h;
  int x; int y;
  int root_w; int root_h;
  float grid_x; float grid_y;
  int depth;
  int colors;
  Colormap cmap;
  XColor* pal;
  GC* gcs;

  /* DiamondTouch-related entries */
  dt_device device;
  dt_table* table;
  dt_frame* frame[DT_MAX_USERS];
  dt_event* event;
};

#ifdef HAVE_DOUBLE_BUFFER_EXTENSION
# define TARGET(c) ((c)->back_buf ? (c)->back_buf : \
                    (c)->pix_buf ? (c)->pix_buf : (c)->win)
#else  /* !HAVE_DOUBLE_BUFFER_EXTENSION */
# define TARGET(c) ((c)->pix_buf ? (c)->pix_buf : (c)->win)
#endif /* !HAVE_DOUBLE_BUFFER_EXTENSION */

#define INVERT(y) (c->table->yn - y)  /* table origin is bottom-left */
#define MAX(a,b) (a>=b?a:b)
#define MIN(a,b) (a<=b?a:b)

void dt_init(struct dtv_context* c);
void dt_uninit(struct dtv_context* c);

void
dtv_init(Display* dpy, Window win, struct dtv_context* c)
{
  int i, j;
  XGCValues val;

  memset(c, 0, sizeof(*c));

  dt_init(c);

  c->dpy = dpy;
  c->win = win;

  c->rawsignals = get_boolean_resource("rawsignals", "Boolean");
  c->crosshairs = get_boolean_resource("crosshairs", "Boolean");
  c->touchlabels = get_boolean_resource("touchlabels", "Boolean");
  c->bboxes = get_boolean_resource("bboxes", "Boolean");

  {
    XWindowAttributes xgwa;  /* return param for XGetWindowAttributes */
    Window child;  /* (unused) return param for XTranslateCoordinates */

    /* get the window's attributes */
    XGetWindowAttributes(c->dpy, c->win, &xgwa);
    c->w = xgwa.width;
    c->h = xgwa.height;
    c->depth = xgwa.depth;
    c->cmap = xgwa.colormap;

    /* ask for ConfigureNotify events */
    xgwa.your_event_mask |= StructureNotifyMask;
    XSelectInput(c->dpy, c->win, xgwa.your_event_mask);

    /* get the root window's dimensions */
    c->root = RootWindow(c->dpy, DefaultScreen(c->dpy));
    XGetWindowAttributes(c->dpy, c->root, &xgwa);
    c->root_w = xgwa.width;
    c->root_h = xgwa.height;

    /* get the window's origin (c->x, c->y) w.r.t. the root window */
    XTranslateCoordinates(c->dpy, c->win, c->root, 0, 0,
        &(c->x), &(c->y), &child);
  }

  if (get_boolean_resource("doubleBuffer", "Boolean"))
  {
#ifdef HAVE_DOUBLE_BUFFER_EXTENSION
    c->back_buf = xdbe_get_backbuffer(c->dpy, c->win, XdbeUndefined);
#endif /* HAVE_DOUBLE_BUFFER_EXTENSION */

#ifdef HAVE_DOUBLE_BUFFER_EXTENSION
    if (!c->back_buf)
#endif /* HAVE_DOUBLE_BUFFER_EXTENSION */
      c->pix_buf = XCreatePixmap(dpy, win, c->root_w, c->root_h, c->depth);
  }

  val.function = GXcopy;
  c->copy_gc = XCreateGC(c->dpy, TARGET(c), GCFunction, &val);

  c->grid_x = (float)c->root_w / (float)c->table->xn;
  c->grid_y = (float)c->root_h / (float)c->table->yn;

  c->mono = get_boolean_resource("mono", "Boolean");

  c->shades = get_integer_resource("shades", "Integer");
  if (c->shades < 2)
    c->shades = 2;

  if (!c->mono)
  {
    /* color space is cross-product of axis shades, plus crosshair color */
    c->colors = 1;  /* background */
    if (c->rawsignals)
      c->colors += c->shades * c->shades - 1;
    if (c->crosshairs || c->touchlabels || c->bboxes)
      c->colors += 1;
    c->pal = calloc(c->colors, sizeof(XColor));

    c->pal[0].pixel = BlackPixel(c->dpy, DefaultScreen(c->dpy));

    if (c->rawsignals)
    {
      /* cross-product of red shades (for x) and blue shades (for y) */
      for (i = 0; i < c->shades; ++i)
        for (j = 0; j < c->shades; ++j)
        {
          c->pal[i*c->shades+j].flags = DoRed|DoBlue;
          c->pal[i*c->shades+j].red = i * 0xFFFF / (c->shades - 1);
          c->pal[i*c->shades+j].blue = j * 0xFFFF / (c->shades - 1);
        }
    }

    if (c->crosshairs || c->touchlabels || c->bboxes)
    {
      /* full-intensity green for crosshair lines */
      c->pal[c->colors - 1].flags = DoGreen;
      c->pal[c->colors - 1].green = 0xFFFF;
    }

    for (i = 0; i < c->colors; ++i)
    {
      XColor color;
      color = c->pal[i];
      if (XAllocColor (c->dpy, c->cmap, &color))
        c->pal[i].pixel = color.pixel;
      else
      {
        free_colors(c->dpy, c->cmap, c->pal, i);
        exit(EXIT_FAILURE);
      }
    }

    if (c->colors < 2)  /* color allocation failure */
    {
      c->mono = 1;
      free(c->pal);
    }

  }

  if (c->mono)  /* don't else this with the previous if! */
  {
    c->colors = 2;
    c->pal = calloc(2, sizeof(XColor));
    c->pal[0].pixel = BlackPixel(c->dpy, DefaultScreen(c->dpy));
    c->pal[1].pixel = WhitePixel(c->dpy, DefaultScreen(c->dpy));
  }    

  {
    unsigned long valmask = GCForeground;
    c->gcs = calloc(c->colors, sizeof(GC));
    for (i = 0; i < c->colors; ++i)
    {
      val.foreground = c->pal[i].pixel;    
      c->gcs[i] = XCreateGC(c->dpy, TARGET(c), valmask, &val);
    }
  }
}

void
do_dtv(struct dtv_context* c) 
{
  dt_user_id user_id;
  dt_io_error ioerr;

  if (!dt_device_read(c->device, c->table, c->frame, &ioerr))
  {
    fprintf(stderr, "dt_device_read failed\n");
    exit(EXIT_FAILURE);
  }

  /*
  for (user_id = 0; user_id < DT_MAX_USERS; ++user_id)
    dt_fi_interpret_frame(c->event, c->frame[user_id]);
  */

  /* "Paint It Black" */
  XFillRectangle(c->dpy, TARGET(c), c->gcs[0], 0 - c->x, 0 - c->y,
      c->root_w, c->root_h);

  if (c->rawsignals && !c->mono)
  {
    /* Represent raw signals as a c->table->xn by c->table->yn grid. */
    int i, j, strongest_x, strongest_y, xshade, yshade, color;
    for (j = 0; j < c->table->yn; ++j)
      for (i = 0; i < c->table->xn; ++i)
      {
        strongest_x = strongest_y = xshade = yshade = 0;
        for (user_id = 0; user_id < DT_MAX_USERS; ++user_id)
        {
          strongest_x = MAX(strongest_x, c->frame[user_id]->x[i]);
          strongest_y = MAX(strongest_y, c->frame[user_id]->y[j]);
        }
        xshade = strongest_x * c->shades / (DT_MAX_SIGNAL + 1);
        yshade = strongest_y * c->shades / (DT_MAX_SIGNAL + 1);

        color = xshade * c->shades + yshade;
        /* color = MIN(xshade, yshade) * c->shades; */
        XFillRectangle(c->dpy, TARGET(c), c->gcs[color],
            (i * c->grid_x - c->x), (j * c->grid_y - c->y),
            c->grid_x + 1, c->grid_y + 1); 
      }
  }

  if (c->crosshairs)
    for (user_id = 0; user_id < DT_MAX_USERS; ++user_id)
    {
      dt_1dtouch* touches;
      int i;
#ifdef HOWFAST
      static float last_x, last_y;
#endif /* HOWFAST */

      touches = d4_get_x_touches(user_id, c->frame[user_id]);
      for (i = 0; touches[i].valid; ++i)
      {
        int x = ((int)(touches[i].ctr * c->root_w / c->table->xn) - c->x);
        XDrawLine(c->dpy, TARGET(c), c->gcs[c->colors - 1],
            x, 0 - c->y, x, c->root_h - c->y);
      }

#ifdef HOWFAST
      if (user_id == 0)
      {
        if (last_x >= 0.0 && touches[0].ctr >= 0.0)
          printf("dx=%f ", touches[0].ctr - last_x);
        last_x = touches[0].ctr;
      }
#endif /* HOWFAST */

      touches = d4_get_y_touches(user_id, c->frame[user_id]);
      for (i = 0; touches[i].valid; ++i)
      {
        int y = ((int)(touches[i].ctr * c->root_h / c->table->yn) - c->y);
        XDrawLine(c->dpy, TARGET(c), c->gcs[c->colors - 1],
            0 - c->x, y, c->root_w - c->x, y);
      }

#ifdef HOWFAST
      if (user_id == 0)
      {
        if (last_y >= 0.0 && touches[0].ctr >= 0.0)
          printf("dy=%f\n", touches[0].ctr - last_y);
        last_y = touches[0].ctr;
      }
#endif /* HOWFAST */

    }

  if (c->touchlabels || c->bboxes)
    for (user_id = 0; user_id < DT_MAX_USERS; ++user_id)
    {
      dt_2dtouch* touches;
      int i;

      touches = d4_get_2dtouches(user_id, c->frame[user_id]);
      for (i = 0; touches[i].status != EOL; ++i)
        if (touches[i].status == ALIVE)
        {
          if (c->touchlabels)
          {
            int x, y;
            char msg[4];
            XTextItem xti;

            x = ((int)(touches[i].x.ctr * c->root_w / c->table->xn) - c->x);
            y = ((int)(touches[i].y.ctr * c->root_h / c->table->yn) - c->y);

            snprintf(msg, 4, "%d", i);
            xti.chars = msg;
            xti.nchars = strlen(xti.chars);
            xti.delta = 0;
            xti.font = None;

            XDrawText(c->dpy, TARGET(c), c->gcs[c->colors - 1],
                x, y, &xti, 1);
          }
          if (c->bboxes)
          {
            int x1, x2, y1, y2;

            x1 = ((int)(touches[i].x.beg * c->root_w / c->table->xn) - c->x);
            x2 = ((int)(touches[i].x.end * c->root_w / c->table->xn) - c->x);
            y1 = ((int)(touches[i].y.beg * c->root_h / c->table->yn) - c->y);
            y2 = ((int)(touches[i].y.end * c->root_h / c->table->yn) - c->y);

            XDrawLine(c->dpy, TARGET(c), c->gcs[c->colors - 1],
                x1, y1, x2, y1);
            XDrawLine(c->dpy, TARGET(c), c->gcs[c->colors - 1],
                x2, y1, x2, y2);
            XDrawLine(c->dpy, TARGET(c), c->gcs[c->colors - 1],
                x2, y2, x1, y2);
            XDrawLine(c->dpy, TARGET(c), c->gcs[c->colors - 1],
                x1, y2, x1, y1);
          }
        }
    }

#ifdef HAVE_DOUBLE_BUFFER_EXTENSION
  if (c->back_buf)
  {
      XdbeSwapInfo info[1];
      info[0].swap_window = c->win;
      info[0].swap_action = XdbeUndefined;
      XdbeSwapBuffers(c->dpy, info, 1);
  } else
#endif /* HAVE_DOUBLE_BUFFER_EXTENSION */
    if (c->pix_buf)
      XCopyArea(c->dpy, c->pix_buf, c->win, c->copy_gc,
          0, 0, c->w, c->h, 0, 0);

  XSync(c->dpy, False);
}

void
screenhack(Display *dpy, Window win) 
{
  struct dtv_context c;
  int delay;

  delay = get_integer_resource("delay", "Integer");

  dtv_init(dpy, win, &c);

  while (1)
  {
    do_dtv(&c); 

    while (XPending (dpy))
    {
      XEvent event;
      XNextEvent (dpy, &event);
      switch (event.type)
      {
        case ConfigureNotify:
          {
            Window child;  /* return param for XTranslateCoordinates */

            c.w = event.xconfigure.width;
            c.h = event.xconfigure.height;

            /* get the window's origin (c->x, c->y) w.r.t. the root window */
            XTranslateCoordinates(c.dpy, c.win, c.root, 0, 0,
                &(c.x), &(c.y), &child);
          }
          break;
        default:
          screenhack_handle_event(dpy, &event);
          break;
      }
    }
    if (delay) usleep(delay);
  }
}

void
dt_init(struct dtv_context* c)
{
  dt_user_id user_id;

  c->table = dt_get_table((dt_user_id)0);

  if (c->table->status != DT_TABLE_STATUS_OK)
  {
    fprintf(stderr, "can't find DiamondTouch device\n");
    exit(EXIT_FAILURE);
  }

  c->device = dt_device_open(c->table, &(c->table->devname[0][0]));
  if (c->device == DT_BAD_DEVICE)
  {
    fprintf(stderr, "can't open device \"%s\"\n",
        &(c->table->devname[0][0]));
    exit(EXIT_FAILURE);
  }

  for (user_id = 0; user_id < DT_MAX_USERS; ++user_id)
    if (!(c->frame[user_id] = dt_frame_new(c->table)))
    {
      fprintf(stderr, "can't make dt_frame\n");
      exit(EXIT_FAILURE);
    }

  d4_init();

  /*
  for (user_id = 0; user_id < DT_MAX_USERS; ++user_id)
  {
    dt_fi_user_initialize(user_id);
    dt_me_initialize(user_id);
  }

  if (!(c->event = (dt_event *)calloc(sizeof(dt_event), 1)))
  {
    fprintf(stderr, "can't make dt_event\n");
    exit(EXIT_FAILURE);
  }
  */
}

void
dt_uninit(struct dtv_context* c)
{
  dt_user_id user_id;

  free(c->event);
  for (user_id = 0; user_id < DT_MAX_USERS; ++user_id)
    dt_frame_free(c->frame[user_id]);

  /*
  for (user_id = 0; user_id < DT_MAX_USERS; ++user_id)
    dt_fi_user_uninitialize(user_id);
  */

  if (dt_device_close(c->table, c->device) < 0)
  {
    fprintf(stderr, "dt_device_close failed\n");
    exit(1);
  }

  d4_uninit();
}
