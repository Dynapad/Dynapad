#include "escheme.h"

#define SCH_UNIXSTR(arg) \
  SCHEME_BYTE_STR_VAL(scheme_char_string_to_byte_string(arg))

#define UNIX_SCHSTR(arg) \
  scheme_byte_string_to_char_string(scheme_make_byte_string(arg))

#include "defs.h"
#include "pad.h"
#include "point.h"
#include "line.h"
#include "button.h"
#include "view.h"
#include "pad-string.h"
#include "renderer.h"
#include "events.h"
#include "noisedata.h"
#include "misc.h"
#include "win.h"
#include "global.h"
#include "imagedata.h"
#include "imagemagick.h"
#include "image.h"
#include "text.h"
#include "fontdata.h"

#include <unistd.h>

#include <signal.h>

static void (*oldsig)(int);

#ifdef CYGWIN
#include <sys/cygwin.h>
#endif

#include <X11/extensions/XInput.h>

Scheme_Object *dynapad_typetag = NULL;
Scheme_Object *rect_typetag = NULL;
Scheme_Object *oval_typetag = NULL;
Scheme_Object *button_typetag = NULL;
Scheme_Object *line_typetag = NULL;
Scheme_Object *polygon_typetag = NULL;
Scheme_Object *imagedata_typetag = NULL;
Scheme_Object *image_typetag = NULL;
Scheme_Object *text_typetag = NULL;
Scheme_Object *panel_typetag = NULL;
Scheme_Object *group_typetag = NULL;
Scheme_Object *layer_typetag = NULL;

static Scheme_Object *
sch_wish(int argc, Scheme_Object **argv) {
    Pad_MainLoop();
    return UNIX_SCHSTR("Dynapad created");
}

static Scheme_Object *
sch_makedynapad(int argc, Scheme_Object **argv) {
    char *name, buf[1024];
    Pad *pad;
    Scheme_Object *obj, *sobj;

    if (!SCHEME_CHAR_STRINGP(argv[0]))
        scheme_wrong_type("dynapad% initialization", "string", 0, argc, argv);

    // is-a? argv[1] dynapad%

    name = SCH_UNIXSTR(argv[0]);
    sobj = argv[1];
    scheme_dont_gc_ptr(sobj);

    /*
   * mred SIGINT handler breaks out of current execution and
   * returns to user prompt.
   * Pad_Create_win replaces SIGINT handler with one that exits,
   * so we must restore handler after Pad_Create_win.
   */
    oldsig = signal(SIGINT, SIG_IGN);
    Pad_Create_win(name);
    signal(SIGINT, oldsig);

    pad = Pad_Win::Get_pad_from_name(Pad_GetUid(name));

    obj = scheme_make_cptr(pad, dynapad_typetag);
    pad->userdata = sobj;

    return obj;
}

static Scheme_Object *
sch_moveto(int argc, Scheme_Object **argv) {
    Pad *pad;
    double x, y, zoom;
    int animtime;
    Pad_Bool twostep;
    Scheme_Object *car, *list;
    Scheme_Object *result = scheme_false;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_moveto", "dynapad%", 0, argc, argv);
    if (!SCHEME_LISTP(argv[1]))
        scheme_wrong_type("sch_moveto", "list of x y zoom", 1, argc, argv);
    if (!SCHEME_INTP(argv[2]))
        scheme_wrong_type("sch_moveto", "anim integer", 2, argc, argv);
    if (!SCHEME_BOOLP(argv[3]))
        scheme_wrong_type("sch_moveto", "twostep boolean", 3, argc, argv);

    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);

    list = argv[1];
    if (scheme_list_length(list) != 3)
        scheme_wrong_type("sch_moveto", "list of (x y zoom)", 1, argc, argv);

    car = SCHEME_CAR(list);
    if (!SCHEME_REALP(car))
        scheme_wrong_type("sch_moveto", "(x y zoom) with real-valued x", 1, argc, argv);
    x = scheme_real_to_double(car);

    list = SCHEME_CDR(list);
    car = SCHEME_CAR(list);
    if (!SCHEME_REALP(car))
        scheme_wrong_type("sch_moveto", "(x y zoom) with real-valued y", 1, argc, argv);
    y = scheme_real_to_double(car);

    list = SCHEME_CDR(list);
    car = SCHEME_CAR(list);
    if (!SCHEME_REALP(car))
        scheme_wrong_type("sch_moveto", "(x y zoom) with real-valued zoom", 1, argc, argv);
    zoom = scheme_real_to_double(car);

    animtime = SCHEME_INT_VAL(argv[2]);
    twostep = (argv[3] == scheme_true) ? TRUE : FALSE;

    if (pad->view->Animate_to(x, y, zoom, animtime, twostep) == TRUE)
        result = scheme_true;

    return result;
}

static Scheme_Object *
sch_center(int argc, Scheme_Object **argv) {
    Pad *pad;
    Pad_List objs;
    double x, y, zoom;
    int animtime;
    Pad_Bool twostep;
    Scheme_Object *list, *car;
    Scheme_Object *result = scheme_false;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_center", "dynapad%", 0, argc, argv);
    if (!SCHEME_LISTP(argv[1]))
        scheme_wrong_type("sch_center", "list of dynapad objects", 1, argc, argv);
    if (!SCHEME_INTP(argv[2]))
        scheme_wrong_type("sch_center", "anim integer", 2, argc, argv);
    if (!SCHEME_BOOLP(argv[3]))
        scheme_wrong_type("sch_center", "twostep boolean", 3, argc, argv);
    if (!SCHEME_REALP(argv[4]))
        scheme_wrong_type("sch_center", "x real number", 4, argc, argv);
    if (!SCHEME_REALP(argv[5]))
        scheme_wrong_type("sch_center", "y real number", 5, argc, argv);
    if (!SCHEME_REALP(argv[6]))
        scheme_wrong_type("sch_center", "zoom real number", 6, argc, argv);

    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);
    list = argv[1];
    animtime = SCHEME_INT_VAL(argv[2]);
    twostep = (argv[3] == scheme_true) ? TRUE : FALSE;
    x = scheme_real_to_double(argv[4]);
    y = scheme_real_to_double(argv[5]);
    zoom = scheme_real_to_double(argv[6]);

    while (!SCHEME_NULLP(list)) {
        car = SCHEME_CAR(list);
        if (!SCHEME_CPTRP(car))
            scheme_wrong_type("sch_center", "dynapad object of some kind", -1, 0, &car);
        objs.Push((Pad_Object *) SCHEME_CPTR_VAL(car));

        list = SCHEME_CDR(list);
    }

    pad->view->Center(objs, animtime, twostep, x, y, zoom);

    return result;
}

static Scheme_Object *
sch_centerbbox(int argc, Scheme_Object **argv) {
    Pad *pad;
    Pad_List objs;
    double x, y, zoom;
    double bb[4];
    int i;
    int animtime;
    Pad_Bool twostep;
    Scheme_Object *list, *car;
    Scheme_Object *result = scheme_false;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_centerbbox", "dynapad%", 0, argc, argv);
    if (!SCHEME_LISTP(argv[1]))
        scheme_wrong_type("sch_centerbbox", "bbox list", 1, argc, argv);
    if (!SCHEME_INTP(argv[2]))
        scheme_wrong_type("sch_centerbbox", "anim integer", 2, argc, argv);
    if (!SCHEME_BOOLP(argv[3]))
        scheme_wrong_type("sch_centerbbox", "twostep boolean", 3, argc, argv);
    if (!SCHEME_REALP(argv[4]))
        scheme_wrong_type("sch_centerbbox", "x real number", 4, argc, argv);
    if (!SCHEME_REALP(argv[5]))
        scheme_wrong_type("sch_centerbbox", "y real number", 5, argc, argv);
    if (!SCHEME_REALP(argv[6]))
        scheme_wrong_type("sch_centerbbox", "zoom real number", 6, argc, argv);

    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);
    list = argv[1];
    animtime = SCHEME_INT_VAL(argv[2]);
    twostep = (argv[3] == scheme_true) ? TRUE : FALSE;
    x = scheme_real_to_double(argv[4]);
    y = scheme_real_to_double(argv[5]);
    zoom = scheme_real_to_double(argv[6]);

    i = 0;
    while (!SCHEME_NULLP(list)) {
        car = SCHEME_CAR(list);
        if (!SCHEME_REALP(car))
            scheme_wrong_type("sch_centerbbox", "bbox real", -1, 0, &car);
        bb[i++] = scheme_real_to_double(car);
        list = SCHEME_CDR(list);
    }

    pad->view->Center(bb[0], bb[1], bb[2], bb[3], animtime, twostep, x, y, zoom);

    return result;
}

static Scheme_Object *
sch_getview(int argc, Scheme_Object **argv) {
    Pad *pad;
    float x, y, zoom;
    Scheme_Object *result = scheme_false;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_getview", "dynapad%", 0, argc, argv);

    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);

    pad->view->Get_view(x, y, zoom);
    result =
        scheme_make_pair(scheme_make_float(x),
                         scheme_make_pair(scheme_make_float(y),
                                          scheme_make_pair(scheme_make_float(zoom), scheme_null)));

    return result;
}

static Scheme_Object *
sch_xy_in_poly(int argc, Scheme_Object **argv) {
    Pad_Object *padobject;
    Pad_Point pxy;
    Pad_PList pts;
    float x, y;
    Scheme_Object *result = scheme_false;

    if (!SCHEME_CPTRP(argv[2]) || polygon_typetag != SCHEME_CPTR_TYPE(argv[2]))
        scheme_wrong_type("sch_xy_in_poly", "polygon%", 2, argc, argv);

    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[2]);
    padobject->Get_coords(pts, FALSE);

    if (SCHEME_INTP(argv[0])) {
        x = (float) SCHEME_INT_VAL(argv[0]);
    } else {
        if (SCHEME_REALP(argv[0]))
            x = scheme_real_to_double(argv[0]);
    }

    if (SCHEME_INTP(argv[1])) {
        y = (float) SCHEME_INT_VAL(argv[1]);
    } else {
        if (SCHEME_REALP(argv[1]))
            y = scheme_real_to_double(argv[1]);
    }
    pxy = new Pad_Point(x, y);

    if (pxy.In_polygon(pts))
        result = scheme_true;

    return result;
}

static Scheme_Object *
sch_zoom(int argc, Scheme_Object **argv) {
    Pad *pad;
    double x, y, zfac;
    int animtime;
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_zoom", "dynapad%", 0, argc, argv);
    if (!SCHEME_REALP(argv[1]))
        scheme_wrong_type("sch_zoom", "real number", 1, argc, argv);
    if (!SCHEME_INTP(argv[2]))
        scheme_wrong_type("sch_zoom", "integer", 2, argc, argv);
    if (!SCHEME_REALP(argv[3]))
        scheme_wrong_type("sch_zoom", "real number", 3, argc, argv);
    if (!SCHEME_REALP(argv[4]))
        scheme_wrong_type("sch_zoom", "real number", 4, argc, argv);

    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);
    zfac = scheme_real_to_double(argv[1]);
    animtime = SCHEME_INT_VAL(argv[2]);
    x = scheme_real_to_double(argv[3]);
    y = scheme_real_to_double(argv[4]);

    pad->view->Zoom_around(x, y, zfac, animtime);

    return result;
}

static Scheme_Object *
sch_modifier(int argc, Scheme_Object **argv) {
    Pad *pad;
    char *op;
    Scheme_Object *result = scheme_true;
    char *modifier;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_modifier", "dynapad%", 0, argc, argv);
    if (!SCHEME_SYMBOLP(argv[1]))
        scheme_wrong_type("sch_modifier", "op symbol", 1, argc, argv);

    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);
    op = SCHEME_SYM_VAL(argv[1]);

    if (!strcmp(op, "get")) {
        return UNIX_SCHSTR(pad->Get_modifier());
    }

    if (argc != 3)
        scheme_wrong_count("sch_modifier", 2, 3, argc, argv);

    if (!SCHEME_CHAR_STRINGP(argv[2]))
        scheme_wrong_type("sch_modifier", "string", 2, argc, argv);

    modifier = SCH_UNIXSTR(argv[2]);

    if (!strcmp(op, "create")) {
        Pad_AddBindingModifier(modifier);
        return result;
    }

    if (!strcmp(op, "delete")) {
        if (!Pad_DeleteBindingModifier(modifier))
            scheme_warning((char *) "sch_modifier: delete failed %s", modifier);
        return result;
    }

    if (!strcmp(op, "set")) {
        if (!pad->Set_modifier(modifier))
            scheme_warning((char *) "sch_modifier: set failed %s", modifier);
        return result;
    }

    scheme_warning((char *) "modifier: invalid operator, given %s", op);
    return scheme_false;
}

static const char *
    find_ops[] = {
        "enclosed",
        "overlapping",
        0
    };

static Scheme_Object *
sch_find(int argc, Scheme_Object **argv) {
    Pad *pad;
    Pad_Win *win;
    char buf[1024], *op, *p;
    const char **pp;
    int findargc = 5;
    char *findargv[5];
    int matchtype = -1;
    int i;
    Pad_Bool groupmembers_flag = FALSE;
    Pad_List objs;
    Pad_Iterator oi;
    Pad_Object *obj;
    Scheme_Object *result = scheme_true;
    Scheme_Object *l, *car;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_find", "dynapad%", 0, argc, argv);
    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);
    win = pad->Get_win();
    argv++;
    argc--;

    if (SCHEME_SYMBOLP(argv[0]) &&
        !strcmp(SCHEME_SYM_VAL(argv[0]), "groupmembers")) {
        groupmembers_flag = TRUE;
        argv++;
        argc--;
    }

    if (!SCHEME_SYMBOLP(argv[0]))
        scheme_wrong_type("sch_find", "op symbol", 0, argc, argv);
    op = SCHEME_SYM_VAL(argv[0]);
    for (pp = find_ops; *pp; pp++) {
        if (!strcmp(op, *pp))
            break;
    }
    if (!*pp) {
        scheme_warning((char *) "sch_find: invalid find operator %s", op);
        return scheme_false;
    }
    argv++;
    argc--;

    if (!SCHEME_LISTP(argv[0]))
        scheme_wrong_type("sch_find", "list", 0, argc, argv);
    if (scheme_list_length(argv[0]) != 4)
        scheme_warning((char *) "sch_find: expected (llx lly urx ury), given %V",
                       argv[0]);

    l = argv[0];

    // need to ensure points are in order llx lly urx urx

    findargv[0] = strcpy(buf, op);
    for (i = 1, p = buf; i < 5; i++) {
        car = SCHEME_CAR(l);
        if (!SCHEME_REALP(car))
            scheme_warning((char *) "sch_find: expect real, given %V", car);
        p += strlen(p) + 1;
        sprintf(p, "%f", scheme_real_to_double(car));
        findargv[i] = p;
        l = SCHEME_CDR(l);
    }

    if (!pad->Find_eval(objs, (ClientData) win, groupmembers_flag, matchtype, findargc, findargv))
        scheme_warning((char *) "sch_find: Find_eval failed %s %s %s %s %s",
                       findargv[0], findargv[1], findargv[2], findargv[3], findargv[4]);

    result = scheme_null;
    DOLIST(oi, objs, Pad_Object, obj) {
        // must test userdata because only objects created by dynapad have userdata
        // eg UCSD logo is created by Pad++ and so doesn't have userdata
        if (obj->userdata && obj->findable)
            result = scheme_append(result,
                                   scheme_make_pair((Scheme_Object *) obj->userdata, scheme_null));
    }

    return result;
}

static Scheme_Object *
sch_pick(int argc, Scheme_Object **argv) {
    Pad *pad;
    Pad_Win *win;
    Pad_Event *padEvent;
    Pad_Object *padobject;
    Scheme_Object *result = scheme_null;
    XEvent xevent, *eventPtr = &xevent;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_pick", "dynapad%", 0, argc, argv);
    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);
    win = pad->Get_win();
    padEvent = new Pad_Event(win, eventPtr);

    eventPtr = new XEvent();
    eventPtr->type = ButtonPress;
    eventPtr->xbutton.button = Button1;


    padEvent->pt.x = scheme_real_to_double(argv[1]);
    padEvent->pt.y = scheme_real_to_double(argv[2]);
    padobject = pad->Find_pick(padEvent);

    result = scheme_null;
    if (padobject->userdata) // && padobject->findable)
        result = (Scheme_Object *) padobject->userdata;

    return result;
}

static Scheme_Object *
sch_cpanzoom(int argc, Scheme_Object **argv) {
    Pad *pad;
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_cpanzoom", "dynapad%", 0, argc, argv);

    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);

    if (argc == 1) {
        return pad->defaultEvents ? scheme_true : scheme_false;
    }

    if (!SCHEME_BOOLP(argv[1]))
        scheme_wrong_type("sch_cpanzoom", "boolean", 1, argc, argv);

    pad->Init_events(argv[1] == scheme_true ? TRUE : FALSE);

    return result;
}

static Scheme_Object *
sch_makerect(int argc, Scheme_Object **argv) {
    Pad *pad;
    Pad_Rectangle *padrect;
    Scheme_Object *sobj, *obj;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_makerect", "dynapad%", 0, argc, argv);

    // is-a? argv[1] rect%

    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);
    sobj = argv[1];
    scheme_dont_gc_ptr(sobj);

    padrect = new Pad_Rectangle(pad);
    padrect->userdata = sobj;
    obj = scheme_make_cptr(padrect, rect_typetag);

    return obj;
}

static Scheme_Object *
sch_coords(int argc, Scheme_Object **argv) {
    Scheme_Object *ctype;
    Pad_Object *padobject;
    Scheme_Object *list, *car;
    int listlen;
    Pad_Point pt;
    Pad_PList coords;
    double x, y;
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]))
        scheme_wrong_type("sch_coords",
                          "rect%, oval%, line%, or polygon%", 0, argc, argv);

    ctype = SCHEME_CPTR_TYPE(argv[0]);
    if (rect_typetag != ctype && oval_typetag != ctype &&
        line_typetag != ctype && polygon_typetag != ctype)
        scheme_wrong_type("sch_coords",
                          "rect%, oval%, line%, or polygon%", 0, argc, argv);

    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[0]);

    if (argc == 1) {
        int i;
        Pad_PList pts;

        padobject->Get_coords(pts, FALSE);
        result = scheme_null;
        for (i = 0; i < pts.Length(); i++) {
            result =
                scheme_append(result,
                              scheme_make_pair(scheme_make_float(pts.Nth(i)->x),
                                               scheme_make_pair(scheme_make_float(pts.Nth(i)->y), scheme_null)));
        }
        return result;
    }

    list = argv[1];
    if (!SCHEME_LISTP(list))
        scheme_wrong_type("sch_coords", "list", 1, argc, argv);

    listlen = scheme_list_length(list);

    if (listlen % 2 != 0)
        scheme_warning((char *) "sch_coords: requires even number of points, given %V",
                       list);

    if ((rect_typetag == ctype || oval_typetag == ctype) && listlen != 4)
        scheme_warning((char *) "sch_coords: rect and oval require four values, given %V",
                       list);

    coords.Make_empty();
    while (!SCHEME_NULLP(list)) {
        car = SCHEME_CAR(list);
        if (!SCHEME_REALP(car))
            scheme_wrong_type("sch_coords", "real number", -1, 0, &car);
        x = scheme_real_to_double(car);

        list = SCHEME_CDR(list);
        car = SCHEME_CAR(list);

        car = SCHEME_CAR(list);
        if (!SCHEME_REALP(car))
            scheme_wrong_type("sch_coords", "real number", -1, 0, &car);
        y = scheme_real_to_double(car);

        pt.Set(x, y);
        coords.Push_last(&pt);

        list = SCHEME_CDR(list);
    }
    padobject->Set_coords(coords, FALSE);

    return result;
}

static Scheme_Object *
sch_delete(int argc, Scheme_Object **argv) {
    Pad_Object *padobject;

    if (!SCHEME_CPTRP(argv[0]))
        scheme_wrong_type("sch_delete", "pad object", 0, argc, argv);

    // is-a? argv[0] dynaobject%

    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[0]);

    delete padobject;
    scheme_gc_ptr_ok(argv[0]);

    return scheme_true;
}

static Scheme_Object *
sch_makeoval(int argc, Scheme_Object **argv) {
    Pad *pad;
    Pad_Oval *padoval;
    Scheme_Object *sobj, *obj;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_makeoval", "dynapad%", 0, argc, argv);

    // is-a? argv[1] oval%

    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);
    sobj = argv[1];
    scheme_dont_gc_ptr(sobj);

    padoval = new Pad_Oval(pad);
    padoval->userdata = sobj;
    obj = scheme_make_cptr(padoval, oval_typetag);

    return obj;
}

static Scheme_Object *
sch_makebutton(int argc, Scheme_Object **argv) {
    Pad *pad;
    Pad_Button *padbutton;
    Scheme_Object *sobj, *obj;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_makebutton", "dynapad%", 0, argc, argv);

    // is-a? argv[1] button%

    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);
    sobj = argv[1];
    scheme_dont_gc_ptr(sobj);

    padbutton = new Pad_Button(pad);
    padbutton->userdata = sobj;
    obj = scheme_make_cptr(padbutton, button_typetag);

    return obj;
}

static Scheme_Object *
sch_makeline(int argc, Scheme_Object **argv) {
    Pad *pad;
    Pad_Line *padline;
    Scheme_Object *sobj, *obj;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_makeline", "dynapad%", 0, argc, argv);

    // is-a? argv[1] line%

    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);
    sobj = argv[1];
    scheme_dont_gc_ptr(sobj);

    padline = new Pad_Line(pad);
    padline->userdata = sobj;
    obj = scheme_make_cptr(padline, line_typetag);

    return obj;
}

static Scheme_Object *
sch_makepolygon(int argc, Scheme_Object **argv) {
    Pad *pad;
    Pad_Polygon *padpolygon;
    Scheme_Object *sobj, *obj;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_makepolygon", "dynapad%", 0, argc, argv);

    // is-a? argv[1] line%

    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);
    sobj = argv[1];
    scheme_dont_gc_ptr(sobj);

    padpolygon = new Pad_Polygon(pad);
    padpolygon->userdata = sobj;
    obj = scheme_make_cptr(padpolygon, polygon_typetag);

    return obj;
}

static Scheme_Object *
sch_slide(int argc, Scheme_Object **argv) {
    Pad_Object *padobject;
    Scheme_Object *result = scheme_true;
    double dx, dy;

    if (!SCHEME_CPTRP(argv[0]))
        scheme_wrong_type("sch_slide", "dynaobject%", 0, argc, argv);
    if (!SCHEME_REALP(argv[1]))
        scheme_wrong_type("sch_slide", "real number", 1, argc, argv);
    if (!SCHEME_REALP(argv[2]))
        scheme_wrong_type("sch_slide", "real number", 2, argc, argv);

    // is-a? argv[1] dynaobject%

    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[0]);
    dx = scheme_real_to_double(argv[1]);
    dy = scheme_real_to_double(argv[2]);

    if (!padobject->Slide(dx, dy, TRUE)) {
        scheme_warning((char *) "sch_slide: failed");
    }

    return result;
}

static Scheme_Object *
sch_scale(int argc, Scheme_Object **argv) {
    Pad_Object *padobject;
    Scheme_Object *result = scheme_true;
    double x, y, fac;

    if (!SCHEME_CPTRP(argv[0]))
        scheme_wrong_type("sch_scale", "dynaobject%", 0, argc, argv);
    if (!SCHEME_REALP(argv[1]))
        scheme_wrong_type("sch_scale", "real number", 1, argc, argv);
    if (!SCHEME_REALP(argv[2]))
        scheme_wrong_type("sch_scale", "real number", 2, argc, argv);
    if (!SCHEME_REALP(argv[3]))
        scheme_wrong_type("sch_scale", "real number", 3, argc, argv);

    // is-a? argv[1] dynaobject%

    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[0]);
    fac = scheme_real_to_double(argv[1]);
    x = scheme_real_to_double(argv[2]);
    y = scheme_real_to_double(argv[3]);

    if (!padobject->Scale(x, y, fac, TRUE)) {
        scheme_warning((char *) "sch_scale: failed");
    }

    return result;
}

static Scheme_Object *
sch_position(int argc, Scheme_Object **argv) {
    Pad_Object *padobject;
    Scheme_Object *result = scheme_true;
    Scheme_Object *l, *car;
    int i;
    float x, y, zoom;

    if (!SCHEME_CPTRP(argv[0]))
        scheme_wrong_type("sch_position", "dynaobject%", 0, argc, argv);

    // is-a? argv[0] dynaobject%

    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[0]);

    if (argc == 1) {
        padobject->Get_abs_position(x, y, zoom, TRUE);
        result =
            scheme_make_pair(scheme_make_float(x),
                             scheme_make_pair(scheme_make_float(y),
                                              scheme_make_pair(scheme_make_float(zoom), scheme_null)));
    } else {
        if (!SCHEME_LISTP(argv[1]))
            scheme_wrong_type("sch_position", "list", 2, argc, argv);
        if (scheme_list_length(argv[1]) != 3)
            scheme_warning((char *) "sch_position: expected (x y zoom), given %V",
                           argv[1]);

        l = argv[1];
        for (i = 0; i < 3; i++) {
            car = SCHEME_CAR(l);
            if (!SCHEME_REALP(car))
                scheme_warning((char *) "sch_position: expect real, given %V", car);
            switch (i) {
                case 0:
                    x = scheme_real_to_double(car);
                    break;
                case 1:
                    y = scheme_real_to_double(car);
                    break;
                case 2:
                    zoom = scheme_real_to_double(car);
                    break;
            }
            l = SCHEME_CDR(l);
        }

        if (!padobject->Set_abs_position(x, y, zoom, TRUE)) {
            scheme_warning((char *) "sch_position: failed");
        }
    }

    return result;
}

static Scheme_Object *
sch_bbox(int argc, Scheme_Object **argv) {
    Pad_Object *padobject;
    Scheme_Object *result = scheme_false;
    int i;
    float bbox[4];

    if (!SCHEME_CPTRP(argv[0]))
        scheme_wrong_type("sch_bbox", "dynaobject%", 0, argc, argv);

    // is-a? argv[0] dynaobject%

    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[0]);

    padobject->Get_global_bbox(bbox);

    result = scheme_null;
    for (i = 0; i < 4; i++) {
        result =
            scheme_append(result,
                          scheme_make_pair(scheme_make_float(bbox[i]), scheme_null));
    }

    return result;
}

static Scheme_Object *
sch_width(int argc, Scheme_Object **argv) {
    Pad_Object *padobject;
    Scheme_Object *result = scheme_true;
    double width;

    if (!SCHEME_CPTRP(argv[0]))
        scheme_wrong_type("sch_width", "dynaobject%", 0, argc, argv);

    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[0]);

    if (argc == 1) {
        width = padobject->Get_width();
        return scheme_make_float(width);
    }

    if (!SCHEME_REALP(argv[1]))
        scheme_wrong_type("sch_width", "real number", 1, argc, argv);

    width = scheme_real_to_double(argv[1]);

    if (!padobject->Set_width(width)) {
        scheme_warning((char *) "sch_width: failed");
    }

    return result;
}

static Scheme_Object *
sch_height(int argc, Scheme_Object **argv) {
    Pad_Object *padobject;
    Scheme_Object *result = scheme_true;
    double height;

    if (!SCHEME_CPTRP(argv[0]))
        scheme_wrong_type("sch_height", "dynaobject%", 0, argc, argv);

    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[0]);

    if (argc == 1) {
        height = padobject->Get_height();
        return scheme_make_float(height);
    }

    if (!SCHEME_REALP(argv[1]))
        scheme_wrong_type("sch_height", "real number", 1, argc, argv);

    height = scheme_real_to_double(argv[1]);

    if (!padobject->Set_height(height)) {
        scheme_warning((char *) "sch_height: failed");
    }

    return result;
}

/* not used ??? */
/* used directly, ugh, as sch_imagep */
static Scheme_Object *
sch_imagep(int argc, Scheme_Object **argv) {
    char *imagepath;

    if (!SCHEME_CHAR_STRINGP(argv[0]) && !SCHEME_PATHP(argv[0]))
        scheme_wrong_type("sch_imagep", "string or path imagepath", 0, argc, argv);

    if (SCHEME_CHAR_STRINGP(argv[0]))
        imagepath = SCH_UNIXSTR(argv[0]);
    else
        imagepath = SCHEME_PATH_VAL(argv[0]);

    return Pad_Imagep(imagepath) ? scheme_true : scheme_false;
}

static Scheme_Object *
sch_makeimagedata(int argc, Scheme_Object **argv) {
    Pad *pad;
    Pad_ImageData *imagedata;
    Pad_Win *win;
    char *imagepath, *expandpath;
    Scheme_Object *sobj, *obj;
    Pad_Bool newimage;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_makeimagedata", "dynapad%", 0, argc, argv);

    // is-a? argv[1] imagedata%

    if (!SCHEME_CHAR_STRINGP(argv[2]) && !SCHEME_PATHP(argv[2]))
        scheme_wrong_type("sch_makeimagedata", "string or path imagepath", 2, argc, argv);

    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);
    sobj = argv[1];
    scheme_dont_gc_ptr(sobj);

    if (SCHEME_CHAR_STRINGP(argv[2]))
        imagepath = SCH_UNIXSTR(argv[2]);
    else
        imagepath = SCHEME_PATH_VAL(argv[2]);

    expandpath = scheme_expand_filename(imagepath, -1, NULL, NULL, 0);
    if (expandpath == NULL)
        scheme_warning((char *) "sch_makeimagedata: expand imagepath failed %s",
                       imagepath);

    win = pad->Get_win();

    imagedata = win->renderer->Alloc_image(expandpath, &newimage);
    if (!imagedata) {
        scheme_warning((char *) "sch_makeimagedata: Alloc failed %s %s",
                       imagepath, expandpath);
        return scheme_false;
    }
    imagedata->userdata = sobj;
    obj = scheme_make_cptr(imagedata, imagedata_typetag);

    return obj;
}

static Scheme_Object *
sch_freeimagedata(int argc, Scheme_Object **argv) {
    Pad *pad;
    Pad_ImageData *imagedata;
    Pad_Win *win;
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_freeimagedata", "dynapad%", 0, argc, argv);
    if (!SCHEME_CPTRP(argv[1]) ||
        imagedata_typetag != SCHEME_CPTR_TYPE(argv[1]))
        scheme_wrong_type("sch_freeimagedata", "imagedata%", 1, argc, argv);

    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);
    imagedata = (Pad_ImageData *) SCHEME_CPTR_VAL(argv[1]);

    win = pad->Get_win();

    if (!win->renderer->Free_image(imagedata))
        scheme_warning((char *) "sch_freeimagedata: Free failed %s", imagedata->name);

    return result;
}

static Scheme_Object *
sch_makeimage(int argc, Scheme_Object **argv) {
    Pad *pad;
    Pad_Image *image;
    Scheme_Object *sobj, *obj;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_makeimage", "dynapad%", 0, argc, argv);

    // is-a? argv[1] image%

    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);
    sobj = argv[1];
    scheme_dont_gc_ptr(sobj);

    image = new Pad_Image(pad);
    image->userdata = sobj;
    obj = scheme_make_cptr(image, image_typetag);

    return obj;
}

static Scheme_Object *
sch_imagedata(int argc, Scheme_Object **argv) {
    Pad_Image *image;
    Pad_ImageData *imagedata;
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]) || image_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_imagedata", "image%", 0, argc, argv);

    image = (Pad_Image *) SCHEME_CPTR_VAL(argv[0]);

    if (argc == 1) {
        imagedata = image->Get_imagedata();
        return (Scheme_Object *) imagedata->userdata;
    }

    if (argv[1] == scheme_false) {
        image->Set_imagedata(NULL);
        return scheme_true;
    }

    if (!SCHEME_CPTRP(argv[1]) ||
        imagedata_typetag != SCHEME_CPTR_TYPE(argv[1]))
        scheme_wrong_type("sch_imagedata", "imagedata%", 1, argc, argv);

    imagedata = (Pad_ImageData *) SCHEME_CPTR_VAL(argv[1]);

    image->Set_imagedata(imagedata);

    return result;
}

static Scheme_Object *
sch_imagepath(int argc, Scheme_Object **argv) {
    Pad_ImageData *imagedata;
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]) ||
        imagedata_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_imagepath", "imagedata%", 0, argc, argv);

    imagedata = (Pad_ImageData *) SCHEME_CPTR_VAL(argv[0]);
    result = scheme_make_path(imagedata->name);

    return result;
}

static Scheme_Object *
sch_imagedim(int argc, Scheme_Object **argv) {
    Pad_ImageData *imagedata;
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]) ||
        imagedata_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_imagedim", "imagedata%", 0, argc, argv);

    imagedata = (Pad_ImageData *) SCHEME_CPTR_VAL(argv[0]);
    result =
        scheme_make_pair(scheme_make_float(imagedata->width),
                         scheme_make_pair(scheme_make_float(imagedata->height), scheme_null));

    return result;
}

// Access to individual pixels.  Not suggested for large-scale image processing.
static Scheme_Object *
sch_imagedata_rgb(int argc, Scheme_Object **argv) {
    Pad_ImageData *imagedata;
    Scheme_Object *result = scheme_false;
    int row, col;
    int r, g, b;

    if (!SCHEME_CPTRP(argv[0]) ||
        imagedata_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_imagedata_rgb", "imagedata%", 0, argc, argv);

    if (!SCHEME_INTP(argv[1])) {
        scheme_wrong_type("sch_imagedata_rgb", "row integer", 1, argc, argv);
        return scheme_false;
    }
    if (!SCHEME_INTP(argv[2])) {
        scheme_wrong_type("sch_imagedata_rgb", "col integer", 2, argc, argv);
        return scheme_false;
    }

    row = SCHEME_INT_VAL(argv[1]);
    col = SCHEME_INT_VAL(argv[2]);

    imagedata = (Pad_ImageData *) SCHEME_CPTR_VAL(argv[0]);

    if (argc == 6 || argc == 7) {
        // Set Color
        float red = 0;
        float grn = 0;
        float blu = 0;
        float alp = 0;
        if (SCHEME_INTP(argv[3])) {
            red = (float) SCHEME_INT_VAL(argv[3]);
        } else {
            if (SCHEME_REALP(argv[3]))
                red = scheme_real_to_double(argv[3]);
        }
        if (SCHEME_INTP(argv[4])) {
            grn = (float) SCHEME_INT_VAL(argv[4]);
        } else {
            if (SCHEME_REALP(argv[4]))
                grn = scheme_real_to_double(argv[4]);
        }
        if (SCHEME_INTP(argv[5])) {
            blu = (float) SCHEME_INT_VAL(argv[5]);
        } else {
            if (SCHEME_REALP(argv[5]))
                blu = scheme_real_to_double(argv[5]);
        }
        if (red > 1.0) red = 1.0;
        if (grn > 1.0) grn = 1.0;
        if (blu > 1.0) blu = 1.0;

        if (argc == 7) {
            if (SCHEME_INTP(argv[6])) {
                alp = (float) SCHEME_INT_VAL(argv[6]);
            } else {
                if (SCHEME_REALP(argv[6]))
                    alp = scheme_real_to_double(argv[6]);
            }
            imagedata->Set_pixel(row, col, red, grn, blu, alp);
        } else {
            imagedata->Set_pixel(row, col, red, grn, blu);
        }
        return scheme_true;
    } else {
        // Get Color
        if (imagedata->Get_pixel(row, col, &r, &g, &b)) {
            result =
                scheme_make_pair(scheme_make_integer(r),
                                 scheme_make_pair(scheme_make_integer(g),
                                                  scheme_make_pair(scheme_make_integer(b), scheme_null)));
        }
        return result;
    }

}

static Scheme_Object *
sch_maketext(int argc, Scheme_Object **argv) {
    Pad *pad;
    Pad_Text *padtext;
    Scheme_Object *sobj, *obj;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_maketext", "dynapad%", 0, argc, argv);

    // is-a? argv[1] text%

    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);
    sobj = argv[1];
    scheme_dont_gc_ptr(sobj);

    padtext = new Pad_Text(pad);
    padtext->userdata = sobj;
    obj = scheme_make_cptr(padtext, text_typetag);

    return obj;
}

static Scheme_Object *
sch_settext(int argc, Scheme_Object **argv) {
    Pad_Text *padtext;
    char *text;
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]) || text_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_settext", "text%", 0, argc, argv);
    if (!SCHEME_CHAR_STRINGP(argv[1]))
        scheme_wrong_type("sch_settext", "string", 1, argc, argv);

    padtext = (Pad_Text *) SCHEME_CPTR_VAL(argv[0]);
    text = SCH_UNIXSTR(argv[1]);

    if (!padtext->Set_text(text))
        scheme_warning((char *) "sch_settext: settext failed %s", text);

    return result;
}

static Scheme_Object *
sch_gettext(int argc, Scheme_Object **argv) {
    Pad_Text *padtext;
    Pad_String padstring;
    Scheme_Object *result = scheme_false;

    if (!SCHEME_CPTRP(argv[0]) || text_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_gettext", "text%", 0, argc, argv);

    padtext = (Pad_Text *) SCHEME_CPTR_VAL(argv[0]);

    if (!padtext->Get_text(padstring))
        scheme_warning((char *) "sch_gettext: gettext failed");

    return UNIX_SCHSTR(padstring.Get());
}

static Scheme_Object *
sch_inserttext(int argc, Scheme_Object **argv) {
    Pad_Text *padtext;
    char *index, *str;
    int iargc, i;
    char *iargv[3];
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]) || text_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_inserttext", "text%", 0, argc, argv);
    if (!SCHEME_CHAR_STRINGP(argv[1]))
        scheme_wrong_type("sch_inserttext", "index string", 1, argc, argv);
    if (!SCHEME_CHAR_STRINGP(argv[2]))
        scheme_wrong_type("sch_inserttext", "string", 2, argc, argv);

    padtext = (Pad_Text *) SCHEME_CPTR_VAL(argv[0]);
    index = SCH_UNIXSTR(argv[1]);
    str = SCH_UNIXSTR(argv[2]);

    iargc = 3;
    iargv[0] = (char *) "";
    iargv[1] = index;
    iargv[2] = str;

    if (padtext->Insert(iargc, iargv) == PAD_ERROR)
        scheme_warning((char *) "sch_inserttext: failed ~V ~s", index, str);

    return result;
}

static Scheme_Object *
sch_marktext(int argc, Scheme_Object **argv) {
    Pad_Text *padtext;
    char *op, *mark, *index;
    int iargc, i;
    char *iargv[4];
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]) || text_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_marktext", "text%", 0, argc, argv);
    if (argc != 2 && argc != 4)
        scheme_warning((char *) "sch_marktext: expects 2 or 4 arguments, given %d", argc);

    for (i = 1; i < argc; i++)
        if (!SCHEME_CHAR_STRINGP(argv[i]))
            scheme_wrong_type("sch_marktext", "string", i, argc, argv);

    padtext = (Pad_Text *) SCHEME_CPTR_VAL(argv[0]);
    op = SCH_UNIXSTR(argv[1]);

    if (!strcmp(op, "set")) {
        mark = SCH_UNIXSTR(argv[2]);
        index = SCH_UNIXSTR(argv[3]);
        iargc = 4;
        iargv[0] = (char *) "";
        iargv[1] = (char *) "set";
        iargv[2] = mark;
        iargv[3] = index;
        if (padtext->Mark(iargc, iargv) == PAD_ERROR)
            scheme_warning((char *) "sch_marktext: failed %s %s %s", op, mark, index);

        return result;
    }

    return result;
}

static Scheme_Object *
sch_deletetext(int argc, Scheme_Object **argv) {
    Pad_Text *padtext;
    char *index;
    int iargc, i;
    char *iargv[2];
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]) || text_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_deletetext", "text%", 0, argc, argv);
    if (!SCHEME_CHAR_STRINGP(argv[1]))
        scheme_wrong_type("sch_deletetext", "index string", 1, argc, argv);

    for (i = 1; i < argc; i++)
        if (!SCHEME_CHAR_STRINGP(argv[i]))
            scheme_wrong_type("sch_marktext", "string", i, argc, argv);

    padtext = (Pad_Text *) SCHEME_CPTR_VAL(argv[0]);
    index = SCH_UNIXSTR(argv[1]);

    iargc = 2;
    iargv[0] = (char *) "";
    iargv[1] = index;
    if (padtext->Del(iargc, iargv) == PAD_ERROR)
        scheme_warning((char *) "sch_deletetext: failed");

    return result;
}

static Scheme_Object *
sch_xinputdevices(int argc, Scheme_Object **argv) {
    Pad *pad;
    Scheme_Object *result = scheme_null;
    int i;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_xinputdevices", "dynapad%", 0, argc, argv);

    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);

    result = scheme_null;
    for (i = 0; i < num_xinputdevices; i++) {
        XDeviceInfo *device = &xinputdevices[i];
        Scheme_Object *name, *xid;
        name = UNIX_SCHSTR(device->name);
        xid = scheme_make_integer(device->id);

        if (device->use != IsXExtensionDevice)
            continue;

        result = scheme_append(result,
                               scheme_make_pair(
                                   scheme_make_pair(name,
                                                    scheme_make_pair(xid, scheme_null)),
                                   scheme_null));
    }

    return result;
}

static int
Apply(Pad_Object *obj, ClientData data, Pad_Event *) {
    scheme_apply((Scheme_Object *) data, 0, NULL);
    return (PAD_OK);
}

static int
EvalEvent(Pad_Object *padobject, ClientData data, Pad_Event *padevent) {
    Scheme_Object *event, *argv[100], *result; // yes, someday it will be too small
    Scheme_Object *proclist, *proc, *obj, *eventstr;
    int argc = 7;
    int tabletargc = 13;
    int xid = -1;
    int p = 0;
    int tiltx = 0;
    int tilty = 0;
    int state = 0;
    int button = -1;

    switch (padevent->eventPtr->type) {
        case MotionType: {
            XDeviceMotionEvent *e = (XDeviceMotionEvent *) padevent->eventPtr;
            xid = e->deviceid;
            p = e->axis_data[2];
            tiltx = e->axis_data[3];
            tilty = e->axis_data[4];
            state = e->state;
        }
            break;
        case ButtonPressType:
        case ButtonReleaseType: {
            XDeviceButtonEvent *e = (XDeviceButtonEvent *) padevent->eventPtr;
            xid = e->deviceid;
            p = e->axis_data[2];
            tiltx = e->axis_data[3];
            tilty = e->axis_data[4];
            state = e->state;
            button = e->button;
        }
            break;
        case ProximityInType:
        case ProximityOutType: {
            XProximityNotifyEvent *e = (XProximityNotifyEvent *) padevent->eventPtr;
            xid = e->deviceid;
        }
            break;
    }
    proclist = SCHEME_CAR((Scheme_Object *) data);
    obj = SCHEME_CAR(SCHEME_CDR((Scheme_Object *) data));
    eventstr = SCHEME_CAR(SCHEME_CDDR((Scheme_Object *) data));

    argv[0] = scheme_make_float(padevent->pt.x);
    argv[1] = scheme_make_float(padevent->pt.y);
    argv[2] = eventstr;
    {
        Scheme_Object *key = scheme_false;
        if ((padevent->eventPtr->type == KeyPress) || (padevent->eventPtr->type == KeyRelease)) {
            int numChars;
            char numStorage[64];

            numChars = XLookupString(&padevent->eventPtr->xkey, numStorage,
                                     sizeof(numStorage), (KeySym *) NULL, (XComposeStatus *) NULL);
            numStorage[numChars] = '\0';
            key = UNIX_SCHSTR(numStorage);
        }
        argv[3] = key;
    }
    if (!padobject) {
        argv[4] = scheme_false;
    } else {
        if (padobject->userdata) {
            argv[4] = (Scheme_Object *) padobject->userdata;
        } else {
            argv[4] = scheme_false;
        }
    }
    argv[5] = scheme_make_float(padevent->stickyPt.x);
    argv[6] = scheme_make_float(padevent->stickyPt.y);
    argv[7] = scheme_make_integer(xid);
    argv[8] = scheme_make_integer(p);
    argv[9] = scheme_make_integer(tiltx);
    argv[10] = scheme_make_integer(tilty);
    argv[11] = scheme_make_integer(state);
    argv[12] = scheme_make_integer(button);

    if (padevent->extension == TRUE) {
        event = scheme_make_struct_instance(
            scheme_eval(scheme_intern_symbol("struct:tablet-event"),
                        scheme_get_env(scheme_current_config())),
            tabletargc, argv);
    } else {
        event = scheme_make_struct_instance(
            scheme_eval(scheme_intern_symbol("struct:event"),
                        scheme_get_env(scheme_current_config())),
            argc, argv);
    }

    while (!SCHEME_NULLP(proclist)) {
        proc = SCHEME_CAR(proclist);

//        intptr_t err_buf_offset = offsetof(Scheme_Thread, error_buf);
//        Thread_Local_Variables *tlv = scheme_get_thread_local_variables();
//        Scheme_Thread *sct = scheme_current_thread;
//        int *error_buf = *(sct->error_buf);

//        if (scheme_setjmp(scheme_error_buf)) {
//            scheme_warning((char *) "sch_bind EvalEvent: failed");
//            result = scheme_true;
//        } else {
            result = scheme_eval(
                scheme_make_pair(proc,
                                 scheme_make_pair(obj,
                                                  scheme_make_pair(event, scheme_null))),
                scheme_get_env(scheme_current_config()));
//        }

        if (result == scheme_false)
            break;
        proclist = SCHEME_CDR(proclist);
    }
    return (result != scheme_false ? PAD_OK : PAD_BREAK);
    // Think about whether also want to allow PAD_RETURN and PAD_CONTINUE ???
}

static Scheme_Object *
sch_bind(int argc, Scheme_Object **argv) {
    Pad_Object *padobject;
    Pad_Callback *callback;
    Pad_Win *win;
    Pad_Bool rc;
    char *event;
    Scheme_Object *sexp, *spad, *sproclist, *sevent;
    Scheme_Object *list, *car;
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]))
        scheme_wrong_type("sch_bind", "dynapad object of somekind", 0, argc, argv);

    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[0]);

    win = padobject->pad->Get_win();
    spad = (Scheme_Object *) padobject->pad->userdata;

    if (argc == 3) {
        if (!SCHEME_CHAR_STRINGP(argv[1]))
            scheme_wrong_type("bind", "string", 1, argc, argv);
        sevent = argv[1];
        event = SCH_UNIXSTR(sevent);

        sproclist = argv[2];

        if (sproclist == scheme_null) {
            if (dynapad_typetag == SCHEME_CPTR_TYPE(argv[0])) {
                rc = Pad_DeleteBinding(win->bindingTable,
                                       Pad_GetUid("all"),
                                       event);
            } else {
                rc = Pad_DeleteBinding(win->bindingTable,
                                       padobject,
                                       event);
            }
            if (rc != PAD_OK)
                scheme_warning((char *) "sch_bind: Pad_DeleteBinding failed");
            return result;
        }

        if (!SCHEME_LISTP(sproclist))
            scheme_wrong_type("bind", "proc list", 2, argc, argv);

        list = sproclist;
        while (!SCHEME_NULLP(list)) {
            car = SCHEME_CAR(list);
            if (!SCHEME_PROCP(car))
                scheme_wrong_type("bind", "proc", -1, 0, &car);
            scheme_check_proc_arity("bind", 2, -1, 0, &car);
            list = SCHEME_CDR(list);
        }

        sexp =
            scheme_make_pair(sproclist,
                             scheme_make_pair(spad,
                                              scheme_make_pair(sevent, scheme_null)));
        scheme_dont_gc_ptr(sexp);
        callback = new Pad_Callback(EvalEvent, sexp);
        if (dynapad_typetag == SCHEME_CPTR_TYPE(argv[0])) {
            rc = Pad_CreateBinding(win->bindingTable,
                                   Pad_GetUid("all"),
                                   event,
                                   callback);
        } else {
            rc = Pad_CreateBinding(win->bindingTable,
                                   padobject,
                                   event,
                                   callback);
        }
        if (!rc)
            scheme_warning((char *) "sch_bind: Pad_CreateBinding failed");
        return result;
    }

    if (argc == 2) {
        Pad_Callback *callback;

        if (!SCHEME_CHAR_STRINGP(argv[1]))
            scheme_wrong_type("bind", "string", 1, argc, argv);
        sevent = argv[1];
        event = SCH_UNIXSTR(sevent);

        result = scheme_null;

        if (dynapad_typetag == SCHEME_CPTR_TYPE(argv[0]))
            callback = Pad_GetCallback(win->bindingTable, Pad_GetUid("all"), event);
        else
            callback = Pad_GetCallback(win->bindingTable, padobject, event);

        if (callback)
            result = SCHEME_CAR((Scheme_Object *) callback->GetClientData());

        return result;
    }

    if (argc == 1) {
        Pad_List events;
        Pad_String *event;
        Pad_Iterator li;

        if (dynapad_typetag == SCHEME_CPTR_TYPE(argv[0]))
            Pad_GetAllBindings(win->bindingTable, Pad_GetUid("all"), events);
        else
            Pad_GetAllBindings(win->bindingTable, padobject, events);

        result = scheme_null;
        DOLIST(li, events, Pad_String, event) {
            result = scheme_append(result,
                                   scheme_make_pair(UNIX_SCHSTR(event->Get()), scheme_null));
            delete event;
        }
        return result;
    }

    return result;
}

static Scheme_Object *
sch_bind_to_tag(int argc, Scheme_Object **argv) {
    Pad *pad;
    Pad_Object *padobject;
    Pad_Callback *callback;
    Pad_Win *win;
    Pad_Bool rc;
    char *event;
    char *tagname;
    Scheme_Object *sexp, *spad, *sproclist, *sevent, *stagname;
    Scheme_Object *list, *car;
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("bind_to_tag", "dynapad%", 0, argc, argv);
    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);
    win = pad->Get_win();

    if (!SCHEME_CHAR_STRINGP(argv[1]))
        scheme_wrong_type("bind_to_tag", "string", 1, argc, argv);
    stagname = argv[1];
    tagname = SCH_UNIXSTR(stagname);

    if (argc == 4) {
        if (!SCHEME_CHAR_STRINGP(argv[2]))
            scheme_wrong_type("bind", "string", 2, argc, argv);
        sevent = argv[2];
        event = SCH_UNIXSTR(sevent);

        sproclist = argv[3];

        if (sproclist == scheme_null) {
            rc = Pad_DeleteBinding(win->bindingTable,
                                   Pad_GetUid(tagname),
                                   event);
            if (rc != PAD_OK)
                scheme_warning((char *) "sch_bind: Pad_DeleteBinding failed");
            return result;
        }

        if (!SCHEME_LISTP(sproclist))
            scheme_wrong_type("bind", "proc list", 3, argc, argv);

        list = sproclist;
        while (!SCHEME_NULLP(list)) {
            car = SCHEME_CAR(list);
            if (!SCHEME_PROCP(car))
                scheme_wrong_type("bind", "proc", -1, 0, &car);
            scheme_check_proc_arity("bind", 2, -1, 0, &car);
            list = SCHEME_CDR(list);
        }

        sexp =
            scheme_make_pair(sproclist,
                             scheme_make_pair(spad,
                                              scheme_make_pair(sevent, scheme_null)));
        scheme_dont_gc_ptr(sexp);
        callback = new Pad_Callback(EvalEvent, sexp);

        rc = Pad_CreateBinding(win->bindingTable,
                               Pad_GetUid(tagname),
                               event,
                               callback);
        if (!rc)
            scheme_warning((char *) "sch_bind: Pad_CreateBinding failed");
        return result;
    }

    if (argc == 4) {
        Pad_Callback *callback;

        if (!SCHEME_CHAR_STRINGP(argv[1]))
            scheme_wrong_type("bind", "string", 3, argc, argv);
        sevent = argv[1];
        event = SCH_UNIXSTR(sevent);

        result = scheme_null;

        callback = Pad_GetCallback(win->bindingTable, Pad_GetUid(tagname), event);

        if (callback)
            result = SCHEME_CAR((Scheme_Object *) callback->GetClientData());

        return result;
    }

    if (argc == 3) {
        Pad_List events;
        Pad_String *event;
        Pad_Iterator li;

        Pad_GetAllBindings(win->bindingTable, Pad_GetUid(tagname), events);

        result = scheme_null;
        DOLIST(li, events, Pad_String, event) {
            result = scheme_append(result,
                                   scheme_make_pair(UNIX_SCHSTR(event->Get()), scheme_null));
            delete event;
        }
        return result;
    }

    return result;
}

/* not used ??? */
static Scheme_Object *
sch_add_tag(int argc, Scheme_Object **argv) {
    Pad_Object *padobject;
    char *tagname;
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]))
        scheme_wrong_type("sch_add_tag", "dynapad object of somekind", 0, argc, argv);

    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[0]);

    if (!SCHEME_CHAR_STRINGP(argv[1]))
        scheme_wrong_type("sch_add_tag", "tagname", 1, argc, argv);

    tagname = SCH_UNIXSTR(argv[1]);

    padobject->Add_tag(tagname);

    return result;
}

/* not used ??? */
static Scheme_Object *
sch_delete_tag(int argc, Scheme_Object **argv) {
    Pad_Object *padobject;
    char *tagname;
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]))
        scheme_wrong_type("sch_delete_tag", "dynapad object of somekind", 0, argc, argv);

    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[0]);

    if (!SCHEME_CHAR_STRINGP(argv[1]))
        scheme_wrong_type("sch_delete_tag", "tagname", 1, argc, argv);

    tagname = SCH_UNIXSTR(argv[1]);

    padobject->Delete_tag(tagname);

    return result;
}

static Scheme_Object *
sch_bindtags(int argc, Scheme_Object **argv) {
    Pad_Object *padobject;
    Pad *pad;
    Pad_Bool rc;
    const char *order;

    if (!SCHEME_CPTRP(argv[0]))
        scheme_wrong_type("sch_bindtags", "dynapad object of somekind", 0, argc, argv);

    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[0]);

    if (argc == 2) {

        if (!SCHEME_CHAR_STRINGP(argv[1]))
            scheme_wrong_type("bindtags", "string", 1, argc, argv);
        order = SCH_UNIXSTR(argv[1]);

        if (strcmp(order, "general") && strcmp(order, "specific"))
            scheme_warning((char *) "sch_bindtags: expects general or specific, given %V", argv[1]);

        if (dynapad_typetag == SCHEME_CPTR_TYPE(argv[0])) {
            pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);
            if (!strcmp(order, "specific"))
                pad->idEventFirst = TRUE;
            else
                pad->idEventFirst = FALSE;
        } else {
            padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[0]);
            rc = padobject->Set_event_order(order);
            if (!rc)
                scheme_warning((char *) "sch_bindtags: Set_event_order failed");
        }

        return scheme_true;
    }

    if (dynapad_typetag == SCHEME_CPTR_TYPE(argv[0])) {
        pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);
        if (pad->idEventFirst)
            order = (char *) "specific";
        else
            order = (char *) "general";
    } else {
        padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[0]);
        order = padobject->Get_event_order();
    }

    return UNIX_SCHSTR(order);
}

static int
Eval(ClientData data) {
    // Maybe Eval should return the scheme result?
    // Tcl would put the result into interp->result
    // See Fire_render_script
    scheme_eval((Scheme_Object *) data, scheme_get_env(scheme_current_config()));
    return (PAD_OK);
}

static Scheme_Object *
sch_zoomaction(int argc, Scheme_Object **argv) {
    Pad_Object *padobject;
    Pad_ZoomAction *zoomaction;
    double size;
    Pad_Win *win;
    Pad_Bool rc;
    Scheme_Object *sexp, *sthis, *sgrow, *sshrink, *ssize;
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]))
        scheme_wrong_type("sch_zoomaction", "dynapad object of somekind", 0, argc, argv);

    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[0]);

    win = padobject->pad->Get_win();

    if (argc == 5) {
        // is-a? argv[1] dynaobject%
        sthis = argv[1];

        if (!SCHEME_REALP(argv[2]))
            scheme_wrong_type("sch_zoomaction", "size", 2, argc, argv);
        ssize = argv[2];
        size = scheme_real_to_double(argv[2]);

        sgrow = argv[3];
        if (!SCHEME_PROCP(sgrow) && sgrow != scheme_false)
            scheme_wrong_type("sch_zoomaction", "grow proc", 3, argc, argv);

        sshrink = argv[4];
        if (!SCHEME_PROCP(sshrink) && sshrink != scheme_false)
            scheme_wrong_type("sch_zoomaction", "shrink proc", 4, argc, argv);

        zoomaction = new Pad_ZoomAction;
        zoomaction->actionSize = (int) UNITSTOVALUE(win, size);
        zoomaction->userActionSize = size;
        if (sgrow == scheme_false)
            zoomaction->growScript = NULL;
        else {
            sexp =
                scheme_make_pair(sgrow,
                                 scheme_make_pair(sthis,
                                                  scheme_make_pair(ssize, scheme_null)));
            scheme_dont_gc_ptr(sexp);
            zoomaction->growScript = new Pad_Callback(Eval, sexp);
        }
        if (sshrink == scheme_false)
            zoomaction->shrinkScript = NULL;
        else {
            sexp =
                scheme_make_pair(sshrink,
                                 scheme_make_pair(sthis,
                                                  scheme_make_pair(ssize, scheme_null)));
            scheme_dont_gc_ptr(sexp);
            zoomaction->shrinkScript = new Pad_Callback(Eval, sexp);
        }
        padobject->Modify_zoomaction(zoomaction);
        return result;
    }

    return result;
}

static Scheme_Object *
sch_renderscript(int argc, Scheme_Object **argv) {
    Pad_Object *padobject;
    Pad_Win *win;
    Pad_Callback *callback;
    Pad_Bool rc;
    Scheme_Object *sexp, *sthis, *sproc;
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]))
        scheme_wrong_type("sch_renderscript", "dynapad object of somekind", 0, argc, argv);

    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[0]);

    win = padobject->pad->Get_win();

    if (argc == 3) {
        // is-a? argv[1] dynaobject%
        sthis = argv[1];

        sproc = argv[2];
        if (!SCHEME_PROCP(sproc) && sproc != scheme_false)
            scheme_wrong_type("sch_renderscript", "proc", 2, argc, argv);

        if (sproc == scheme_false)
            callback = NULL;
        else {
            sexp =
                scheme_make_pair(sproc,
                                 scheme_make_pair(sthis, scheme_null));
            scheme_dont_gc_ptr(sexp);
            callback = new Pad_Callback(Eval, sexp);
        }

        padobject->Set_renderscript(callback);

        return result;
    }

    return result;
}

static Scheme_Object *
sch_renderitem(int argc, Scheme_Object **argv) {
    Pad_Object *padobject;
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]))
        scheme_wrong_type("sch_renderitem", "dynapad object of somekind", 0, argc, argv);

    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[0]);

    padobject->Render();

    return result;
}

static Scheme_Object *
sch_renderimage(int argc, Scheme_Object **argv) {
    Pad_Object *padobject;
    Pad_Win *win;
    Pad_ImageData *imagedata;
    float x0, y0, x1, y1;
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]))
        scheme_wrong_type("sch_renderimage", "dynapad object of somekind", 0, argc, argv);
    if (!SCHEME_CPTRP(argv[1]) || imagedata_typetag != SCHEME_CPTR_TYPE(argv[1]))
        scheme_wrong_type("sch_renderimage", "imagedata%", 1, argc, argv);
    if (!SCHEME_REALP(argv[2]))
        scheme_wrong_type("sch_renderimage", "real", 2, argc, argv);
    if (!SCHEME_REALP(argv[3]))
        scheme_wrong_type("sch_renderimage", "real", 3, argc, argv);

    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[0]);
    imagedata = (Pad_ImageData *) SCHEME_CPTR_VAL(argv[1]);
    x0 = scheme_real_to_double(argv[2]);
    y0 = scheme_real_to_double(argv[2]);

    win = padobject->pad->Get_win();

    x0 = UNITSTOX(win, x0);
    y0 = UNITSTOY(win, y0);
    x1 = x0 + imagedata->width;
    y1 = y0 + imagedata->height;
    if (!Pad_renderer->Draw_image(imagedata, x0, y0, x1, y1, FALSE))
        scheme_warning((char *) "sch_renderimage: Pad_renderer->Draw_image failed");

    return result;
}

static Scheme_Object *
sch_renderline(int argc, Scheme_Object **argv) {
    Pad_Object *padobject;
    Pad_Win *win;
    Scheme_Object *list, *car;
    int len;
    float x, y;
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]))
        scheme_wrong_type("sch_renderline", "dynapad object of somekind", 0, argc, argv);
    if (!SCHEME_LISTP(argv[1]))
        scheme_wrong_type("sch_renderline", "list of x y", 1, argc, argv);

    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[0]);
    list = argv[1];

    win = padobject->pad->Get_win();

    len = scheme_list_length(list);
    if (len < 4)
        scheme_warning((char *) "sch_renderline: expects xy list length >= 4, given %V", list);
    if (len % 2 != 0)
        scheme_warning((char *) "sch_renderline: expects even number of xy, given %V", list);

    Pad_renderer->Begin_line();
    while (!SCHEME_NULLP(list)) {
        car = SCHEME_CAR(list);
        if (!SCHEME_REALP(car))
            scheme_wrong_type("sch_renderline", "real number", -1, 0, &car);
        x = scheme_real_to_double(car);

        list = SCHEME_CDR(list);
        car = SCHEME_CAR(list);

        car = SCHEME_CAR(list);
        if (!SCHEME_REALP(car))
            scheme_wrong_type("sch_renderline", "real number", -1, 0, &car);
        y = scheme_real_to_double(car);

        Pad_renderer->V2f(UNITSTOX(win, x), UNITSTOY(win, y));

        list = SCHEME_CDR(list);
    }
    Pad_renderer->End_line();

    return result;
}

static Scheme_Object *
sch_renderpolygon(int argc, Scheme_Object **argv) {
    Pad_Object *padobject;
    Pad_Win *win;
    Scheme_Object *list, *car;
    int len;
    float x, y;
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]))
        scheme_wrong_type("sch_renderpolygon", "dynapad object of somekind", 0, argc, argv);
    if (!SCHEME_LISTP(argv[1]))
        scheme_wrong_type("sch_renderpolygon", "list of x y", 1, argc, argv);

    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[0]);
    list = argv[1];

    win = padobject->pad->Get_win();

    len = scheme_list_length(list);
    if (len < 4)
        scheme_warning((char *) "sch_renderpolygon: expects xy list length >= 4, given %V", list);
    if (len % 2 != 0)
        scheme_warning((char *) "sch_renderpolygon: expects even number of xy, given %V", list);

    Pad_renderer->Begin_polygon();
    while (!SCHEME_NULLP(list)) {
        car = SCHEME_CAR(list);
        if (!SCHEME_REALP(car))
            scheme_wrong_type("sch_renderpolygon", "real number", -1, 0, &car);
        x = scheme_real_to_double(car);

        list = SCHEME_CDR(list);
        car = SCHEME_CAR(list);

        car = SCHEME_CAR(list);
        if (!SCHEME_REALP(car))
            scheme_wrong_type("sch_renderpolygon", "real number", -1, 0, &car);
        y = scheme_real_to_double(car);

        Pad_renderer->V2f(UNITSTOX(win, x), UNITSTOY(win, y));

        list = SCHEME_CDR(list);
    }
    Pad_renderer->End_polygon();

    return result;
}

static Scheme_Object *
sch_rendertext(int argc, Scheme_Object **argv) {
    Pad_Object *padobject;
    Pad_Win *win;
    char *str;
    int level;
    float x, y;
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]))
        scheme_wrong_type("sch_rendertext", "dynapad object of somekind", 0, argc, argv);
    if (!SCHEME_CHAR_STRINGP(argv[1]))
        scheme_wrong_type("sch_rendertext", "string", 1, argc, argv);
    if (!SCHEME_REALP(argv[2]))
        scheme_wrong_type("sch_rendertext", "real", 2, argc, argv);
    if (!SCHEME_REALP(argv[3]))
        scheme_wrong_type("sch_rendertext", "real", 3, argc, argv);

    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[0]);
    str = SCH_UNIXSTR(argv[1]);
    x = scheme_real_to_double(argv[2]);
    y = scheme_real_to_double(argv[3]);

    win = padobject->pad->Get_win();

    level = 2;
    Pad_renderer->Draw_string(str, level, UNITSTOX(win, x), UNITSTOY(win, y), 0.0);

    return result;
}


static Scheme_Object *
sch_rendercolor(int argc, Scheme_Object **argv) {
    Pad_Object *padobject;
    char *color;
    Pad_Color *padcolor;
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]))
        scheme_wrong_type("sch_rendercolor", "dynapad object of somekind", 0, argc, argv);
    if (!SCHEME_CHAR_STRINGP(argv[1]))
        scheme_wrong_type("sch_rendercolor", "string colorname or #RRBBGG", 1, argc, argv);

    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[0]);
    color = SCH_UNIXSTR(argv[1]);

    padcolor = Pad_AllocColorByName(color);
    if (!color)
        scheme_warning((char *) "sch_rendercolor: Pad_AllocColorByName failed for %s", color);

    Pad_renderer->Set_color(padcolor);

    return result;
}

static Scheme_Object *
sch_fill(int argc, Scheme_Object **argv) {
    Pad_Object *padobject;
    Pad_String padstring;
    char *color;
    Pad_Color *padcolor;
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]))
        scheme_wrong_type("sch_fill", "dynapad object of somekind", 0, argc, argv);

    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[0]);

    if (argc == 1) {
        padobject->Get_fillname(padstring);
        return UNIX_SCHSTR(padstring.Get());
    }

    if (!SCHEME_CHAR_STRINGP(argv[1]))
        scheme_wrong_type("sch_fill", "string colorname or #RRBBGG", 1, argc, argv);

    color = SCH_UNIXSTR(argv[1]);

    padcolor = Pad_AllocColorByName(color);
    if (!color)
        scheme_warning((char *) "sch_fill: Pad_AllocColorByName failed for %s", color);

    padobject->Set_fill(color);

    return result;
}

static Scheme_Object *
sch_pen(int argc, Scheme_Object **argv) {
    Pad_Object *padobject;
    Pad_String padstring;
    char *color;
    Pad_Color *padcolor;
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]))
        scheme_wrong_type("sch_pen", "dynapad object of somekind", 0, argc, argv);

    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[0]);

    if (argc == 1) {
        padobject->Get_pen_name(padstring);
        return UNIX_SCHSTR(padstring.Get());
    }

    if (!SCHEME_CHAR_STRINGP(argv[1]))
        scheme_wrong_type("sch_pen", "string colorname or #RRBBGG", 1, argc, argv);

    color = SCH_UNIXSTR(argv[1]);

    padcolor = Pad_AllocColorByName(color);
    if (!color)
        scheme_warning((char *) "sch_pen: Pad_AllocColorByName failed for %s", color);

    padobject->Set_pen(color);

    return result;
}

static Scheme_Object *
sch_penwidth(int argc, Scheme_Object **argv) {
    Pad_Object *padobject;
    float width;
    Scheme_Object *result = scheme_true;
    Pad_Bool absLineStyle = FALSE;

    if (!SCHEME_CPTRP(argv[0]))
        scheme_wrong_type("sch_penwidth", "line% or text%", 0, argc, argv);

    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[0]);

    if (argc == 1) {
        width = padobject->Get_penwidth();
        return scheme_make_float(width);
    }

    if (argc == 3) {
        if (!SCHEME_BOOLP(argv[2]))
            scheme_wrong_type("sch_penwidth", "boolean", 2, argc, argv);
        absLineStyle = (argv[2] == scheme_true);
    }

    if (!SCHEME_REALP(argv[1]))
        scheme_wrong_type("sch_penwidth", "width", 1, argc, argv);

    width = scheme_real_to_double(argv[1]);

    if (!padobject->Set_penwidth(width, absLineStyle))
        scheme_warning((char *) "sch_penwidth: Set_penwidth failed for %V", argv[1]);

    return result;
}

static Scheme_Object *
sch_abslinestyle(int argc, Scheme_Object **argv) {
    Pad_Object *padobject;
    Scheme_Object *result = scheme_true;
    Pad_Bool absLineStyle = FALSE;

    if (!SCHEME_CPTRP(argv[0]))
        scheme_wrong_type("sch_penwidth", "line%", 0, argc, argv);

    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[0]);

    absLineStyle = padobject->Get_abslinestyle();
    return absLineStyle == TRUE ? scheme_true : scheme_false;
}

static Scheme_Object *
sch_focus(int argc, Scheme_Object **argv) {
    Pad_Object *padobject;
    int focus;
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]))
        scheme_wrong_type("sch_focus", "dynapad object of some kind", 0, argc, argv);
    if (!SCHEME_BOOLP(argv[1]))
        scheme_wrong_type("sch_focus", "boolean", 1, argc, argv);

    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[0]);
    focus = (argv[1] == scheme_true);

    // Pad_Text object deletes itself if "" and loses focus
    if (!focus && text_typetag == SCHEME_CPTR_TYPE(argv[0])) {
        Pad_Text *padtext = (Pad_Text *) padobject;
        if (!padtext->text || ((padtext->lines == 1) && (strlen(padtext->text[0].Get()) == 0)))
            result = scheme_false;
    }

    padobject->pad->view->Update_focus(focus ? padobject : NULL);

    return result;
}

static Scheme_Object *
sch_getfocus(int argc, Scheme_Object **argv) {
    Pad *pad;
    Scheme_Object *sfocus = scheme_false;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_getfocus", "dynapad%", 0, argc, argv);

    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);

    // Pad_focus is global, maybe should be per dynapad?

    // must test userdata because only objects created by dynapad have userdata
    // eg UCSD logo is created by Pad++ and so doesn't have userdata
    if (Pad_focus && Pad_focus->userdata && Pad_focus->findable)
        sfocus = (Scheme_Object *) Pad_focus->userdata;

    return sfocus;
}

static Scheme_Object *
sch_padid(int argc, Scheme_Object **argv) {
    Pad_Object *padobject;
    Scheme_Object *result = scheme_false;

    if (!SCHEME_CPTRP(argv[0]))
        scheme_wrong_type("sch_padid", "dynapad object of somekind", 0, argc, argv);

    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[0]);

    return scheme_make_integer(padobject->id);
}

static Scheme_Object *
sch_font(int argc, Scheme_Object **argv) {
    Pad_Text *padtext;
    Pad_String padstring;
    char *font;
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]) || text_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_font", "text%", 0, argc, argv);

    padtext = (Pad_Text *) SCHEME_CPTR_VAL(argv[0]);

    if (argc == 1) {
        padtext->Get_font(padstring);
        return UNIX_SCHSTR(padstring.Get());
    }

    if (!SCHEME_CHAR_STRINGP(argv[1]))
        scheme_wrong_type("sch_font", "string font", 1, argc, argv);

    font = SCH_UNIXSTR(argv[1]);

    padtext->Set_font(font);

    return result;
}

static Scheme_Object *
sch_fontnames(int argc, Scheme_Object **argv) {
    Pad *pad;
    Pad_List fonts;
    Pad_Iterator li;
    Pad_String *name;
    Scheme_Object *result = scheme_null;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_fontnames", "dynapad%", 0, argc, argv);

    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);

    Pad_FontData::Get_available_fonts(fonts);
    result = scheme_null;
    DOLIST(li, fonts, Pad_String, name) {
        result = scheme_append(result,
                               scheme_make_pair(UNIX_SCHSTR(name->Get()), scheme_null));
        delete name;
    }

    return result;
}

static Scheme_Object *
sch_background(int argc, Scheme_Object **argv) {
    Pad *pad;
    Pad_Win *win;
    Pad_String padstring;
    char *color;
    Pad_Color *padcolor;
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_background", "dynapad%", 0, argc, argv);

    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);

    win = pad->Get_win();

    if (argc == 1) {
        win->background.Get(padstring);
        return UNIX_SCHSTR(padstring.Get());
    }

    if (!SCHEME_CHAR_STRINGP(argv[1]))
        scheme_wrong_type("sch_background", "string colorname or #RRBBGG", 1, argc, argv);

    color = SCH_UNIXSTR(argv[1]);

    padcolor = Pad_AllocColorByName(color);
    if (!color)
        scheme_warning((char *) "sch_background: Pad_AllocColorByName failed for %s", color);

    win->background.Set(color);
    win->view->Damage();

    return result;
}

static Scheme_Object *
sch_update(int argc, Scheme_Object **argv) {
    Pad *pad;
    Pad_Win *win;
    int dissolve_speed = 0;
    Pad_Bool withRefinement = FALSE;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_update", "dynapad%", 0, argc, argv);

    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);
    win = pad->Get_win();

    if (argc > 1) {
        if (!SCHEME_INTP(argv[1]))
            scheme_wrong_type("sch_update", "integer", 1, argc, argv);
        dissolve_speed = SCHEME_INT_VAL(argv[1]);
    }
    if (argc > 2) {
        if (!SCHEME_BOOLP(argv[2]))
            scheme_wrong_type("sch_update", "boolean", 2, argc, argv);
        withRefinement = (argv[2] == scheme_true);
    }

    win->view->Update_display(dissolve_speed, withRefinement);

    return scheme_true;
}

static Scheme_Object *
sch_cursor(int argc, Scheme_Object **argv) {
    Pad *pad;
    Pad_Win *win;
    int cursorType;
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_cursor", "dynapad%", 0, argc, argv);

    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);
    win = pad->Get_win();

    if (argc == 2) {
        if (!SCHEME_INTP(argv[1]))
            scheme_wrong_type("sch_cursor", "integer", 2, argc, argv);
        cursorType = SCHEME_INT_VAL(argv[1]);
        win->Set_cursor(cursorType);
    }
    //win->view->Damage();

    return result;
}

static Scheme_Object *
sch_objectlist(int argc, Scheme_Object **argv) {
    Pad *pad;
    Pad_Win *win;
    int findargc = 1;
    char *findargv[1];
    int matchtype = -1;
    Pad_List objs;
    Pad_Iterator oi;
    Pad_Object *obj;
    Pad_Bool groupmembersp;
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_objectlist", "dynapad%", 0, argc, argv);
    if (!SCHEME_BOOLP(argv[1]))
        scheme_wrong_type("sch_objectlist", "boolean groupmembers?", 1, argc, argv);

    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);
    groupmembersp = (argv[1] == scheme_true);

    win = pad->Get_win();

    findargv[0] = Pad_GetUid("all");

    if (!pad->Find_eval(objs, (ClientData) win, groupmembersp, matchtype, findargc, findargv))
        scheme_warning((char *) "sch_objectlist: Find_eval failed %s", findargv[0]);

    result = scheme_null;
    DOLIST(oi, objs, Pad_Object, obj) {
        // must test userdata because only objects created by mrpad have userdata
        // eg UCSD logo is created by Pad++ and so doesn't have userdata
        if (obj->userdata && obj->findable)
            result = scheme_append(result,
                                   scheme_make_pair((Scheme_Object *) obj->userdata, scheme_null));
    }

    return result;
}

static Scheme_Object *
sch_unfindable(int argc, Scheme_Object **argv) {
    Pad *pad;
    Pad_Win *win;
    int findargc = 1;
    char *findargv[1];
    int matchtype = -1;
    Pad_List objs;
    Pad_Iterator oi;
    Pad_Object *obj;
    Pad_Bool groupmembersp;
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_unfindable", "dynapad%", 0, argc, argv);
    if (!SCHEME_BOOLP(argv[1]))
        scheme_wrong_type("sch_unfindable", "boolean groupmembers?", 1, argc, argv);

    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);
    groupmembersp = (argv[1] == scheme_true);

    win = pad->Get_win();

    findargv[0] = Pad_GetUid("all");

    if (!pad->Find_eval(objs, (ClientData) win, groupmembersp, matchtype, findargc, findargv))
        scheme_warning((char *) "sch_unfindable: Find_eval failed %s", findargv[0]);

    result = scheme_null;
    DOLIST(oi, objs, Pad_Object, obj) {
        // must test userdata because only objects created by mrpad have userdata
        // eg UCSD logo is created by Pad++ and so doesn't have userdata
        if (obj->userdata && !obj->findable)
            result = scheme_append(result,
                                   scheme_make_pair((Scheme_Object *) obj->userdata, scheme_null));
    }

    return result;
}

static Scheme_Object *
sch_xftenable(int argc, Scheme_Object **argv) {
    Pad *pad;
    Scheme_Object *result = scheme_true;
    Scheme_Object *sxftenable;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_xftenable", "dynapad%", 0, argc, argv);

    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);

    if (argc == 1) {
        return Pad_FontData::xftenable == TRUE ? scheme_true : scheme_false;
    }

    if (!SCHEME_BOOLP(argv[1]))
        scheme_wrong_type("sch_xftenable", "boolean", 1, argc, argv);
    sxftenable = argv[1];

    Pad_FontData::xftenable = (sxftenable == scheme_true) ? TRUE : FALSE;

    return sxftenable;
}

static Scheme_Object *
sch_visible(int argc, Scheme_Object **argv) {
    Pad *pad;
    Pad_Object *obj;
    Scheme_Object *result = scheme_null;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_visible", "dynapad%", 0, argc, argv);

    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);
    obj = pad->first;
    while (obj) {
        if (obj->Is_visible()) {
            if (obj->findable)
                result = scheme_append(result,
                                       scheme_make_pair((Scheme_Object *) obj->userdata, scheme_null));
        }
        obj = obj->next;
    }
    return result;
}

static Scheme_Object *
sch_anchor(int argc, Scheme_Object **argv) {
    Pad_Object *padobject;
    Pad_Win *win;
    char *anchor;
    Pad_Anchor padanchor;
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]))
        scheme_wrong_type("sch_anchor", "dynaobject% of some kind", 0, argc, argv);

    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[0]);
    win = padobject->pad->Get_win();

    if (argc == 1) {
        return UNIX_SCHSTR(Pad_NameOfAnchor(padobject->Get_anchor()));
    }

    if (!SCHEME_CHAR_STRINGP(argv[1]))
        scheme_wrong_type("sch_anchor", "string anchor", 1, argc, argv);

    anchor = SCH_UNIXSTR(argv[1]);
    if (Pad_GetAnchor(anchor, &padanchor) == PAD_ERROR)
        scheme_warning((char *) "sch_anchor: bad anchor value %s", anchor);

    if (!padobject->Set_anchor(padanchor))
        scheme_warning((char *) "sch_anchor: failed %s", anchor);

    return result;
}

static Scheme_Object *
sch_minsize(int argc, Scheme_Object **argv) {
    Pad_Object *padobject;
    Pad_String *padstring;
    Pad_Bool rc;
    float minsize;
    int fractionp;
    Scheme_Object *car, *list;
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]))
        scheme_wrong_type("sch_minsize", "dynaobject% of some kind", 0, argc, argv);

    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[0]);

    if (argc == 1) {
        minsize = padobject->Get_minsize();
        fractionp = padobject->Get_minsizep();
        if (fractionp)
            minsize /= 100;
        result =
            scheme_make_pair(scheme_make_float(minsize),
                             scheme_make_pair(fractionp ? scheme_true : scheme_false, scheme_null));
        return result;
    }

    if (!SCHEME_LISTP(argv[1]) && scheme_list_length(argv[1]) != 2)
        scheme_wrong_type("sch_minsize", "list (number fraction?)", 1, argc, argv);

    list = argv[1];
    car = SCHEME_CAR(list);
    if (!SCHEME_REALP(car))
        scheme_warning((char *) "sch_minsize: expects number, given ~V", car);
    minsize = scheme_real_to_double(car);

    list = SCHEME_CDR(list);
    car = SCHEME_CAR(list);
    if (!SCHEME_BOOLP(car))
        scheme_warning((char *) "sch_minsize: expects boolean, given ~V", car);
    fractionp = (car == scheme_true);

    if (fractionp)
        rc = padobject->Set_minsize_rel(minsize * 100);
    else
        rc = padobject->Set_minsize_abs(minsize);
    if (!rc)
        scheme_warning((char *) "sch_minsize: Set_minsize failed, given %V", argv[1]);

    return result;
}

static Scheme_Object *
sch_maxsize(int argc, Scheme_Object **argv) {
    Pad_Object *padobject;
    Pad_String *padstring;
    Pad_Bool rc;
    float maxsize;
    int fractionp;
    Scheme_Object *car, *list;
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]))
        scheme_wrong_type("sch_maxsize", "dynaobject% of some kind", 0, argc, argv);

    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[0]);

    if (argc == 1) {
        maxsize = padobject->Get_maxsize();
        fractionp = padobject->Get_maxsizep();
        if (fractionp)
            maxsize /= 100;
        result =
            scheme_make_pair(scheme_make_float(maxsize),
                             scheme_make_pair(fractionp ? scheme_true : scheme_false, scheme_null));
        return result;
    }

    if (!SCHEME_LISTP(argv[1]) && scheme_list_length(argv[1]) != 2)
        scheme_wrong_type("sch_maxsize", "list (number fraction?)", 1, argc, argv);

    list = argv[1];
    car = SCHEME_CAR(list);
    if (!SCHEME_REALP(car))
        scheme_warning((char *) "sch_maxsize: expects number, given ~V", car);
    maxsize = scheme_real_to_double(car);

    list = SCHEME_CDR(list);
    car = SCHEME_CAR(list);
    if (!SCHEME_BOOLP(car))
        scheme_warning((char *) "sch_maxsize: expects boolean, given ~V", car);
    fractionp = (car == scheme_true);

    if (fractionp)
        rc = padobject->Set_maxsize_rel(maxsize * 100);
    else
        rc = padobject->Set_maxsize_abs(maxsize);
    if (!rc)
        scheme_warning((char *) "sch_maxsize: Set_maxsize failed, given %V", argv[1]);

    return result;
}

static Scheme_Object *
sch_faderange(int argc, Scheme_Object **argv) {
    Pad_Object *padobject;
    float faderange;
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]))
        scheme_wrong_type("sch_faderange", "dynaobject% of some kind", 0, argc, argv);

    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[0]);

    if (argc == 1) {
        return scheme_make_float(padobject->Get_faderange());
    }

    if (!SCHEME_REALP(argv[1]))
        scheme_wrong_type("sch_faderange", "real", 1, argc, argv);

    faderange = scheme_real_to_double(argv[1]);

    if (!padobject->Set_faderange(faderange))
        result = scheme_false;
    //scheme_warning((char *)"sch_faderange: Set_faderange failed, given %V", argv[1]);

    return result;
}

static Scheme_Object *
sch_raise(int argc, Scheme_Object **argv) {
    Pad_Object *padobject, *abovethis;
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]))
        scheme_wrong_type("sch_raise", "dynaobject% of some kind", 0, argc, argv);

    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[0]);

    if (argc == 1) {
        if (!padobject->Raise())
            scheme_warning((char *) "sch_raise: failed, given %V", argv[0]);
        return result;
    }

    if (!SCHEME_CPTRP(argv[1])) {
        if (SCHEME_CHAR_STRINGP(argv[1]) && strcmp(SCH_UNIXSTR(argv[1]), "one") == 0) {
            abovethis = padobject->next;
        } else {
            scheme_wrong_type("sch_raise", "dynaobject% of some kind", 1, argc, argv);
        }
    } else {
        abovethis = (Pad_Object *) SCHEME_CPTR_VAL(argv[1]);
    }


    if (!padobject->Raise(abovethis))
        result = scheme_false;
    //scheme_warning((char *)"sch_raise: failed, given %V %V", argv[0], argv[1]);

    return result;
}

static Scheme_Object *
sch_lower(int argc, Scheme_Object **argv) {
    Pad_Object *padobject, *belowthis;
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]))
        scheme_wrong_type("sch_lower", "dynaobject% of some kind", 0, argc, argv);

    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[0]);

    if (argc == 1) {
        if (!padobject->Lower())
            scheme_warning((char *) "sch_lower: failed, given %V", argv[0]);
        return result;
    }

    if (!SCHEME_CPTRP(argv[1])) {
        if (SCHEME_CHAR_STRINGP(argv[1]) && strcmp(SCH_UNIXSTR(argv[1]), "one") == 0) {
            belowthis = padobject->prev;
        } else {
            scheme_wrong_type("sch_lower", "dynaobject% of some kind", 1, argc, argv);
        }
    } else {
        belowthis = (Pad_Object *) SCHEME_CPTR_VAL(argv[1]);
    }

    if (!padobject->Lower(belowthis))
        result = scheme_false;
    //scheme_warning((char *)"sch_lower: failed, given %V %V", argv[0], argv[1]);

    return result;
}

static Scheme_Object *
sch_order(int argc, Scheme_Object **argv) {
    Pad *pad;
    Pad_Object *obj;
    Scheme_Object *list, *car;
    Scheme_Object *result = scheme_null;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_order", "dynapad%", 0, argc, argv);

    if (!SCHEME_LISTP(argv[1]))
        scheme_wrong_type("sch_order", "list of dynaobject%", 1, argc, argv);

    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);
    list = argv[1];

    obj = pad->First();
    while (obj) {
        obj->findme = 0;
        obj = pad->Next(obj);
    }

    while (!SCHEME_NULLP(list)) {
        car = SCHEME_CAR(list);
        if (!SCHEME_CPTRP(car))
            scheme_wrong_type("sch_order", "dynaobject of some kind", -1, 0, &car);
        obj = (Pad_Object *) SCHEME_CPTR_VAL(car);
        // Warn user if object isn't on pad?
        if (obj->pad == pad)
            obj->findme = 1;

        list = SCHEME_CDR(list);
    }

    obj = pad->First();
    while (obj) {
        if (obj->findme) {
            result = scheme_append(result,
                                   scheme_make_pair((Scheme_Object *) obj->userdata, scheme_null));
        }
        obj->findme = 0;
        obj = pad->Next(obj);
    }

    return result;
}

static Scheme_Object *
sch_transparency(int argc, Scheme_Object **argv) {
    Pad_Object *padobject;
    float transparency;
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]))
        scheme_wrong_type("sch_transparency", "dynaobject% of some kind", 0, argc, argv);

    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[0]);

    if (argc == 1) {
        return scheme_make_float(padobject->Get_transparency());
    }

    if (!SCHEME_REALP(argv[1]))
        scheme_wrong_type("sch_transparency", "real between 0 and 1", 1, argc, argv);

    transparency = scheme_real_to_double(argv[1]);

    if (!padobject->Set_transparency(transparency))
        result = scheme_false;
    //scheme_warning((char *)"sch_transparency: failed, given %V", argv[1]);

    return result;
}

static Scheme_Object *
sch_visiblep(int argc, Scheme_Object **argv) {
    Pad_Object *padobject;

    if (!SCHEME_CPTRP(argv[0]))
        scheme_wrong_type("sch_visiblep", "dynaobject% of some kind", 0, argc, argv);

    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[0]);

    return padobject->Is_visible() ? scheme_true : scheme_false;
}

static Scheme_Object *
sch_sticky(int argc, Scheme_Object **argv) {
    Pad_Object *padobject;
    int sticky = -1;
    char *p;
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]))
        scheme_wrong_type("sch_sticky", "dynaobject% of some kind", 0, argc, argv);

    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[0]);

    if (argc == 1) {
        sticky = padobject->Get_sticky();
        switch (sticky) {
            case PAD_STICKY_NONE:
                return scheme_false;
            case PAD_STICKY_ALL:
                return scheme_true;
            case PAD_STICKY_X:
                return UNIX_SCHSTR("x");
            case PAD_STICKY_Y:
                return UNIX_SCHSTR("y");
            case PAD_STICKY_Z:
                return UNIX_SCHSTR("z");
            case PAD_STICKY_VIEW:
                return UNIX_SCHSTR("view");
            default:
                scheme_warning((char *) "sch_sticky: Get_sticky returned unknown value %d", sticky);
        }
    }

    if (argv[1] == scheme_false)
        sticky = PAD_STICKY_NONE;
    else if (argv[1] == scheme_true)
        sticky = PAD_STICKY_ALL;
    else if (SCHEME_CHAR_STRINGP(argv[1])) {
        p = SCH_UNIXSTR(argv[1]);
        if (!strcmp(p, "x"))
            sticky = PAD_STICKY_X;
        else if (!strcmp(p, "y"))
            sticky = PAD_STICKY_Y;
        else if (!strcmp(p, "z"))
            sticky = PAD_STICKY_Z;
        else if (!strcmp(p, "view"))
            sticky = PAD_STICKY_VIEW;
    }

    if (sticky == -1)
        scheme_wrong_type("sch_sticky", "boolean or x y z view", 1, argc, argv);

    if (!padobject->Set_sticky(sticky))
        scheme_warning((char *) "sch_sticky: failed, given %V", argv[1]);

    return result;
}

static Scheme_Object *
sch_findable(int argc, Scheme_Object **argv) {
    Pad_Object *padobject, *below;
    Scheme_Object *result = scheme_false;

    if (!SCHEME_CPTRP(argv[0]))
        scheme_wrong_type("sch_findable", "dynaobject% of some kind", 0, argc, argv);

    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[0]);

    if (argc == 1)
        return padobject->findable ? scheme_true : scheme_false;

    if (!SCHEME_BOOLP(argv[1]))
        scheme_wrong_type("sch_findable", "boolean", 1, argc, argv);

    result = argv[1];
    padobject->findable = (result == scheme_true) ? TRUE : FALSE;

    return result;
}

static Scheme_Object *
sch_boundable(int argc, Scheme_Object **argv) {
    Pad_Object *padobject, *below;
    Scheme_Object *result = scheme_false;

    if (!SCHEME_CPTRP(argv[0]))
        scheme_wrong_type("sch_boundable", "dynaobject% of some kind", 0, argc, argv);

    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[0]);

    if (argc == 1)
        return padobject->boundable ? scheme_true : scheme_false;

    if (!SCHEME_BOOLP(argv[1])) {
        scheme_wrong_type("sch_boundable", "boolean", 1, argc, argv);
        return scheme_false;
    }

    result = argv[1];
    padobject->boundable = (result == scheme_true) ? TRUE : FALSE;
    padobject->Compute_bounding_box();

    return result;
}

static Scheme_Object *
sch_events(int argc, Scheme_Object **argv) {
    Pad_Object *padobject, *below;
    Scheme_Object *result = scheme_false;

    if (!SCHEME_CPTRP(argv[0]))
        scheme_wrong_type("sch_events", "dynaobject% of some kind", 0, argc, argv);

    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[0]);

    if (argc == 1)
        return padobject->Get_events() ? scheme_true : scheme_false;

    if (!SCHEME_BOOLP(argv[1]))
        scheme_wrong_type("sch_events", "boolean", 1, argc, argv);

    result = argv[1];
    padobject->Set_events((result == scheme_false) ? FALSE : TRUE);

    return result;
}

static Scheme_Object *
sch_alwaysrender(int argc, Scheme_Object **argv) {
    Pad_Object *padobject, *below;
    Scheme_Object *result = scheme_false;

    if (!SCHEME_CPTRP(argv[0]))
        scheme_wrong_type("sch_alwaysrender", "dynaobject% of some kind", 0, argc, argv);

    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[0]);

    if (argc == 1)
        return padobject->Get_alwaysrender() ? scheme_true : scheme_false;

    if (!SCHEME_BOOLP(argv[1]))
        scheme_wrong_type("sch_alwaysrender", "boolean", 1, argc, argv);

    result = argv[1];
    padobject->Set_alwaysrender((result == scheme_false) ? FALSE : TRUE);

    return result;
}

static Scheme_Object *
sch_below(int argc, Scheme_Object **argv) {
    Pad_Object *padobject, *below;
    Scheme_Object *result = scheme_false;

    if (!SCHEME_CPTRP(argv[0]))
        scheme_wrong_type("sch_below", "dynaobject% of some kind", 0, argc, argv);

    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[0]);

    for (below = padobject->prev; below != NULL; below = below->prev)
        if (below->findable)
            break;

    if (below != NULL)
        result = (Scheme_Object *) below->userdata;

    return result;
}

static Scheme_Object *
sch_above(int argc, Scheme_Object **argv) {
    Pad_Object *padobject, *above;
    Scheme_Object *result = scheme_false;

    if (!SCHEME_CPTRP(argv[0]))
        scheme_wrong_type("sch_above", "dynaobject% of some kind", 0, argc, argv);

    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[0]);

    for (above = padobject->next; above != NULL; above = above->next)
        if (above->findable)
            break;

    if (above != NULL)
        result = (Scheme_Object *) above->userdata;

    return result;
}

static Scheme_Object *
sch_layer(int argc, Scheme_Object **argv) {
    Pad_Object *padobject;
    int layerId;
    Pad *pad;
    Pad_Win *win;
    Pad_Layer *layer;
    Scheme_Object *result = scheme_false;

    if (!SCHEME_CPTRP(argv[0]))
        scheme_wrong_type("sch_layer", "dynaobject% of some kind", 0, argc, argv);

    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[0]);

    pad = padobject->pad;
    win = pad->Get_win();

    if (argc == 1) {
        if ((layerId = padobject->layerId)) {
            layer = pad->Get_layer_from_id(layerId);
            if (layer->userdata)
                result = (Scheme_Object *) layer->userdata;
        }
        return result;
    }

    if (!SCHEME_CPTRP(argv[1]) || layer_typetag != SCHEME_CPTR_TYPE(argv[1]))
        scheme_wrong_type("sch_layer", "layer% or false", 1, argc, argv);

    layer = (Pad_Layer *) SCHEME_CPTR_VAL(argv[1]);

    padobject->Add_to_layer(layer->name);
    padobject->Damage();

    return result;
}

static Scheme_Object *
sch_makepanel(int argc, Scheme_Object **argv) {
    Pad *pad;
    Pad_Panel *padpanel;
    Scheme_Object *sobj, *obj;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_makepanel", "dynapad%", 0, argc, argv);

    // is-a? argv[1] panel%

    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);
    sobj = argv[1];
    scheme_dont_gc_ptr(sobj);

    padpanel = new Pad_Panel(pad);
    padpanel->userdata = sobj;
    obj = scheme_make_cptr(padpanel, panel_typetag);

    return obj;
}

static Scheme_Object *
sch_panelmoveview(int argc, Scheme_Object **argv) {
    Pad *pad;
    double dx, dy, zfac;
    double x, y;
    float fx, fy, zoom;
    Scheme_Object *car, *list;
    Scheme_Object *result = scheme_true;

    Pad_Panel *padgroup;
    Pad_List objs;
    Pad_Iterator oi;
    Pad_Object *obj;

    float bbox[4];

    if (!SCHEME_CPTRP(argv[0]) || panel_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_panelmoveview", "panel%", 0, argc, argv);

    if (!SCHEME_REALP(argv[1]))
        scheme_wrong_type("sch_panelmoveview", "dzoom dx dy", 1, argc, argv);
    if (!SCHEME_REALP(argv[2]))
        scheme_wrong_type("sch_panelmoveview", "dzoom dx dy", 2, argc, argv);
    if (!SCHEME_REALP(argv[3]))
        scheme_wrong_type("sch_panelmoveview", "dzoom dx dy", 3, argc, argv);

    padgroup = (Pad_Panel *) SCHEME_CPTR_VAL(argv[0]);

    zfac = scheme_real_to_double(argv[1]);
    dx = scheme_real_to_double(argv[2]);
    dy = scheme_real_to_double(argv[3]);

    if (padgroup->Get_sticky() == PAD_STICKY_ALL) {
        padgroup->pad->view->Get_view(fx, fy, zoom);
        dx /= zoom;
        dy /= zoom;
    }

    padgroup->Get_global_bbox(bbox);
    x = bbox[0];
    y = bbox[1];
    padgroup->Get_members(objs);

    padgroup->pad->Scale(objs, x, y, zfac, TRUE, 0);
    padgroup->pad->Slide(objs, dx, dy, TRUE, 0);

    return result;
}

static Scheme_Object *
sch_panelsetfill(int argc, Scheme_Object **argv) {
    Pad_Panel *padobject;
    Pad_String padstring;
    char *color;
    Pad_Color *padcolor;
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]))
        scheme_wrong_type("sch_panelsetfill", "panel%", 0, argc, argv);

    padobject = (Pad_Panel *) SCHEME_CPTR_VAL(argv[0]);

    if (!SCHEME_CHAR_STRINGP(argv[1]))
        scheme_wrong_type("sch_panelsetfill", "string colorname or #RRBBGG", 1, argc, argv);

    color = SCH_UNIXSTR(argv[1]);

    padcolor = Pad_AllocColorByName(color);
    if (!color)
        scheme_warning((char *) "sch_fill: Pad_AllocColorByName failed for %s", color);

    padobject->Set_fill(color);

    return result;
}

static Scheme_Object *
sch_makegroup(int argc, Scheme_Object **argv) {
    Pad *pad;
    Pad_Group *padgroup;
    Scheme_Object *sobj, *obj;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_makegroup", "dynapad%", 0, argc, argv);

    // is-a? argv[1] group%

    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);
    sobj = argv[1];
    scheme_dont_gc_ptr(sobj);

    padgroup = new Pad_Group(pad);
    padgroup->userdata = sobj;
    obj = scheme_make_cptr(padgroup, group_typetag);

    return obj;
}

static Scheme_Object *
sch_members(int argc, Scheme_Object **argv) {
    Pad_Container *padgroup;
    Pad_List objs;
    Pad_Iterator oi;
    Pad_Object *obj;
    Scheme_Object *list, *car;
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]) ||
        (group_typetag != SCHEME_CPTR_TYPE(argv[0]) &&
         panel_typetag != SCHEME_CPTR_TYPE(argv[0])))
        scheme_wrong_type("sch_members", "group%", 0, argc, argv);

    padgroup = (Pad_Container *) SCHEME_CPTR_VAL(argv[0]);

    if (argc == 1) {
        padgroup->Get_members(objs);
        result = scheme_null;
        DOLIST(oi, objs, Pad_Object, obj) {
            result = scheme_append(result,
                                   scheme_make_pair((Scheme_Object *) obj->userdata, scheme_null));
        }
        return result;
    }

    if (!SCHEME_LISTP(argv[1]))
        scheme_wrong_type("sch_members", "list of dynaobject%", 1, argc, argv);

    list = argv[1];
    while (!SCHEME_NULLP(list)) {
        car = SCHEME_CAR(list);
        if (!SCHEME_CPTRP(car))
            scheme_wrong_type("sch_members", "dynaobject of some kind", -1, 0, &car);
        objs.Push((Pad_Object *) SCHEME_CPTR_VAL(car));

        list = SCHEME_CDR(list);
    }

    padgroup->Set_members(&objs);

    return result;
}

static Scheme_Object *
sch_addmember(int argc, Scheme_Object **argv) {
    Pad_Group *padgroup;
    Pad_Object *padobject;
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]) ||
        (group_typetag != SCHEME_CPTR_TYPE(argv[0]) &&
         panel_typetag != SCHEME_CPTR_TYPE(argv[0])))
        scheme_wrong_type("sch_addmember", "group%", 0, argc, argv);
    if (!SCHEME_CPTRP(argv[1]))
        scheme_wrong_type("sch_addmember", "dynaobject of some kind", 1, argc, argv);

    padgroup = (Pad_Group *) SCHEME_CPTR_VAL(argv[0]);
    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[1]);

    if (!padgroup->Add(padobject, TRUE))
        result = scheme_false;
    // if false: scheme_warning((char *)"sch_addmember: failed, given %V", argv[1]);

    return result;
}

static Scheme_Object *
sch_removemember(int argc, Scheme_Object **argv) {
    Pad_Group *padgroup;
    Pad_Object *padobject;
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]) ||
        (group_typetag != SCHEME_CPTR_TYPE(argv[0]) &&
         panel_typetag != SCHEME_CPTR_TYPE(argv[0])))
        scheme_wrong_type("sch_removemember", "group%", 0, argc, argv);
    if (!SCHEME_CPTRP(argv[1]))
        scheme_wrong_type("sch_removemember", "dynaobject of some kind", 1, argc, argv);

    padgroup = (Pad_Group *) SCHEME_CPTR_VAL(argv[0]);
    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[1]);

    padgroup->Remove(padobject, TRUE);
    // if false:  scheme_warning((char *)"sch_removemember: failed, given %V", argv[1]);

    return result;
}

static Scheme_Object *
sch_divisible(int argc, Scheme_Object **argv) {
    Pad_Group *padgroup;
    Pad_Object *obj;
    Scheme_Object *list, *car;
    Scheme_Object *result = scheme_false;

    if (!SCHEME_CPTRP(argv[0]) ||
        (group_typetag != SCHEME_CPTR_TYPE(argv[0])
         &&
         panel_typetag != SCHEME_CPTR_TYPE(argv[0])))
        scheme_wrong_type("sch_divisible", "group%", 0, argc, argv);

    padgroup = (Pad_Group *) SCHEME_CPTR_VAL(argv[0]);

    if (argc == 1)
        return padgroup->Get_divisible() ? scheme_true : scheme_false;

    if (!SCHEME_BOOLP(argv[1]))
        scheme_wrong_type("sch_divisible", "boolean", 1, argc, argv);

    result = argv[1];
    padgroup->Set_divisible(((result == scheme_true) ? TRUE : FALSE));

    return result;
}

static Scheme_Object *
sch_getgroup(int argc, Scheme_Object **argv) {
    Pad_Object *padobject;
    Pad_Group *padgroup;
    Scheme_Object *result;

    if (!SCHEME_CPTRP(argv[0]))
        scheme_wrong_type("sch_getgroup", "dynapad object of somekind", 0, argc, argv);

    padobject = (Pad_Object *) SCHEME_CPTR_VAL(argv[0]);

    result = scheme_false;
    if (padobject) {
        if (padobject->group) {
            result = (Scheme_Object *) padobject->group->userdata;
        }
    }

    return result;
}

static Scheme_Object *
sch_makelayer(int argc, Scheme_Object **argv) {
    Pad *pad;
    Pad_Win *win;
    Pad_Layer *padlayer;
    char *name;
    Scheme_Object *sobj, *obj;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_makelayer", "dynapad%", 0, argc, argv);

    // is-a? argv[1] layer%

    if (!SCHEME_CHAR_STRINGP(argv[2]))
        scheme_wrong_type("sch_makelayer", "string", 2, argc, argv);

    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);
    sobj = argv[1];
    name = SCH_UNIXSTR(argv[2]);
    scheme_dont_gc_ptr(sobj);

    padlayer = pad->Get_layer_from_name(name);
    if (padlayer == NULL) {
        padlayer = pad->Create_layer(name);
        win = pad->Get_win();
        win->view->visibleLayers[padlayer->id] = TRUE;
    }

    padlayer->userdata = sobj;
    obj = scheme_make_cptr(padlayer, layer_typetag);

    return obj;
}

static Scheme_Object *
sch_deletelayer(int argc, Scheme_Object **argv) {
    Pad *pad;
    Pad_Layer *padlayer;
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]) || layer_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_deletelayer", "layer%", 0, argc, argv);

    padlayer = (Pad_Layer *) SCHEME_CPTR_VAL(argv[0]);

    pad = padlayer->pad;

    pad->Delete_layer(padlayer);
    scheme_gc_ptr_ok(argv[0]);

    return result;
}

static Scheme_Object *
sch_layername(int argc, Scheme_Object **argv) {
    Pad_Layer *padlayer;
    Scheme_Object *result = scheme_false;

    if (!SCHEME_CPTRP(argv[0]) || layer_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_layername", "layer%", 0, argc, argv);

    padlayer = (Pad_Layer *) SCHEME_CPTR_VAL(argv[0]);

    result = UNIX_SCHSTR(padlayer->name);

    return result;
}

static Scheme_Object *
sch_layermembers(int argc, Scheme_Object **argv) {
    Pad_Layer *padlayer;
    Pad *pad;
    Pad_List items;
    Pad_Object *obj;
    Pad_Iterator oi;
    Scheme_Object *result = scheme_false;

    if (!SCHEME_CPTRP(argv[0]) || layer_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_layername", "layer%", 0, argc, argv);

    padlayer = (Pad_Layer *) SCHEME_CPTR_VAL(argv[0]);

    pad = padlayer->pad;

    pad->Find_with_layer(padlayer, items, FALSE);

    result = scheme_null;
    DOLIST(oi, items, Pad_Object, obj) {
        if (obj->findable)
            result = scheme_append(result,
                                   scheme_make_pair((Scheme_Object *) obj->userdata, scheme_null));
    }

    return result;
}

static Scheme_Object *
sch_layers(int argc, Scheme_Object **argv) {
    Pad *pad;
    Pad_Layer *padlayer;
    Pad_Iterator oi;
    Scheme_Object *result = scheme_false;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_layers", "dynapad%", 0, argc, argv);

    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);

    result = scheme_null;
    DOLIST(oi, pad->layers, Pad_Layer, padlayer) {
        if (padlayer->userdata)
            result = scheme_append(result,
                                   scheme_make_pair((Scheme_Object *) padlayer->userdata, scheme_null));
    }

    return result;
}

static Scheme_Object *
sch_visiblelayer(int argc, Scheme_Object **argv) {
    Pad *pad;
    Pad_Layer *layer;
    Pad_Win *win;
    int visible;
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]) || layer_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_visiblelayer", "layer%", 0, argc, argv);

    layer = (Pad_Layer *) SCHEME_CPTR_VAL(argv[0]);

    pad = layer->pad;
    win = pad->Get_win();

    if (argc == 1) {
        return win->view->visibleLayers[layer->id] == TRUE ? scheme_true : scheme_false;
    }

    if (!SCHEME_BOOLP(argv[1]))
        scheme_wrong_type("sch_visiblelayer", "visible boolean", 1, argc, argv);
    visible = (argv[1] == scheme_true);

    win->view->visibleLayers[layer->id] = visible ? TRUE : FALSE;
    win->view->invisibleLayers[layer->id] = visible ? FALSE : TRUE;

    win->view->Damage();

    return result;
}

static Scheme_Object *
sch_raiselayer(int argc, Scheme_Object **argv) {
    Pad_Layer *padlayer, *padlayer2, *iter;
    int found = 0;
    Pad *pad;
    Pad_Iterator oi;
    Pad_Bool rc;
    Scheme_Object *result = scheme_false;

    if (!SCHEME_CPTRP(argv[0]) || layer_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_raiselayer", "layer%", 0, argc, argv);

    padlayer = (Pad_Layer *) SCHEME_CPTR_VAL(argv[0]);

    pad = padlayer->pad;

    if (argc == 1) {
        rc = pad->Raise_layer(padlayer);
    } else if (SCHEME_INTP(argv[1])) {
        padlayer2 = padlayer;
        DOLIST(oi, pad->layers, Pad_Layer, iter) {
            padlayer2 = iter;
            if (found) break;
            if (padlayer == iter) found = 1;
        }
        rc = pad->Raise_layer(padlayer, padlayer2);
    } else {
        if (!SCHEME_CPTRP(argv[1]) || layer_typetag != SCHEME_CPTR_TYPE(argv[1]))
            scheme_wrong_type("sch_raiselayer", "layer%", 1, argc, argv);

        padlayer2 = (Pad_Layer *) SCHEME_CPTR_VAL(argv[1]);
        rc = pad->Raise_layer(padlayer, padlayer2);
    }

    return rc == TRUE ? scheme_true : scheme_false;
}

static Scheme_Object *
sch_lowerlayer(int argc, Scheme_Object **argv) {
    Pad_Layer *padlayer, *padlayer2, *iter;
    int found = 0;
    Pad *pad;
    Pad_Iterator oi;
    Pad_Bool rc;
    Scheme_Object *result = scheme_false;

    if (!SCHEME_CPTRP(argv[0]) || layer_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_lowerlayer", "layer%", 0, argc, argv);

    padlayer = (Pad_Layer *) SCHEME_CPTR_VAL(argv[0]);

    pad = padlayer->pad;

    if (argc == 1) {
        rc = pad->Lower_layer(padlayer);
    } else if (SCHEME_INTP(argv[1])) {
        padlayer2 = padlayer;
        DOLIST(oi, pad->layers, Pad_Layer, iter) {
            if (padlayer == iter) found = 1;
            if (found) break;
            padlayer2 = iter;
        }
        rc = pad->Lower_layer(padlayer, padlayer2);
    } else {
        if (!SCHEME_CPTRP(argv[1]) || layer_typetag != SCHEME_CPTR_TYPE(argv[1]))
            scheme_wrong_type("sch_lowerlayer", "layer%", 1, argc, argv);

        padlayer2 = (Pad_Layer *) SCHEME_CPTR_VAL(argv[1]);
        rc = pad->Lower_layer(padlayer, padlayer2);
    }

    return rc == TRUE ? scheme_true : scheme_false;
}

static Scheme_Object *
sch_grab(int argc, Scheme_Object **argv) {
    Pad *pad;
    Pad_Win *win;
    Window winId;
    Pad_ImageData *imagedata;
    int len, i, xywh[4] = {0, 0, 10000, 10000};
    Scheme_Object *list, *car;
    Scheme_Object *sobj, *obj;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_grab", "dynapad%", 0, argc, argv);

    // is-a? argv[1] imagedata%

    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);
    sobj = argv[1];
    scheme_dont_gc_ptr(sobj);

    if (!SCHEME_INTP(argv[2]) && argv[2] != scheme_false)
        scheme_wrong_type("sch_grab", "window ID or false", 2, argc, argv);
    if (SCHEME_INTP(argv[2]))
        winId = SCHEME_INT_VAL(argv[2]);
    else
        winId = None;

    if (SCHEME_LISTP(argv[3])) {
        list = argv[3];
        len = scheme_list_length(list);
        if (len != 4)
            scheme_warning((char *) "sch_grab", "expected list x y w h, given ", list);
        i = 0;
        while (!SCHEME_NULLP(list)) {
            car = SCHEME_CAR(list);
            if (!SCHEME_INTP(car))
                scheme_wrong_type("sch_grab", "xywh integer", -1, 0, &car);
            xywh[i++] = SCHEME_INT_VAL(car);
            list = SCHEME_CDR(list);
        }
    }

    win = pad->Get_win();

    imagedata = win->dpy->Grab(winId, xywh[0], xywh[1], xywh[2], xywh[3]);

    if (!imagedata)
        scheme_warning((char *) "sch_grab", "Grab failed");

    imagedata->userdata = sobj;
    obj = scheme_make_cptr(imagedata, imagedata_typetag);

    return obj;
}

static Scheme_Object *
sch_winid(int argc, Scheme_Object **argv) {
    Pad *pad;
    Pad_Win *win;
    Window winId = None;
    Scheme_Object *result;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_winid", "dynapad%", 0, argc, argv);

    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);
    win = pad->Get_win();

    result = scheme_make_integer(win->id);

    return result;
}

static Scheme_Object *
sch_debugevent(int argc, Scheme_Object **argv) {
    Pad *pad;
    Pad_Win *win;
    Scheme_Object *result;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_winid", "dynapad%", 0, argc, argv);

    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);
    win = pad->Get_win();

    if (argc == 1)
        return win->debugEvent ? scheme_true : scheme_false;

    result = argv[1];
    win->debugEvent = (result == scheme_true) ? (char *) "" : NULL;

    return result;
}

static Scheme_Object *
sch_warp_pointer(int argc, Scheme_Object **argv) {
    Pad *pad;
    Pad_Win *win;
    int dx, dy;
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_warp_pointer", "dynapad%", 0, argc, argv);
    if (!SCHEME_INTP(argv[1]))
        scheme_wrong_type("sch_warp_pointer", "integer dx", 1, argc, argv);
    if (!SCHEME_INTP(argv[2]))
        scheme_wrong_type("sch_warp_pointer", "integer dy", 2, argc, argv);

    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);
    win = pad->Get_win();

    dx = SCHEME_INT_VAL(argv[1]);
    dy = SCHEME_INT_VAL(argv[2]);
    win->Warp(dx, dy);

    return result;
}

static Scheme_Object *
sch_winfo(int argc, Scheme_Object **argv) {
    Pad *pad;
    Pad_Win *win;
    Scheme_Object *result = scheme_true;
    int ix, iy, iw, ih, rootw, rooth;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_winfo", "dynapad%", 0, argc, argv);

    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);
    win = pad->Get_win();

    if (argc == 1) {

        /* get width and height of dynapad window */
        XWindowAttributes window_attributes_return;
        XGetWindowAttributes(win->dpy->display, win->id, &window_attributes_return);
        iw = window_attributes_return.width;
        ih = window_attributes_return.height;

        /* get upper left coordinates of dynapad window (wrt main screen) */
        Window returnWin;
        XTranslateCoordinates(win->dpy->display, win->id,
                              RootWindowOfScreen(win->dpy->screen),
                              0, 0, &ix, &iy, &returnWin);

        XGetWindowAttributes(win->dpy->display, RootWindowOfScreen(win->dpy->screen),
                             &window_attributes_return);
        rootw = window_attributes_return.width;
        rooth = window_attributes_return.height;

        result = scheme_null;
        result = scheme_append(result, scheme_make_pair(scheme_make_integer(ix), scheme_null));
        result = scheme_append(result, scheme_make_pair(scheme_make_integer(iy), scheme_null));
        result = scheme_append(result, scheme_make_pair(scheme_make_integer(iw), scheme_null));
        result = scheme_append(result, scheme_make_pair(scheme_make_integer(ih), scheme_null));
        result = scheme_append(result, scheme_make_pair(scheme_make_integer(rootw), scheme_null));
        result = scheme_append(result, scheme_make_pair(scheme_make_integer(rooth), scheme_null));

    } else {
        if (argc == 2) {
            if (!SCHEME_CHAR_STRINGP(argv[1]))
                scheme_wrong_type("sch_winfo", "string WxH+X+Y", 1, argc, argv);

            char *parsestring = SCH_UNIXSTR(argv[1]);
            int returnBits;
            int x_return, y_return;
            unsigned int width_return, height_return;

            returnBits = XParseGeometry(parsestring, &x_return, &y_return,
                                        &width_return, &height_return);
            // returnBits could be processed in more detail
            // for now assume all values are present and positive.
            XMoveResizeWindow(win->dpy->display, win->id, x_return, y_return,
                              width_return, height_return);
            // (doh! need to get the parent of pad->win or something.  fix later.)

            // code to replace TkDoConfigureNotify(win)
            {
                XEvent event;

                event.xconfigure.x = ix;
                event.xconfigure.y = iy;
                event.xconfigure.width = iw;
                event.xconfigure.height = ih;

                event.type = ConfigureNotify;
                event.xconfigure.serial = LastKnownRequestProcessed(win->dpy->display);
                event.xconfigure.send_event = True;
                event.xconfigure.display = win->dpy->display;
                event.xconfigure.event = win->id;
                event.xconfigure.window = win->id;
                //event.xconfigure.border_width = win->dpy->changes.border_width;
                //if (winPtr->changes.stack_mode == Above) {
                //    event.xconfigure.above = winPtr->changes.sibling;
                //} else {
                //    event.xconfigure.above = None;
                //}
                //event.xconfigure.override_redirect = winPtr->atts.override_redirect;

                // code to replace Tk_HandleEvent(&event);
                XSendEvent(win->dpy->display, win->id, False, 0, &event);
                XNextEvent(win->dpy->display, &event);
            }
        }
    }

    return result;
}

static Scheme_Object *
sch_idle(int argc, Scheme_Object **argv) {
    Pad *pad;
    Pad_Win *win;
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_idle", "dynapad%", 0, argc, argv);

    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);
    win = pad->Get_win();

    if (Pad_renderer->Is_interrupted(win))
        result = scheme_false;

    return result;
}

static Scheme_Object *
sch_rendertime(int argc, Scheme_Object **argv) {
    Pad *pad;
    Scheme_Object *result = scheme_false;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_rendertime", "dynapad%", 0, argc, argv);

    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);

    result = scheme_make_integer(pad->renderTime);

    return result;
}

static Scheme_Object *
sch_viewrendertime(int argc, Scheme_Object **argv) {
    Pad *pad;
    Scheme_Object *result = scheme_false;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_viewrendertime", "dynapad%", 0, argc, argv);

    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);

    result = scheme_make_integer(pad->view->renderTime);

    return result;
}

static Scheme_Object *
sch_dissolvespeed(int argc, Scheme_Object **argv) {
    Pad *pad;
    Pad_Win *win;
    Scheme_Object *result = scheme_false;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_dissolvespeed", "dynapad%", 0, argc, argv);

    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);
    win = pad->Get_win();

    result = scheme_make_integer(win->dissolveSpeed);

    if (argc == 2) {
        if (!SCHEME_INTP(argv[1]))
            scheme_wrong_type("sch_dissolvespeed", "integer 0-3", 1, argc, argv);
        win->dissolveSpeed = SCHEME_INT_VAL(argv[1]);
    }

    return result;
}

static Scheme_Object *
sch_refinedissolvespeed(int argc, Scheme_Object **argv) {
    Pad *pad;
    Pad_Win *win;
    Scheme_Object *result = scheme_false;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_refinedissolvespeed", "dynapad%", 0, argc, argv);

    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);
    win = pad->Get_win();

    result = scheme_make_integer(win->refineDissolveSpeed);

    if (argc == 2) {
        if (!SCHEME_INTP(argv[1]))
            scheme_wrong_type("sch_refinedissolvespeed", "integer 0-3", 1, argc, argv);
        win->refineDissolveSpeed = SCHEME_INT_VAL(argv[1]);
    }

    return result;
}

static Scheme_Object *
sch_desiredframerate(int argc, Scheme_Object **argv) {
    Pad *pad;
    Pad_Win *win;
    Scheme_Object *result = scheme_false;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_desiredframerate", "dynapad%", 0, argc, argv);

    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);
    win = pad->Get_win();

    result = scheme_make_integer(win->desiredFrameRate);

    if (argc == 2) {
        if (!SCHEME_INTP(argv[1]))
            scheme_wrong_type("sch_desiredframerate", "integer > 0", 1, argc, argv);
        win->desiredFrameRate = SCHEME_INT_VAL(argv[1]);
    }

    return result;
}

static Scheme_Object *
sch_refinementdelay(int argc, Scheme_Object **argv) {
    Pad *pad;
    Pad_Win *win;
    Scheme_Object *result = scheme_false;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_refinementdelay", "dynapad%", 0, argc, argv);

    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);
    win = pad->Get_win();

    result = scheme_make_integer(win->refinementDelay);

    if (argc == 2) {
        if (!SCHEME_INTP(argv[1]))
            scheme_wrong_type("sch_refinementdelay", "integer milliseconds", 1, argc, argv);
        win->refinementDelay = SCHEME_INT_VAL(argv[1]);
    }

    return result;
}

static Scheme_Object *
sch_doublebuffer(int argc, Scheme_Object **argv) {
    Pad *pad;
    Pad_Win *win;
    Scheme_Object *result = scheme_false;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_doublebuffer", "dynapad%", 0, argc, argv);

    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);
    win = pad->Get_win();

    result = (win->doubleBuffer ? scheme_true : scheme_false);

    if (argc == 2) {
        if (!SCHEME_BOOLP(argv[1]))
            scheme_wrong_type("sch_doublebuffer", "boolean", 1, argc, argv);
        win->doubleBuffer = (argv[1] == scheme_true ? TRUE : FALSE);
    }

    return result;
}

static Scheme_Object *
sch_fastpan(int argc, Scheme_Object **argv) {
    Pad *pad;
    Pad_Win *win;
    Scheme_Object *result = scheme_false;

    if (!SCHEME_CPTRP(argv[0]) || dynapad_typetag != SCHEME_CPTR_TYPE(argv[0]))
        scheme_wrong_type("sch_fastpan", "dynapad%", 0, argc, argv);

    pad = (Pad *) SCHEME_CPTR_VAL(argv[0]);
    win = pad->Get_win();

    result = (win->fastPan ? scheme_true : scheme_false);

    if (argc == 2) {
        if (!SCHEME_BOOLP(argv[1]))
            scheme_wrong_type("sch_fastpan", "boolean", 1, argc, argv);
        win->fastPan = (argv[1] == scheme_true ? TRUE : FALSE);
    }

    return result;
}

/* not used ??? */
static Scheme_Object *
sch_truncatefile(int argc, Scheme_Object **argv) {
    char *filename;
    int len;
    Scheme_Object *result = scheme_true;

    if (!SCHEME_CHAR_STRINGP(argv[0]) && !SCHEME_PATHP(argv[0]))
        scheme_wrong_type("sch_truncatefile", "string or path", 0, argc, argv);
    if (!SCHEME_INTP(argv[1]))
        scheme_wrong_type("sch_truncatefile", "integer > 0", 1, argc, argv);


    if (SCHEME_CHAR_STRINGP(argv[0]))
        filename = SCH_UNIXSTR(argv[0]);
    else
        filename = SCHEME_PATH_VAL(argv[0]);
    len = SCHEME_INT_VAL(argv[1]);

    result = (!truncate(filename, len)) ? scheme_true : scheme_false;

    return result;
}

/* not used ??? */
static Scheme_Object *
sch_os2unix(int argc, Scheme_Object **argv) {
    Scheme_Object *result = scheme_false;
    char *path, buf[1024];  /* yes, someday it will be too small */

    if (!SCHEME_CHAR_STRINGP(argv[0]))
        scheme_wrong_type("sch_os2unix", "path", 0, argc, argv);
    path = SCH_UNIXSTR(argv[0]);

#ifdef CYGWIN
                                                                                                                            cygwin_conv_to_full_posix_path(path, buf);
    path = buf;
#endif

    return UNIX_SCHSTR(path);
}

/* not used ??? */
static Scheme_Object *
sch_unix2os(int argc, Scheme_Object **argv) {
    Scheme_Object *result = scheme_false;
    char *path, buf[1024];  /* yes, someday it will be too small */

    if (!SCHEME_CHAR_STRINGP(argv[0]))
        scheme_wrong_type("sch_unix2os", "path", 0, argc, argv);
    path = SCH_UNIXSTR(argv[0]);

#ifdef CYGWIN
                                                                                                                            cygwin_conv_to_full_win32_path(path, buf);
    path = buf;
#endif

    return UNIX_SCHSTR(path);
}


Scheme_Object *scheme_reload(Scheme_Env *env) {

    scheme_add_global("wish",
                      scheme_make_prim_w_arity(sch_wish, "wish", 0, 0), env);
    scheme_add_global("sch_makedynapad",
                      scheme_make_prim_w_arity(sch_makedynapad, "sch_makedynapad", 2, 2), env);

    scheme_add_global("sch_moveto",
                      scheme_make_prim_w_arity(sch_moveto, "sch_moveto", 4, 4), env);
    scheme_add_global("sch_center",
                      scheme_make_prim_w_arity(sch_center, "sch_center", 7, 7), env);
    scheme_add_global("sch_centerbbox",
                      scheme_make_prim_w_arity(sch_centerbbox, "sch_centerbbox", 7, 7), env);
    scheme_add_global("sch_getview",
                      scheme_make_prim_w_arity(sch_getview, "sch_getview", 1, 1), env);
    scheme_add_global("sch_zoom",
                      scheme_make_prim_w_arity(sch_zoom, "sch_zoom", 5, 5), env);
    scheme_add_global("sch_modifier",
                      scheme_make_prim_w_arity(sch_modifier, "sch_modifier", 2, 3), env);
    scheme_add_global("sch_find",
                      scheme_make_prim_w_arity(sch_find, "sch_find", 3, 20), env);
    scheme_add_global("sch_pick",
                      scheme_make_prim_w_arity(sch_pick, "sch_pick", 3, 3), env);
    scheme_add_global("sch_cpanzoom",
                      scheme_make_prim_w_arity(sch_cpanzoom, "sch_cpanzoom", 1, 2), env);
    scheme_add_global("sch_getfocus",
                      scheme_make_prim_w_arity(sch_getfocus, "sch_getfocus", 1, 1), env);
    scheme_add_global("sch_fontnames",
                      scheme_make_prim_w_arity(sch_fontnames, "sch_fontnames", 1, 1), env);
    scheme_add_global("sch_background",
                      scheme_make_prim_w_arity(sch_background, "sch_background", 1, 2), env);
    scheme_add_global("sch_update",
                      scheme_make_prim_w_arity(sch_update, "sch_update", 1, 4), env);
    scheme_add_global("sch_cursor",
                      scheme_make_prim_w_arity(sch_cursor, "sch_cursor", 2, 2), env);
    scheme_add_global("sch_objectlist",
                      scheme_make_prim_w_arity(sch_objectlist, "sch_objectlist", 2, 2), env);
    scheme_add_global("sch_unfindable",
                      scheme_make_prim_w_arity(sch_unfindable, "sch_unfindable", 2, 2), env);
    scheme_add_global("sch_visible",
                      scheme_make_prim_w_arity(sch_visible, "sch_visible", 1, 1), env);
    scheme_add_global("sch_grab",
                      scheme_make_prim_w_arity(sch_grab, "sch_grab", 4, 4), env);
    scheme_add_global("sch_winid",
                      scheme_make_prim_w_arity(sch_winid, "sch_winid", 1, 1), env);
    scheme_add_global("sch_debugevent",
                      scheme_make_prim_w_arity(sch_debugevent, "sch_debugevent", 1, 2), env);

    scheme_add_global("sch_makerect",
                      scheme_make_prim_w_arity(sch_makerect, "sch_makerect", 2, 2), env);
    scheme_add_global("sch_coords",
                      scheme_make_prim_w_arity(sch_coords, "sch_coords", 1, 2), env);
    scheme_add_global("sch_width",
                      scheme_make_prim_w_arity(sch_width, "sch_width", 1, 2), env);
    scheme_add_global("sch_height",
                      scheme_make_prim_w_arity(sch_height, "sch_height", 1, 2), env);
    scheme_add_global("sch_makeoval",
                      scheme_make_prim_w_arity(sch_makeoval, "sch_makeoval", 2, 2), env);
    scheme_add_global("sch_makebutton",
                      scheme_make_prim_w_arity(sch_makebutton, "sch_makebutton", 2, 2), env);
    scheme_add_global("sch_makeline",
                      scheme_make_prim_w_arity(sch_makeline, "sch_makeline", 2, 2), env);
    scheme_add_global("sch_makepolygon",
                      scheme_make_prim_w_arity(sch_makepolygon, "sch_makepolygon", 2, 2), env);
    scheme_add_global("sch_xy_in_poly",
                      scheme_make_prim_w_arity(sch_xy_in_poly, "sch_xy_in_poly", 3, 3), env);
    scheme_add_global("sch_xinputdevices",
                      scheme_make_prim_w_arity(sch_xinputdevices, "sch_xinputdevices", 1, 1), env);
    scheme_add_global("sch_bind",
                      scheme_make_prim_w_arity(sch_bind, "sch_bind", 1, 3), env);
    scheme_add_global("sch_bind_to_tag",
                      scheme_make_prim_w_arity(sch_bind_to_tag, "sch_bind_to_tag", 3, 4), env);
    scheme_add_global("sch_bindtags",
                      scheme_make_prim_w_arity(sch_bindtags, "sch_bindtags", 1, 2), env);
    scheme_add_global("sch_delete",
                      scheme_make_prim_w_arity(sch_delete, "sch_delete", 1, 1), env);
    scheme_add_global("sch_slide",
                      scheme_make_prim_w_arity(sch_slide, "sch_slide", 3, 3), env);
    scheme_add_global("sch_scale",
                      scheme_make_prim_w_arity(sch_scale, "sch_scale", 4, 4), env);
    scheme_add_global("sch_position",
                      scheme_make_prim_w_arity(sch_position, "sch_position", 1, 2), env);
    scheme_add_global("sch_bbox",
                      scheme_make_prim_w_arity(sch_bbox, "sch_bbox", 1, 1), env);
    scheme_add_global("sch_warp_pointer",
                      scheme_make_prim_w_arity(sch_warp_pointer, "sch_warp_pointer", 3, 3), env);
    scheme_add_global("sch_winfo",
                      scheme_make_prim_w_arity(sch_winfo, "sch_winfo", 1, 5), env);
    scheme_add_global("sch_maketext",
                      scheme_make_prim_w_arity(sch_maketext, "sch_maketext", 2, 2), env);
    scheme_add_global("sch_settext",
                      scheme_make_prim_w_arity(sch_settext, "sch_settext", 2, 2), env);
    scheme_add_global("sch_gettext",
                      scheme_make_prim_w_arity(sch_gettext, "sch_gettext", 1, 1), env);
    scheme_add_global("sch_inserttext",
                      scheme_make_prim_w_arity(sch_inserttext, "sch_inserttext", 3, 3), env);
    scheme_add_global("sch_marktext",
                      scheme_make_prim_w_arity(sch_marktext, "sch_marktext", 2, 4), env);
    scheme_add_global("sch_deletetext",
                      scheme_make_prim_w_arity(sch_deletetext, "sch_deletetext", 2, 2), env);
    scheme_add_global("sch_padid",
                      scheme_make_prim_w_arity(sch_padid, "sch_padid", 1, 1), env);
    scheme_add_global("sch_focus",
                      scheme_make_prim_w_arity(sch_focus, "sch_focus", 1, 2), env);
    scheme_add_global("sch_font",
                      scheme_make_prim_w_arity(sch_font, "sch_font", 1, 2), env);
    scheme_add_global("sch_anchor",
                      scheme_make_prim_w_arity(sch_anchor, "sch_anchor", 1, 2), env);
    scheme_add_global("sch_zoomaction",
                      scheme_make_prim_w_arity(sch_zoomaction, "sch_zoomaction", 5, 5), env);
    scheme_add_global("sch_renderscript",
                      scheme_make_prim_w_arity(sch_renderscript, "sch_renderscript", 3, 3), env);
    scheme_add_global("sch_renderitem",
                      scheme_make_prim_w_arity(sch_renderitem, "sch_renderitem", 1, 1), env);
    scheme_add_global("sch_renderimage",
                      scheme_make_prim_w_arity(sch_renderimage, "sch_renderimage", 4, 4), env);
    scheme_add_global("sch_renderline",
                      scheme_make_prim_w_arity(sch_renderline, "sch_renderline", 2, 2), env);
    scheme_add_global("sch_rendercolor",
                      scheme_make_prim_w_arity(sch_rendercolor, "sch_rendercolor", 2, 2), env);
    scheme_add_global("sch_renderpolygon",
                      scheme_make_prim_w_arity(sch_renderpolygon, "sch_renderpolygon", 2, 2), env);
    scheme_add_global("sch_rendertext",
                      scheme_make_prim_w_arity(sch_rendertext, "sch_rendertext", 4, 4), env);
    scheme_add_global("sch_minsize",
                      scheme_make_prim_w_arity(sch_minsize, "sch_minsize", 1, 2), env);
    scheme_add_global("sch_maxsize",
                      scheme_make_prim_w_arity(sch_maxsize, "sch_maxsize", 1, 2), env);
    scheme_add_global("sch_faderange",
                      scheme_make_prim_w_arity(sch_faderange, "sch_faderange", 1, 2), env);
    scheme_add_global("sch_raise",
                      scheme_make_prim_w_arity(sch_raise, "sch_raise", 1, 2), env);
    scheme_add_global("sch_lower",
                      scheme_make_prim_w_arity(sch_lower, "sch_lower", 1, 2), env);
    scheme_add_global("sch_order",
                      scheme_make_prim_w_arity(sch_order, "sch_order", 2, 2), env);
    scheme_add_global("sch_transparency",
                      scheme_make_prim_w_arity(sch_transparency, "sch_transparency", 1, 2), env);
    scheme_add_global("sch_visiblep",
                      scheme_make_prim_w_arity(sch_visiblep, "sch_visiblep", 1, 1), env);
    scheme_add_global("sch_sticky",
                      scheme_make_prim_w_arity(sch_sticky, "sch_sticky", 1, 2), env);
    scheme_add_global("sch_findable",
                      scheme_make_prim_w_arity(sch_findable, "sch_findable", 1, 2), env);
    scheme_add_global("sch_boundable",
                      scheme_make_prim_w_arity(sch_boundable, "sch_boundable", 1, 2), env);
    scheme_add_global("sch_events",
                      scheme_make_prim_w_arity(sch_events, "sch_events", 1, 2), env);
    scheme_add_global("sch_alwaysrender",
                      scheme_make_prim_w_arity(sch_alwaysrender, "sch_alwaysrender", 1, 2), env);
    scheme_add_global("sch_below",
                      scheme_make_prim_w_arity(sch_below, "sch_below", 1, 1), env);
    scheme_add_global("sch_above",
                      scheme_make_prim_w_arity(sch_above, "sch_above", 1, 1), env);
    scheme_add_global("sch_layer",
                      scheme_make_prim_w_arity(sch_layer, "sch_layer", 1, 2), env);
    scheme_add_global("sch_idle",
                      scheme_make_prim_w_arity(sch_idle, "sch_idle", 1, 1), env);
    scheme_add_global("sch_rendertime",
                      scheme_make_prim_w_arity(sch_rendertime, "sch_rendertime", 1, 1), env);
    scheme_add_global("sch_viewrendertime",
                      scheme_make_prim_w_arity(sch_viewrendertime, "sch_viewrendertime", 1, 1), env);
    scheme_add_global("sch_dissolvespeed",
                      scheme_make_prim_w_arity(sch_dissolvespeed, "sch_dissolvespeed", 1, 2), env);
    scheme_add_global("sch_refinedissolvespeed",
                      scheme_make_prim_w_arity(sch_refinedissolvespeed, "sch_refinedissolvespeed", 1, 2), env);
    scheme_add_global("sch_desiredframerate",
                      scheme_make_prim_w_arity(sch_desiredframerate, "sch_desiredframerate", 1, 2), env);
    scheme_add_global("sch_refinementdelay",
                      scheme_make_prim_w_arity(sch_refinementdelay, "sch_refinementdelay", 1, 2), env);
    scheme_add_global("sch_doublebuffer",
                      scheme_make_prim_w_arity(sch_doublebuffer, "sch_doublebuffer", 1, 2), env);
    scheme_add_global("sch_fastpan",
                      scheme_make_prim_w_arity(sch_fastpan, "sch_fastpan", 1, 2), env);
    scheme_add_global("sch_xftenable",
                      scheme_make_prim_w_arity(sch_xftenable, "sch_xftenable", 1, 2), env);

    scheme_add_global("sch_imagep",
                      scheme_make_prim_w_arity(sch_imagep, "sch_imagep", 1, 1), env);
    scheme_add_global("sch_makeimagedata",
                      scheme_make_prim_w_arity(sch_makeimagedata, "sch_makeimagedata", 3, 3), env);
    scheme_add_global("sch_makeimage",
                      scheme_make_prim_w_arity(sch_makeimage, "sch_makeimage", 2, 2), env);
    scheme_add_global("sch_imagedata",
                      scheme_make_prim_w_arity(sch_imagedata, "sch_imagedata", 1, 2), env);
    scheme_add_global("sch_imagepath",
                      scheme_make_prim_w_arity(sch_imagepath, "sch_imagepath", 1, 1), env);
    scheme_add_global("sch_imagedim",
                      scheme_make_prim_w_arity(sch_imagedim, "sch_imagedim", 1, 1), env);
    scheme_add_global("sch_imagedata_rgb",
                      scheme_make_prim_w_arity(sch_imagedata_rgb, "sch_imagedata_rgb", 3, 7), env);
    scheme_add_global("sch_freeimagedata",
                      scheme_make_prim_w_arity(sch_freeimagedata, "sch_freeimagedata", 2, 2), env);
    scheme_add_global("sch_fill",
                      scheme_make_prim_w_arity(sch_fill, "sch_fill", 1, 2), env);
    scheme_add_global("sch_pen",
                      scheme_make_prim_w_arity(sch_pen, "sch_pen", 1, 2), env);
    scheme_add_global("sch_penwidth",
                      scheme_make_prim_w_arity(sch_penwidth, "sch_penwidth", 1, 3), env);
    scheme_add_global("sch_abslinestyle",
                      scheme_make_prim_w_arity(sch_abslinestyle, "sch_abslinestyle", 1, 1), env);
    scheme_add_global("sch_add_tag",
                      scheme_make_prim_w_arity(sch_add_tag, "sch_add_tag", 2, 2), env);
    scheme_add_global("sch_delete_tag",
                      scheme_make_prim_w_arity(sch_delete_tag, "sch_delete_tag", 2, 2), env);

    scheme_add_global("sch_panelsetfill",
                      scheme_make_prim_w_arity(sch_panelsetfill, "sch_panelsetfill", 2, 2), env);
    scheme_add_global("sch_panelmoveview",
                      scheme_make_prim_w_arity(sch_panelmoveview, "sch_panelmoveview", 4, 4), env);
    scheme_add_global("sch_makepanel",
                      scheme_make_prim_w_arity(sch_makepanel, "sch_makepanel", 2, 2), env);
    scheme_add_global("sch_makegroup",
                      scheme_make_prim_w_arity(sch_makegroup, "sch_makegroup", 2, 2), env);
    scheme_add_global("sch_members",
                      scheme_make_prim_w_arity(sch_members, "sch_members", 1, 2), env);
    scheme_add_global("sch_addmember",
                      scheme_make_prim_w_arity(sch_addmember, "sch_addmember", 2, 2), env);
    scheme_add_global("sch_removemember",
                      scheme_make_prim_w_arity(sch_removemember, "sch_removemember", 2, 2), env);
    scheme_add_global("sch_divisible",
                      scheme_make_prim_w_arity(sch_divisible, "sch_divisible", 1, 2), env);
    scheme_add_global("sch_getgroup",
                      scheme_make_prim_w_arity(sch_getgroup, "sch_getgroup", 1, 1), env);

    scheme_add_global("sch_makelayer",
                      scheme_make_prim_w_arity(sch_makelayer, "sch_makelayer", 3, 3), env);
    scheme_add_global("sch_deletelayer",
                      scheme_make_prim_w_arity(sch_deletelayer, "sch_deletelayer", 1, 1), env);
    scheme_add_global("sch_layername",
                      scheme_make_prim_w_arity(sch_layername, "sch_layername", 1, 1), env);
    scheme_add_global("sch_layermembers",
                      scheme_make_prim_w_arity(sch_layermembers, "sch_layermembers", 1, 1), env);
    scheme_add_global("sch_layers",
                      scheme_make_prim_w_arity(sch_layers, "sch_layers", 1, 1), env);
    scheme_add_global("sch_visiblelayer",
                      scheme_make_prim_w_arity(sch_visiblelayer, "sch_visiblelayer", 1, 2), env);
    scheme_add_global("sch_raiselayer",
                      scheme_make_prim_w_arity(sch_raiselayer, "sch_raiselayer", 1, 2), env);
    scheme_add_global("sch_lowerlayer",
                      scheme_make_prim_w_arity(sch_lowerlayer, "sch_lowerlayer", 1, 2), env);

    scheme_add_global("sch_truncatefile",
                      scheme_make_prim_w_arity(sch_truncatefile, "sch_truncatefile", 2, 2), env);

    scheme_add_global("sch_os2unix",
                      scheme_make_prim_w_arity(sch_os2unix, "sch_os2unix", 1, 1), env);
    scheme_add_global("sch_unix2os",
                      scheme_make_prim_w_arity(sch_unix2os, "sch_unix2os", 1, 1), env);

    dynapad_typetag = scheme_intern_symbol("dynapad");
    scheme_dont_gc_ptr(dynapad_typetag);
    rect_typetag = scheme_intern_symbol("rect");
    scheme_dont_gc_ptr(rect_typetag);
    oval_typetag = scheme_intern_symbol("oval");
    scheme_dont_gc_ptr(oval_typetag);
    button_typetag = scheme_intern_symbol("button");
    scheme_dont_gc_ptr(button_typetag);
    line_typetag = scheme_intern_symbol("line");
    scheme_dont_gc_ptr(line_typetag);
    polygon_typetag = scheme_intern_symbol("polygon");
    scheme_dont_gc_ptr(polygon_typetag);
    imagedata_typetag = scheme_intern_symbol("imagedata");
    scheme_dont_gc_ptr(imagedata_typetag);
    image_typetag = scheme_intern_symbol("image");
    scheme_dont_gc_ptr(image_typetag);
    text_typetag = scheme_intern_symbol("text");
    scheme_dont_gc_ptr(text_typetag);
    panel_typetag = scheme_intern_symbol("panel");
    scheme_dont_gc_ptr(panel_typetag);
    group_typetag = scheme_intern_symbol("group");
    scheme_dont_gc_ptr(group_typetag);

    return UNIX_SCHSTR("Hello Pad!");
}

Scheme_Object *scheme_initialize(Scheme_Env *env) {
    return scheme_reload(env);
}


Scheme_Object *scheme_module_name() {
    /* This extension doesn't define a module: */
    return scheme_false;
}
