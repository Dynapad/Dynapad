/* $Id: dtmouse.c,v 1.1.1.1 2005/10/09 04:28:49 stanonik Exp $ */

/*
 * Copyright 2001, 2002 Mitsubishi Electric Research Laboratories.
 * All Rights Reserved.
 * 
 * Permission to use, copy and modify this software and its
 * documentation for educational, research and non-profit
 * purposes, without fee, and without a written agreement is
 * hereby granted, provided that the above copyright notice and
 * the following three paragraphs appear in all copies, and the
 * software and documentation is not redistributed.
 * 
 * To request Permission to incorporate this software into
 * commercial products contact MERL - Mitsubishi Electric
 * Research Laboratories, 201 Broadway, Cambridge, MA 02139.
 * 
 * IN NO EVENT SHALL MERL BE LIABLE TO ANY PARTY FOR DIRECT,
 * INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES,
 * INCLUDING LOST PROFITS, ARISING OUT OF THE USE OF THIS
 * SOFTWARE AND ITS DOCUMENTATION, EVEN IF MERL HAS BEEN ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGES.
 * 
 * MERL SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT
 * NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
 * FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE PROVIDED
 * HEREUNDER IS ON AN "AS IS" BASIS, AND MERL HAS NO OBLIGATIONS
 * TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
 * MODIFICATIONS.
 *
 * Author:
 * Sam Shipman
 * shipman@merl.com
 * 617.621.7520
 */

/*
 * dtmouse.c
 *
 * DiamondTouch Mouse Emulation
 *
 * This file contains ANSI C code.
 */

#include <stdlib.h>
#include <math.h>
#include "dt.h"
#include "dtframe.h"
#include "dtmouse.h"

/* Number of frames that must elapse before giving up on a "tap". */
/* #define SHORT_TIME 11 */
#define SHORT_TIME 4

/* Distance (in "raw" coordinates) between two touches beyond which
   they are not recognized as a "tap": 3 cm */
#define SMALL_DISTANCE 6

#define STATE_START 0
#define STATE_MOVE  1
#define STATE_COUNT 2
#define STATE_DRAG  3

#define STATE_0    0
#define STATE_1    1
#define STATE_2    2
#define STATE_3    3
#define STATE_4    4
#define STATE_5    5
#define STATE_6    6
#define STATE_7    7
#define STATE_8    8

#define BUTTON_UP   0
#define BUTTON_DOWN 1

#define LEFT_BUTTON 0
#define MIDDLE_BUTTON 1
#define RIGHT_BUTTON 2

#define NO_USER (-1)

#define BEEP_DURATION 100

/* Per-user mouse emulation info. */
typedef struct {
    dt_table *table;
    int is_left;               /* true if controlling left button */

    int is_down;
    int is_pushed;
    int count;

    int state;
    int count2;
    dt_point place;            /* where the mouse is supposed to be */
    dt_point place_box_ul;     /* keep track of bounding box, too */
    dt_point place_box_lr;     /* keep track of bounding box, too */
    int button_armed;
    dt_user_id user_id;
} dt_me_user;

static int policy = 0;

static int touchHyst = 0;

static int is_beep = 0;

/* FIXME: do synchronization on this variable. */

/* Which user is in click-and-drag mode? */
volatile static int user_dragging = NO_USER;

/*
 * Static Data
 */

/* Mouse emulation per-user information. */
static dt_me_user user[DT_MAX_USERS];

/*
 * Static Function Prototypes
 */

static
void
dt_me_mode_default(dt_mouse_action_list *malp, dt_event *ep);

static
void
dt_me_mode_anti_noise(dt_mouse_action_list *malp, dt_event *ep);

static
unsigned short
button_action(int left, int down);

static
void
set_mouse_action(dt_mouse_action *map,
                 unsigned short action,
                 dt_user_id user_id,
                 unsigned short x,
                 unsigned short y,
                 unsigned int wheel,
                 dt_timestamp time,
                 dt_point box_ul,
                 dt_point box_lr);

static
double
distance(int x0, int y0, int x1, int y1);

static
double
bb_diagonal(dt_event *ep);

int
close_enough(dt_event *ep, dt_me_user *userp);

static
void
mal_set_move_abs(dt_mouse_action_list *malp, dt_me_user *userp,
                       dt_timestamp time);

static
void
mal_set_button_down(dt_mouse_action_list *malp, dt_me_user *userp,
                          dt_timestamp time);

static
void
mal_set_button_up(dt_mouse_action_list *malp, dt_me_user *userp,
                        dt_timestamp time);

static
void
mal_set_button_click(dt_mouse_action_list *malp, dt_me_user *userp,
                           dt_timestamp time);


/*
 * Exported Functions
 */
void
dt_me_initialize(int user_id)
{
    dt_me_user *userp;
    userp = &user[user_id];
    userp->table = dt_get_table((dt_user_id)user_id);
    userp->is_down = 0;
    userp->is_pushed = 0;
    userp->count = 0;

    if (   (policy == DT_MOUSE_MODE_MELS_CAFE)
        || (policy == DT_MOUSE_MODE_WEB_DEMO)
        || (policy == DT_MOUSE_MODE_MAP_DEMO)) {

        userp->is_left = !(user_id & 0x1);
    }
    else {
        userp->is_left = 1;
    }

    userp->state = 0;
    userp->count2 = 0;
    userp->place.x = 0;
    userp->place.y = 0;
    userp->place_box_ul.x = 0;
    userp->place_box_ul.y = 0;
    userp->place_box_lr.x = 0;
    userp->place_box_lr.y = 0;

    userp->button_armed = userp->is_left;
    userp->user_id = (dt_user_id)user_id;
    return;
}


void
dt_me_uninitialize(int user_id)
{
    return;
}


int
dt_me_get_mode(void)
{
    return policy;
}


void
dt_me_set_mode(int p)
{
    if ((p >= 0) && (p < DT_NUM_MOUSE_MODES)) {
        policy = p;
    }
}


int
dt_me_get_beep(void)
{
    return is_beep;
}


void
dt_me_set_beep(int is_on)
{
    is_beep = is_on;
}


void
dt_me_update(dt_mouse_action_list *malp, dt_event *ep)
{
    dt_me_user *userp;
#if 0
    extern void mytrace2(int a, int b);
#endif

    userp = &user[ep->user_id];

    /* The default behavior is to do nothing. */
    malp->n = 0;

    /* For older mouse modes. */
    if (ep->touch) {
        if (userp->is_pushed) {
            userp->count++;
        }
        else {
            userp->is_pushed = 1;
            userp->count = 1;
        }
    }
    else {
        if (userp->is_pushed) {
            userp->is_pushed = 0;
            userp->count = 1;
        }
        else {
            userp->count++;
        }
    }

    /* We always move the (one and only) mouse before any other
       operation (e.g., button down) in case it was being "shared" by
       another user and moved away from where the current user thinks
       it is.  How well this will work for any random application
       depends on how that application tracks the mouse, and whether
       it is "multi-user aware".  Some single-user applications might
       get confused if multiple users are passing the mouse back and
       forth.  */

    switch (policy) {
    case DT_MOUSE_MODE_DEFAULT:
    case DT_MOUSE_MODE_WEB_DEMO:
        dt_me_mode_default(malp, ep);
        break;

    case DT_MOUSE_MODE_PAINT:
    case DT_MOUSE_MODE_MAP_DEMO:
        if (userp->is_pushed && (userp->count > touchHyst)
            && !userp->is_down) {

            /* Send mouse-button-down. */
            userp->place = ep->max_point;
            userp->place_box_ul.x = ep->box_ul.x * DT_INTER;
            userp->place_box_ul.y = ep->box_ul.y * DT_INTER;
            userp->place_box_lr.x = ep->box_lr.x * DT_INTER;
            userp->place_box_lr.y = ep->box_lr.y * DT_INTER;
            mal_set_button_down(malp, userp, ep->time);
            userp->is_down = 1;
        }
        else if (!userp->is_pushed && (userp->count > touchHyst)
                 && userp->is_down) {

            mal_set_button_up(malp, userp, ep->time);
            userp->is_down = 0;
        }
        else if (ep->touch) {
            /* Just move the mouse. */
            userp->place = ep->max_point;
            userp->place_box_ul.x = ep->box_ul.x * DT_INTER;
            userp->place_box_ul.y = ep->box_ul.y * DT_INTER;
            userp->place_box_lr.x = ep->box_lr.x * DT_INTER;
            userp->place_box_lr.y = ep->box_lr.y * DT_INTER;
            mal_set_move_abs(malp, userp, ep->time);
        }
        break;

    case DT_MOUSE_MODE_MELS_CAFE:
        if (userp->is_pushed) {
            if ((userp->count > touchHyst) && !userp->is_down) {
                /* move mouse and do click */
                userp->place = ep->max_point;
                userp->place_box_ul.x = ep->box_ul.x * DT_INTER;
                userp->place_box_ul.y = ep->box_ul.y * DT_INTER;
                userp->place_box_lr.x = ep->box_lr.x * DT_INTER;
                userp->place_box_lr.y = ep->box_lr.y * DT_INTER;
                mal_set_button_click(malp, userp, ep->time);
                userp->is_down = 1;
            }
        }
        else {
            if ((userp->count > touchHyst) && userp->is_down) {
                userp->is_down = 0;
            }
        }
        break;

    case DT_MOUSE_MODE_MOVE_ONLY:
        /* If touching, move the cursor. */
        if (ep->touch) {
            userp->place = ep->max_point;
            userp->place_box_ul.x = ep->box_ul.x * DT_INTER;
            userp->place_box_ul.y = ep->box_ul.y * DT_INTER;
            userp->place_box_lr.x = ep->box_lr.x * DT_INTER;
            userp->place_box_lr.y = ep->box_lr.y * DT_INTER;
            mal_set_move_abs(malp, userp, ep->time);
        }
        break;

    case DT_MOUSE_MODE_ANTI_NOISE:
        dt_me_mode_anti_noise(malp, ep);
        break;

    default:
        ASSERT(0);
        break;
    }
}


/*
 * Static Functions
 */

static
unsigned short
button_action(int left, int down)
{
    if (left) {
        if (down) {
            return DT_MOUSE_ACTION_LEFT_DOWN;
        }
        else {
            return DT_MOUSE_ACTION_LEFT_UP;
        }
    }
    else {
        if (down) {
            return DT_MOUSE_ACTION_RIGHT_DOWN;
        }
        else {
            return DT_MOUSE_ACTION_RIGHT_UP;
        }
    }
}


static
void
set_mouse_action(dt_mouse_action *map,
                 unsigned short action,
                 dt_user_id user_id,
                 unsigned short x,
                 unsigned short y,
                 unsigned int wheel,
                 dt_timestamp time,
                 dt_point box_ul,
                 dt_point box_lr)
{
    map->action = action;
    map->user_id = user_id;
    map->data.move.x = x;
    map->data.move.y = y;
    if (map->action == DT_MOUSE_ACTION_WHEEL) {
        map->data.wheel = wheel;
    }
    map->time = time;
    map->box_ul = box_ul;
    map->box_lr = box_lr;
}


static
double
distance(int x0, int y0, int x1, int y1)
{
    int x;
    int y;
    int sumsqs;

    x = x1 - x0;
    y = y1 - y0;
    sumsqs = x * x + y * y;
    return sqrt((double)sumsqs);
}


static
double
bb_diagonal(dt_event *ep)
{
    return distance((int)ep->box_ul.x, (int)ep->box_ul.y,
                    (int)ep->box_lr.x, (int)ep->box_lr.y);
}


int
close_enough(dt_event *ep, dt_me_user *userp)
{
    return (distance(userp->place.x, userp->place.y, ep->max_point.x,
                     ep->max_point.y) < (double)(DT_INTER *
                                                 SMALL_DISTANCE));
#if 0
    int xdiff = ep->max_point.x - userp->place.x;
    int ydiff = ep->max_point.y - userp->place.y;

    if (xdiff < 0) xdiff = -xdiff;
    if (ydiff < 0) ydiff = -ydiff;
    if ((xdiff > DT_INTER * SMALL_DISTANCE)
        || (ydiff > DT_INTER * SMALL_DISTANCE)) {
    }
#endif
}


/*
 * Modes
 */

static
void
dt_me_mode_default(dt_mouse_action_list *malp, dt_event *ep)
{
    dt_me_user *userp;
    int F;
    double dist;
#if 0
    extern void mytrace2(int a, int b);
#endif

    userp = &user[ep->user_id];

    /* Calculate "number of fingers." */
    dist = bb_diagonal(ep);
    F = 0;
    if (ep->touch) {
        F = 1;
        if ((dist >= 7.0) && (dist < 20.0)) {   /* [2.5, 10) centimeters */
            F = 2;
        }
    }

#if 0
    if (ep->user_id == 1) {
        mytrace2(userp->state, F);
    }
#endif

    /* state machine (not a true FSM) */

    switch (userp->state) {
    case STATE_0:
        if (user_dragging == (int)ep->user_id) {
            user_dragging = NO_USER;
        }

        if (F == 0) {
            userp->state = STATE_0;
        }
        else if (F == 1) {
            userp->button_armed = userp->is_left;    /* "left" button */
            userp->place = ep->max_point;
            userp->place_box_ul.x = ep->box_ul.x * DT_INTER;
            userp->place_box_ul.y = ep->box_ul.y * DT_INTER;
            userp->place_box_lr.x = ep->box_lr.x * DT_INTER;
            userp->place_box_lr.y = ep->box_lr.y * DT_INTER;
            if (user_dragging == NO_USER) {
                mal_set_move_abs(malp, userp, ep->time);
            }
            userp->state = STATE_1;
        }
        else if (F == 2) {
            userp->state = STATE_0;
        }
        else {
            ASSERT(0);
        }
        break;

    case STATE_1:
        /*
         * NOTE: this is a HACK until we make the MAL bigger.
         */
        if (user_dragging == (int)ep->user_id) {
            user_dragging = NO_USER;
        }

        if (F == 0) {
            userp->count2 = SHORT_TIME;
            userp->state = STATE_2;
        }
        else if (F == 1) {
            userp->place = ep->max_point;
            userp->place_box_ul.x = ep->box_ul.x * DT_INTER;
            userp->place_box_ul.y = ep->box_ul.y * DT_INTER;
            userp->place_box_lr.x = ep->box_lr.x * DT_INTER;
            userp->place_box_lr.y = ep->box_lr.y * DT_INTER;
            if (user_dragging == NO_USER) {
                mal_set_move_abs(malp, userp, ep->time);
            }
            userp->state = STATE_1;
        }
        else if (F == 2) {
            userp->button_armed = !userp->is_left; /* "right" button */
            userp->state = STATE_1;
        }
        else {
            ASSERT(0);
        }
        break;

    case STATE_2:
        if ((F == 0) && (userp->count2 == 0)) {
            if (user_dragging == NO_USER) {
                mal_set_button_click(malp, userp, ep->time);
            }
            userp->state = STATE_0;
        }
        else if (F == 0) {
            userp->count2--;
            ASSERT(userp->count2 >= 0);
            userp->state = STATE_2;
        }
        else if ((F == 1) && !close_enough(ep, userp)) {
            if (user_dragging == NO_USER) {
                mal_set_button_click(malp, userp, ep->time);
            }
            userp->button_armed = userp->is_left;    /* "left" button */
            userp->state = STATE_1;
        }
        else if (F == 1) {
            userp->count2 = SHORT_TIME;
            userp->state = STATE_3;
        }
        else if (F == 2) {
            userp->state = STATE_7;
        }
        else {
            ASSERT(0);
        }
        break;

    case STATE_3:
        if (F == 0) {
            userp->count2 = SHORT_TIME;
            userp->state = STATE_4;
        }
        else if ((F == 1) && (userp->count2 == 0)
                 && (user_dragging != NO_USER)) {
            userp->place = ep->max_point;
            userp->place_box_ul.x = ep->box_ul.x * DT_INTER;
            userp->place_box_ul.y = ep->box_ul.y * DT_INTER;
            userp->place_box_lr.x = ep->box_lr.x * DT_INTER;
            userp->place_box_lr.y = ep->box_lr.y * DT_INTER;
            /* Can't move now because another user is dragging. */
            userp->state = STATE_1;
        }
        else if ((F == 1) && (userp->count2 == 0)) {
            user_dragging = ep->user_id;
            mal_set_button_down(malp, userp, ep->time);
            userp->state = STATE_5;
        }
        else if (F == 1) {
            userp->count2--;
            ASSERT(userp->count2 >= 0);
            userp->state = STATE_3;
        }
        else if (F == 2) {
            userp->state = STATE_7;
        }
        else {
            ASSERT(0);
        }
        break;

    case STATE_4:
        if ((F == 0) && (userp->count2 == 0)) {
            mal_set_button_click(malp, userp, ep->time);
            userp->state = STATE_0;
        }
        else if (F == 0) {
            userp->count2--;
            ASSERT(userp->count2 >= 0);
            userp->state = STATE_4;
        }
        else if ((F == 1) && !close_enough(ep, userp)) {
            if (user_dragging == NO_USER) {
                mal_set_button_click(malp, userp, ep->time);
            }
            userp->state = STATE_0;
        }
        else if (F == 1) {
            userp->place = ep->max_point;
            userp->place_box_ul.x = ep->box_ul.x * DT_INTER;
            userp->place_box_ul.y = ep->box_ul.y * DT_INTER;
            userp->place_box_lr.x = ep->box_lr.x * DT_INTER;
            userp->place_box_lr.y = ep->box_lr.y * DT_INTER;
            if (user_dragging == NO_USER) {
                mal_set_move_abs(malp, userp, ep->time);
            }
            userp->state = STATE_6;
        }
        else if (F == 2) {
            userp->state = STATE_7;
        }
        else {
            ASSERT(0);
        }
        break;

    case STATE_5:
        /* We're dragging, so it's OK to move. */
        if (F == 0) {
            mal_set_button_up(malp, userp, ep->time);
            userp->state = STATE_0;
        }
        else if (F == 1) {
            userp->place = ep->max_point;
            userp->place_box_ul.x = ep->box_ul.x * DT_INTER;
            userp->place_box_ul.y = ep->box_ul.y * DT_INTER;
            userp->place_box_lr.x = ep->box_lr.x * DT_INTER;
            userp->place_box_lr.y = ep->box_lr.y * DT_INTER;
            mal_set_move_abs(malp, userp, ep->time);
            userp->state = STATE_5;
        }
        else if (F == 2) {
            mal_set_button_up(malp, userp, ep->time);
            userp->state = STATE_7;
        }
        else {
            ASSERT(0);
        }
        break;

    case STATE_6:
        if ((F == 0) && (user_dragging != NO_USER)) {
            userp->state = STATE_0;
            /* This transition is important because we're
               abandoning the attempt to double-click.  Just
               checking locally in the transition would make it
               possible to execute only one click of the
               double-click. */
        }
        else if (F == 0) {
            /*
             * NOTE: this is a HACK until we make the MALP bigger.
             */
            user_dragging = ep->user_id;
            /* We're "dragging," so it's OK to click. */
            mal_set_button_click(malp, userp, ep->time);
            userp->state = STATE_8;
        }
        else if (F == 1) {
            userp->place = ep->max_point;
            userp->place_box_ul.x = ep->box_ul.x * DT_INTER;
            userp->place_box_ul.y = ep->box_ul.y * DT_INTER;
            userp->place_box_lr.x = ep->box_lr.x * DT_INTER;
            userp->place_box_lr.y = ep->box_lr.y * DT_INTER;
            if (user_dragging == NO_USER) {
                mal_set_move_abs(malp, userp, ep->time);
            }
            userp->state = STATE_6;
        }
        else if (F == 2) {
            userp->state = STATE_7;
        }
        else {
            ASSERT(0);
        }
        break;

    case STATE_7:
        if (user_dragging == (int)ep->user_id) {
            user_dragging = NO_USER;
        }
        if (F == 0) {
            userp->state = STATE_0;
        }
        else if (F == 1) {
            userp->state = STATE_7;
        }
        else if (F == 2) {
            userp->state = STATE_7;
        }
        else {
            ASSERT(0);
        }
        break;

    case STATE_8:
        /*
         * NOTE: this is a HACK until we make the MAL bigger.
         */
        /* OK to do mouse control here because we wouldn't be here
           if we're not the ones "dragging". */
        if (F == 0) {
            mal_set_button_click(malp, userp, ep->time);
            userp->state = STATE_0;
        }
        else if (F == 1) {
            mal_set_button_click(malp, userp, ep->time);
            /* Note that we're updating the user's mouse position
               AFTER doing the click. This is a hack. */
            userp->place = ep->max_point;
            userp->place_box_ul.x = ep->box_ul.x * DT_INTER;
            userp->place_box_ul.y = ep->box_ul.y * DT_INTER;
            userp->place_box_lr.x = ep->box_lr.x * DT_INTER;
            userp->place_box_lr.y = ep->box_lr.y * DT_INTER;
            userp->state = STATE_1;
        }
        else if (F == 2) {
            mal_set_button_click(malp, userp, ep->time);
            userp->state = STATE_0;
        }
        else {
            ASSERT(0);
        }
        break;

    default:
        ASSERT(0);    /* invalid state */
        break;
    } /* end of switch statement */
}


static
void
dt_me_mode_anti_noise(dt_mouse_action_list *malp, dt_event *ep)
{
    dt_me_user *userp;
    int F;
    double dist;
#if 0
    extern void mytrace2(int a, int b);
#endif

    userp = &user[ep->user_id];

    /* Calculate "number of fingers." */
    dist = bb_diagonal(ep);
    F = 0;
    if (ep->touch) {
        F = 1;
        if (dist >= 7.0) {   /* 2.5 centimeters */
            F = 2;
        }
    }

#if 0
    if (ep->user_id == 1) {
        mytrace2(userp->state, F);
    }
#endif

    /* state machine (not a true FSM) */

    switch (userp->state) {
    case STATE_0:
        if (user_dragging == (int)ep->user_id) {
            user_dragging = NO_USER;
        }
        if (F == 0) {
            userp->state = STATE_0;
        }
        else if ((F == 1) || (F == 2)) {
            userp->button_armed = userp->is_left;
            userp->place = ep->max_point;
            userp->place_box_ul.x = ep->box_ul.x * DT_INTER;
            userp->place_box_ul.y = ep->box_ul.y * DT_INTER;
            userp->place_box_lr.x = ep->box_lr.x * DT_INTER;
            userp->place_box_lr.y = ep->box_lr.y * DT_INTER;
            if (user_dragging == NO_USER) {
                mal_set_move_abs(malp, userp, ep->time);
            }
            userp->state = STATE_1;
        }
        else {
            ASSERT(0);
        }
        break;

    case STATE_1:
        if (F == 0) {
            userp->state = STATE_0;
        }
        else if (F == 1) {
            userp->place = ep->max_point;
            userp->place_box_ul.x = ep->box_ul.x * DT_INTER;
            userp->place_box_ul.y = ep->box_ul.y * DT_INTER;
            userp->place_box_lr.x = ep->box_lr.x * DT_INTER;
            userp->place_box_lr.y = ep->box_lr.y * DT_INTER;
            if (user_dragging == NO_USER) {
                mal_set_move_abs(malp, userp, ep->time);
            }
            userp->state = STATE_2;
        }
        else if (F == 2) {
            userp->place = ep->max_point;
            userp->place_box_ul.x = ep->box_ul.x * DT_INTER;
            userp->place_box_ul.y = ep->box_ul.y * DT_INTER;
            userp->place_box_lr.x = ep->box_lr.x * DT_INTER;
            userp->place_box_lr.y = ep->box_lr.y * DT_INTER;
            if (user_dragging == NO_USER) {
                mal_set_move_abs(malp, userp, ep->time);
            }
            userp->state = STATE_3;
        }
        else {
            ASSERT(0);
        }
        break;

    case STATE_2:
        if (F == 0) {
            userp->state = STATE_0;
        }
        else if ((F == 1) && (user_dragging != NO_USER)) {
            userp->place = ep->max_point;
            userp->place_box_ul.x = ep->box_ul.x * DT_INTER;
            userp->place_box_ul.y = ep->box_ul.y * DT_INTER;
            userp->place_box_lr.x = ep->box_lr.x * DT_INTER;
            userp->place_box_lr.y = ep->box_lr.y * DT_INTER;
            /* Can't push button or move now because another user is
               dragging. */
            userp->state = STATE_2;
        }
        else if (F == 1) {
            user_dragging = ep->user_id;
            userp->place = ep->max_point;
            userp->place_box_ul.x = ep->box_ul.x * DT_INTER;
            userp->place_box_ul.y = ep->box_ul.y * DT_INTER;
            userp->place_box_lr.x = ep->box_lr.x * DT_INTER;
            userp->place_box_lr.y = ep->box_lr.y * DT_INTER;
            /* Disable for safe mode. */
            mal_set_button_down(malp, userp, ep->time);
            /*if (is_beep) Beep(500, BEEP_DURATION);*/
            userp->state = STATE_6;
        }
        else if (F == 2) {
            userp->place = ep->max_point;
            userp->place_box_ul.x = ep->box_ul.x * DT_INTER;
            userp->place_box_ul.y = ep->box_ul.y * DT_INTER;
            userp->place_box_lr.x = ep->box_lr.x * DT_INTER;
            userp->place_box_lr.y = ep->box_lr.y * DT_INTER;
            if (user_dragging == NO_USER) {
                mal_set_move_abs(malp, userp, ep->time);
            }
            userp->state = STATE_3;
        }
        else {
            ASSERT(0);
        }
        break;

    case STATE_3:
        if (F == 0) {
            userp->state = STATE_0;
        }
        else if ((F == 1) && (user_dragging != NO_USER)) {
            userp->place = ep->max_point;
            userp->place_box_ul.x = ep->box_ul.x * DT_INTER;
            userp->place_box_ul.y = ep->box_ul.y * DT_INTER;
            userp->place_box_lr.x = ep->box_lr.x * DT_INTER;
            userp->place_box_lr.y = ep->box_lr.y * DT_INTER;
            /* Can't push button or move now because another user is
               dragging. */
            userp->state = STATE_2;
        }
        else if (F == 1) {
            user_dragging = ep->user_id;
            userp->place = ep->max_point;
            userp->place_box_ul.x = ep->box_ul.x * DT_INTER;
            userp->place_box_ul.y = ep->box_ul.y * DT_INTER;
            userp->place_box_lr.x = ep->box_lr.x * DT_INTER;
            userp->place_box_lr.y = ep->box_lr.y * DT_INTER;
            /* Disable for safe mode. */
            mal_set_button_down(malp, userp, ep->time);
            /*if (is_beep) Beep(500, BEEP_DURATION);*/
            userp->state = STATE_6;
        }
        else if (F == 2) {
            userp->place = ep->max_point;
            userp->place_box_ul.x = ep->box_ul.x * DT_INTER;
            userp->place_box_ul.y = ep->box_ul.y * DT_INTER;
            userp->place_box_lr.x = ep->box_lr.x * DT_INTER;
            userp->place_box_lr.y = ep->box_lr.y * DT_INTER;
            if (user_dragging == NO_USER) {
                mal_set_move_abs(malp, userp, ep->time);
            }
            userp->state = STATE_4;
        }
        else {
            ASSERT(0);
        }
        break;

    case STATE_4:
        if (F == 0) {
            userp->state = STATE_0;
        }
        else if ((F == 1) && (user_dragging != NO_USER)) {
            userp->place = ep->max_point;
            userp->place_box_ul.x = ep->box_ul.x * DT_INTER;
            userp->place_box_ul.y = ep->box_ul.y * DT_INTER;
            userp->place_box_lr.x = ep->box_lr.x * DT_INTER;
            userp->place_box_lr.y = ep->box_lr.y * DT_INTER;
            /* Can't push button or move now because another user is
               dragging. */
            userp->state = STATE_2;
        }
        else if (F == 1) {
            user_dragging = ep->user_id;
            userp->place = ep->max_point;
            userp->place_box_ul.x = ep->box_ul.x * DT_INTER;
            userp->place_box_ul.y = ep->box_ul.y * DT_INTER;
            userp->place_box_lr.x = ep->box_lr.x * DT_INTER;
            userp->place_box_lr.y = ep->box_lr.y * DT_INTER;
            /* Disable for safe mode. */
            mal_set_button_down(malp, userp, ep->time);
            /*if (is_beep) Beep(500, BEEP_DURATION);*/
            userp->state = STATE_6;
        }
        else if ((F == 2) && (user_dragging != NO_USER)) {
            userp->place = ep->max_point;
            userp->place_box_ul.x = ep->box_ul.x * DT_INTER;
            userp->place_box_ul.y = ep->box_ul.y * DT_INTER;
            userp->place_box_lr.x = ep->box_lr.x * DT_INTER;
            userp->place_box_lr.y = ep->box_lr.y * DT_INTER;
            /* Can't push button or move now because another user is
               dragging. */
            userp->state = STATE_4;
        }
        else if (F == 2) {
            user_dragging = ep->user_id;
            userp->place = ep->max_point;
            userp->place_box_ul.x = ep->box_ul.x * DT_INTER;
            userp->place_box_ul.y = ep->box_ul.y * DT_INTER;
            userp->place_box_lr.x = ep->box_lr.x * DT_INTER;
            userp->place_box_lr.y = ep->box_lr.y * DT_INTER;
            userp->button_armed = !userp->is_left;    /* "right" button */
            /* Disable for safe mode. */
            mal_set_button_down(malp, userp, ep->time);
            /*if (is_beep) Beep(1000, BEEP_DURATION);*/
            userp->state = STATE_6;
        }
        else {
            ASSERT(0);
        }
        break;

    case STATE_5:
        if (F == 0) {
            userp->state = STATE_0;
        }
        else if (F == 1) {
            userp->place = ep->max_point;
            userp->place_box_ul.x = ep->box_ul.x * DT_INTER;
            userp->place_box_ul.y = ep->box_ul.y * DT_INTER;
            userp->place_box_lr.x = ep->box_lr.x * DT_INTER;
            userp->place_box_lr.y = ep->box_lr.y * DT_INTER;
            if (user_dragging == NO_USER) {
                mal_set_move_abs(malp, userp, ep->time);
            }
            userp->state = STATE_2;
        }
        else if (F == 2) {
            userp->place = ep->max_point;
            userp->place_box_ul.x = ep->box_ul.x * DT_INTER;
            userp->place_box_ul.y = ep->box_ul.y * DT_INTER;
            userp->place_box_lr.x = ep->box_lr.x * DT_INTER;
            userp->place_box_lr.y = ep->box_lr.y * DT_INTER;
            if (user_dragging == NO_USER) {
                mal_set_move_abs(malp, userp, ep->time);
            }
            userp->state = STATE_4;
        }
        else {
            ASSERT(0);
        }
        break;

    case STATE_6:
        /* We're dragging, so it's OK to move. */
        if (F == 0) {
            userp->state = STATE_7;
        }
        else if (F == 1) {
            userp->place = ep->max_point;
            userp->place_box_ul.x = ep->box_ul.x * DT_INTER;
            userp->place_box_ul.y = ep->box_ul.y * DT_INTER;
            userp->place_box_lr.x = ep->box_lr.x * DT_INTER;
            userp->place_box_lr.y = ep->box_lr.y * DT_INTER;
            mal_set_move_abs(malp, userp, ep->time);
            userp->state = STATE_6;
        }
        else if (F == 2) {
            userp->place = ep->max_point;
            userp->place_box_ul.x = ep->box_ul.x * DT_INTER;
            userp->place_box_ul.y = ep->box_ul.y * DT_INTER;
            userp->place_box_lr.x = ep->box_lr.x * DT_INTER;
            userp->place_box_lr.y = ep->box_lr.y * DT_INTER;
            mal_set_move_abs(malp, userp, ep->time);
            userp->state = STATE_6;
        }
        else {
            ASSERT(0);
        }
        break;

    case STATE_7:
        /* We're dragging, so it's OK to move. */
        if (F == 0) {
            userp->state = STATE_8;
        }
        else if (F == 1) {
            userp->place = ep->max_point;
            userp->place_box_ul.x = ep->box_ul.x * DT_INTER;
            userp->place_box_ul.y = ep->box_ul.y * DT_INTER;
            userp->place_box_lr.x = ep->box_lr.x * DT_INTER;
            userp->place_box_lr.y = ep->box_lr.y * DT_INTER;
            mal_set_move_abs(malp, userp, ep->time);
            userp->state = STATE_6;
        }
        else if (F == 2) {
            userp->place = ep->max_point;
            userp->place_box_ul.x = ep->box_ul.x * DT_INTER;
            userp->place_box_ul.y = ep->box_ul.y * DT_INTER;
            userp->place_box_lr.x = ep->box_lr.x * DT_INTER;
            userp->place_box_lr.y = ep->box_lr.y * DT_INTER;
            mal_set_move_abs(malp, userp, ep->time);
            userp->state = STATE_6;
        }
        else {
            ASSERT(0);
        }
        break;

    case STATE_8:
        /* We're dragging, so it's OK to move. */
        if (F == 0) {
            /*if (is_beep) Beep(2000, BEEP_DURATION);*/
            /* Disable for safe mode. */
            mal_set_button_up(malp, userp, ep->time);
            userp->state = STATE_0;
        }
        else if (F == 1) {
            userp->place = ep->max_point;
            userp->place_box_ul.x = ep->box_ul.x * DT_INTER;
            userp->place_box_ul.y = ep->box_ul.y * DT_INTER;
            userp->place_box_lr.x = ep->box_lr.x * DT_INTER;
            userp->place_box_lr.y = ep->box_lr.y * DT_INTER;
            mal_set_move_abs(malp, userp, ep->time);
            userp->state = STATE_6;
        }
        else if (F == 2) {
            userp->place = ep->max_point;
            userp->place_box_ul.x = ep->box_ul.x * DT_INTER;
            userp->place_box_ul.y = ep->box_ul.y * DT_INTER;
            userp->place_box_lr.x = ep->box_lr.x * DT_INTER;
            userp->place_box_lr.y = ep->box_lr.y * DT_INTER;
            mal_set_move_abs(malp, userp, ep->time);
            userp->state = STATE_6;
        }
        else {
            ASSERT(0);
        }
        break;
    }
}

static
void
mal_set_move_abs(dt_mouse_action_list *malp, dt_me_user *userp,
                       dt_timestamp time)
{
    malp->n = 1;
    set_mouse_action(&malp->action[0],
                     DT_MOUSE_ACTION_MOVE_ABS,
                     userp->user_id,
                     userp->place.x,
                     userp->place.y,
                     0,
                     time,
                     userp->place_box_ul,
                     userp->place_box_lr);
}


static
void
mal_set_button_down(dt_mouse_action_list *malp, dt_me_user *userp,
                          dt_timestamp time)
{
    malp->n = 2;
    set_mouse_action(&malp->action[0],
                     DT_MOUSE_ACTION_MOVE_ABS,
                     userp->user_id,
                     userp->place.x,
                     userp->place.y,
                     0,
                     time++,
                     userp->place_box_ul,
                     userp->place_box_lr);
    set_mouse_action(&malp->action[1],
                     button_action(userp->button_armed, BUTTON_DOWN),
                     userp->user_id,
                     userp->place.x,    /* courtesy */
                     userp->place.y,    /* courtesy */
                     0,
                     time,
                     userp->place_box_ul,
                     userp->place_box_lr);
}


static
void
mal_set_button_up(dt_mouse_action_list *malp, dt_me_user *userp,
                        dt_timestamp time)
{
    malp->n = 2;
    set_mouse_action(&malp->action[0],
                     DT_MOUSE_ACTION_MOVE_ABS,
                     userp->user_id,
                     userp->place.x,
                     userp->place.y,
                     0,
                     time++,
                     userp->place_box_ul,
                     userp->place_box_lr);
    set_mouse_action(&malp->action[1],
                     button_action(userp->button_armed, BUTTON_UP),
                     userp->user_id,
                     userp->place.x,    /* courtesy */
                     userp->place.y,    /* courtesy */
                     0,
                     time,
                     userp->place_box_ul,
                     userp->place_box_lr);
}


static
void
mal_set_button_click(dt_mouse_action_list *malp, dt_me_user *userp,
                        dt_timestamp time)
{
    malp->n = 3;
    set_mouse_action(&malp->action[0],
                     DT_MOUSE_ACTION_MOVE_ABS,
                     userp->user_id,
                     userp->place.x,
                     userp->place.y,
                     0,
                     time++,
                     userp->place_box_ul,
                     userp->place_box_lr);
    set_mouse_action(&malp->action[1],
                     button_action(userp->button_armed, BUTTON_DOWN),
                     userp->user_id,
                     userp->place.x,    /* courtesy */
                     userp->place.y,    /* courtesy */
                     0,
                     time++,
                     userp->place_box_ul,
                     userp->place_box_lr);
    set_mouse_action(&malp->action[2],
                     button_action(userp->button_armed, BUTTON_UP),
                     userp->user_id,
                     userp->place.x,    /* courtesy */
                     userp->place.y,    /* courtesy */
                     0,
                     time,
                     userp->place_box_ul,
                     userp->place_box_lr);
}
