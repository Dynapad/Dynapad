/* $Id: dtmouse.h,v 1.1.1.1 2005/10/09 04:28:50 stanonik Exp $ */

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
 * dtmouse.h
 *
 * DiamondTouch mouse emulator
 */
#ifndef DTMOUSE_H
#define DTMOUSE_H

/* mouse emulation modes */
#define DT_MOUSE_MODE_DEFAULT     0
#define DT_MOUSE_MODE_MELS_CAFE   1
#define DT_MOUSE_MODE_PAINT       2
#define DT_MOUSE_MODE_WEB_DEMO    3
#define DT_MOUSE_MODE_MOVE_ONLY   4
#define DT_MOUSE_MODE_MAP_DEMO    5
#define DT_MOUSE_MODE_ANTI_NOISE  6

#define DT_NUM_MOUSE_MODES      7

/* Maximum number of mouse actions in a dt_mouse_action_list. */
#define DT_MOUSE_MAX_CMDS 5

/* mouse actions */
#define DT_MOUSE_ACTION_NONE        0
#define DT_MOUSE_ACTION_MOVE_ABS    1
#define DT_MOUSE_ACTION_MOVE_REL    2
#define DT_MOUSE_ACTION_LEFT_DOWN   3
#define DT_MOUSE_ACTION_LEFT_UP     4
#define DT_MOUSE_ACTION_MIDDLE_DOWN 5
#define DT_MOUSE_ACTION_MIDDLE_UP   6
#define DT_MOUSE_ACTION_RIGHT_DOWN  7
#define DT_MOUSE_ACTION_RIGHT_UP    8
#define DT_MOUSE_ACTION_WHEEL       9

/*
 * mouse action
 *
 * Coordinates are in *table* coordinates.  The system-dependent code
 * to move the mouse must translate them to display coordinates.
 */
typedef struct {
    dt_timestamp     time;
    union {
        dt_point     move;
        unsigned int wheel;
    } data;
    dt_point         box_ul;    /* upper-left corner of bounding box */
    dt_point         box_lr;    /* lower-right corner of bounding box */
    unsigned short   action;
    dt_user_id       user_id;
} dt_mouse_action;

/* mouse action list */
typedef struct {
    int              n;
    dt_mouse_action  action[DT_MOUSE_MAX_CMDS];
} dt_mouse_action_list;


/*
 * Exported Function Prototypes
 */
void
DT_LIB_API
dt_me_initialize(int user_id);

void
DT_LIB_API
dt_me_uninitialize(int user_id);

int
DT_LIB_API
dt_me_get_mode();

void
DT_LIB_API
dt_me_set_mode(int p);

int
DT_LIB_API
dt_me_get_beep();

void
DT_LIB_API
dt_me_set_beep(int is_on);

void
DT_LIB_API
dt_me_update(dt_mouse_action_list *map, dt_event *ep);


/*
 * Custom VB support for DTMap demo; likely to go away or change.
 * (These functions are defined in dtlib.c.  The are declared here
 * because they are in the mouse emulation layer.
 */

long
DT_LIB_API
dt_vb_device_open_user(long uid);

void
DT_LIB_API
dt_vb_device_close_user(long uid, long device);

void
DT_LIB_API
dt_vb_comb_fi_me_init(long uid, long me_mode);

void
DT_LIB_API
dt_vb_comb_fi_me_uninit(long uid);

long
DT_LIB_API
dt_vb_comb_io_fi_me_read(dt_device device, long uid, long *maa);


#endif /* DTMOUSE_H */
