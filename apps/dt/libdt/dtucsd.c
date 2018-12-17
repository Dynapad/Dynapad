/* $Id: dtucsd.c,v 1.1.1.1 2005/10/09 04:28:49 stanonik Exp $ */

#define DT_LIB_CORE 1

#include <limits.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "dt.h"
#include "dtucsd.h"

#define MAX(a,b) (a>=b?a:b)
#define MIN(a,b) (a<=b?a:b)
#define CLIP(x,min,max) MAX(min,MIN(x,max))

#define MAX_X_TOUCHES (DT_X/2 + 1)  /* theoretical bound */
#define MAX_Y_TOUCHES (DT_Y/2 + 1)
#define MAX_2DTOUCHES (MAX(MAX_X_TOUCHES,MAX_Y_TOUCHES))  /* something */
#define MAX_TOUCH_MOVE 8.0

/* arrays for storing and returning (X,Y-dissociated) touch structures */
static dt_1dtouch* x_touches[DT_MAX_USERS];
static dt_1dtouch* y_touches[DT_MAX_USERS];

static dt_2dtouch* touches[DT_MAX_USERS];

/* a signal above this (dynamic) threshold is part of a center */
static unsigned int threshold;

static dt_1dtouch* d4_get_1dtouches(unsigned char* uchar_signals, int len,
    dt_1dtouch* touches);
static void d4_find_touch(unsigned int* vals, int len,
    unsigned int* pos_deltas, unsigned int* neg_deltas,
    int left_edge, int right_edge, dt_1dtouch* touch);
static float d4_find_center(unsigned int* signals, int len,
    int left_end, int right_end);
int d4_find_closest_touch(dt_1dtouch* touches, dt_1dtouch touch);

void
d4_init(void)
{
  int i;
  dt_user_id user_id;

  for (user_id = 0; user_id < DT_MAX_USERS; ++user_id)
  {
    x_touches[user_id] = malloc(MAX_X_TOUCHES * sizeof(**x_touches));
    if (x_touches[user_id] == NULL)
    {
      perror("malloc");
      exit(EXIT_FAILURE);
    }

    y_touches[user_id] = malloc(MAX_Y_TOUCHES * sizeof(**y_touches));
    if (y_touches[user_id] == NULL)
    {
      perror("malloc");
      exit(EXIT_FAILURE);
    }

    touches[user_id] = malloc(MAX_2DTOUCHES * sizeof(**touches));
    if (touches[user_id] == NULL)
    {
      perror("malloc");
      exit(EXIT_FAILURE);
    }

    for (i = 0; i < MAX_X_TOUCHES; ++i)
      x_touches[user_id][i].valid = 0;
    for (i = 0; i < MAX_Y_TOUCHES; ++i)
      y_touches[user_id][i].valid = 0;
    for (i = 0; i < MAX_2DTOUCHES; ++i)
      touches[user_id][i].status = EOL;
  }
}

void
d4_uninit(void)
{
  dt_user_id user_id;

  for (user_id = 0; user_id < DT_MAX_USERS; ++user_id)
  {
    free(x_touches[user_id]);
    free(y_touches[user_id]);
    free(touches[user_id]);
  }
}

dt_1dtouch*
d4_get_x_touches(dt_user_id user_id, dt_frame* fp)
{
  return d4_get_1dtouches(fp->x, DT_X, x_touches[user_id]);
}

dt_1dtouch*
d4_get_y_touches(dt_user_id user_id, dt_frame* fp)
{
  return d4_get_1dtouches(fp->y, DT_Y, y_touches[user_id]);
}

dt_1dtouch*
d4_get_1dtouches(unsigned char* uchar_signals, int len, dt_1dtouch* touches)
{
  unsigned int signals[len], pos_deltas[len+1], neg_deltas[len+1];
  unsigned int max, min, v;
  int i, in_touch, left_edge;
  dt_1dtouch* touch;

  /* Copy into signals array and compute min and max signal values. */
  max = 0; min = UINT_MAX;
  for (i = 0; i < len; ++i)
  {
    signals[i] = uchar_signals[i];
    min = MIN(min, signals[i]);
    max = MAX(max, signals[i]);
  }

  /* Set the touch threshold. */
  threshold = (max-min > 40) ? min + (max-min)/5 : UINT_MAX;

  /* Compute deltas (change in signal between antennas). */
  pos_deltas[0] = signals[0] - min; neg_deltas[0] = 0;
  for (i = 1; i < len; ++i)
  {
    int delta = signals[i] - signals[i-1];
    if (delta > 0)
    { pos_deltas[i] = delta; neg_deltas[i] = 0; }
    else
    { neg_deltas[i] = -delta; pos_deltas[i] = 0; }
  }
  pos_deltas[len] = min - signals[len]; pos_deltas[len] = 0;

  /* Locate and scrutinize intervals of signals above the threshold. */
  touch = touches; in_touch = 0; left_edge = -1;
  for (i = 0; i <= len; ++i)
  {
    v = (i < len) ? signals[i] : 0;

    if (!in_touch && v >= threshold)
    {
      in_touch = 1;
      left_edge = i-1;
    }

    if (in_touch && v < threshold)
    {
      in_touch = 0;
      d4_find_touch(signals, len, pos_deltas, neg_deltas, left_edge, i,
          touch++);
    }
  }

  touch->valid = 0;  /* mark end of valid touches */

  return touches;
}

void
d4_find_touch(unsigned int* vals, int len,
    unsigned int* pos_deltas, unsigned int* neg_deltas,
    int left_edge, int right_edge, dt_1dtouch* touch)
{
  touch->ctr = d4_find_center(vals, len, left_edge, right_edge) + 1.0/2.0;

  {
    int dwl, dwr;  /* delta window left, right */
    for (dwl = left_edge+1; dwl >= 0 && pos_deltas[dwl] > 0; --dwl);
    for (dwr = left_edge+1; dwr <= len && pos_deltas[dwr] > 0; ++dwr);
    touch->beg = d4_find_center(pos_deltas, len+1, dwl, dwr);

    for (dwl = right_edge; dwl >= 0 && neg_deltas[dwl] > 0; --dwl);
    for (dwr = right_edge; dwr <= len && neg_deltas[dwr] > 0; ++dwr);
    touch->end = d4_find_center(neg_deltas, len+1, dwl, dwr);
  }

  touch->valid = 1;
}

float
d4_find_center(unsigned int* signals, int len, int left_end, int right_end)
{
  /* Compute a weighted average position for a contiguous segment of signals
   * between left_end and right_end, inclusive. */
  unsigned int left_end_v, right_end_v, baseline;
  int i, w, total_weight;
  float weighted_sum;

  /* Use the lower of the two endpoint values as a baseline. */
  left_end_v = (left_end >= 0) ? signals[left_end] : DT_MAX_SIGNAL;
  right_end_v = (right_end < len) ? signals[right_end] : DT_MAX_SIGNAL;
  baseline = MIN(left_end_v, right_end_v);

  total_weight = 0; weighted_sum = 0.0;
  for (i = MAX(left_end, 0); i <= MIN(right_end, len-1); ++i)
  {
    w = signals[i] - baseline;
    weighted_sum += (float)(w * i);
    total_weight += w;
  }

  /* When all signals in the range are equal (e.g. there is just one), they
   * are all equal to the baseline, so total_weight = 0. */
  if (total_weight)
    return (weighted_sum / (float)total_weight);
  else
    return (((float)MAX(left_end, 0) + (float)MIN(right_end, len-1)) / 2.0);
}

dt_2dtouch* 
d4_get_2dtouches(dt_user_id user_id, dt_frame* fp)
{
  static touch_id_t touch_id = 0;
  int i, j;
  dt_1dtouch* my_x_touches = x_touches[user_id];
  dt_1dtouch* my_y_touches = y_touches[user_id];
  dt_2dtouch* my_2dtouches = touches[user_id];
  int x_touch_of[MAX_2DTOUCHES];
  int y_touch_of[MAX_2DTOUCHES];
  unsigned char x_touch_explained[MAX_X_TOUCHES];
  unsigned char y_touch_explained[MAX_Y_TOUCHES];

  d4_get_x_touches(user_id, fp);  /* this might be duplicated effort */
  d4_get_y_touches(user_id, fp);

  /* If there isn't at least one touch in each dimension, bail out. */
  if (!my_x_touches[0].valid || !my_y_touches[0].valid)
  {
    for (i = 0; i < MAX_2DTOUCHES; ++i)
      my_2dtouches[i].status = EOL;
    return my_2dtouches;
  }

  for (i = 0; my_x_touches[i].valid; ++i)
    x_touch_explained[i] = 0;
  for (i = 0; my_y_touches[i].valid; ++i)
    y_touch_explained[i] = 0;

  for (i = 0; my_2dtouches[i].status != EOL; ++i)
    if (my_2dtouches[i].status == ALIVE)
    {
      /* Find closest x and y touches. */
      x_touch_of[i] = d4_find_closest_touch(my_x_touches, my_2dtouches[i].x);
      y_touch_of[i] = d4_find_closest_touch(my_y_touches, my_2dtouches[i].y);
      x_touch_explained[x_touch_of[i]] = 1;
      y_touch_explained[y_touch_of[i]] = 1;
    }

  /* Don't let touches share intersections (kill the more distant one). */
  for (i = 0; my_2dtouches[i].status != EOL; ++i)
    for (j = i + 1; my_2dtouches[j].status != EOL; ++j)
      if (my_2dtouches[i].status == ALIVE && my_2dtouches[j].status == ALIVE
          && x_touch_of[i] == x_touch_of[j] && y_touch_of[i] == y_touch_of[j])
      {
        if (pow(my_2dtouches[i].x.ctr - my_x_touches[x_touch_of[i]].ctr, 2) +
            pow(my_2dtouches[i].y.ctr - my_y_touches[y_touch_of[i]].ctr, 2) >
            pow(my_2dtouches[j].x.ctr - my_x_touches[x_touch_of[j]].ctr, 2) +
            pow(my_2dtouches[j].y.ctr - my_y_touches[y_touch_of[j]].ctr, 2))
          my_2dtouches[i].status = DEAD;
        else
          my_2dtouches[j].status = DEAD;
      }

  /* Don't let touches teleport. */
  for (i = 0; my_2dtouches[i].status != EOL; ++i)
      if (my_2dtouches[i].status == ALIVE)
      {
        if (pow(my_2dtouches[i].x.ctr - my_x_touches[x_touch_of[i]].ctr, 2) +
            pow(my_2dtouches[i].y.ctr - my_y_touches[y_touch_of[i]].ctr, 2)
            > pow(MAX_TOUCH_MOVE, 2))
          my_2dtouches[i].status = DEAD;
      }

  for (i = 0; my_2dtouches[i].status != EOL; ++i)
    if (my_2dtouches[i].status == ALIVE)
    {
      my_2dtouches[i].x = my_x_touches[x_touch_of[i]];
      my_2dtouches[i].y = my_y_touches[y_touch_of[i]];
    }

  /* If there is at least one new coordinate pair, make a touchpoint. */
  for (i = 0; my_x_touches[i].valid && x_touch_explained[i]; ++i);
  if (my_x_touches[i].valid)
  {
    for (j = 0; my_y_touches[j].valid && y_touch_explained[j]; ++j);
    if (my_y_touches[j].valid)
    {
      int k;
      for (k = 0; my_2dtouches[k].status == ALIVE; ++k);
      my_2dtouches[k].status = ALIVE;
      my_2dtouches[k].x = my_x_touches[i];
      my_2dtouches[k].y = my_y_touches[j];
      my_2dtouches[k].user_id = user_id;
      my_2dtouches[k].touch_id = touch_id++;
    }
  }

  return my_2dtouches;
}

int
d4_find_closest_touch(dt_1dtouch* touches, dt_1dtouch touch)
{
  int i;

  for (i = 0; touches[i].valid && touches[i].ctr < touch.ctr; ++i);

  if (!touches[i].valid)  /* stepped past last center */
    return (i-1);
  else if (i > 0 &&
      fabs(touch.ctr - touches[i-1].ctr) <= fabs(touch.ctr - touches[i].ctr))
    return (i-1);
  else
    return i;
}
