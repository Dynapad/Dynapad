/*
  Pad_PList

  An array-style list of Pad_Point's.  This list is an actual array of points,
  *not* an array of pointers to points.  Thus, pushing a point onto this list
  really makes a copy of that point.
*/

#include "plist.h"

#define PAD_LIST_CLASS Pad_PList
#define LIST_TYPE Pad_Point
#define LIST_PTR *
#define LIST_REF &
#include "glist.i"
#undef PAD_LIST_CLASS
#undef LIST_TYPE
#undef LIST_PTR
#undef LIST_REF

