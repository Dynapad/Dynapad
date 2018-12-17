
#ifndef PLIST_INCL
#define PLIST_INCL

#define PAD_LIST_CLASS Pad_PList
#define LIST_TYPE Pad_Point
#define LIST_PTR *
#define LIST_REF &

#include "point.h"

#include "glist.h"
#undef PAD_LIST_CLASS
#undef LIST_TYPE
#undef LIST_PTR
#undef LIST_REF

#endif
