/*
"(c) Copyright 1993-1997 Pad++ Consortium {University of New Mexico (UNM),
and New York University (NYU)}, All Rights Reserved."  
Licensee can not remove or obscure any of the
copyright or trademark notices in this software.

IN NO EVENT SHALL THE AUTHORS OR DISTRIBUTORS BE LIABLE TO ANY PARTY
FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
ARISING OUT OF THE USE OF THIS SOFTWARE, ITS DOCUMENTATION, OR ANY
DERIVATIVES THEREOF, EVEN IF THE AUTHORS HAVE BEEN ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.

THE AUTHORS AND DISTRIBUTORS SPECIFICALLY DISCLAIM ANY WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE, AND NON-INFRINGEMENT.  THIS SOFTWARE
IS PROVIDED ON AN "AS IS" BASIS, AND THE AUTHORS AND DISTRIBUTORS HAVE
NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
MODIFICATIONS.

See the file "License" for general information on usage and
redistribution, and the file "LicenseTerms" for the specific license
agreement on usage and redistribution of this file, and the Pad++
software in general.
*/

#include "defs.h"
#include <fstream>
using namespace std;
                                // Some Linux's want this to be at the top...
#include <sys/types.h>
                                // But LITTLE_ENDIAN gets redefined badly, so
                                // undefine it, and let <math.h> that gets included
                                // below by pad.h define it.
#undef LITTLE_ENDIAN
#include "pad.h"

#include "pad-string.h"
#include "list.h"
#include "ilist.h"
#include "view.h"
#include "portal.h"
#include "point.h"
#include "text.h"
#include "image.h"
#include "imagedata.h"
#include "line.h"
#include "group.h"
#include "alias.h"
#include "win.h"
#include "restorer.h"
#include "renderer.h"
#include "group.h"
#include "bind.h"
#include "callback.h"
#include "tree.h"
#include "alias.h"
#include "global.h"
#include "button.h"
#include "container.h"
#include "tkwin.h"
#include "scrollbar.h"
#include "menu.h"
#include "misc.h"

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <signal.h>
#include <fcntl.h>
#include <string.h>
#include <sys/utsname.h>


#  include <unistd.h>         // for getcwd() prototype
#  include <netdb.h>


//////////////////////////////////////////////
//              Pad definitions
//////////////////////////////////////////////  

Pad::~Pad()
{
    if (idTable) {
        delete idTable;
        idTable = NULL;
    }
    if (tagTable) {
        delete tagTable;
        tagTable = NULL;
    }
    if (typeTable) {
        delete typeTable;
        typeTable = NULL;
    }
    if (optionTable) {
        delete optionTable;
        optionTable = NULL;
    }
    if (optionIdTable) {
        delete optionIdTable;
        optionIdTable = NULL;
    }
    pad = NULL;
    if (treeroot) {
        delete treeroot;
        treeroot = NULL;
    }
    view->pad = NULL;
}

Pad::Pad(Pad_View *newView)
{
    _type = PAD_PAD;
    padFlags = PAD_NO_MASK;
    objid = 1;
    pad = this;
    view = newView;
    view->pad = this;
    first = NULL;
    last = NULL;
    level = 0;
    drawingOrder = 0;
    mode = 0;
    idEventFirst = FALSE;
    treeroot = NULL;
    writeRelative = FALSE;
    defaultEvents = FALSE;
    renderTime = 0;

    idTable = new Pad_HashTable();

    tagTable = new Pad_HashTable();

    typeTable = new Pad_HashTable();

    optionTable = new Pad_HashTable(sizeof(Pad_OptionKey) / sizeof(int));
    optionIdTable = new Pad_HashTable(PAD_STRING_TABLE);    // Table of option names to ids (for builtin types only)

    Add_tag("Pad");

                                // Make the special layers "all"
    Add_to_layer("all");
    
    Set_id();
    Set_findable(FALSE);
    objectsRendered = 0;

                                // Don't initialize default event handlers for pads
    Init_events(FALSE);
}

//
// Create a new master treeroot for the pad surface
// (if there isn't already one).
//
void
Pad::Create_treeroot(void) {
    if (!treeroot) {
        treeroot = new Pad_TreeNode((Pad_TreeNode *)NULL, this);
        treeroot->Get_layout()->Get_link()->Set_transparency(0);
        treeroot->Get_layout()->Get_link()->Set_events(FALSE);
        treeroot->Get_obj()->Delete_tag("treenode");
        treeroot->Get_obj()->Add_tag("treeroot");
    }
}

//
// This function should be used in all the places that pad->view->win is
//   directly referenced so that the underlying implementation can change
//
Pad_Win *
Pad::Get_win(void)
{
  return view->win;
}

//
// Given a layer name, return a pointer to the layer,
// or none if no such layer exists.
//
Pad_Layer *
Pad::Get_layer_from_name(const char *name)
{
    Pad_Layer *layer;

    layer = (Pad_Layer *)layerNameTable.Get(Pad_GetUid(name));
    return(layer);
}

//
// Given a layer id, return a pointer to the layer,
// or none if no such layer exists.
//
Pad_Layer *
Pad::Get_layer_from_id(int layerId)
{
    Pad_Layer *layer;

    layer = (Pad_Layer *)layerIdTable.Get((void *)(uintptr_t)layerId);
    return(layer);
}

//
// Create a new layer for future use.
// If layer of this name already exists, 
// a pointer to that layer is returned.
// Notify views of its existance
//
Pad_Layer *
Pad::Create_layer(const char *name)
{
    int i;
    int len;
    int layerId;
    Pad_Bool *layerIds;
    Pad_Portal *portal;
    Pad_Layer *layer, *layerPtr;
    Pad_Iterator oi;
    Pad_HashTableIterator hi;

    // If layer already exists, return it
    layer = Get_layer_from_name(name);
    if (layer) {
        return(layer);
    }

    // Find unused layer with lowest id
    len = pad->layerNameTable.Length() + 1;
    layerIds = new Pad_Bool[len];
    DOTIMES(i, len) {
        layerIds[i] = FALSE;
    }
    DOTABLE(hi, pad->layerNameTable, Pad_Layer, layerPtr) {
        layerIds[layerPtr->id] = TRUE;
    }
    DOTIMES(i, len) {
        if (!layerIds[i]) {
            layerId = i;
            break;
        }
    }

    // Create new layer
    layer = new Pad_Layer(this);
    layer->name = Pad_GetUid(name);
    layer->id = layerId;
    layerNameTable.Set((void *) layer->name, layer);
    layerIdTable.Set((void *)(uintptr_t)layer->id, layer);
    delete [] layerIds;

    layers.Push_last(layer);


    // Update all views to know about this new layer.
    view->Update_layers();
    DOLIST(oi, view->win->lookonPortals, Pad_Portal, portal) {
        portal->Update_layers();
    }
    DOLIST(oi, view->win->viewPortals, Pad_Portal, portal) {
        if (portal->lookon == view->win) {
            portal->Update_layers();
        }
    }

    return(layer);
}

//
// Delete <layer> and all objects on that layer.
//
void
Pad::Delete_layer(Pad_Layer *layer)
{
    int i;
    Pad_List items;
    Pad_IList ids;
    Pad_Object *obj;
    Pad_Iterator oi;

                                // Delete items on layer
                                // Carefully, built list of item ids,
                                // and then delete items by id.  This
                                // is safe in case deleting one object
                                // results in the deletion of another.
    Find_with_layer(layer, items, FALSE);
    layer->first = NULL;
    layer->last = NULL;
    DOLIST(oi, items, Pad_Object, obj) {
        ids.Push_last(obj->id);
    }
    for (i=0; i<ids.Length(); i++) {
        pad->Delete_obj(ids.Nth(i));
    }

                                // Update layer order
    layers.Remove(layer);
}

//
// Raise the layer specified by <name> to be the top-most layer
//
Pad_Bool
Pad::Raise_layer(Pad_Layer *layer)
{
    Pad_Bool rc = TRUE;
    Pad_List items;
    Pad_Object *obj;
    Pad_Iterator oi;

    if (!layer) {
        return(TRUE);
    }
                                // Raise items on layer
    Find_with_layer(layer, items, FALSE);
                                // Set layer to empty so items will 
                                // raise all the way to the top
    layer->first = NULL;
    layer->last = NULL;
    DOLIST(oi, items, Pad_Object, obj) {
        obj->Raise();
    }

                                // Update layer order
    layers.Remove(layer);
    layers.Push_last(layer);

    return(rc);
}

//
// Lower the layer specified by <name> to be the bottom-most layer
//
Pad_Bool
Pad::Lower_layer(Pad_Layer *layer)
{
    Pad_Bool rc = TRUE;
    Pad_List items;
    Pad_Object *obj;
    Pad_Iterator oi;

    if (!layer) {
        return(TRUE);
    }
                                // Raise items on layer
    Find_with_layer(layer, items, FALSE);
                                // Set layer to empty so items will 
                                // lower all the way to the bottom
    layer->first = NULL;
    layer->last = NULL;
    items.Reverse();
    DOLIST(oi, items, Pad_Object, obj) {
        obj->Lower();
    }

                                // Update layer order
    layers.Remove(layer);
    layers.Push(layer);

    return(rc);
}

//
// Raise layer name1 one above layer name2.
// If there are no objecs in layer name2 then objects in layer name1
// are moved above the first non-empty layer below name2.
//
Pad_Bool
Pad::Raise_layer(Pad_Layer *layer1, Pad_Layer *layer2)
{
    Pad_Bool rc = TRUE;
    Pad_Layer *realLayer2;

    realLayer2 = layer2;
    if (!layer1 || !layer2 || (layer1 == layer2)) {
        return(TRUE);
    }

    // if layer2 doesn't have any objects yet raise above the first layer
    // below it which has something.
    if (!layer2->first) {
        int i = layers.Index(layer2)-1;
        while ((i >= 0) && (layer2=(Pad_Layer*)layers.Nth(i)) && 
               (layer2->first == NULL)) {
            i--;
        }

        if ((i < 0) || (layer1 == layer2)) {
            // couldn't find a non-empty layer which layer1 could go after
            return(TRUE);
        }
    }
   
    // remove layer1 objects from display list
    if (layer1->first) {
        if (layer1->first->prev) {
            layer1->first->prev->next = layer1->last->next;
        }
        if (layer1->last->next) {
            layer1->last->next->prev = layer1->first->prev;
        }

        // take care of special cases for pad's first and last pointers
        if (first == layer1->first) { 
            first = layer1->last->next;
            if (first) {
                first->prev = NULL;
            }
        }
        if (last == layer1->last) {
            last = layer1->first->prev;
            if (last) {
                last->next = NULL;
            }
        }

        layer1->first->prev = NULL;
        layer1->last->next = NULL;
    }

    // insert layer1 objects after layer2 objects
    if (layer1->first) {
        layer1->first->prev = layer2->last;
        if (layer2->last) {
            if (layer1->last) {
              layer1->last->next = layer2->last->next;
            } 
            layer2->last->next = layer1->first;
            if (layer1->last && layer1->last->next) {
                layer1->last->next->prev = layer1->last;
            }
        }

        // take care of special cases for pad's first and last pointer
        if (last == layer2->last) { 
            last = layer1->last;
        }
    }

    // fix layers list
    layers.Remove(layer1);
    layers.Insert_after(layer1, realLayer2);

    // fix objects drawing orders
    layer2->first->Propogate_drawing_order();

    Damage();
    return(rc);
}

//
// Lower layer name1 one below layer name2.
// If there are no objects in layer name2 then objects in layer name1
// are moved below the first non-empty layer above name2.
//
Pad_Bool
Pad::Lower_layer(Pad_Layer *layer1, Pad_Layer *layer2)
{
    Pad_Bool rc = TRUE;
    Pad_Layer *realLayer2;

    realLayer2 = layer2;
    if (!layer1 || !layer2 || (layer1 == layer2)) {
        return(TRUE);
    }

    // if layer2 doesn't have any objects then lower below the first layer
    // above it which has something.
    if (!layer2->first) {
        int i = layers.Index(layer2)+1;
        while ((i < layers.Length()) && (layer2=(Pad_Layer*)layers.Nth(i)) && 
               (layer2->first == NULL)) {
            i++;
        }

        if ((i >= layers.Length()) || (layer1 == layer2)) {
            // couldn't find a non-empty layer which layer1 could go below
          return(TRUE);
        }
    }
   
    // remove layer1 objects from display list
    if (layer1->first) {
        if (layer1->first->prev) {
            layer1->first->prev->next = layer1->last->next;
        }
        if (layer1->last->next) {
          layer1->last->next->prev = layer1->first->prev;
        }

        // take care of special cases for pad's first and last pointer
        if (first == layer1->first) { 
            first = layer1->last->next;
            if (first) {
                first->prev = NULL;
            }
        }
        if (last == layer1->last) {
            last = layer1->first->prev;
            if (last) {
                last->next = NULL;
            }
        }
        layer1->first->prev = NULL;
        layer1->last->next = NULL;
    }

    // insert layer1 objects before layer2 objects
    if (layer1->last) {
        layer1->last->next = layer2->first;
        if (layer2->first) {
            if (layer1->first) {
                layer1->first->prev = layer2->first->prev;
            }
            layer2->first->prev = layer1->last;
            if (layer1->first && layer1->first->prev) {
                layer1->first->prev->next = layer1->first;
            }
        }
                
        // take care of special cases for pad's first and last pointer
        if (first == layer2->first) { 
            first = layer1->first;
        }
    }

    // fix layers list
    layers.Remove(layer1);
    layers.Insert_before(layer1, realLayer2);

    // fix objects drawing order
    if (layer1->first) {
        layer1->first->Propogate_drawing_order();
    }

    Damage();
    return(rc);
}

//
// Return TRUE if the arbitrary line from p1-p2 intersects
// the horizontal line from p3-p4.
//
static Pad_Bool
Line_intersects_hor_line(Pad_Point &p1, Pad_Point &p2, Pad_Point &p3, Pad_Point &p4)
{
    float t;
    float x;
    Pad_Bool intersect = FALSE;

    if (p1.y != p2.y) {
        t = (p3.y - p1.y) / (p2.y - p1.y);
        if ((t >= 0) && (t <= 1)) {
            intersect = TRUE;
            x = p1.x + t*(p2.x - p1.x);
            if (((x < p3.x) && (x < p4.x)) ||
                ((x > p3.x) && (x > p4.x))) {
                intersect = FALSE;
            }
        }
    }

    return(intersect);
}

//
// Return TRUE if the arbitrary line from p1-p2 intersects
// the vertical line from p3-p4.
//
static Pad_Bool
Line_intersects_ver_line(Pad_Point &p1, Pad_Point &p2, Pad_Point &p3, Pad_Point &p4)
{
    float t;
    float y;
    Pad_Bool intersect = FALSE;
    
    if (p1.x != p2.x) {
        t = (p3.x - p1.x) / (p2.x - p1.x);
        if ((t >= 0) && (t <= 1)) {
            intersect = TRUE;
            y = p1.y + t*(p2.y - p1.y);
            if (((y < p3.y) && (y < p4.y)) ||
                ((y > p3.y) && (y > p4.y))) {
                intersect = FALSE;
            }
        }
    }

    return(intersect);
}

//
// Find the point where <this> point aligns with the specified path and type.
// <type> may be PAD_ANCHOR_HORIZONTAL, or PAD_ANCHOR_VERTICAL.
// For example, if <type> is PAD_ANCHOR_HORIZONTAL, see if there is an intersection
// with the horizontal line at <this> point going through the path.
// If just one point, then extend it to be a horizontal or vertical line.
// Return true if an alignment point was found, and false otherwise.
//
static Pad_Bool
Compute_alignment_point(int orientation, Pad_Point &point, Pad_PList &points, Pad_Point &resultPoint)
{
    int i, len;
    float t;
    Pad_Point *p1, *p2;
    Pad_Bool intersect = FALSE;

    len = points.Length();
    if (len == 1) {
                                // If just one point, then extend it to be a horizontal or vertical line
        intersect = TRUE;
        p1 = points.Nth(0);
        switch (orientation) {
          case PAD_LAYOUT_HORIZONTAL:
            resultPoint.Set(p1->x, point.y);
            break;
          case PAD_LAYOUT_VERTICAL:
            resultPoint.Set(point.x, p1->y);
            break;
        }
    } else {
        for (i=0; i<len-1; i++) {
            p1 = points.Nth(i);
            p2 = points.Nth(i+1);
            switch (orientation) {
              case PAD_LAYOUT_HORIZONTAL:
                if (p1->y != p2->y) {
                    t = (point.y - p1->y) / (p2->y - p1->y);
                    if ((t >= 0) && (t <= 1)) {
                        intersect = TRUE;
                        resultPoint.Set(p1->x + t*(p2->x - p1->x), point.y);
                    }
                }
                break;
              case PAD_LAYOUT_VERTICAL:
                if (p1->x != p2->x) {
                    t = (point.x - p1->x) / (p2->x - p1->x);
                    if ((t >= 0) && (t <= 1)) {
                        intersect = TRUE;
                        resultPoint.Set(point.x, p1->y + t*(p2->y - p1->y));
                    }
                }
                break;
            }
            if (intersect) {
                break;
            }
        }
    }

    return(intersect);
}

//
// Return TRUE if <bbox> intersects the path specified by <coords>
//
static Pad_Bool
BBox_intersects_path(Pad_BBox &bbox, Pad_PList &coords) {
    int i, len;
    Pad_Point *p1, *p2;
    Pad_Point p3, p4;
    Pad_Bool intersect;

    intersect = FALSE;

    len = coords.Length();
    for (i=0; i<len-1; i++) {
        p1 = coords.Nth(i);
        p2 = coords.Nth(i+1);
                                // Test left edge
        p3.Set(bbox.Xmin(), bbox.Ymin());
        p4.Set(bbox.Xmin(), bbox.Ymax());
        if (Line_intersects_ver_line(*p1, *p2, p3, p4)) {
            intersect = TRUE;
            break;
        }
                                // Test right edge
        p3.Set(bbox.Xmax(), bbox.Ymin());
        p4.Set(bbox.Xmax(), bbox.Ymax());
        if (Line_intersects_ver_line(*p1, *p2, p3, p4)) {
            intersect = TRUE;
            break;
        }
                                // Test top edge
        p3.Set(bbox.Xmin(), bbox.Ymax());
        p4.Set(bbox.Xmax(), bbox.Ymax());
        if (Line_intersects_hor_line(*p1, *p2, p3, p4)) {
            intersect = TRUE;
            break;
        }
                                // Test bottom edge
        p3.Set(bbox.Xmin(), bbox.Ymin());
        p4.Set(bbox.Xmax(), bbox.Ymin());
        if (Line_intersects_hor_line(*p1, *p2, p3, p4)) {
            intersect = TRUE;
            break;
        }
    }

    return(intersect);
}

//
// Align the specified items so that their bboxs line up on the specified side.
// If <anchor> is true, then line up by anchor point instead of by bbox.
// Align items to item that is furthest in direction specified.
//
Pad_Bool
Pad::Align(Pad_List &objs, int type, Pad_Bool anchor)
{
    float edge, objEdge;
    Pad_Bool first = TRUE;
    Pad_Iterator oi;
    Pad_Object *obj;
    Pad_BBox bb;
    Pad_PList coords;
    Pad_Point point;

    if (objs.Is_empty()) {
        return(TRUE);
    }

                                // First find most extreme object
    DOLIST(oi, objs, Pad_Object, obj) {
        if (anchor) {
            bb.Set(obj->anchorpt.x, obj->anchorpt.y, obj->anchorpt.x, obj->anchorpt.y);
        } else {
            obj->Get_global_bbox(bb);
        }
        switch (type) {
          case PAD_LAYOUT_LEFT:
            objEdge = bb.Xmin();
            break;
          case PAD_LAYOUT_RIGHT:
            objEdge = bb.Xmax();
            break;
          case PAD_LAYOUT_BOTTOM:
            objEdge = bb.Ymin();
            break;
          case PAD_LAYOUT_TOP:
            objEdge = bb.Ymax();
            break;
        default:
            Pad_errorString = "Invalid alignment type.";
            return(FALSE);
        }
        if (first) {
            edge = objEdge;
            first = FALSE;
        } else {
            switch (type) {
              case PAD_LAYOUT_LEFT:
              case PAD_LAYOUT_BOTTOM:
                edge = MIN(edge, objEdge);
                break;
              case PAD_LAYOUT_RIGHT:
              case PAD_LAYOUT_TOP:
                edge = MAX(edge, objEdge);
                break;
            }
        }
    }

    point.Set(edge, edge);
    coords.Push_last(&point);
    Align(objs, type, coords, FALSE, anchor);
    
    return(TRUE);
}

//
// Align the specified items so that their bboxs line up on the specified side.
// If <anchor> is true, then line up by anchor point instead of by bbox.
// Align items along the path specified by <coords>.  If there is only one
// point in <coords>, then align to the line at that point.  If there is
// more than one point in <coords>, then align to that path.
// If <overlapOnly> is true, then items should only be aligned if they 
// overlap the specified edge.
//
Pad_Bool
Pad::Align(Pad_List &objs, int type, Pad_PList &coords, Pad_Bool overlapOnly, Pad_Bool anchor)
{
    int orientation;
    float dx, dy;
    float edge;
    Pad_Object *obj;
    Pad_Point point;
    Pad_Point resultPoint1, resultPoint2;
    Pad_Bool intersection1, intersection2;
    Pad_BBox bb;
    Pad_Iterator oi;

    switch (type) {
      case PAD_LAYOUT_LEFT:
      case PAD_LAYOUT_RIGHT:
        orientation = PAD_LAYOUT_HORIZONTAL;
        break;
      case PAD_LAYOUT_BOTTOM:
      case PAD_LAYOUT_TOP:
        orientation = PAD_LAYOUT_VERTICAL;
        break;
      default:
        Pad_errorString = "Invalid alignment type.";
        return(FALSE);
    }

    DOLIST(oi, objs, Pad_Object, obj) {
        if (anchor) {
            bb.Set(obj->anchorpt.x, obj->anchorpt.y, obj->anchorpt.x, obj->anchorpt.y);
        } else {
            obj->Get_global_bbox(bb);
        }
        dx = 0.0;
        dy = 0.0;
        if (!overlapOnly || BBox_intersects_path(bb, coords)) {
            switch (type) {
              case PAD_LAYOUT_LEFT:
                point.Set(bb.Xmin(), bb.Ymin());
                intersection1 = Compute_alignment_point(orientation, point, coords, resultPoint1);
                point.Set(bb.Xmin(), bb.Ymax());
                intersection2 = Compute_alignment_point(orientation, point, coords, resultPoint2);

                if (intersection1 && intersection2) {
                    edge = MAX(resultPoint1.x, resultPoint2.x);
                } else if (intersection1) {
                    edge = resultPoint1.x;
                } else if (intersection2) {
                    edge = resultPoint2.x;
                } else {
                    continue;
                }
                dx = edge - bb.Xmin();
                break;
              case PAD_LAYOUT_RIGHT:
                point.Set(bb.Xmax(), bb.Ymin());
                intersection1 = Compute_alignment_point(orientation, point, coords, resultPoint1);
                point.Set(bb.Xmax(), bb.Ymax());
                intersection2 = Compute_alignment_point(orientation, point, coords, resultPoint2);

                if (intersection1 && intersection2) {
                    edge = MIN(resultPoint1.x, resultPoint2.x);
                } else if (intersection1) {
                    edge = resultPoint1.x;
                } else if (intersection2) {
                    edge = resultPoint2.x;
                } else {
                    continue;
                }
                dx = edge - bb.Xmax();
                break;
              case PAD_LAYOUT_BOTTOM:
                point.Set(bb.Xmin(), bb.Ymin());
                intersection1 = Compute_alignment_point(orientation, point, coords, resultPoint1);
                point.Set(bb.Xmax(), bb.Ymin());
                intersection2 = Compute_alignment_point(orientation, point, coords, resultPoint2);

                if (intersection1 && intersection2) {
                    edge = MAX(resultPoint1.y, resultPoint2.y);
                } else if (intersection1) {
                    edge = resultPoint1.y;
                } else if (intersection2) {
                    edge = resultPoint2.y;
                } else {
                    continue;
                }
                dy = edge - bb.Ymin();
                break;
              case PAD_LAYOUT_TOP:
                point.Set(bb.Xmin(), bb.Ymax());
                intersection1 = Compute_alignment_point(orientation, point, coords, resultPoint1);
                point.Set(bb.Xmax(), bb.Ymax());
                intersection2 = Compute_alignment_point(orientation, point, coords, resultPoint2);

                if (intersection1 && intersection2) {
                    edge = MIN(resultPoint1.y, resultPoint2.y);
                } else if (intersection1) {
                    edge = resultPoint1.y;
                } else if (intersection2) {
                    edge = resultPoint2.y;
                } else {
                    continue;
                }
                dy = edge - bb.Ymax();
                break;
            }
        }
        obj->Slide(dx, dy, TRUE);
    }

    return(TRUE);
}

//
// Utility sorting callback to sort items horizontally by bbox center
//
static int 
Hor_sort(const void *item1, const void *item2)
{
    float bb1ctr, bb2ctr;
    Pad_BBox bb1, bb2;

    (*(Pad_Object **)item1)->Get_global_bbox(bb1);
    (*(Pad_Object **)item2)->Get_global_bbox(bb2);

    bb1ctr = bb1.Xctr();
    bb2ctr = bb2.Xctr();
    return((int)(bb1ctr - bb2ctr));
}

//
// Utility sorting callback to sort items vertically by bbox center
//
static int 
Ver_sort(const void *item1, const void *item2)
{
    float bb1ctr, bb2ctr;
    Pad_BBox bb1, bb2;

    (*(Pad_Object **)item1)->Get_global_bbox(bb1);
    (*(Pad_Object **)item2)->Get_global_bbox(bb2);

    bb1ctr = bb1.Yctr();
    bb2ctr = bb2.Yctr();
    return((int)(bb1ctr - bb2ctr));
}

//
// Compute the length of the path specified by the list
// of coordinates.
//
static float
Path_length(Pad_PList &coords)
{
    int i;
    float len;

    len = 0.0;
    for (i=0; i<(coords.Length() - 1); i++) {
        len += coords.Nth(i)->Distance(*coords.Nth(i+1));
    }

    return(len);
}

//
// Return the object in <objs> whose center is closest to
// the specified point.
//
static Pad_Object *
Closest_obj_to_point(Pad_Point &point, Pad_List &objs)
{
    Pad_Bool first = TRUE;
    float dist, minDist;
    Pad_Object *obj;
    Pad_Object *closestObj = NULL;
    Pad_Iterator oi;
    Pad_Point ctr;
    Pad_BBox bb;

    DOLIST(oi, objs, Pad_Object, obj) {
        obj->Get_global_bbox(bb);
        ctr.Set(bb.Xctr(), bb.Yctr());
        dist = point.Distance(ctr);
        if (first) {
            minDist = dist;
            closestObj = obj;
            first = FALSE;
        } else {
            if (dist < minDist) {
                minDist = dist;
                closestObj = obj;
            }
        }
    }

    return(closestObj);
}

//
// Compute length of the object along the specified direction.
// <dir> specifies direction with a vector.
//
static float
Obj_len_along_dir(Pad_Object *obj, Pad_Point &origDir)
{
    float len;
    Pad_Point pt, dir;
    Pad_BBox bb;

    if ((origDir.x == 0.0) && (origDir.y == 0.0)) {
        return(0.0);
    }

    dir = origDir;
    obj->Get_global_bbox(bb);
                                // Flip direction so it is always in the first quadrant
    if (dir.x < 0) {
        dir.x *= -1;
    }
    if (dir.y < 0) {
        dir.y *= -1;
    }

    if (dir.x >= dir.y) {
        pt.Set(bb.Width(), bb.Width() * (dir.y / dir.x));
    } else {
        pt.Set(bb.Height() * (dir.x / dir.y), bb.Height());
    }
    len = pt.Vector_length();

    return(len);
}

//
// Distribute the specified items so that the space between them
// is equalized horizontally or vertically (by bbox).
//
Pad_Bool
Pad::Distribute(Pad_List &objs, int type)
{
    int numObjs;
    float totalDim, totalSpace, space;
    float extremeMin, extremeMax, objExtremeMin, objExtremeMax;
    Pad_Bool first = TRUE;
    Pad_Bool rc;
    Pad_Iterator oi;
    Pad_Object *obj;
    Pad_BBox bb;

    if (objs.Is_empty()) {
        return(TRUE);
    }

                                // First, compute space between objects
    totalDim = 0.0;
    numObjs = 0;
    DOLIST(oi, objs, Pad_Object, obj) {
        obj->Get_global_bbox(bb);
        switch (type) {
          case PAD_LAYOUT_HORIZONTAL:
              objExtremeMin = bb.Xmin();
              objExtremeMax = bb.Xmax();
              totalDim += bb.Width();
            break;
          case PAD_LAYOUT_VERTICAL:
              objExtremeMin = bb.Ymin();
              objExtremeMax = bb.Ymax();
              totalDim += bb.Height();
            break;
        default:
            Pad_errorString = "Invalid distribution type.";
            return(FALSE);
        }
        if (first) {
            extremeMin = objExtremeMin;
            extremeMax = objExtremeMax;
            first = FALSE;
        } else {
            extremeMin = MIN(extremeMin, objExtremeMin);
            extremeMax = MAX(extremeMax, objExtremeMax);
        }
        numObjs++;
    }
    totalSpace = (extremeMax - extremeMin) - totalDim;
    space = totalSpace / (numObjs - 1);

                                // Now call function to actually distribute objects
    rc = Distribute(objs, type, space);

    return(rc);
}

//
// Distribute the specified items so that the space between them
// is specified by <space>
//
Pad_Bool
Pad::Distribute(Pad_List &objs, int type, float space)
{
    int i, numObjs;
    float dx, dy;
    float position;
    Pad_Iterator oi;
    Pad_Object *obj;
    Pad_BBox bb;
    typedef Pad_Object *Pad_ObjectPtr;
    Pad_ObjectPtr *sortedObjs;

    if (objs.Is_empty()) {
        return(TRUE);
    }

                                // Sort objects horizontally or vertically, as appropriate
    i = 0;
    numObjs = objs.Length();
    sortedObjs = new Pad_ObjectPtr[numObjs];

    DOLIST(oi, objs, Pad_Object, obj) {
        sortedObjs[i++] = obj;
    }
    switch (type) {
      case PAD_LAYOUT_HORIZONTAL:
        qsort(sortedObjs, numObjs, sizeof(Pad_ObjectPtr), Hor_sort);
        sortedObjs[0]->Get_global_bbox(bb);
        position = bb.Xmin();
        break;
      case PAD_LAYOUT_VERTICAL:
        qsort(sortedObjs, numObjs, sizeof(Pad_ObjectPtr), Ver_sort);
        sortedObjs[0]->Get_global_bbox(bb);
        position = bb.Ymin();
        break;
      default:
        Pad_errorString = "Invalid distribution type.";
        delete [] sortedObjs;
        return(FALSE);
    }
        
                                // Then, distribute objects
    for (i=0; i<numObjs; i++) {
        obj = sortedObjs[i];
        obj->Get_global_bbox(bb);
        dx = 0.0;
        dy = 0.0;
        switch (type) {
          case PAD_LAYOUT_HORIZONTAL:
            dx = position - bb.Xmin();
            position += bb.Width();
            break;
          case PAD_LAYOUT_VERTICAL:
            dy = position - bb.Ymin();
            position += bb.Height();
            break;
        }
        obj->Slide(dx, dy, TRUE);
        position += space;
    }
    
    delete [] sortedObjs;

    return(TRUE);
}

//
// Distribute the specified items so that the space between them
// will be distributed along the path specified by the coordinates
// with equal spacing between items.
//
Pad_Bool
Pad::Distribute(Pad_List &objs, Pad_PList &coords)
{
    int numObjs;
    float space;
    float pathLength;
    float totalDim;
    Pad_Bool rc;
    Pad_Iterator oi;
    Pad_Object *obj;
    Pad_BBox bb;

    if (objs.Is_empty() || coords.Is_empty()) {
        return(TRUE);
    }

                                // First, compute total path length
    pathLength = Path_length(coords);

                                // Next, compute average total box dimensions
    totalDim = 0.0;
    numObjs = 0;
    DOLIST(oi, objs, Pad_Object, obj) {
        obj->Get_global_bbox(bb);
        totalDim += bb.Avg_dim();
        numObjs++;
    }

                                // Then, compute space between objects
    space = (pathLength - totalDim) / (numObjs - 1);

                                // Finally, call function to actually distribute objects
    rc = Distribute(objs, coords, space, TRUE);

    return(rc);
}

//
// Distribute the specified items so that the space between them
// will be distributed along the path specified by the coordinates
// with <space> between each item.  If the items take up more space
// than is available on the specified path, they will continue along
// an extension of the last portion of the path.
// If <exact> is true, then make sure that items are distributed 
// from beginning to end of coordinate list.  If not, recompute spacing,
// and try again.
//
Pad_Bool
Pad::Distribute(Pad_List &objsOrig, Pad_PList &coords, float space, Pad_Bool exact)
{
#define MAX_DIST_ERROR 0.01        // Max error percentage if exact spacing requested
    int          numCoords;
    int          numObjs;
    int          index;                // Current path point index
    float        dx, dy;
    float        error;
    float        segmentLen;        // Length of current path segment
    float        curDist;        // Distance from current path point to current point
    float        newDist;        // Distance from current point to new point
    float        pathLength;    // Total length of distribution path
    Pad_Object * obj;
    Pad_BBox     bb;
    Pad_Point    pt;                // Current point along path
    Pad_Point    dir;                // Current direction of path
    Pad_List     objs;                // Unplaced objects
    Pad_Bool     loop = FALSE;

    if ((objsOrig.Is_empty()) || (coords.Is_empty())) {
        return(TRUE);
    }

    // The following is an O(n^2) algorithm to place objects
    // along the path.  Move along the path, searching through all
    // the unplaced items for the nearest one.  Then, move to the
    // next place on the path, computing the size of the object
    // based on the current direction of the path.

    pathLength = Path_length(coords);
    numCoords = coords.Length();
    numObjs = objsOrig.Length();
    do {
                                // First, initialize things
        objs = objsOrig;
        index = 0;
        curDist = 0.0;
        pt = coords.Nth(0);
        dir.Set(coords.Nth(1)->x - pt.x, coords.Nth(1)->y - pt.y);
        dir.Vector_normalize();
        segmentLen = pt.Distance(*coords.Nth(1));
        do {
                                // Find closest object to current point on path,
                                // and place it on path, accounting for object dimension
                                // along path direction.  If we're on our second
                                // iteration through the objects, then just use the
                                // same order we used the first time.
            if (loop) {
                obj = Closest_obj_to_point(pt, objs);
                objs.Remove(obj);
            } else {
                obj = (Pad_Object *)objs.Pop();
            }

            newDist = Obj_len_along_dir(obj, dir);
            obj->Get_global_bbox(bb);
            dx = pt.x + (0.5 * dir.x * newDist) - bb.Xctr();
            dy = pt.y + (0.5 * dir.y * newDist) - bb.Yctr();
            obj->Slide(dx, dy, TRUE);

                                // Find next point on path, and update path direction
                                // If no more points, keep following same direction.
            if (!objs.Is_empty()) {
                do {
                    if (((curDist + newDist + space) < segmentLen) || (index == (numCoords-2))) {
                        curDist += newDist + space;
                        pt.x += dir.x * (newDist + space);
                        pt.y += dir.y * (newDist + space);
                        break;
                    } else {
                        index++;
                        pt = coords.Nth(index);
                        dir.Set(coords.Nth(index+1)->x - pt.x, coords.Nth(index+1)->y - pt.y);
                        dir.Vector_normalize();
                        newDist -= (segmentLen - curDist);
                        curDist = 0.0;
                        segmentLen = pt.Distance(*coords.Nth(index + 1));
                    }
                } while (1);
            }
        } while (!objs.Is_empty());
                                // If exact spacing was requested, then check to see how
                                // we did.  If the initial estimate of spacing was
                                // inaccurate, we could be substantially off - so,
                                // modify the space between objects and try again.
        error = (curDist + newDist - segmentLen)/pathLength;
        if (exact && (ABS(error) > MAX_DIST_ERROR)) {
            loop = TRUE;
            exact = FALSE;
            space -= (curDist + newDist - segmentLen) / (numObjs - 1);
        } else {
            loop = FALSE;
        }
    } while (loop);

    return(TRUE);
}

//
// Position the specified objects relative to a target object.
// Specify target point by a point on the unit square, and
// specify the source point by a point on the unit square.
//
Pad_Bool
Pad::Layout_position(Pad_Object *target, Pad_Point &targetPt, Pad_List &objs, Pad_Point &pt, int animationTime)
{
    Pad_Bool rc;
    Pad_BBox targetBBox;

    target->Get_global_bbox(targetBBox);
    rc = Layout_position(targetBBox, targetPt, objs, pt, animationTime);

    return(rc);
}

//
// Position the specified objects relative to a bounding box.
// Specify target point by a point on the unit square, and
// specify the source point by a point on the unit square.
//
Pad_Bool
Pad::Layout_position(Pad_BBox &targetBBox, Pad_Point &targetPt, Pad_List &objs, Pad_Point &srcPt, int animationTime)
{
    float dx, dy;
    float targetX, targetY;
    float srcX, srcY;
    Pad_BBox srcBBox;
    Pad_Object *obj;
    Pad_Iterator oi;
    Pad_PList deltas;
    Pad_Point delta;

    targetX = LERP(targetPt.x, targetBBox.Xmin(), targetBBox.Xmax());
    targetY = LERP(targetPt.y, targetBBox.Ymin(), targetBBox.Ymax());
    DOLIST(oi, objs, Pad_Object, obj) {
        obj->Get_global_bbox(srcBBox);
        srcX = LERP(srcPt.x, srcBBox.Xmin(), srcBBox.Xmax());
        srcY = LERP(srcPt.y, srcBBox.Ymin(), srcBBox.Ymax());
        dx = targetX - srcX;
        dy = targetY - srcY;
        if (animationTime == 0) {
            obj->Slide(dx, dy, TRUE);
        } else {
            delta.Set(dx, dy);
            deltas.Push_last(&delta);
        }
    }

    if (animationTime > 0) {
        Slide(objs, deltas, TRUE, animationTime);
    }

    return(TRUE);
}

//
// Scale the specified objects so that their bounding boxes
// are scaled (width or height) to the reference object.
// Scale the objects based on <scale>.
// Objects are scaled around their anchor points.
//
Pad_Bool
Pad::Layout_size(int type, Pad_Object *refObj, Pad_List &objs, float scale)
{
    float dim, refDim, ds;
    Pad_BBox bb, refBBox;
    Pad_Object *obj;
    Pad_Iterator oi;

    refObj->Get_global_bbox(refBBox);
    switch (type) {
      case PAD_LAYOUT_WIDTH:
        refDim = refBBox.Width();
        break;
      case PAD_LAYOUT_HEIGHT:
        refDim = refBBox.Height();
        break;
    }

    DOLIST(oi, objs, Pad_Object, obj) {
        obj->Get_global_bbox(bb);
        switch (type) {
          case PAD_LAYOUT_WIDTH:
            dim = bb.Width();
            break;
          case PAD_LAYOUT_HEIGHT:
            dim = bb.Height();
            break;
        }

        ds = refDim * scale / dim;
        obj->Scale(obj->anchorpt.x, obj->anchorpt.y, ds);
    }

    return(TRUE);
}

//
// Scale the specified objects so that their bounding boxes
// are scaled (width or height) to an absolute dimension
// specified by <newDim>.
// Objects are scaled around their anchor points.
//
Pad_Bool
Pad::Layout_size(int type, Pad_List &objs, float newDim)
{
    float dim, ds;
    Pad_BBox bb;
    Pad_Object *obj;
    Pad_Iterator oi;

    DOLIST(oi, objs, Pad_Object, obj) {
        obj->Get_global_bbox(bb);
        switch (type) {
          case PAD_LAYOUT_WIDTH:
            dim = bb.Width();
            break;
          case PAD_LAYOUT_HEIGHT:
            dim = bb.Height();
            break;
        }

        ds = newDim / dim;
        obj->Scale(obj->anchorpt.x, obj->anchorpt.y, ds);
    }

    return(TRUE);
}

//
// Position the objects so that their anchor points
// are snapped to <grid>.
//
Pad_Bool
Pad::Snap(float grid, Pad_List &objs)
{
    float x, y, s;
    Pad_Object *obj;
    Pad_Iterator oi;

    DOLIST(oi, objs, Pad_Object, obj) {
        obj->Get_rel_position(x, y, s);
        x = ((int)((x + (SIGN(x) * 0.5 * grid)) / grid)) * grid;
        y = ((int)((y + (SIGN(y) * 0.5 * grid)) / grid)) * grid;
        obj->Set_rel_position(x, y, s);
    }

    return(TRUE);
}

//
// Scale the object around the point (x, y) by zmult
// Animation the motion over <animationTime> milliseconds.
//
Pad_Bool
Pad::Scale(Pad_List &objs, float x, float y, float zmult, Pad_Bool transformp, int animationTime)
{
    Pad_Iterator oi;
    Pad_Object *obj;

    if (animationTime <= 0) {
        DOLIST(oi, objs, Pad_Object, obj) {
            obj->Scale(x, y, zmult, transformp);
        }
    } else {
        float deltaz, prevz, desiredz;
        float lerp, oldLerp, straightLerp;
        float elapsedTime;
        long  startSec, startUsec;
        Pad_Time time;
        Pad_Bool rc;

                                // Record current time
        time.Update();
        startSec = time.Get_sec();
        startUsec = time.Get_usec();

        lerp = 0.0;
        oldLerp = lerp;
        prevz = 1.0;

        do {
                                // Scale objects a little bit
            desiredz = LERP(lerp, 1.0, zmult);
            deltaz = desiredz / prevz;
            prevz = desiredz;
            DOLIST(oi, objs, Pad_Object, obj) {
                obj->Scale(x, y, deltaz, transformp);
            }
            view->Update_display();

                                // Calculate total elapsed time
            time.Update();
            elapsedTime = (1000000 * (time.Get_sec() - startSec) + (time.Get_usec() - startUsec)) / 1000;

                                // Calculate next interpolation step
            straightLerp = elapsedTime / animationTime;
            oldLerp = lerp;
            lerp = Pad_Siso_lerp(straightLerp);
                                // Check for interruption
            rc = Pad_renderer->Is_interrupted(view->win);
        } while ((elapsedTime < animationTime) && !rc);

                                // Correct scale so it finally is correct
        desiredz = zmult;
        deltaz = desiredz / prevz;
        DOLIST(oi, objs, Pad_Object, obj) {
            obj->Scale(x, y, deltaz, transformp);
        }
        view->Update_display();
    }

    return(TRUE);
}

//
// Slide (translate) the objects by <delta>.
// If <transform> is TRUE, then slide amount independent of group membership
// (i.e., <delta> is modified based on size of groups).
// Animation the motion over <animationTime> milliseconds.
//
Pad_Bool
Pad::Slide(Pad_List &objs, float dx, float dy, Pad_Bool transformp, int animationTime)
{
    Pad_Point delta;
    Pad_PList deltas;
    Pad_Iterator oi;
    Pad_Object *obj;

    delta.Set(dx, dy);
    DOLIST(oi, objs, Pad_Object, obj) {
        deltas.Push_last(&delta);
    }

    return(Slide(objs, deltas, transformp, animationTime));
}

Pad_Bool
Pad::Slide(Pad_List &objs, Pad_PList &deltas, Pad_Bool transformp, int animationTime)
{
    Pad_Iterator oi;
    Pad_Object *obj;
    Pad_Point *delta;

    if (animationTime <= 0) {
        DOLIST(oi, objs, Pad_Object, obj) {
            delta = deltas.Pop();
            obj->Slide(delta->x, delta->y, transformp);
        }
    } else {
        int i;
        float lerp, straightLerp;
        float elapsedTime;
        long  startSec, startUsec;
        Pad_Time time;
        Pad_Bool rc;
        Pad_Point *start;

                                // Record current time
        time.Update();
        startSec = time.Get_sec();
        startUsec = time.Get_usec();

        lerp = 0.0;

        start = new Pad_Point[objs.Length()];
        i = 0;
        DOLIST(oi, objs, Pad_Object, obj) {
            start[i].Set(obj->Get_rel_position_x(), obj->Get_rel_position_y());
            i++;
        }

        do {
                                // Slide objects a little bit
            i = 0;
            DOLIST(oi, objs, Pad_Object, obj) {
                delta = deltas.Nth(i);
                obj->Set_rel_position(start[i].x + (lerp * delta->x), start[i].y + (lerp * delta->y), obj->Get_rel_position_z());
                i++;
            }
            view->Update_display();

                                // Calculate total elapsed time
            time.Update();
            elapsedTime = (1000000 * (time.Get_sec() - startSec) + (time.Get_usec() - startUsec)) / 1000;

                                // Calculate next interpolation step
            straightLerp = elapsedTime / animationTime;
            lerp = Pad_Siso_lerp(straightLerp);
                                // Check for interruption
            rc = Pad_renderer->Is_interrupted(view->win);
        } while ((elapsedTime < animationTime) && !rc);

                                // When done, put each object at exact point in case there was rounding error
        i = 0;
        DOLIST(oi, objs, Pad_Object, obj) {
            delta = deltas.Nth(i);
            obj->Set_rel_position(start[i].x + delta->x, start[i].y + delta->y , obj->Get_rel_position_z());
            i++;
        }
        view->Update_display();

        delete [] start;
    }

    return(TRUE);
}

//
// Control generation of events.
// If <state> is FALSE, then no events will be generated
// by Pad_Object::Generate_event().  If <state> is TRUE (the default),
// then events will be generated as usual.  Events might be temporarily
// turned off during an animation to speed things up.
//
void
Pad::Set_event_control(Pad_Bool state)
{
    if (state) {
        padFlags &= ~PADROOT_EVENTSOFF;
    } else {
        padFlags |= PADROOT_EVENTSOFF;
    }
}

//
// This controls generate of <Create> event.
// By default, the <Create> event is generated by the Pad_Object constructor.
// In some cases, the creator may like to delay the generation of this event.
// In this case, call this method with <state> equal to FALSE, and
// the generation of the <Create> event will be left up to the creator.
// Remember to set it back to TRUE when finished.
//
void
Pad::Set_create_event_control(Pad_Bool state)
{
    if (state) {
        padFlags |= PADROOT_MANUAL_CREATE_EVENT;
    } else {
        padFlags &= ~PADROOT_MANUAL_CREATE_EVENT;
    }
}

//
// Set "active" bit.  This means that the pad window
// currently has the focus.
//
void
Pad::Set_active(Pad_Bool active)
{
    if (active) {
        padFlags |= PADROOT_ACTIVE;
    } else {
        padFlags &= ~PADROOT_ACTIVE;
    }
}

Pad_Bool
Pad::Get_active(void)
{
    return ((padFlags & PADROOT_ACTIVE) ? TRUE : FALSE);
}

//
// Return the object specified by <obj_id>, 
// or NULL if there is none.
//
Pad_Object *
Pad::Get_object_from_id(intptr_t id)
{
    Pad_Object *obj;

                                // This pad widget could have been
                                // deleted and this can be called
                                // on a timer callback, so check for that.
    if (!idTable) {
        return(NULL);
    }

    obj = (Pad_Object *)idTable->Get((char *)id);

    return(obj);
}

//
// Return a list of Pad_Object's having the tag <tag>
//
void
Pad::Get_objects_from_tag(char *tag, Pad_List &objs)
{
    Pad_Uid uid;
    Pad_List *tagObjs;

    uid = Pad_GetUid(tag);
    tagObjs = (Pad_List *)pad->tagTable->Get((void *) uid);
    if (tagObjs) {
        objs = tagObjs;
    }
}

//
// Return a list of id's having the tag <tag>
//
void
Pad::Get_ids_from_tag(char *tag, Pad_IList &ids)
{

    Pad_Uid uid;
    Pad_List *tagObjs;
    Pad_Iterator oi;
    Pad_Object *obj;

    uid = Pad_GetUid(tag);
    tagObjs = (Pad_List *)pad->tagTable->Get((void *) uid);
    if (tagObjs) {
        DOLIST(oi, *tagObjs, Pad_Object, obj) {
            ids.Push_last(obj->id);
        }
    }
}

//
// Delete object by the specified id.
// Return true if successful, or false if the object doesn't exist,
// or it was locked.
//
Pad_Bool
Pad::Delete_obj(intptr_t id)
{
    Pad_Bool rc;
    Pad_Object *obj;

    obj = Get_object_from_id(id);
    if (obj && !obj->Get_lock()) {
        delete obj;
        rc = TRUE;
    } else {
        rc = FALSE;
    }

    return(rc);
}

//
// Mark entire pad surface as changed, and register it
// for re-rendering.
//
void 
Pad::Damage(void)
{
    view->Damage();
}

//
// Render the entire pad surface.
// Be careful.  This gets called recursively because of portals.
//
// Return true to continue rendering other objects, or false if rendering
// was interrupted.
//
Pad_Bool
Pad::Render(void)
{
    Pad_Bool rc = TRUE;

    rc = Render_display_list(first);

    if (view->win->debugBB) {
        Render_bounding_box();
    }

    return(rc);
}

//
// Render the objects on the display list starting with <obj>.
// While rendering, determine if the system has become slow.
// If it has, then start rendering with the fast versions
// of objects instead of the regular ones.
//
Pad_Bool
Pad::Render_display_list(Pad_Object *obj)
{
#define OBJECTS_PER_TIME_CHECK 3
    int count = 0;
    int      renderSpeed;
    Pad_Bool interrupted = FALSE;
    Pad_Time time;
    long     startSec, startUsec;
    int      renderTime;        // Time (in ms) of current render so far
    long     desiredTime = (1000 / view->win->desiredFrameRate);
    Pad_Bool visible, viewable;

                                // Get the current time
    time.Update();
    startSec = time.Get_sec();
    startUsec = time.Get_usec();

    while (obj) 
      {
          visible = FALSE;
          viewable = FALSE;
          if (Pad_Is_overlapping(obj->Get_global_bbox()->Get(), Pad_prc->activeBBox.Get())) {
              viewable = TRUE;
              if (obj->Check_for_render()) {
                  visible = TRUE;
                                // Every once in a while, check to see how much time has elapsed
                  if (!Pad_prc->slow && (level == 0) && (count++ % OBJECTS_PER_TIME_CHECK) == 0) {
                      time.Update();
                      renderTime = ((time.Get_sec() - startSec) * 1000) + ((time.Get_usec() - startUsec) / 1000);
                      if (renderTime > desiredTime) {
                                // System has gotten slow.  So, request objects to draw themselves
                                // very simply, and schedule a refinement
                          Pad_prc->slow = TRUE;
                          Pad_prc->win->refiningRestorer->Make_full();
                      }
                  }

                  renderSpeed = 0;
                  if (Pad_prc->slow) {
                      if (!obj->Get_alwaysrender() && !obj->Is_sticky()) {
                          if (obj->pixelDim < view->win->smallObjSize) {
                              renderSpeed = 3;  // Don't render small objects
                          } else if (obj->pixelDim < view->win->mediumObjSize) {
                              renderSpeed = 2;   // Render medium size objects ugly
                          } else {
                              renderSpeed = 1;   // Render large objects a bit faster 
                          }
                      }
                  }
                  
                  if (renderSpeed < 3) {
                      if (!Render_object(obj, renderSpeed)) {
                                // If interrupted, render other objects at level 0.
                          if (level > 0) {
                              float w, h;

                              interrupted = TRUE;
                              level = 0;
                              obj->Compute_dimensions(w, h);     // Re-compute dimensions as render level can get updated here
                              Render_object(obj, renderSpeed);   // Re-render this object
                          }
                      }
                  }
              }
          }
          /*
          if (obj->findable && obj->visible ^ visible)
            cerr << (visible ? "visible " : "invisible ") << obj->id << endl;
          if (obj->findable && obj->viewable ^ viewable)
            cerr << (viewable ? "enter " : "leave ") << obj->id << endl;
          */
          obj->visible = visible;
          obj->viewable = viewable;
          obj = obj->next;
      }
      time.Update();
      this->renderTime = ((time.Get_sec() - startSec) * 1000) + ((time.Get_usec() - startUsec) / 1000);

    return(!interrupted);
#undef OBJECTS_PER_TIME_CHECK
}

//
// Return true to continue rendering, or false to interrupt
//
Pad_Bool 
Pad::Render_object(Pad_Object *obj, int renderSpeed)
{
    int result;
    float fade;
    float transparency;
    float currentTransparency;
    Pad_Bool visible;
    Pad_Bool rc = TRUE;
    Pad_Bool callback = FALSE;
    Pad_Bool renderScriptRefinement = FALSE;
    Pad_Bool prevDamageEnabled;
    Pad_Bool clipped = FALSE;
    Pad_View *view;
    Pad_BBox *bb;
    Pad_ObjectHandle *handle;

    fade = obj->Compute_fade();

                                // Multiply the current transparency by this object's
    view = (Pad_View *)Pad_prc->views.Last();
    transparency = fade * obj->Get_transparency();
    if (transparency > 0) {
        visible = TRUE;
    } else {
                                // Note, even if an object is transparent, we must
                                // continue in case we need to fire a zoomaction.
        visible = FALSE;
    }
    currentTransparency = Pad_renderer->Get_transparency();
    Pad_renderer->Set_transparency(currentTransparency * transparency);

    bb = obj->Get_bbox();
    Pad_renderer->Push_object(obj);

                                // Turn on clipping if required
    if (obj->Check_render_clipped()) {
        Pad_prc->win->activeRestorer->Push_clip(bb->Get());
        clipped = TRUE;
    }

    Pad_prc->objects.Push(obj);
    Pad_prc->result = 1;
                                // Fire zoom actions
                                // (even if not visible)
    Pad_List *zoomActions = obj->Get_zoomaction();
    if (zoomActions) {
        Pad_Iterator zi;
        Pad_ZoomAction *zoomAction;
        Pad_ZoomActionSize *prevSize;
        Pad_Callback *script = NULL;
        Pad_Bool found;

        Pad_List *prevSizes = obj->Get_prevsizes();
        found = FALSE;
        DOLIST(zi, *prevSizes, Pad_ZoomActionSize, prevSize) {
            if (prevSize->view == view) {
                found = TRUE;
                break;
            }
        }
        if (!found) {
            prevSize = new Pad_ZoomActionSize;
            prevSize->pixelDim = obj->pixelDim;
            prevSize->view = view;
            prevSizes->Push_last(prevSize);
        }

        DOLIST(zi, *zoomActions, Pad_ZoomAction, zoomAction) {
            if ((prevSize->pixelDim < zoomAction->actionSize) &&
                (obj->pixelDim >= zoomAction->actionSize)) {
                script = zoomAction->growScript;
            }
            else if ((prevSize->pixelDim > zoomAction->actionSize) &&
                     (obj->pixelDim <= zoomAction->actionSize)) {
                script = zoomAction->shrinkScript;
            }
        }
        
        prevSize->pixelDim = obj->pixelDim;           // Remember size

        if (script) {
            prevDamageEnabled = obj->pad->view->win->damageEnabled;
            obj->pad->view->win->damageEnabled = FALSE;
            handle = new Pad_ObjectHandle(obj);
            result = script->Eval();
                                // Be careful, this object could have been deleted by script
            if (handle->Get_object() == NULL) {
                delete handle;
                return(TRUE);
            }
            delete handle;
            if (result == PAD_ERROR) {
                Pad_Background_error("Pad threshold callback");
            }
            obj->pad->view->win->damageEnabled = prevDamageEnabled;
            rc = Pad_prc->result;
            callback = TRUE;
        }
    }
                                // Fire render callbacks
    Pad_Callback *renderScript = obj->Get_renderscript();
    if (visible && renderScript) {
        handle = new Pad_ObjectHandle(obj);
        rc = obj->Fire_render_script(renderScriptRefinement);
                                // Be careful, this object could have been deleted by script
        if (handle->Get_object() == NULL) {
            delete handle;
            return(TRUE);
        }
        delete handle;
        callback = TRUE;
    }
                                // Call object render method if visible
                                // and no callbacks were fired.
    if (visible && (callback == FALSE)) {
        if (renderSpeed > 0) {
            if (renderSpeed == 1) {
                rc = obj->Render_medium();
            } else {
                rc = obj->Render_fast();
            }
        } else {
            rc = obj->Render();
        }
    }

    if (clipped) {
        Pad_prc->win->activeRestorer->Pop_clip();
    }
                        // Test if there is more to render for this object
    if (visible &&
        ((obj->Continue_refinement()) || (renderScript && (renderScriptRefinement))))
      {
          Pad_prc->win->refiningRestorer->Add_rect_to_refiner(bb->Get());
      }
    if (visible) {
        view->pad->objectsRendered++;
    }

    Pad_prc->objects.Pop();
    Pad_renderer->Pop_object();
    Pad_renderer->Set_transparency(currentTransparency);

    return(rc);
}

//
// Render the bounding boxes of all the objects.
//
void 
Pad::Render_bounding_box(void)
{
    float *viewBB;
    Pad_Object *obj;
    Pad_View *view;
    Pad_BBox *bb;
    
    view = (Pad_View *)Pad_prc->views.Last();
    viewBB = view->viewBBox.Get();

    Pad_renderer->Set_abs_line_width(0);
    Pad_renderer->Set_color(&Pad_Color::black);

    obj = pad->First();
    while (obj) {
        bb = obj->Get_global_bbox();
        if (Pad_Is_overlapping(bb->Get(), viewBB)) {
            Pad_renderer->Draw_box(bb->Xmin(), bb->Ymin(), bb->Xmax(), bb->Ymax());
        }
        obj = Next(obj);
    }    
}

//
// Find top-most object that this event hit,
// going through portals and divisible groups,
// firing PortalIntercepts along the way.
//
Pad_Object *
Pad::Find_pick(Pad_Event *event)
{
    int result;
    int lastDrawNum;
    Pad_Bool found;
    Pad_Object *eventObj;
    Pad_Portal *portal;
    Pad_Bool passThroughPortal;
    Pad_View *currentView;
    Pad_String eventType;
    Pad_Point origPt;
    float origMag;

    if (!first) {                // No items at all
        return(this);
    }

    currentView = view;

    lastDrawNum = first->drawingOrder - 1;

    do {
        eventObj = _Pick_recurse(currentView, event, NULL, currentView->pad->last, 
                                      lastDrawNum, found);

                                // Now, if object is a portal, then fire PortalIntercept, 
                                // and go through it
        passThroughPortal = FALSE;
        if (eventObj && (eventObj->Type() == PAD_PORTAL)) 
            {
                portal = (Pad_Portal *)eventObj;
                origPt = event->pt;
                origMag = event->mag;
                portal->Screen_to_local(*event);
                if (!portal->Pick_border(event, currentView->win->closeEnough) && portal->lookon) 
                    {
                                // Store event that is generating intercept into a string for event info
                        if (event->eventPtr) {
                            eventType = Pad_GetEventName(event->eventPtr->type);
                        }
                                // Fire PortalIntercept
                        result = portal->Generate_event(Pad_PortalInterceptNotify, &event->portals, eventType.Get());
                        if (result != PAD_BREAK) {
                                // If return code not break, then pass through portal
                            event->pt = origPt;
                            event->mag = origMag;
                            portal->Pass_through(event);
                            event->portals.Push_last(portal);
                            lastDrawNum = portal->lookon->view->pad->first->drawingOrder - 1;
                            currentView = portal->lookon->view;
                            event->win = currentView->win;
                            passThroughPortal = TRUE;
                        }
                    }
                if (!passThroughPortal) {
                                // Didn't go through portal, so restore event
                    event->pt = origPt;
                    event->mag = origMag;
                }
            }
    } while (passThroughPortal);
                                // If event doesn't hit an object, send it to the pad surface
    if (!eventObj && (currentView->pad->Gets_events())) {
        eventObj = currentView->pad;
    }

    return(eventObj);
}

//
// Pick the item through <portals>, transformed by <objects>
//
Pad_Object *
Pad::Find_pick(Pad_Event *event, Pad_List &portals, Pad_List &objects)
{
    int lastDrawNum;
    Pad_Point pt;
    float mag;
    Pad_Bool found;
    Pad_Object *eventObj, *obj;
    Pad_Portal *portal;
    Pad_Iterator oi;
    Pad_List objectsCopy, portalsCopy;
    Pad_String eventName;
    Pad_View *currentView;

    if (!first) {                // No items at all
        return(this);
    }

    pt = event->pt;
    mag = event->mag;

    DOLIST(oi, portals, Pad_Portal, portal) {
        event->pt = pt;
        event->mag = mag;
                                // Store event that is generating intercept into a string for event info
        if (event->eventPtr) {
            eventName = Pad_GetEventName(event->eventPtr->type);
        }
        portal->Generate_event(Pad_PortalInterceptNotify, &portalsCopy, eventName.Get());
                                // Ignore return code of PortalIntercept since portal list was grabbed
        portal->Pass_through(event);
        portalsCopy.Push_last(portal);
    }

    currentView = view;
    lastDrawNum = first->drawingOrder - 1;
        
    pt = event->pt;
    mag = event->mag;

    objectsCopy = objects;
    event->objects = objects;
    event->portals = portals;
    eventObj = _Pick_recurse(currentView, event, NULL, last, lastDrawNum, found);

                                // Now transform the point by the list of grabbed objects.
    DOLIST(oi, objectsCopy, Pad_Object, obj) {
        obj->transform.Invert(event);
    }
                                // Copy the transformed point to object coordinates
    event->objPt = event->pt;
    event->objMag = event->mag;
                                // Restore the pad coordinate
    event->pt = pt;
    event->mag = mag;

    if (!eventObj && Gets_events()) {
        eventObj = this;
    }

    return(eventObj);
}

Pad_Object *
Pad::_Pick_recurse(Pad_View *currentView, Pad_Event *event, Pad_Object *eventObj, Pad_Object *last, 
                   int &lastDrawNum, Pad_Bool &found)
{
    int otype;
    float globalWidth, globalHeight;
    Pad_Object *obj;
    Pad_Bool divisible;
    Pad_Point pt;
    float mag;
    Pad_Group *grp;
    Pad_View *viewObj;

    found = FALSE;
    obj = last;
    while (obj)
      {
          if (!found) {
              pt = event->pt;
              mag = event->mag;
              otype = obj->Type();
                                // Necessary to check size of object
              obj->Pad_Object::Compute_dimensions(event, globalWidth, globalHeight);
                                // Convert event to object's coordinates
              obj->transform.Invert(event);
                                // Find portal item is being picked within, or top-level view if none
              viewObj = (Pad_View *)event->portals.Last();
              if (!viewObj) {
                  viewObj = view;
              }
                                // Pick objects that are visible, get events, and see the event.
                                // Only test real pick method
                                // if event is within bbox of object.
              if (obj->Pad_Object::Pick(event, view->win->closeEnough) &&
                  obj->Check_render_size() &&
                  obj->Check_render_layer(viewObj) &&
                  obj->Pick(event, view->win->closeEnough))
                {
                                // Find out if the event should go to a sub-component
                                // of the object.
                    if (obj->Is_group()) {
                        if (event->Get_divisible() == AUTOMATIC) {
                            divisible = ((Pad_Group *)obj)->Get_divisible();
                        } else {
                            divisible = (Pad_Bool)event->Get_divisible();
                        }
                    } else {
                        divisible = FALSE;
                    }
                    
                    if (!divisible) {
                        if (obj->Gets_events() &&
                            (obj->drawingOrder > lastDrawNum))
                          {
                                // Don't pick object if it is the portal
                                // being viewed through.
                              if ((otype != PAD_PORTAL) ||
                                  (((Pad_Portal *)obj)->serial != event->serial)) 
                                {
                                    lastDrawNum = obj->drawingOrder;
                                    eventObj = obj;
                                    event->objPt = event->pt;
                                    event->objMag = event->mag;
                                    event->objects.Push_last(obj);
                                    found = TRUE;
                                    if (otype == PAD_PORTAL) {
                                        ((Pad_Portal *)obj)->serial = event->serial;
                                    }
                                }
                          }
                                // If item is a divisible group, then check its members.
                    } else {
                        Pad_Inset *inset;
                        Pad_BBox bb;

                        grp = (Pad_Group *)obj;
                        if (!grp->members.Is_empty()) {
                            grp->Get_bbox(bb);
                            inset = grp->Get_inset();
                                // Don't pick members in groups borders
                            if ((event->pt.x > (bb.Xmin() + inset->left)) &&
                                (event->pt.y > (bb.Ymin() + inset->bottom)) &&
                                (event->pt.x < (bb.Xmax() - inset->right)) &&
                                (event->pt.y < (bb.Ymax() - inset->top))) 
                                {
                                    event->objects.Push_last(obj);
                                    eventObj = _Pick_recurse(currentView, event, eventObj, 
                                                             (Pad_Object *)grp->members.First(), 
                                                             lastDrawNum, found);
                                    if (!found) {
                                        event->objects.Remove(obj);
                                    }
                                }
                        }
                                // If members weren't picked, check if should pick group object.
                        if (!found) {
                            if (grp->Pick_group(event)) {
                                event->objects.Push_last(obj);
                                eventObj = obj;
                                event->objPt = event->pt;
                                event->objMag = event->mag;
                                lastDrawNum = obj->drawingOrder;
                                found = TRUE;
                            }
                        }
                    }
                }
              event->pt = pt;
              event->mag = mag;
          }

          obj = obj->prev;        // Check previous item on display list
      }

    return(eventObj);
}

//
// Return the first object in the drawing order
//
Pad_Object *
Pad::First(void)
{
    return(first);
}

//
// Return the last object in the drawing order
//
Pad_Object *
Pad::Last(void)
{
    return(last);
}

//
// Return the next object after <obj> in the drawing order.
// Search groups depth-first.
//
Pad_Object *
Pad::Next(Pad_Object *obj)
{
    if (obj->Is_group() && !((Pad_Group *)obj)->members.Is_empty()) {
                                // Group members in reverse display list order
        return((Pad_Object *)((Pad_Group *)obj)->members.Last());
    }
    while (!obj->next && obj->group) {
        obj = obj->group;
    }
    return(obj->next);
}

//
// Return the previous object before <obj> in the drawing order.
// Search groups depth-first
Pad_Object *
Pad::Prev(Pad_Object *obj)
{
    if (obj->Is_group() && !((Pad_Group *)obj)->members.Is_empty()) {
                                // Group members in reverse display list order
        return((Pad_Object *)((Pad_Group *)obj)->members.First());
    }
    while (!obj->prev && obj->group) {
        obj = obj->group;
    }
    return (obj->prev);
}

//
// Construct list of ids specified by tagorid in display-list order, where tagorid can be
//   id of an item
//   tag an item
//
void 
Pad::Find_ids_with_tagorid(char *tagorid, Pad_IList &ids, Pad_Bool groups)
{
    int id;
    Pad_Object *obj;
    Pad_Iterator oi;
    Pad_List objs;
    
    ids.Make_empty();
    if (!strcmp(tagorid, "all")) {
        Find_all_ids(ids, groups);
        return;
    }


    if (isdigit(tagorid[0])) {
        id = atoi(tagorid);
        obj = Get_object_from_id(id);
        if (obj) {
            objs.Push(obj);
        }
    } else {
        Get_objects_from_tag(tagorid, objs);
    }

    DOLIST(oi, objs, Pad_Object, obj) {
        if ((groups || !obj->group) && obj->Is_findable())
          {
              ids.Push_last(obj->id);
          }
    }
}

//
// Construct list of all ids in display-list order.
//
void 
Pad::Find_all_ids(Pad_IList &ids, Pad_Bool groups)
{
    Pad_Object *obj;


    ids.Make_empty();
    obj = First();
    while (obj) {
        if ((groups || !obj->group) && obj->Is_findable())
          {
              ids.Push_last(obj->id);
          }
        obj = Next(obj);        // Get next object in display list
    }
}

//
// Construct list of all objects in display-list order.
//
void 
Pad::Find_all(Pad_List &items, Pad_Bool groups)
{
    Pad_Object *obj;

    items.Make_empty();
    obj = First();
    while (obj) {
        if ((groups || !obj->group) && obj->Is_findable())
          {
              items.Push_last(obj);
          }
        obj = Next(obj);        // Get next object in display list
    }
}
//
// Construct list of items specified by tagorid in display-list order, where tagorid can be
//   id of an item
//   tag an item
// Note that this returns item #1 if asked for by id or tag (but not by "all").
//
void 
Pad::Find_with_tagorid(char *tagorid, Pad_List &items, Pad_Bool groups, Pad_Bool append)
{
    int id;
    Pad_Object *obj;
    Pad_Iterator oi;
    Pad_List objs;
    
    if (!append) {
        items.Make_empty();
    }
    if (!strcmp(tagorid, "all")) {
        Find_all(items, groups);
        return;
    }

    if (isdigit(tagorid[0])) {
        id = atoi(tagorid);
        obj = Get_object_from_id(id);
        if (obj) {
            objs.Push(obj);
        }
    } else {
        Get_objects_from_tag(tagorid, objs);
    }

    DOLIST(oi, objs, Pad_Object, obj) {
        if (groups || !obj->group)
          {
              items.Push_last(obj);
          }
    }
}

//
// Construct list of items containing the specified text in display-list order
//
void 
Pad::Find_with_text(char *text, Pad_List &items, Pad_Bool groups)
{
    Pad_Object *obj;

    items.Make_empty();
    obj = First();
    while (obj) {
        if ((groups || !obj->group) && 
            (obj->Is_findable()) &&
            (obj->Contains_text(text))) 
          {
              items.Push_last(obj);
          }
        obj = Next(obj);        // Get next object in display list
    }
}

//
// Construct list of items containing the specified layer in display-list order
//
void 
Pad::Find_with_layer(Pad_Layer *layer, Pad_List &items, Pad_Bool groups)
{
    int requestedLayerId;
    Pad_Object *obj;

    items.Make_empty();
    if (!layer) {
        return;
    }
    requestedLayerId = layer->id;
    obj = First();
    while (obj) {
        if ((groups || !obj->group) && 
            (obj->Is_findable()) &&
            (obj->layerId == requestedLayerId))
          {
              items.Push_last(obj);
          }
        obj = Next(obj);        // Get next object in display list
    }
}

//
// Construct list of items containing the specified text in their info slot
// in display-list order
//
void 
Pad::Find_with_info(char *infoptr, Pad_List &items, Pad_Bool groups)
{
    Pad_Object *obj;
    const char *info;

    items.Make_empty();
    obj = First();
    while (obj) {
        if ((groups || !obj->group) && 
            (obj->Is_findable()) &&
            (info = obj->Get_info()) &&
            (strstr(info, infoptr)))
          {
            items.Push_last(obj);
          }
        obj = Next(obj);        // Get next object in display list
    }
}

//
// Construct list of items of the specified type in display-list order.
//
void 
Pad::Find_with_type(const char *type_name, Pad_List &items, Pad_Bool groups)
{
    int otype, objType;
    Pad_Object *obj;
    Pad_Type *type;
    Pad_Uid typeUid;
    Pad_String *userType;

    typeUid = Pad_GetUid(type_name);
    type = (Pad_Type *)typeTable->Get((void *) typeUid);
    items.Make_empty();
    if (type) {
        objType = type->id;

        obj = First();
        while (obj) {
            otype = obj->Type();
            userType = obj->Get_usertype();
            if ((groups || !obj->group) && 
                (obj->Is_findable()) && 
                (((otype == objType) || (userType && (*userType == typeUid)))))
                {
                    items.Push_last(obj);
                }
            obj = Next(obj);        // Get next object in display list
        }
    }
}

//
// Construct list of sticky items of the specified type in display-list order.
// stickyType may be any valid sticky type, or -1 for all sticky objects.
//
void 
Pad::Find_with_sticky(int stickyType, Pad_List &items, Pad_Bool groups)
{
    Pad_Object *obj;
    Pad_Iterator oi;

    items.Make_empty();
    DOLIST(oi, view->stickyObjects, Pad_Object, obj) {
        if (((stickyType == -1) || (stickyType == obj->Get_sticky())) &&
            (groups || !obj->group) &&
            (obj->Is_findable()))
            {
                items.Push_last(obj);
            }
    }
}

//
// Selects the item just after (above) the one given by tagOrId in the display list.  If
// tagOrId denotes more than one item, then the first (lowest) of these items in the display
// list is used.
//
Pad_Object *
Pad::Find_above(char *tagorid, Pad_Bool groups)
{
    Pad_Object *obj;
    Pad_Object *above = NULL;
    Pad_List objs;

    Find_with_tagorid(tagorid, objs, groups);
    obj = (Pad_Object *)objs.Last();
    if (obj) {
        do {
            above = Next(obj);
            obj = above;
        } while (above && above->Is_findable());
    }

    return(above);
}

//
// Selects the item just before (below) the one given by tagOrId in the display list.  If
// tagOrId denotes more than one item, then the first (lowest) of these items in the display
// list is used.
//
Pad_Object *
Pad::Find_below(char *tagorid, Pad_Bool groups)
{
    Pad_Object *obj;
    Pad_Object *below = NULL;
    Pad_List objs;

    Find_with_tagorid(tagorid, objs, groups);
    obj = (Pad_Object *)objs.First();
    if (obj) {
        do {
            below = Prev(obj);
            obj = below;
        } while (below && below->Is_findable());
    }

    return(below);
}

//
// Selects the item closest to the point given by event.  If more than one item is at the
// same closest distance (e.g. two items  overlap the point), then the top-most of these
// items (the last one in the display list) is used.  If halo is specified, then it must be
// a non-negative value.  Any item closer than halo to the point is considered to overlap
// it.  
//
// The start argument may be used to step circularly through all the closest items.
// If start is specified, it names an item using a tag or id (if by tag, it selects the
// first item in the display list with the given tag). Instead of selecting the top-most
// closest item, this form will select the topmost  closest item that is below start in
// the display list; if no such item exists, then the selection behaves as if the start
// argument had not been specified.
//
Pad_Object *
Pad::Find_closest(Pad_Event *event, float halo, char *start_tagorid, Pad_Bool groups)
{
    Pad_Object *obj;
    Pad_Object *last;
    Pad_Object *closestObj = NULL;
    Pad_List objs;
    Pad_Point pt;
    float mag;

                                // Find the item to end the search with
                                // (Tk's notation of start is messed up).
    last = Last();
    if (start_tagorid) {
        Find_with_tagorid(start_tagorid, objs, groups);
        obj = (Pad_Object *)objs.First();
        if (obj) {
            last = obj;
        }
    }

    obj = First();
    while (obj) {
                                // Check to see if at end of list.
        if (obj == last) {
            if (closestObj) {
                break;
            } else {
                if (last == Last()) {
                    break;
                } else {
                    last = Last();
                }
            }
        }
        if ((groups || !obj->group) && obj->Is_findable())
          {
              pt = event->pt;
              mag = event->mag;
              obj->transform.Invert(event);
              if (obj->Pick(event, halo)) {
                  closestObj = obj;
              }
              event->pt = pt;
              event->mag = mag;
          }
        obj = Next(obj);        // Get next object in display list
    }

    return(closestObj);
}
//
// Front end to calling this Pad's find command function.
//    Return value of TRUE, means no errors; FALSE, means error.  The actual answer is 
//    returned in the Pad_List
//
Pad_Bool
Pad::Find_eval(Pad_List &list, ClientData clientData, Pad_Bool groupMembers, int matchType, int argc, char** argv )
{
    Pad_Bool  ans=FALSE;

    ans = find.Eval(this, list, clientData, groupMembers, matchType, argc, argv);
    return (ans);

}

//
// Construct list of items completely enclosed by the specified bounding box in display-list order.
// This doesn't return the pad surface.
//
void 
Pad::Find_enclosed(float x1, float y1, float x2, float y2, Pad_List &items, Pad_Bool groups)
{
    float bb[4];
    Pad_Object *obj;

                                // Normalize input bbox
    if (x1 > x2) {
        Pad_Swap(x1, x2);
    }
    if (y1 > y2) {
        Pad_Swap(y1, y2);
    }
        
    items.Make_empty();
    obj = First();
    while (obj) {
        if ((groups || !obj->group) && 
            obj->Is_findable())
          {
              obj->Get_global_bbox(bb);
              
              if ((bb[XMIN] >= x1) &&
                  (bb[YMIN] >= y1) &&
                  (bb[XMAX] <= x2) &&
                  (bb[YMAX] <= y2)) 
                {
                    items.Push_last(obj);
                }
          }
        obj = Next(obj);        // Get next object in display list
    }
}

//
// Construct list of items overlapping the specified bounding box in display-list order.
// This doesn't return the pad surface.
//
void 
Pad::Find_overlapping(float x1, float y1, float x2, float y2, Pad_List &items, Pad_Bool groups)
{
    float bb[4];
    Pad_Object *obj;

                                // Normalize input bbox
    if (x1 > x2) {
        Pad_Swap(x1, x2);
    }
    if (y1 > y2) {
        Pad_Swap(y1, y2);
    }

    items.Make_empty();
    obj = First();
    while (obj) {
        if ((groups || !obj->group) && 
            obj->Is_findable())
          {
              obj->Get_global_bbox(bb);
              
              if ((bb[XMIN] <= x2) &&
                  (bb[YMIN] <= y2) &&
                  (bb[XMAX] >= x1) &&
                  (bb[YMAX] >= y1)) 
                {
                    items.Push_last(obj);
                }
          }
        obj = Next(obj);        // Get next object in display list
    }
}

//
// Set the current modifier.
// Return TRUE if successful, or FALSE otherwise.
//
Pad_Bool 
Pad::Set_modifier(const char *modifier)
{
    if (modifier[0] == '\0') {
        mode = 0;
        return(TRUE);
    } else {
        mode = Pad_GetBindingModifier(modifier);
        if (mode) {
            return(TRUE);
        } else {
            return(FALSE);
        }
    }
}

//
// Return the current modifier
//
const char *
Pad::Get_modifier(void)
{
    return(Pad_GetBindingModifier(mode));
}

//
// Convenient routine for creating pad object instances.  This does all
// normal object types except the user defined type.
//
Pad_Object * 
Pad::Create_object(int type)
{
    Pad_Object *obj = NULL;

    switch (type) {
    case PAD_ALIAS:
        obj = new Pad_Alias(this);
        break;
    case PAD_BUTTON:
        obj = new Pad_Button(this);
        break;
    case PAD_CHECKBOX:
        obj = new Pad_Checkbox(this);
        break;
    case PAD_MENUITEM:
        obj = new Pad_MenuItem(this);
        break;
    case PAD_CHECKBOXMENUITEM:
        obj = new Pad_CheckboxMenuItem(this);
        break;
    case PAD_MENU:
        obj = new Pad_Menu(this);
        break;
    case PAD_CHOICEMENU:
        obj = new Pad_ChoiceMenu(this);
        break;
    case PAD_MENUBAR:
        obj = new Pad_MenuBar(this);
        break;
    case PAD_CANVAS:
        obj = new Pad_Canvas(this);
        break;
    case PAD_CONTAINER:
        obj = new Pad_Container(this);
        break;
    case PAD_FRAME:
        obj = new Pad_Frame(this);
        break;
    case PAD_GROUP:
        obj = new Pad_Group(this);
        break;
    case PAD_IMAGE:
        obj = new Pad_Image(this);
        break;
    case PAD_LABEL:
        obj = new Pad_Label(this);
        break;
    case PAD_LINE:
        obj = new Pad_Line(this);
        break;
    case PAD_PANEL:
        obj = new Pad_Panel(this);
        break;
    case PAD_OVAL:
        obj = new Pad_Oval(this);
        break;
    case PAD_POLYGON:
        obj = new Pad_Polygon(this);
        break;
    case PAD_PORTAL:
        obj = new Pad_Portal(this);
        break;
    case PAD_RECTANGLE:
        obj = new Pad_Rectangle(this);
        break;
    case PAD_SCROLLBAR:
        obj = new Pad_Scrollbar(this);
        break;
    case PAD_SPLINE:
        obj = new Pad_Spline(this);
        break;
    case PAD_TEXT:
        obj = new Pad_Text(this);
        break;
    case PAD_TEXTFILE:
        obj = new Pad_TextFile(this);
        break;
    case PAD_TEXTFIELD:
        obj = new Pad_TextField(this);
        break;
    case PAD_TEXTAREA:
        obj = new Pad_TextArea(this);
        break;
    case PAD_WINDOW:
        obj = new Pad_Window(this);
        break;
    default:
        obj = NULL;
        break;
    }

    return(obj);
}


//////////////////////////////////////////////////////////////
//
// Definitions of Pad default event handlers
//
//////////////////////////////////////////////////////////////

                                // Variables to control zooming of frames
static float zoomSpeed = 1.05;
static float zoomAmt = 1;
static float zoomX=0, zoomY=0;     // Zooming center
static float panX=0, panY=0;       // Panning point
static Pad_TimerToken zoomTimer = NULL;

//
// Call this once to define event handlers for all Pads
//
void
Pad::Init_events(Pad_Bool status)
{
    Pad_Callback *callback;

                                // Define a special event modifier called "Run".
                                // This modifier is used for the default event handlers
                                // for all widgets.
    Pad_AddBindingModifier("Run");

                                // If turning events on
    if (status) {
                                // If already initialized for this window, then return.
                                // Else, initalize
        if (defaultEvents) {
            return;
        }
        defaultEvents = TRUE;


                                // Pan on Button 1
        callback = new Pad_Callback(Pan_press, view->win);
        Pad_CreateBinding(view->win->bindingTable, Pad_GetUid("all"), "<ButtonPress-1>", callback);
        
        callback = new Pad_Callback(Pan_drag, view->win);
        Pad_CreateBinding(view->win->bindingTable, Pad_GetUid("all"), "<B1-Motion>", callback);
        
        callback = new Pad_Callback(Pan_release, view->win);
        Pad_CreateBinding(view->win->bindingTable, Pad_GetUid("all"), "<ButtonRelease-1>", callback);

                                // Zoom in on Button 2
        callback = new Pad_Callback(Zoom_in_press, view->win);
        Pad_CreateBinding(view->win->bindingTable, Pad_GetUid("all"), "<ButtonPress-2>", callback);
        
        callback = new Pad_Callback(Zoom_in_drag, view->win);
        Pad_CreateBinding(view->win->bindingTable, Pad_GetUid("all"), "<B2-Motion>", callback);
        
        callback = new Pad_Callback(Zoom_in_release, view->win);
        Pad_CreateBinding(view->win->bindingTable, Pad_GetUid("all"), "<ButtonRelease-2>", callback);

                                // Zoom out on Button 3
        callback = new Pad_Callback(Zoom_out_press, view->win);
        Pad_CreateBinding(view->win->bindingTable, Pad_GetUid("all"), "<ButtonPress-3>", callback);
        
        callback = new Pad_Callback(Zoom_out_drag, view->win);
        Pad_CreateBinding(view->win->bindingTable, Pad_GetUid("all"), "<B3-Motion>", callback);
        
        callback = new Pad_Callback(Zoom_out_release, view->win);
        Pad_CreateBinding(view->win->bindingTable, Pad_GetUid("all"), "<ButtonRelease-3>", callback);
    } else {
                                // Turning events off
                                // If not already initialized for this window, then return.
                                // Else, unitialize
        if (!defaultEvents) {
            return;
        }
        defaultEvents = FALSE;

        Pad_DeleteBinding(view->win->bindingTable, Pad_GetUid("all"), "<ButtonPress-1>");
        Pad_DeleteBinding(view->win->bindingTable, Pad_GetUid("all"), "<B1-Motion>");
        Pad_DeleteBinding(view->win->bindingTable, Pad_GetUid("all"), "<ButtonRelease-1>");
        Pad_DeleteBinding(view->win->bindingTable, Pad_GetUid("all"), "<ButtonPress-2>");
        Pad_DeleteBinding(view->win->bindingTable, Pad_GetUid("all"), "<B2-Motion>");
        Pad_DeleteBinding(view->win->bindingTable, Pad_GetUid("all"), "<ButtonRelease-2>");
        Pad_DeleteBinding(view->win->bindingTable, Pad_GetUid("all"), "<ButtonPress-3>");
        Pad_DeleteBinding(view->win->bindingTable, Pad_GetUid("all"), "<B3-Motion>");
        Pad_DeleteBinding(view->win->bindingTable, Pad_GetUid("all"), "<ButtonRelease-3>");
    }
}

//
// Timer callback to actually do the zooming
//
static void
Zoom_callback(void *clientData) {
    Pad_Win *win = (Pad_Win *)clientData;

    win->view->Zoom_around(zoomX, zoomY, zoomAmt);
    zoomTimer = Pad_CreateTimerHandler(10, Zoom_callback, win);
}

//
// Event handlers for panning
//
int
Pad::Pan_press(Pad_Object *, ClientData, Pad_Event *padEvent)
{
    panX = padEvent->pt.x;
    panY = padEvent->pt.y;

    return(PAD_OK);
}

int
Pad::Pan_drag(Pad_Object *, ClientData clientData, Pad_Event *padEvent)
{
    float dx, dy;
    Pad_Win *win = (Pad_Win *)clientData;

    dx = padEvent->pt.x - panX;
    dy = padEvent->pt.y - panY;
    win->view->Set_view(win->view->xview - dx, win->view->yview - dy, win->view->zoom, FALSE);

    return(PAD_OK);
}

int
Pad::Pan_release(Pad_Object *, ClientData, Pad_Event *)
{
    return(PAD_OK);
}

//
// Event handlers for zooming in
//
int
Pad::Zoom_in_press(Pad_Object *, ClientData clientData, Pad_Event *padEvent)
{
    Pad_Win *win = (Pad_Win *)clientData;

    zoomAmt = zoomSpeed;
    zoomX = padEvent->pt.x;
    zoomY = padEvent->pt.y;
    Zoom_callback(win);

    return(PAD_OK);
}

int
Pad::Zoom_in_drag(Pad_Object *, ClientData, Pad_Event *padEvent)
{
    zoomX = padEvent->pt.x;
    zoomY = padEvent->pt.y;

    return(PAD_OK);
}

int
Pad::Zoom_in_release(Pad_Object *, ClientData, Pad_Event *)
{
    Pad_DeleteTimerHandler(zoomTimer);

    return(PAD_OK);
}

//
// Event handlers for zooming out
//
int
Pad::Zoom_out_press(Pad_Object *, ClientData clientData, Pad_Event *padEvent)
{
    Pad_Win *win = (Pad_Win *)clientData;

    zoomAmt = 1.0 / zoomSpeed;
    zoomX = padEvent->pt.x;
    zoomY = padEvent->pt.y;
    Zoom_callback(win);

    return(PAD_OK);
}

int
Pad::Zoom_out_drag(Pad_Object *, ClientData, Pad_Event *padEvent)
{
    zoomX = padEvent->pt.x;
    zoomY = padEvent->pt.y;

    return(PAD_OK);
}

int
Pad::Zoom_out_release(Pad_Object *, ClientData, Pad_Event *)
{
    Pad_DeleteTimerHandler(zoomTimer);

    return(PAD_OK);
}

/*
//////////////////////////////////////////////
//              Pad_FindTreeNode definitions
//////////////////////////////////////////////  
//
// A Pad_FindTreeNode is a node on an expression tree.
// The content of a Tree node is either a find command with it's arguments.
// (The arguments can be of a pad class, plain string, float, or intereger.
// Basicly anything depending on what find allows for commands).  Or the
// content of a node will be just an expresion operation.  Which can be a && or ||.
// So the following would make a 3 node tree:
//
// .pad find withtag foo && !withinfo bar
//
//                     && node
//                      /  \
//               withtag    withinfo (contains not flag)
//               node       node
//
// notice that the not (!) is not a node.  It is kept inside of the node that
// it operates on.  for:
//
// .pad find withtag foo && !(withinfo bar || withlayer main)
//
//  the operation node for || contains the not flag
*/


// generic constructor
Pad_FindTreeNode::Pad_FindTreeNode()
{
    
    _pad = NULL;
    _operation = -1;
    _left = NULL;
    _right = NULL;
    _matchType = PAD_FIND_EXACT;
    _idOrSticky = 0;
    _uid = NULL;
    _string = NULL;
    _regExpToken = NULL;
    _event  = NULL;
    _x1 = 0;
    _y1 = 0;
    _x2 = 0;
    _y2 = 0;
    _not = FALSE;
    _passedObject = FALSE;
}


// operation constructor
Pad_FindTreeNode::Pad_FindTreeNode(Pad_FindTreeNode *left, Pad_FindTreeNode *right, int operation)
{
    
    _operation = operation;
    _left = left;
    _right = right;

    _pad = NULL;
    _matchType = PAD_FIND_EXACT;
    _idOrSticky = 0;
    _uid = NULL;
    _string = NULL;
    _regExpToken = NULL;
    _event  = NULL;
    _x1 = 0;
    _y1 = 0;
    _x2 = 0;
    _y2 = 0;
    _not = FALSE;
    _noShortCircuit = FALSE;
    _passedObject = FALSE;
}

// find command constructor
Pad_FindTreeNode::Pad_FindTreeNode(Pad *pad, int command, int matchType, int idOrSticky, Pad_Uid uid, char *string, Pad_RegExp regExpToken, Pad_Event *event, float x1, float y1, float x2, float y2 )
{
    _pad = pad;
    _operation = command;
    _matchType = matchType;
    _regExpToken = regExpToken;
    _idOrSticky = idOrSticky;
    _uid = uid;
    _string = string;
    _event  = event;
    _x1 = x1;
    _y1 = y1;
    _x2 = x2;
    _y2 = y2;

    _not = FALSE;
    _passedObject = FALSE;
    _noShortCircuit = FALSE;
    _left = NULL;
    _right = NULL;
}

Pad_FindTreeNode::~Pad_FindTreeNode()
{
    delete _right;
    delete _left;
    delete _event;
    _left = NULL;
    _right = NULL;
    _event = NULL;
}

//
//  Evaluate the tree node.  If the find tree node an expresion opperation (&&, ||), 
//  evaluate the left child, then the right, and then either AND or OR
//  the result.  If the _noShortCircuit flag is true, then short circuit evaluation 
//  will NOT be used.  If the node is a find command, then evaluate that command
//  using the arguments that are in this object.  If the node has a "not" associated
//  with it, take the compliment of the node's evaluatation answer.
//  
//
//        Return codes are as follows:    -1 : error
//                                         0  : False
//                                         1  : True
//

int
Pad_FindTreeNode::Evaluate(Pad_Object *obj)
{

    const char *info;
    const char *layerName;
    char *typeName;
    float bb[4];    
    float mag;
    Pad_Point pt;
    int ans=FALSE;;
    Pad_Bool ansLeft;
    Pad_Bool ansRight;
    Pad_Bool strToStrCmp=FALSE;                     // true if there a string to string comparison
    Pad_Uid tag = NULL;
    Pad_List objs;
    Pad_String string;
    Pad_Iterator   ti;
  
    switch (_operation) {
        case PAD_FIND_AND:  
            if (!_noShortCircuit) {
              ans = (_left->Evaluate(obj) && _right->Evaluate(obj));
            } else {
              ansLeft  = _left->Evaluate(obj);
              ansRight = _right->Evaluate(obj);
              ans = ansLeft && ansRight;
            }
            break;
        case PAD_FIND_OR:   
            if (!_noShortCircuit) {
              ans = (_left->Evaluate(obj) || _right->Evaluate(obj));
            } else {
              ansLeft  = _left->Evaluate(obj);
              ansRight = _right->Evaluate(obj);
              ans = ansLeft || ansRight;
            }
            break;
        case PAD_SEARCH_ALL:
            ans =  TRUE;
              break;
        case PAD_SEARCH_ABOVE:
            if (_idOrSticky == -1) {                                      // id was never set
                ans = FALSE;
            } else if (_passedObject) {
                ans = TRUE;
            } else {
                if (obj->id == _idOrSticky) {
                    _passedObject = TRUE;
                }
                ans = FALSE;
            }
              break;
        case PAD_SEARCH_BELOW:
            if (_idOrSticky == -1) {                                     // id was never set
                ans = FALSE;
            } else if ( _passedObject) {
                ans = FALSE;
            } else {
                if (obj->id == _idOrSticky) {
                    _passedObject = TRUE;
                    ans = FALSE;
                 } else {
                    ans = TRUE;
                 }
            }
              break;
        case PAD_SEARCH_CLOSEST:
              pt = _event->pt;
              mag = _event->mag;
              obj->transform.Invert(_event);
              if (obj->Pick(_event, _x2)) {
                  ans=TRUE;
              } else {
                  ans=FALSE;
              }
              _event->pt = pt;
              _event->mag = mag;

              break;
        case PAD_SEARCH_WITHTAG:
            if (_idOrSticky != -1) { 
                ans =  (obj->id == _idOrSticky);
            } else if (_matchType == PAD_FIND_EXACT) {                     // exact expression 
                ans =  (obj->tags.Member(_uid));
            } else  {                                                      // regular/glob expression
                DOLISTI(ti, obj->tags, Pad_Uid, tag) {
                    if (_matchType == PAD_FIND_REGEXP) {                   // regular expression
                        ans = Pad_RegExpExec(_regExpToken, tag);
                        if (ans == -1) {
                            Pad_errorString = _string;
                            Pad_errorString += ": ";
                            //Pad_errorString += interp->result;
                            return(-1);
                        }
                    } else {                                               // glob expression
                        if (_string == NULL || tag == NULL) {
                            Pad_errorString = "INTERNAL ERROR, Please Contact Developers with this code: FC1 ";
                            return(-1);
                        }
                        ans = Pad_StringMatch(tag, _string);
                    }
                    if (ans) {                                            // have ans, break out of tag list loop
                        break;
                    }
                }
            } 
              break;
        case PAD_SEARCH_WITHTEXT:
            if (obj->Get_text(string)) {
                strToStrCmp = TRUE;
            } else {
                ans = FALSE;
            }
              break;
        case PAD_SEARCH_WITHNAME:
            if (obj->Get_name(string)) {
                strToStrCmp = TRUE;
            } else {
                ans = FALSE;
            }
            break;
        case PAD_SEARCH_WITHTYPE:
            typeName = obj->Type_name();
            if (typeName) {
                strToStrCmp = TRUE;
                string = typeName;
            } else {
                ans = FALSE;
            }
            break;
        case PAD_SEARCH_WITHINFO:
            info = obj->Get_info();
            if (info) {
                strToStrCmp = TRUE;
                string = info;
            } else {
                ans = FALSE;
            }
              break;
        case PAD_SEARCH_WITHLAYER:
            layerName = obj->Get_layer();
            if (layerName){
                strToStrCmp = TRUE;
                string = layerName;
            } else {
                ans = FALSE;
            }
              break;
        case PAD_SEARCH_WITHSTICKY:
            ans =  (_idOrSticky == -1) || (_idOrSticky == obj->Get_sticky());
              break;
        case PAD_SEARCH_ENCLOSED:
            obj->Get_global_bbox(bb);
            ans = (bb[XMIN] >= _x1) && (bb[YMIN] >= _y1) &&
                  (bb[XMAX] <= _x2) && (bb[YMAX] <= _y2);
              break;
        case PAD_SEARCH_OVERLAPPING:
            obj->Get_global_bbox(bb);
            ans = (bb[XMIN] <= _x2) && (bb[YMIN] <= _y2) &&
                  (bb[XMAX] >= _x1) && (bb[YMAX] >= _y1);
              break;
        default: 
            Pad_errorString = "INTERNAL ERROR, Please Contact Developers with this code: FC3 ";
            return(-1);
    }
    if (strToStrCmp) {
        if (_matchType == PAD_FIND_EXACT) {                        // exact expression 
            if (!strcmp(string.Get(),_string)){
                ans = TRUE;
            } else {
                ans = FALSE;
            }
        } else if (_matchType == PAD_FIND_REGEXP) {                // regular expression
              ans = Pad_RegExpExec(_regExpToken, string.Get());
              if (ans == -1) {
                  Pad_errorString = _string;
                  Pad_errorString += ": ";
                  //Pad_errorString += interp->result;
                  return(-1);
              }
        } else {                                                   // glob expression
            ans = Pad_StringMatch(string.Get(), _string);
        }
    }

    if (_not) {
       return(!ans);
    } else {
       return(ans);
    }
}

//
// Print the given node.  Ie) print the tree
//


void
Pad_FindTreeNode::Print()
{
  
    if (_not) {
      cerr << "!";
    }
    if (_left != NULL) {
        cerr << "(";
        _left->Print();
        cerr << ")";
    }
    switch (_operation) {
        case PAD_FIND_AND:  
          cerr << " && ";
          break;
        case PAD_FIND_OR:   
          cerr << " || ";
          break;
        case PAD_SEARCH_ALL:
          cerr << " all ";
          break;
        case PAD_SEARCH_ABOVE:
          cerr << " above " << _idOrSticky;
          break;
        case PAD_SEARCH_BELOW:
          cerr << " below " << _idOrSticky;
          break;
        case PAD_SEARCH_CLOSEST:
          cerr << " closest " <<  _x1 << " " << _y1 << " " << _x2 << " " << _idOrSticky;
          break;
        case PAD_SEARCH_WITHTAG:
          cerr << " withtag " << _idOrSticky;
          break;
        case PAD_SEARCH_WITHTEXT:
          cerr << " withtext " << _string;
          break;
        case PAD_SEARCH_WITHNAME:
          cerr << " withname " << _string;
          break;
        case PAD_SEARCH_WITHTYPE:
          cerr << " withtype ";
          break;
        case PAD_SEARCH_WITHINFO:
          cerr << " withinfo " << _string;
          break;
        case PAD_SEARCH_WITHLAYER:
          cerr << " withlayer ";
          break;
        case PAD_SEARCH_WITHSTICKY:
          cerr << " withsticky " << _idOrSticky;;
          break;
        case PAD_SEARCH_ENCLOSED:
          cerr << " enclosed " << _x1 << " " << _y1 << " " << _x2 << " " << _y2;
          break;
        case PAD_SEARCH_OVERLAPPING:
          cerr << " overlapping " << _x1 << " " << _y1 << " " << _x2 << " " << _y2;
          break;
        default: cerr << "Switch fell through:" << _operation << endl;
    }
    
    if (_right != NULL) {
        cerr << "(";
        _right->Print();
        cerr << ")";
    }
}

void _Pad_Print_argv(int argc, char **argv)
{
  for (int i=0; i < argc; i++) {
    cerr << "argv[" << i <<"]: +" << argv[i] <<"+"<< endl;
  }
}


//////////////////////////////////////////////
//              Pad_Find definitions
//////////////////////////////////////////////  
//
//  This is the find command.  The Pad_Find object holds the find expression tree.
//  It creates this every time Pad_Find::Eval is called.  Eval takes the arguments
//  that it needs and goes off and parses the data, builds the find expression tree,
//  evaluates that tree and then deletes that tree.  It returns the answer (the id of
//  objects found) in the list that was passed to the Eval function.
//  The reason for almost all of the private variable are just temporary, used as the
//  commands are being parsed.  After the command is parsed, it creates a new
//  pad_findTreeNode object that contains this data.
//
//


// contructor
Pad_Find::Pad_Find()
{
    _pad=NULL;
    _matchType = PAD_FIND_EXACT;
    _noShortCircuit = FALSE;
    _expr = NULL;
    _exactWithtag = FALSE;
    _commandCnt = 0;
    _tkwin = NULL;
    _idOrSticky = 0;
    _uid = 0; 
    _string = NULL;
    _event = NULL;
    _x1 = 0;
    _y1 = 0;
    _x2 = 0;
    _y2 = 0;
}

// contructor
Pad_Find::Pad_Find(Pad *pad)
{
    _pad = pad;
    _matchType = PAD_FIND_EXACT;
    _noShortCircuit = FALSE;
    _expr = NULL;
    _exactWithtag = FALSE;
    _commandCnt = 0;
    _tkwin = NULL;
    _idOrSticky = 0;
    _uid = 0; 
    _string = NULL;
    _event = NULL;
    _x1 = 0;
    _y1 = 0;
    _x2 = 0;
    _y2 = 0;
}

// destructor
Pad_Find::~Pad_Find()
{
    delete _expr;                                // delete the expr tree, if there is one
    _expr = NULL;
}

//
// Eval
//
// This is the front end to Pad_Find class.  Given argument are parsed and then evalated.
// The parsing involves building a tree of Pad_FindTreeNodes and then evaluating that every
// object on the pad surface against it.  Pad_Objects that make the tree's evaluation return
// true are added to the given Pad_List.
//
// If it is found that the given command is just a search for id's associated with a tag,
// a "short cut" will be used,  It will not use the above technique, instead it will use 
// the Tag hash table.
//
// Expected Data:
//
//     It is expected that Eval will be passed an array of string pointers and a count of how
//     many pointers are in this array, argv and argc.  &, |, !, ( and ) are all "reservered"
//     find commands.  They are treated special by the parser.  If you wish to have these
//     as an arguement to one of the find commands and not be treated specail you must escape
//     it with a black slash. No quotes are used, which is why you must escape everything.
//
//     if you wanted: to seach for this text "Unix! && Windows"
//
//     From Tcl To Pad you enter:  .pad find withtext "Unix\\! \\&\\& Windows\\!"
//
//     Pad_Find wants:  argv[0] = withtext
//                      argv[1] = Unix\! \&\& Windows\!
//    
//     This is needed to be done, because TCL will not pass the quotes.  This is a solution
//     chose for this problem.  (See Pad_Find::Get_tokens.
//
//
// a return value of FALSE means that there was an error that can be found in Pad_errorString.
// a return value of TRUE  means that there where NO errors and the result of the find coomand
// can be found in the given Pad_List.
//

Pad_Bool
Pad_Find::Eval(Pad *pad, Pad_List &list, ClientData clientData, Pad_Bool groupMembers, int matchType, int argc, char** argv )
{
    Pad_Object    *obj;
    Pad_List       objs;
    Pad_List      *tagObjs;
    Pad_Uid        *uid;
    Pad_Iterator   oi;

    int            tokenc = 0;
    char         **tokenv = NULL;
    int            ans = 0;
    Pad_Bool       returnAns;

    _pad           = pad;
    _tkwin         = (Pad_TkWin *)clientData;
    _exactWithtag  = FALSE;                                                  // assume this to start
    _commandCnt    = 0;                                                      // set the number of commands to 0

    if (argc == 0 ) {
        return (FALSE); 
    }

    if ((matchType != PAD_FIND_REGEXP) && (matchType != PAD_FIND_GLOB)) {
        _matchType = PAD_FIND_EXACT;                                         // default
    } else {
        _matchType = matchType;
    }

    tokenv = _Get_tokens(argc, argv, tokenc);                                // put raw command into a more parsable
                                                                             // format
    if (tokenv) {
        _expr = _Parse(tokenc, tokenv);
    } else {
        _expr = NULL;
    }

    if (_expr) {
        //if (_exactWithtag && (_commandCnt == 1)) {                            // use hash table to get answer-quicker
        if (0) {                            // use hash table to get answer-quicker
            //if (_idOrSticky != -1) { 
                //list.Push_last(obj);
            //} else {
            
            uid = _expr->Get_uid();
            tagObjs = (Pad_List *)_pad->tagTable->Get((void *) *uid);
            if (tagObjs) {
                objs = tagObjs;
            }
            DOLIST(oi, objs, Pad_Object, obj) {
                if (groupMembers || !obj->group) {
                    list.Push_last(obj);
                }
            }
            returnAns = TRUE;
        } else {                                                           // go through each pad object-slower
            list.Make_empty();
            obj = _pad->First();
            while (obj) {
                if ((obj->group  && !groupMembers) || !obj->Is_findable()) {
                    obj = _pad->Next(obj);
                } else {
                    ans = _expr->Evaluate(obj);
                    if (ans == -1) {
                        obj = NULL;
                    } else {
                        if (ans) {
                            list.Push_last(obj);
                        }
                        obj = _pad->Next(obj);
                    }
                }
            }
            if (ans == -1) {                                                      // error
                returnAns = FALSE;
            } else {
                returnAns = TRUE;
            }
        }
    } else {                                                                      // error (_expr = NULL)
        returnAns = FALSE;
    } 


    delete _expr;                                                                 // delete the tree
    _expr = NULL;
    if (tokenv) {
        for (int i=0; i < tokenc; i++) {
            delete tokenv[i];
        }
        delete [] tokenv;                                                        // delete the token list
    }

    return (returnAns);
}

//
// Get_tokens.
//
// Given a list of "raw" commands, argv.  Parse through it and return a list of tokens, tokenv.
// Convert argv to tokens.  Any character will be taken literaly if preceeded by a \.
// The tokens are: !, &, |, (, and ).  These tokens will have special meaning.
//
// example:   argv[0] = "(!withtag"    becomes   tokenv[0] = "("
//            argv[1] = "foo\))"                 tokenv[1] = "!"
//                                               tokenv[2] = "withtag"
//                                               tokenv[3] = "foo)"
//                                               tokenv[4] = ")"
//
//

char **
Pad_Find::_Get_tokens(int argc, char **argv, int &tokenc)
{
    char     **tokenv = NULL;
    Pad_String string;
    int        size = 0;
    int        s    = 0;
    int        c    = 0;
    Pad_Bool   newToken = FALSE;
    int        tokenLength = FALSE;
    char       cur = ' ';
    char       prev = ' ';

   
    // Find out how many tokens.

    size = 0;
    for (s=0; s < argc; s++) {
        if (argv[s] == NULL) {
            Pad_errorString = "INTERNAL ERROR, Please Contact Developers with this code: GT1";
            return(NULL);
        }
        c = 0;
        prev = cur;
        cur = argv[s][c];
        while (cur != '\0') {
            switch (cur) {
              case '(':
              case ')':
              case '!':
                if (prev != '\\') {
                    if (tokenLength != 0) {                  // have we started a token yet?  If so it stopped
                        size++;                              // now, so count it
                    }
                    newToken = TRUE;                         // and cout this new token too
                    tokenLength = 1; 
                } else {
                    tokenLength++; 
                }
                break;
              case '&':
              case '|':
                if (prev == cur) {
                    if (tokenLength > 1) {                   // have we started a token yet?  If so it stopped
                        size++;                              // now, so count it
                    }
                    newToken = TRUE;                         // and cout this new token too
                    tokenLength = 1; 
                } else if (prev == '\\') { 
                    tokenLength++;                           // something other than what it was, corrects for the
                    cur = ' ';                               //    "&&&" or "|||" case        
                } else {          
                    tokenLength++;
                }
                break;
             case '\\':
                if (prev == '\\') {                          // add it to the string or  ignore it
                   tokenLength++;
                } else {                                     // ignore it
                }
           
                break;
             default:
                tokenLength++;
            }
            prev = cur;
            c++;
            cur = argv[s][c];
            if (newToken || (cur == '\0')) {
                size++;
                newToken = FALSE;
                tokenLength = 0;
            }
        }
    }

    // Build the new token array.
    tokenv = new char* [size];

    tokenc = 0;
    string = "";
    c = 0;
    for (s=0; s < argc; s++) {
        if (argv[s] == NULL) {
            Pad_errorString = "INTERNAL ERROR, Please Contact Developers with this code: GT1.1";
            return(NULL);
        }
        c = 0;
        prev = cur;
        cur = argv[s][c];
        while (cur != '\0') {
            switch (cur) {
              case '(':
              case ')':
              case '!':
                if (prev != '\\') {
                    if (string.Length()!= 0) {                            // save current string
                        tokenv[tokenc] = new char[string.Length()+1];     // make the room for the new string
                        strcpy(tokenv[tokenc], string.Get());             // copy it over
                        tokenc++;
                    }
                    string = "";
                    string += cur;
                    newToken = TRUE;
                } else {
                    string += cur;
                }
                break;
              case '&':
              case '|':
                if (prev == cur) {
                    if (string.Length() > 1) {                                    // save current string
                        tokenv[tokenc] = new char[string.Length()];               // make the room for the new string
                                                                                  // dont use + 1.  We only want
                                                                                  // to include all but the last ch
                        strncpy(tokenv[tokenc], string.Get(), string.Length()-1); // copy it over (except the last ch)
                        *(tokenv[tokenc]+string.Length()-1) = '\0';               // put the delimter
                        tokenc++;                                                 // it will be &, dont want it.
                    }
                    string = "";
                    string += cur;                               // make it a && or ||
                    string += cur;
                    newToken = TRUE;
                } else if (prev == '\\') { 
                    string += cur;                               // something other than what it was, corrects for the
                    cur = ' ';                                   //    "&&&" or "|||" case        
                } else {          
                    string += cur;
                }
                break;
              case '\\':                                         //
                if (prev == '\\') {                              // add it to the string or  ignore it
                    string += cur;                               //
                } else {                                         // ignore it
                }
           
                break;
              default:
                string += cur;
            }
            prev = cur;
            c++;
            cur = argv[s][c];
            if (newToken || (cur == '\0')) {
                newToken = FALSE;
                tokenv[tokenc] = new char[string.Length()+1];     // make the room for the new string
                strcpy(tokenv[tokenc], string.Get());           // copy it over
                tokenc++;
                string="";                                      // reset the string
            }
        }
    }
    if (tokenc != size) {
        Pad_errorString = "INTERNAL ERROR, Please Contact Developers with this code: GT1.2";
        return(NULL);
    }
    //_Pad_Print_argv(argc, argv);
    //_Pad_Print_argv(tokenc, tokenv);
    return(tokenv);
}

//
//  Parse the find command given in tokenv and tokenc.
//
//  This is a recursive function that calls itself when a ( is found.
//    When this occurs, the distance between the ( and the ) is sent as the tokenc.
//  When A command element is found (withtag foo), _Passe_command is called to parse
//    that command element.
//
//  A return value of NULL means that there was an error.  Other wise a pointer to the
//  tree is returned.
//

Pad_FindTreeNode *
Pad_Find::_Parse(int tokenc, char **tokenv)
{
    int i=0;                                               
    int range=0;
    Pad_Bool nicht=FALSE;                                   
    char   *next;
    Pad_FindTreeNode *leftExp, *rightExp, *oper;

   
    if (!strcmp(tokenv[0], "!")) {
        nicht = TRUE;
        i++;
    } 
    if (!strcmp(tokenv[i],"(")) {
        i++;                                                              // skip the (
        range     = _Compute_range(tokenc-i, tokenv+i);
        if (range == -1) {
            return(NULL);
        }
        leftExp = _Parse(range, tokenv+i);                               // send it the sublist
        i  = i + range;                                                  // get past the "find command"
        i++;                                                             // skip the )
    } else {
        leftExp = _Parse_command(tokenc-i, tokenv+i, range);
        i  = i + range;                                                  // get past the find element command
    }
    if (leftExp == NULL) {
        return(NULL);
    }
    if (nicht) {
        leftExp->Set_not();
        nicht = FALSE;
    }
 
    oper = leftExp;                                                      // set oper in case we never go in the loop
   
    while ( i < tokenc) {
        next = tokenv[i];
        i++;
        if (i >= tokenc) {
           Pad_errorString = "unknown searchCommand: "; 
           Pad_errorString += next;
           Pad_errorString += "\n try all, above, below, withinfo, withlayer, withname, withsticky, withtag, withtext, withtype, or enclosed, overlapping, ";
            delete leftExp;
            leftExp = NULL;
           return(NULL);
        }
        if (!strcmp(tokenv[i], "!")) {
            nicht = TRUE;
            i++;
        } 
        if (!strcmp(tokenv[i],"(")) {
            i++;                                                         // skip the (
            range   = _Compute_range(tokenc-i, tokenv+i);
            if (range == -1) {
                return(NULL);
            }
            rightExp = _Parse(range, tokenv+i);                          // send it the sublist
            i  = i + range;                                              // get past the "find command"
            i++;                                                         // skip the )
        } else {
           rightExp = _Parse_command(tokenc-i, tokenv+i, range);
           i  = i + range;                                               // get past the find command
        }
        if (rightExp == NULL) {                                          // error, delete the nodes alread
            delete leftExp;                                              //        created.
            leftExp = NULL;
            return(NULL);
        }
        if (nicht) {
            rightExp->Set_not();
            nicht = FALSE;
        }
        if (i > tokenc) {
            Pad_errorString = "INTERNAL ERROR, Please Contact Developers with this code: FC5";
            return(NULL);
        }

        if (!strcmp(next,"&&")) { 
            oper =  new Pad_FindTreeNode(leftExp, rightExp, PAD_FIND_AND);
        } else if (!strcmp(next,"||")) {
            oper =  new Pad_FindTreeNode(leftExp, rightExp, PAD_FIND_OR);
        } else {
            Pad_errorString = "INTERNAL ERROR, Please Contact Developers with this code: FC6";
            return(NULL);
        }
        if (oper == NULL) {
            Pad_errorString = "INTERNAL ERROR, Please Contact Developers with this code: FC6.1";
            return(NULL);
        }
        leftExp = oper;                                                  // the operator is now the left child
    }
    return(oper);
}
    
//
// Return the distance between ( and ), ignoring the parenthesis in between.
// The command sting (tokenv) is sent without the starting (.
// A return value of -1 means an error has occured
//
int
Pad_Find::_Compute_range(int tokenc, char **tokenv)
{
    int i=0,opened=0;

    while (i < tokenc) {
        if (!strcmp(tokenv[i],"(")) { 
            opened++;
        } else if (!strcmp(tokenv[i],")")) { 
            if (opened == 0) {
               return(i);
            } else {
               opened-- ;
            }
       } 
        i++;
    }
    Pad_errorString = "error: no closing parenthesis";
    return(-1);
}

//
// Given the matchType (regexp, glob, exact) and if only the first ID is wanted, set the appropiate fields.
// That is to say load the date that is dependent of the matchType and if only the first ID is wanted.
// When only one id is wanted (onlyFirstId is TRUE) and the matchType is set to exact, return the first id 
// in the display list (the lowest) with that tag.  In the case of when the mathType is not exact, 
// it will return the id of the first id in the display list with the FIRST tag to match the tag.  Ie)
//
//     foobar  ---->  2 3 4 
//     foofoo  ---->  5 8
// 
//  if the tagorid is foo* it will return 2.
//
//  in the case of when the tagorid is an interger, ie the ID.  That id is set, regardless of the matchType.
//  so that means, if the first char in tagorid is a digit, then it will be assumed that a digit should be set
//  and not to treat it as a regular expression.

Pad_Bool
Pad_Find::_Resolve_tagorid(Pad_Bool onlyFirstId, char *tagorid)
{
    int             found = FALSE;
    char           *tag;                                   // the current tag when going through the list
    char           *firstTag;                              // used to keep track of which tag is smaller (what strcmp thinks)
    Pad_List        objs;
    Pad_Object      *obj;
    Pad_HashSearch  hashSearchPtr;                         // hash search ptr (see man Tcl_FirstHashEntry)
    Pad_List   *next;                                 // next entry in the hash table

    if (isdigit(tagorid[0])) { 
        _idOrSticky = atoi(tagorid);
        return(TRUE);
    } 
    if (_matchType == PAD_FIND_REGEXP) {                   // need to do no matter if onlyFirstId or not
        _regExpToken = Pad_RegExpCompile(tagorid);
        if (_regExpToken == NULL) {
            //Pad_errorString = _interp->result;
            return (FALSE);
        }
            
    }
    if (!onlyFirstId) {
        switch (_matchType) {
          case PAD_FIND_EXACT: 
            _uid = Pad_GetUid(tagorid);
            break;
          case PAD_FIND_GLOB:
            _string = tagorid;
            break;
          case PAD_FIND_REGEXP:
            // done above
            break;
        } 
        return (TRUE);
    }
    if (onlyFirstId) {
        if (_matchType == PAD_FIND_EXACT) {
            _pad->Find_with_tagorid(tagorid, objs, TRUE);
            obj = (Pad_Object *)objs.First();
            if (obj) {
                _idOrSticky = obj->id;
            }
        } else {
            next = (Pad_List *)_pad->tagTable->Init(hashSearchPtr, (void *)tag);
            found = FALSE;
            firstTag = NULL;
            while (next) {
                if (_matchType == PAD_FIND_REGEXP) {                        // regexp
                    found = Pad_RegExpExec(_regExpToken, tag);
                    if (found == -1) {
                        //Pad_errorString += _interp->result;                 // error
                        return (FALSE);
                    }
                } else {                                                    // glob
                    found = Pad_StringMatch(tag, tagorid);
                }
                if (found) {
                    if (firstTag == NULL || (strcmp(firstTag, tag) > 0)) {  // tag is less then firstTag
                        firstTag = tag;
                    }
                }
                found = FALSE;
                next = (Pad_List *)_pad->tagTable->Init(hashSearchPtr, (void *)tag);
            }
            if (firstTag) {
                _pad->Find_with_tagorid(firstTag, objs, TRUE);
                obj = (Pad_Object *)objs.First();
                if (obj) {
                    _idOrSticky = obj->id;
                }
            }
        }
    }
    return (TRUE);
}

//
//  Parse the given find command, parse it and return the FindNode containing the given Find command.
//  If there was an error in the find command, return NULL.
//
// _Parse_command
//
// Parse the given find command.  The current find command is passed in.  So Range used to tell the
// calling function the distance to where the command stopped.  A return value of NULL means that
// there was an error.  Otherwise the Pad_FindTreeNode is returned, which contains the find command.
//
//

Pad_FindTreeNode *
Pad_Find::_Parse_command(int tokenc, char **tokenv, int &range)
{
    int             argcount = 0; 
    int             searchType;             
    Pad_FindTreeNode *node;


    _idOrSticky     = -1;
    _uid            = NULL;
    _string         = NULL;
    _event          = NULL;
    _x1             = 0;
    _y1             = 0;
    _x2             = 0;
    _y2             = 0;
    _noShortCircuit = FALSE;
    //_matchType  = _matchType;  May want each command to have a different matchType in the future

        

    while ((argcount < tokenc) && 
            strcmp(tokenv[argcount],"&&") && 
            strcmp(tokenv[argcount],"||") && 
            strcmp(tokenv[argcount],"(") ){
        argcount++;
    }

    if (!strcmp(tokenv[0], "all")) {
        if (argcount == 1) {
            searchType = PAD_SEARCH_ALL;          
        } else {
            Pad_errorString = "wrong # args: searchCommand should be \"all\"";
            return(NULL);
        }
    } else if (!strcmp(tokenv[0], "above")) {
        if (argcount == 2) {    
            _noShortCircuit = TRUE;
            searchType = PAD_SEARCH_ABOVE;
            if (!_Resolve_tagorid(TRUE, tokenv[1]))  {                          // get first id
                return (NULL);
            }
        } else {
            Pad_errorString = "wrong # args: searchCommand should be \"allabove tagorid\"";
            return(NULL);
        }
    } else if (!strcmp(tokenv[0], "below")) {
        if (argcount == 2) {
            _noShortCircuit = TRUE;
            searchType = PAD_SEARCH_BELOW;
            if (!_Resolve_tagorid(TRUE, tokenv[1]))  {                          // get first id
                return (NULL);
            }
        } else {
            Pad_errorString = "wrong # args: searchCommand should be \"allbelow tagorid\"";
            return(NULL);
        }
    } else if (!strcmp(tokenv[0], "closest")) {
        if ((argcount >= 3) && (argcount <= 4)) {
            searchType = PAD_SEARCH_CLOSEST;
            _x1 = ATOXF(_tkwin, tokenv[1]);                                     // x
            _y1 = ATOYF(_tkwin, tokenv[2]);                                     // y
            _event = new Pad_Event(_tkwin);                                     // MUST be deleted by
                                                                                // who it is sent to
            _event->Update_pad_coords(_x1,_y1);
            _event->mag = 1.0;                                                  // Search should be independent 
            _event->objMag = _event->mag;                                       // of current view
            if (argcount >= 4) {
                _x2 = ATOVALUEF(_tkwin, tokenv[3]);                             // halo
                if (_x2 < 0) {
                    Pad_errorString = "halo must be greater than zero.";
                }
            } else {
                _x2 = 0.0;                                                      // halo
            }
        } else {
            Pad_errorString = "wrong # args: searchCommand should be \"closest x y ?halo \"";
            return(NULL);
        }
    } else if (!strcmp(tokenv[0], "enclosed")) {
        if (argcount == 5) {
            searchType = PAD_SEARCH_ENCLOSED;
            _x1 = ATOXF(_tkwin, tokenv[1]);
            _y1 = ATOYF(_tkwin, tokenv[2]);
            _x2 = ATOXF(_tkwin, tokenv[3]);
            _y2 = ATOYF(_tkwin, tokenv[4]);
                                // Normalize input bbox
            if (_x1 > _x2) {
                Pad_Swap(_x1, _x2);
            }
            if (_y1 > _y2) {
                Pad_Swap(_y1, _y2);
            }
        } else {
            Pad_errorString = "wrong # args: searchCommand should be \"enclosed x1 y1 x2 y2\"";
            return(NULL);
        }
    } else if (!strcmp(tokenv[0], "overlapping")) {
        if (argcount == 5) {
            searchType = PAD_SEARCH_OVERLAPPING;
            _x1 = ATOXF(_tkwin, tokenv[1]);
            _y1 = ATOYF(_tkwin, tokenv[2]);
            _x2 = ATOXF(_tkwin, tokenv[3]);
            _y2 = ATOYF(_tkwin, tokenv[4]);
                                // Normalize input bbox
            if (_x1 > _x2) {
                Pad_Swap(_x1, _x2);
            }
            if (_y1 > _y2) {
                Pad_Swap(_y1, _y2);
            }
        } else {
            Pad_errorString = "wrong # args: searchCommand should be \"overlapping x1 y1 x2 y2\"";
            return(NULL);
        }
    } else if (!strcmp(tokenv[0], "withlayer")) {
        if (argcount == 2) {
            searchType = PAD_SEARCH_WITHLAYER;
            if (_matchType == PAD_FIND_EXACT || _matchType == PAD_FIND_GLOB) {  // exact && glob
                _string = tokenv[1];
            } else if (_matchType == PAD_FIND_REGEXP) {                         // regexp
                _regExpToken = Pad_RegExpCompile(tokenv[1]);
                _string = tokenv[1];
            }
        } else {
            Pad_errorString = "wrong # args: searchCommand should be \"withlayer layer\"";
            return(NULL);
        }
    } else if (!strcmp(tokenv[0], "withinfo")) {
        if (argcount == 2) {
            searchType = PAD_SEARCH_WITHINFO;
            if (_matchType == PAD_FIND_EXACT || _matchType == PAD_FIND_GLOB) {  // exact && glob
                _string = tokenv[1];
            } else if (_matchType == PAD_FIND_REGEXP) {                         // regexp
                _regExpToken = Pad_RegExpCompile(tokenv[1]);
                _string = tokenv[1];
            }
        } else {
            Pad_errorString = "wrong # args: searchCommand should be \"withinfo infoString\"";
            return(NULL);
        }
    } else if (!strcmp(tokenv[0], "withsticky")) {
        if (argcount == 2) {
            searchType = PAD_SEARCH_WITHSTICKY;
            if (!strcasecmp(tokenv[1], "all")) {
                _idOrSticky = -1;
            } else if (!strcasecmp(tokenv[1], "1")) {
                _idOrSticky = PAD_STICKY_ALL;
            } else if (!strcasecmp(tokenv[1], "z")) {
                _idOrSticky = PAD_STICKY_Z;
            } else if (!strcasecmp(tokenv[1], "view")) {
                _idOrSticky = PAD_STICKY_VIEW;
            } else {
                Pad_errorString = "Invalid sticky type.  Must be all, 1, z, or view";
                return(NULL);
            }
        } else {
            Pad_errorString = "wrong # args: searchCommand should be \"withsticky stickyType\"";
            return(NULL);
        }
    } else if (!strcmp(tokenv[0], "withtag")) {
        if (argcount == 2) {
            if (!strcmp(tokenv[1],"all")) {
                searchType = PAD_SEARCH_ALL;
            } else {
                if (_matchType == PAD_FIND_EXACT) {
                    _exactWithtag = TRUE;
                }
                searchType = PAD_SEARCH_WITHTAG;
                if (!_Resolve_tagorid(FALSE, tokenv[1]))  {                     // get first id
                    return (NULL);
                }
                _string = tokenv[1];
            }
        } else {
            Pad_errorString = "wrong # args: searchCommand should be \"withtag tagOrId\"";
            return(NULL);
        }
    } else if (!strcmp(tokenv[0], "withtext")) {
        if (argcount == 2) {
            searchType = PAD_SEARCH_WITHTEXT;
            if (_matchType == PAD_FIND_EXACT || _matchType == PAD_FIND_GLOB) {  // exact && glob
                _string = tokenv[1];
            } else if (_matchType == PAD_FIND_REGEXP) {                         // regexp
                _regExpToken = Pad_RegExpCompile(tokenv[1]);
                _string = tokenv[1];
            }
        } else {
            Pad_errorString = "wrong # args: searchCommand should be \"withtext text\"";
            return(NULL);
        }
    } else if (!strcmp(tokenv[0], "withname")) {
        if (argcount == 2) {
            searchType = PAD_SEARCH_WITHNAME;
            if (_matchType == PAD_FIND_EXACT || _matchType == PAD_FIND_GLOB) {  // exact && glob
                _string = tokenv[1];
            } else if (_matchType == PAD_FIND_REGEXP) {                         // regexp
                _regExpToken = Pad_RegExpCompile(tokenv[1]);
                _string = tokenv[1];
            }
        } else {
            Pad_errorString = "wrong # args: searchCommand should be \"withname name\"";
            return(NULL);
        }
    } else if (!strcmp(tokenv[0], "withtype")) {
        if (argcount == 2) {
            searchType = PAD_SEARCH_WITHTYPE;
            if (_matchType == PAD_FIND_EXACT || _matchType == PAD_FIND_GLOB) {  // exact && glob
                _string = tokenv[1];
            } else if (_matchType == PAD_FIND_REGEXP) {                         // regexp
                _regExpToken = Pad_RegExpCompile(tokenv[1]);
                _string = tokenv[1];
            }
        } else {
            Pad_errorString = "wrong # args: searchCommand should be \"withtype type\"";
            return(NULL);
        }
    } else {
        Pad_errorString = "unknown searchCommand: "; 
        Pad_errorString += tokenv[0];
        Pad_errorString += "\n try all, above, below, closest, withinfo, withlayer, withname, withsticky, withtag, withtext, withtype, or enclosed, overlapping, ";
        return(NULL);
    }

    node = new Pad_FindTreeNode(_pad, searchType, _matchType, _idOrSticky, _uid, _string, _regExpToken, 
                                _event, _x1, _y1, _x2, _y2);
    _commandCnt++;

    if (_noShortCircuit) {
        node->Set_no_short_circuit();
    }

    range = argcount;
    return(node);
}

