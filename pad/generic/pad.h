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

#ifndef PAD_H
#define PAD_H

class Pad_TreeNode;
class Pad_View;
class Pad_Event;
class Pad_StreamData;

#include "defs.h"
#include "list.h"
#include "ilist.h"
#include "object.h"
#include "view.h"
#include "hashtab.h"
#include "misc.h"
#include "tkwin.h"
#include "callback.h"
#include "misc.h"

struct sockaddr;
struct hostent;

//
// One item for each active URL being fetched
//
class Pad_UrlItem {
  public:
    Pad *       pad;
    ClientData  clientData;
    int         token;
    Pad_String  urlname;
    Pad_Bool    (*updateCb)(ClientData clientData, int token, char *data, int len);
    Pad_Bool    (*doneCb)(ClientData clientData, int token);
    Pad_Bool    (*errorCb)(ClientData clientData, int token, char *data, int len);
    Pad_String  host;
    Pad_String  doc;
    int         port;
    int         bufIndex;
    int         bufLen;
    char *      buf;
    Pad_Bool    readingFirstBatch;
    Pad_Bool    readingHeader;
    Pad_Bool    errorOcurred;
    FILE *      fp;
    Pad_String *filename;
    Pad_String *varname;
    Pad_String  docType;
    Pad_String  docLastModified;
    int         docLength;

    void     Close(void);
    Pad_Bool Connect(void);
    void     Error(char *err);
    Pad_Bool Fetch(void);
    void     Finished(void);
    Pad_Bool Open(void);
    void     Parse_url(char *url);
    void     Read_data(void);

    ~Pad_UrlItem();
    Pad_UrlItem(Pad *pad, ClientData clientData, char *urlname, char *filename, char *varname,
		Pad_Bool (*updateCb)(ClientData clientData, int token, char *data, int len),
		Pad_Bool (*doneCb)(ClientData clientData, int token),
		Pad_Bool (*errorCb)(ClientData clientData, int token, char *data, int len));
};



//
//  Find Tree Node.  Hold the find commands and find expression operations (&& and ||)
//

class Pad_FindTreeNode
{
  public:
    int Evaluate(Pad_Object *obj);
    void Print();

    Pad_Uid *Get_uid() {return &_uid;};
    void    Set_not() { _not = TRUE;};
    void    Set_match_type(int m) { _matchType = m;};
    void    Set_no_short_circuit() { _noShortCircuit = TRUE;};
    void    Set_pad(Pad *pad) { _pad = pad;};

    Pad_FindTreeNode();
                                                                               //operation constructor
    Pad_FindTreeNode(Pad_FindTreeNode *left, Pad_FindTreeNode *right, int operation);
                                                                               //find command constructors
    Pad_FindTreeNode(Pad *pad, int command, int matchType, int idOrSticky, Pad_Uid uid, 
                     char *string, Pad_RegExp regExpToken, Pad_Event *event, float x1, 
                     float y1, float x2, float y2 );
    ~Pad_FindTreeNode();

  private:
    Pad              *_pad;                                       // the pad to search on
    int               _matchType;                                 // match type, (regexp, exact, glob)
    Pad_Bool          _noShortCircuit;                            // whether or not to do a SC eval
    int               _operation;                                 // either: &&, || or a find command.

                                                                  // arguments for the commands
    int               _idOrSticky;                                  // id or sticky type
    Pad_Uid            _uid;                                         // uid, used when given just a tag
    char             *_string;                                      // used for withtag, withinfo, etc.
    Pad_RegExp        _regExpToken;                                 // parsed regular expression 
                                                                    //     (see man Pad_RegExpMatch)
    Pad_Event        *_event;                                       // used for closest
    float             _x1;                                          // used when the find command
    float             _y1;                                          //      has floats as its args
    float             _x2;
    float             _y2;
    Pad_Bool          _not;                                       // take the compliment of the answer?
    Pad_Bool          _passedObject;                              // flag used by above and below

    Pad_FindTreeNode     *_left;                                      // left expr
    Pad_FindTreeNode     *_right;                                     // right expr
};

//
//  Find command class.  Every Pad will have one.
//

class Pad_Find
{
  public:
    Pad_Find();
    Pad_Find(Pad *pad);
    ~Pad_Find();
    Pad_Bool Eval(Pad *pad, Pad_List &list, ClientData clientData, Pad_Bool groupMembers, int matchType, int argc, char ** argv);

  private:
                                             // variables only used by Pad_Find
    Pad_FindTreeNode   *_expr;                 // the expression tree
    Pad_Bool            _exactWithtag;         // are one of the find cmds an exact withtag?
    int                 _commandCnt;           // the number of command elements for this command.
                                               // Note: if both exactWithtag and _singeCommand
                                               //       we may use the hash table to solve it

                                             // variables to be passed to Pad_FindTree Nodes that
                                             // will NOT change from node to node
    Pad                *_pad;
    Pad_TkWin          *_tkwin;

                                             // variables to be passed to Pad_FindTree Nodes that
                                             // WILL change from node to node
              
    int                 _noShortCircuit;       // True if all expression need to be evaluated.
    int                 _matchType;            //   the match Type (exact, reg, blob) currently 
                                               //   this cannot change from node to node, but may
                                               //   may in the future so it is here.

    int                 _idOrSticky;           // id or sticky type
    Pad_Uid              _uid;                  // uid, used when given just a tag
    char               *_string;               // used for withtag, withinfo, etc.
    Pad_RegExp          _regExpToken;          // parsed regular expr ( see man Pad_RegExpMatch)
    Pad_Event          *_event;                // used for closest
    float               _x1;                   // used when the find command
    float               _y1;                   //      has floats as its args
    float               _x2; 
    float               _y2;

    char            **_Get_tokens(int argc, char **argv, int &tokenc);
    Pad_FindTreeNode *_Parse(int argc, char **argv);
    int               _Compute_range(int argc, char **argv);
    Pad_Bool          _Resolve_tagorid(Pad_Bool onlyFistId, char *tagorid);
    Pad_FindTreeNode *_Parse_command(int argc, char **argv, int &range);

};
    
void _Pad_Print_argv(int argc, char **argv);
    


//
// Flags for the Pad class
//
#define PADROOT_EVENTSOFF    (1<<0)  // Events should not be generated
#define PADROOT_READING      (1<<1)  // Currently reading a file
#define PADROOT_MANUAL_CREATE_EVENT (1<<2)  // True if <Create> event should be generated manually
#define PADROOT_ACTIVE       (1<<3)  // True if pad window is active (i.e., cursor in it)


//
// The root of the Pad tree
//
class Pad : public Pad_Object
{
  public:
   unsigned char padFlags;	     // OR'd combination of flags
   Pad_Object *first;		     // First object of drawing order
   Pad_Object *last;		     // Last object of drawing order
   Pad_View *  view;		     // Pointer back up to view
   int         level;                // Current refinement render level
   int         objid;		     // Id counter for objects
Pad_HashTable *idTable;		     // Hash table for object id's
Pad_HashTable *tagTable;	     // Hash table for object tag's
Pad_HashTable *typeTable;            // List of all types
Pad_HashTable *optionTable;	     // Mapping of Pad_OptionKey to Pad_Option
Pad_HashTable *optionIdTable;	     // Mapping of Pad_Option string name to Pad_Option id (for builtin options only)
   Pad_List    layers;               // List of layers (Pad_Layer *) in display-list order
Pad_HashTable  layerNameTable;	     // List of all layers indexed by layer name
Pad_HashTable  layerIdTable;	     // List of all layers indexed by layer id
Pad_TreeNode  *treeroot;             // root of all Pad_TreeNodes on the pad
   int         mode;		     // Mode of event
   Pad_Bool    idEventFirst;	     // TRUE if new objects should get specific events first
   Pad_List    urlInactiveItems;     // List of inactive Pad_UrlItem's waiting to be started
   Pad_List    urlActiveItems;       // List of active Pad_UrlItem's currently being fetched
   Pad_List    userTypes;	     // List of Pad_UserType's, user-defined object types
   Pad_String  writeFilename;        // When writing a file, this stores the name of the file being written to
   Pad_String  writeFiledir;         // When writing a file, this stores the dir of the file being written to
   Pad_Bool    writeRelative;        // When writing a file, this is true if pathnames should be relative
   Pad_Bool    defaultEvents;        // True if default event handlers are set
   int         renderTime;           // Time (in ms) of most recent render

				     // 
				     // Debugging variables
				     // 
   int         objectsRendered;      // Number of objects rendered for the most recent render

            Pad_Bool     Align(Pad_List &objs, int type, Pad_Bool anchor=FALSE);
            Pad_Bool     Align(Pad_List &objs, int type, Pad_PList &coords, Pad_Bool overlapOnly=FALSE, 
			       Pad_Bool anchor=FALSE);
            Pad_Layer  * Create_layer(char *name);
    virtual Pad_Object * Create_object(int type);
            void         Create_treeroot(void);
    virtual void         Damage(void);                          // Mark the pad surface as changed
            void         Delete_layer(Pad_Layer *layer);
            Pad_Bool     Delete_obj(int id);	                // Delete object by id
            Pad_Bool     Distribute(Pad_List &objs, int type);
            Pad_Bool     Distribute(Pad_List &objs, int type, float space);
            Pad_Bool     Distribute(Pad_List &objs, Pad_PList &coords);
            Pad_Bool     Distribute(Pad_List &objs, Pad_PList &coords, float space, Pad_Bool exact=FALSE);
            void         Find_all(Pad_List &items, Pad_Bool groups);
            void         Find_all_ids(Pad_IList &ids, Pad_Bool groups);
            Pad_Object * Find_above(char *tagorid, Pad_Bool groups);
            Pad_Object * Find_below(char *tagorid, Pad_Bool groups);
            Pad_Object * Find_closest(Pad_Event *event, float halo, char *start,
				     Pad_Bool groups);
            Pad_Bool     Find_eval(Pad_List &list, ClientData clientData, 
                                   Pad_Bool groupMembers, int matchType, int argc, char **argv);
            void         Find_enclosed(float x1, float y1, float x2, float y2, Pad_List &items, 
				      Pad_Bool groups);
            void         Find_ids_with_tagorid(char *tagorid, Pad_IList &ids, Pad_Bool groups);
            void         Find_overlapping(float x1, float y1, float x2, float y2, Pad_List &items, 
					 Pad_Bool groups);
            Pad_Object * Find_pick(Pad_Event *event);
            Pad_Object * Find_pick(Pad_Event *event, Pad_List &portals, Pad_List &objects);
            void         Find_with_layer(Pad_Layer *layer, Pad_List &items, Pad_Bool groups);
            void         Find_with_info(char *info, Pad_List &items, Pad_Bool groups);
            void         Find_with_sticky(int stickyType, Pad_List &items, Pad_Bool groups);
            void         Find_with_tagorid(char *tagorid, Pad_List &items, Pad_Bool groups, Pad_Bool append=FALSE);
            void         Find_with_text(char *text, Pad_List &items, Pad_Bool groups);
            void         Find_with_type(char *type, Pad_List &items, Pad_Bool groups);
            Pad_Object * First();
            Pad_Bool     Get_active(void);
            void         Get_ids_from_tag(char *tag, Pad_IList &ids);
            Pad_Layer *  Get_layer_from_name(char *name);
            Pad_Layer *  Get_layer_from_id(int layerId);
            char *       Get_modifier(void);
            Pad_Object * Get_object_from_id(int id);
            void         Get_objects_from_tag(char *tag, Pad_List &objs);
            Pad_Win *    Get_win(void);
            void         Init_events(Pad_Bool status);
            Pad_Object * Last();
            Pad_Bool     Layout_position(Pad_Object *target, Pad_Point &targetPt, Pad_List &objs, Pad_Point &pt, int animationTime=0);
            Pad_Bool     Layout_position(Pad_BBox &targetBBox, Pad_Point &targetPt, Pad_List &objs, Pad_Point &pt, int animationTime=0);
            Pad_Bool     Layout_size(int type, Pad_Object *refObj, Pad_List &objs, float scale=1.0);
            Pad_Bool     Layout_size(int type, Pad_List &objs, float scale);
            Pad_Bool     Lower_layer(Pad_Layer *layer);
            Pad_Bool     Lower_layer(Pad_Layer *layer1, Pad_Layer *layer2);
            Pad_Object * Next(Pad_Object *obj);
            Pad_Object * Prev(Pad_Object *obj);
            Pad_Bool     Raise_layer(Pad_Layer *layer);
            Pad_Bool     Raise_layer(Pad_Layer *layer1, Pad_Layer *layer2);
    virtual Pad_Bool     Render(void);
    virtual Pad_Bool     Render_display_list(Pad_Object *obj);
    virtual Pad_Bool     Render_object(Pad_Object *obj, int renderSpeed=0);
            void         Render_bounding_box(void); 
            Pad_Bool     Scale(Pad_List &objs, float x, float y, float zmult, Pad_Bool transform=FALSE, int animationTime=0);
            void         Set_active(Pad_Bool active);
            void         Set_event_control(Pad_Bool state);
            void         Set_create_event_control(Pad_Bool state);
            Pad_Bool     Set_modifier(char *modifier);
            Pad_Bool     Slide(Pad_List &objs, float dx, float dy, Pad_Bool transform=FALSE, int animationTime=0);
            Pad_Bool     Slide(Pad_List &objs, Pad_PList &delta, Pad_Bool transform=FALSE, int animationTime=0);
            Pad_Bool     Snap(float grid, Pad_List &objs);

				// Event Handlers
    static Pad_EventCallbackProc(Pan_press);
    static Pad_EventCallbackProc(Pan_drag);
    static Pad_EventCallbackProc(Pan_release);
    static Pad_EventCallbackProc(Zoom_in_press);
    static Pad_EventCallbackProc(Zoom_in_drag);
    static Pad_EventCallbackProc(Zoom_in_release);
    static Pad_EventCallbackProc(Zoom_out_press);
    static Pad_EventCallbackProc(Zoom_out_drag);
    static Pad_EventCallbackProc(Zoom_out_release);

    virtual ~Pad();
    Pad(Pad_View *view);

    PAD_TYPE("pad");
    
  private:
    Pad_Find    find;                 // This Pad's find command. 
    Pad_Object *_Pick_recurse(Pad_View *view, Pad_Event *event, Pad_Object *event_obj, Pad_Object *last,
			      int &last_draw_num, Pad_Bool &found);
};

#endif

