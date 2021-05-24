#lang racket/base
(require
 racket/class
 (for-syntax racket/base syntax/parse))

(provide (except-out (all-defined-out)))

(define-syntax (define-stubs stx)
  (syntax-parse stx
    [(_ (~or* (name return-value) name) ...)
     (case (system-type 'gc)
       [(cgc)
        #'(begin
            (require (rename-in dynapad/libdynapad [sch_position sch_position-real]))
            (provide (except-out (all-from-out dynapad/libdynapad) sch_position-real))
            (provide sch_position)
            ; sch_position: expects argument of type <dynaobject%>; given: #f
            (define (sch_position . rest)
              (apply sch_position-real rest))
            )]
       [else
        #'(begin
            (begin
              (provide name)
              (define (name . rest)
                (displayln
                 (format "~a was called with args: ~a" 'name rest))
                (~? return-value))) ...)])]))

(define-syntax (dpnames stx)
  (syntax-parse stx
    [(_ (~or* (name return-value) name) ...)
     #'(class object% (super-new)
         (define/public (name . rest)
           (msg this 'name rest)
           (~? return-value)) ...)]))

(define (msg this name args)
  (displayln (format "object ~a method ~a was called with args: ~a" this name args)))

(define test-object%
  ; FIXME this doesn't really work, because
  ; we need the actual objects
  (dpnames
   delete-all
   deletable?
   write-all
   [id 42]
   ))

(define-stubs
  sch_above
  sch_abslinestyle
  sch_add
  sch_addmember
  sch_add_tag
  sch_alwaysrender
  sch_anchor
  sch_background
  sch_bbox
  sch_below
  sch_bind
  sch_bindtags
  sch_bind_to_tag
  sch_boundable
  sch_center
  sch_centerbbox
  sch_coords
  sch_cpanzoom
  sch_create
  sch_createPreview
  sch_cursor
  sch_cursor_by_name
  sch_debugevent
  sch_delete
  sch_deletelayer
  sch_delete_tag
  sch_deletetext
  sch_desiredframerate
  sch_dissolvespeed
  sch_divisible
  sch_doublebuffer
  sch_dt
  sch_dtclose
  sch_dtmode
  sch_dtopen
  sch_dtread
  sch_dt_touches
  sch_events
  sch_faderange
  sch_fastpan
  [sch_fill (error 'bt)] ; XXX can't hit the codepath that causes the error w/o the c++, have to have an object select and hit Fill
  sch_find
  sch_findable
  sch_flip
  sch_flop
  [sch_focus "focus value"]
  sch_font
  sch_fontnames
  sch_freeimagedata
  sch_getfocus
  sch_getgroup
  sch_gettext
  sch_getview
  sch_grab
  sch_height
  sch_idle
  sch_imagedata
  sch_imagedata_rgb
  sch_imagedim
  sch_imagep
  sch_imagepath
  sch_inserttext
  sch_layer
  sch_layermembers
  sch_layername
  sch_layers
  sch_lower
  sch_lowerlayer
  sch_make
  sch_makebutton
  [sch_makedynapad 'test-cpointer] ; cptr
  sch_makegroup
  sch_makeimage
  sch_makeimagedata
  sch_makelayer
  sch_makeline
  sch_makeoval
  sch_makepanel
  sch_makepolygon
  sch_makerect
  sch_maketext
  sch_marktext
  sch_maxsize
  [sch_members '()]
  sch_minsize
  [sch_modifier "modifier value"]
  sch_moveto
  sch_mysql
  sch_mysql_affected_rows
  sch_mysql_change_user
  sch_mysql_close
  sch_mysql_connect
  sch_mysql_errno
  sch_mysql_error
  sch_mysql_escape_string
  sch_mysql_init
  sch_mysql_insert_id
  sch_mysql_query
  sch_mysql_real_connect
  sch_mysql_select_db
  sch_mysql_store_result
  [sch_objectlist (list (make-object test-object%))] ; FIXME this doesn't work we need the real objects
  sch_order
  sch_os
  sch_os2unix
  sch_padid
  sch_panelmoveview
  sch_panelsetfill
  [sch_pen (error 'bt)] ; XXX can't hit the codepath that causes the error w/o the c++, have to have an object select and hit Pen
  sch_penwidth
  sch_pick
  sch_position
  sch_raise
  sch_raiselayer
  sch_refinedissolvespeed
  sch_refinementdelay
  sch_removemember
  sch_rendercolor
  sch_renderimage
  sch_renderitem
  sch_renderline
  sch_renderpolygon
  sch_renderscript
  sch_rendertext
  sch_rendertime
  sch_resize
  sch_rotate
  sch_scale
  sch_settext
  sch_slide
  sch_sticky
  sch_transparency
  sch_truncatefile
  sch_unfindable
  sch_unix
  sch_unix2os
  sch_update
  sch_viewrendertime
  sch_visible
  sch_visiblelayer
  sch_visiblep
  sch_warp
  sch_warp_pointer
  sch_width
  sch_winfo
  sch_winid
  wish ; XXX note that this one lacks a prefix
  sch_xftenable
  sch_xinputdevices
  sch_xy
  sch_xy_in_poly
  sch_zoom
  sch_zoomaction)
