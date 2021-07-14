
;=== halo manager ================================================

;--- callbacks for updating halo layout --------------------------

(define (add-halo-layout-callbacks obj)
  (send obj afterdelete-callbacks   'add  (lambda (obj) (update-halo-layout-delete)) 'halo)

  (send obj slide-callbacks    'add  (lambda (obj dx dy) (update-halo-layout)) 'halo)
  (send obj scale-callbacks    'add  (lambda (obj fac x y) (update-halo-layout)) 'halo)

  (send obj width-callbacks    'add  (lambda (obj f) (update-halo-layout)) 'halo)
  (send obj height-callbacks   'add  (lambda (obj f) (update-halo-layout)) 'halo)
  (send obj position-callbacks 'add  (lambda (obj f) (update-halo-layout)) 'halo)

  (if (has-method? obj 'coords-callbacks)
      (send obj coords-callbacks 'add  (lambda (obj l) (update-halo-layout))) 'halo)

  ;  ; undo all this when unselected
  ;  (send obj select-callbacks (list (lambda (obj f)
  ;    (when (not f)
  ;      (remove-halo-callbacks obj)))))

  ;(update-halo-layout)
  )

(define (remove-halo-callbacks obj)
  (send obj afterdelete-callbacks   'remove 'name 'halo)

  (send obj slide-callbacks    'remove 'name 'halo)
  (send obj scale-callbacks    'remove 'name 'halo)

  (send obj width-callbacks    'remove 'name 'halo)
  (send obj height-callbacks   'remove 'name 'halo)
  (send obj position-callbacks 'remove 'name 'halo)

  (if (has-method? obj 'coords-callbacks) (send obj coords-callbacks 'remove 'name 'halo))
  (update-halo-layout)
  )


(define (select-mode!) (changemode dynapad "Select"))
(define (run-mode!)    (changemode dynapad "Run"))
(define (any-selected?) (not (null? (send dynapad selected))))
(define (more-than-one-selected?) (> (length (send dynapad selected)) 1))


;--- layout utilities --------------------------------------------

; Halo buttons are positioned by evaluating a bunch of layout lambdas,
; one lambda per button.  Each lambda should be defined to position
; one halo button, in some manner, with respect to the bbox described
; by *halo_ref_bbox* (a global variable).
;
; here is a simple example:
; (add-lambda-to-halo-update
;    (lambda ()
;      (send halo_btn1 position
;            (list (car *halo_ref_bbox*) (cadr *halo_ref_bbox*) 1.0)))
; )
;
; The lambda takes no arguments--it is lambda-bound to one button and
; the reference bbox is a global variable.

(define *halo_ref_bbox* '(0 0 1 1))

;--- This is the central layout function ------
(define (update-halo-layout . xy)
  ; first compute the shared reference bbox
  (set! *halo_ref_bbox* (compute-halo-reference-bbox xy))
  ; then evaluate each layout function in the list
  (map (lambda (fnc_obj_pair) ((car fnc_obj_pair))) *halo_update_list*)
  )

(define *halo_update_list* ())
(define (add-lambda-to-halo-update new_lambda)
  (push! (list new_lambda #f) *halo_update_list*))

; I simplified the implementation by assuming that halos will operate on
; just the selected object (or the list of selected objects).  This means
; that re-positioning the halo buttons is done with respect to the bounding
; box of the selected objects.
; This function computes that bbox.  It returns an adjusted bbox if
; the actual bbox is too small.
;
; Also, if no objects are selected then the bbox is centered
; around the mouse_xy argument.

(define (compute-halo-reference-bbox mouse_xy)
  (define bbox_ref
    (if (or (null? mouse_xy) (and (any-selected?) (not (equal? *halo_mode* 'main))))
        (bbunion-objects (send dynapad selected))
        ;else
        (append mouse_xy mouse_xy))
    )
  (define bc_ref (bbcenter bbox_ref))
  (define w_ref  (bbwidth  bbox_ref))
  (define h_ref  (bbheight bbox_ref))
  (define pz (send dynapad getzoom))

  (set! w_ref (max w_ref (/ (* 3.0 *halosize*) pz)))
  (set! h_ref (max h_ref (/ (* 3.5 *halosize*) pz)))

  (set! bbox_ref (list
                  (- (car bc_ref) (* 0.5 w_ref))
                  (- (cadr bc_ref) (* 0.5 h_ref))
                  (+ (car bc_ref) (* 0.5 w_ref))
                  (+ (cadr bc_ref) (* 0.5 h_ref))
                  ))
  bbox_ref
  )

;--- layout lambda generator -------------------------------------
; This utility is just for convenience.
;
; The layout-lambdas can be specified directly, for example:
;
;   (add-lambda-to-halo-update
;     (lambda ()
;         (define bbox_ref *halo_ref_bbox*)
;         (define bbox_btn (send halo_btn1 bbox))
;         (send halo_btn1 slide
;           (- (b2 bbox_ref) (b0 bbox_btn))
;           (- (b3 bbox_ref) (b1 bbox_btn)))))
;
; The above lambda slides the button (halo_btn1) until its lower-left corner
; is in the same position as the upper-right corner of the ref bbox.
;
;   note: b0 b1 b2 b3 are functions that return parts of a bounding box.
;     LowerLeft of button    == (list (b0 bbox_btn) (b1 bbox_btn) )
;      and
;     UpperRight of ref bbox == (list (b2 bbox_ref) (b3 bbox_ref) )
;
; The function below generates a lambda which performs the same sort of
; positioning.  Its arguments include two functions that specify a corner
; of the reference bbox and two other functions that specify a corner of
; the button's bbox.  The generated lambda will slide the button until
; these two corners coincide.
;
; The example lambda above would be generated like this:
;   (add-lambda-to-halo-update
;     (make-layout-function  halo_btn1  b2 b3   b0 b1   0 0) )
;
(define (make-layout-function
         btn                   ; the button to be positioned
         fnc-xref fnc-yref     ; corner of reference bbox
         fnc-xbtn fnc-ybtn     ; corner of button bbox
         xoff yoff             ; offsets (see explanation below)
         )
  (list
   (lambda ()
     (define bbox_ref *halo_ref_bbox*)
     (define bbox_btn (send btn bbox))

     (define dx (- (fnc-xref bbox_ref) (fnc-xbtn bbox_btn)))
     (define dy (- (fnc-yref bbox_ref) (fnc-ybtn bbox_btn)))
     (define pz (send dynapad getzoom))
     (if (not (= xoff 0)) (set! dx (+ dx (* xoff (/ *halosize* pz)))))
     (if (not (= yoff 0)) (set! dy (+ dy (* yoff (/ *halosize* pz)))))
     (send btn slide dx dy)

     ;make sure the button is visible
     (send btn layer halo_layer)
     )
   #f ; place holder for application specific datum (e.g. object or actor reference)
   )
  )

; The offset parameters adjust the position
; by one button-width (or button-height).
; For example, if one button is positioned at
; the upper right corner of the ref bbox:
;     (make-layout-function  halo_btn1  b2 b3   b0 b1   0 0)
; another button can be positioned just to its
; left by using a negative offset in the x direction
;     (make-layout-function  halo_btn1  b2 b3   b0 b1  -1 0)


;--- miscellaneous -----------------------------------------------

(define (make-into-halo-button obj)
  (send obj layer hide_layer)
  (send obj findable #f)
  (send obj sticky "z")

  ; prevent select events
  (send obj bind "<Select-ButtonPress-1>"   (lambda (w e) #f))
  (send obj bind "<Select-ButtonRelease-1>" (lambda (w e) #f))
  (send obj bind "<Shift-Select-ButtonPress-1>"   (lambda (w e) #f))
  (send obj bind "<Shift-Select-ButtonRelease-1>" (lambda (w e) #f))

  (push! obj *global_halo_button_list*)
  )

; global variables
(define *global_halo_button_list* ())

(define *halo_mode* 'main)  ; mode is OBJECT halo or MAIN halo

(define *halosize* 30)
(define (set!-halo-button-size wh) (set! *halosize* wh))

(define halo_layer (make-object layer% dynapad "halo"))
(define hide_layer (make-object layer% dynapad "hidehalo"))
(send hide_layer visible #f)


;--- modification of standard run/select bindings ----------------

; When an dynaobject is selected I need to add the appropriate
; halo layout callbacks
;

; Wedge a function into these Select events
(define *orig-Select-ButtonPress-1*
  (car (send dynapad bind "<Select-ButtonPress-1>")))
(define *orig-Select-ButtonRelease-1*
  (car (send dynapad bind "<Select-ButtonRelease-1>")))
(define *orig-Shift-Select-ButtonPress-1*
  (car (send dynapad bind "<Shift-Select-ButtonPress-1>")))
(define *orig-Shift-Select-ButtonRelease-1*
  (car (send dynapad bind "<Shift-Select-ButtonRelease-1>")))

(define (add-halo-callbacks? obj)
  (when (not (is-a? obj select%))
    (when (not (is-a? obj dynapad%))
      (set! obj (get-top-group obj))
      (if (not (send obj selected?)) (add-halo-layout-callbacks obj)))))

(send dynapad bind "<Select-ButtonPress-1>"
      (lambda (w e)
        (add-halo-callbacks? (event-obj e))
        (*orig-Select-ButtonPress-1* w e)
        (if (any-selected?)
            (update-halo-layout (event-x e)(event-y e))
            )
        ))

(send dynapad bind "<Select-ButtonRelease-1>"
      (lambda (w e)
        (*orig-Select-ButtonRelease-1* w e)
        (if (any-selected?)
            ;(update-halo-layout (event-x e)(event-y e))
            (show-objecthalo)
            ;else
            (begin
              (hide-all-halo-buttons)
              (run-mode!)
              )
            )
        #f
        ))

(send dynapad bind "<Control-Select-ButtonRelease-1>"
      (lambda (w e)
        (*orig-Select-ButtonRelease-1* w e)
        (update-halo-layout (event-x e)(event-y e))
        #f
        ))

(send dynapad bind "<Shift-Select-ButtonPress-1>"
      (lambda (w e)
        (add-halo-callbacks? (event-obj e))
        (*orig-Shift-Select-ButtonPress-1* w e)
        ))
(send dynapad bind "<Shift-Select-ButtonRelease-1>"
      (lambda (w e)
        (*orig-Shift-Select-ButtonRelease-1* w e)
        (update-halo-layout (event-x e)(event-y e))
        ))


(send dynapad bind "<Control-ButtonPress-1>" (lambda (w e) (handle-meta-click e) #f))
(send dynapad bind "<Control-Select-ButtonPress-1>" (lambda (w e) (handle-meta-click e) #f))

(define (handle-meta-click e)
  (define obj (event-obj e))

  (when (not (is-a? obj select%))
    (when (not (is-a? obj dynapad%))
      (set! obj (get-top-group obj))
      )

    (select-mode!)
    (when (not (is-a? obj dynapad%))
      (send obj select)
      (add-halo-layout-callbacks obj)
      )
    (if (is-a? obj dynapad%)
        (begin
          ;(set! *halo_ref_bbox* (list (event-x e) (event-y e) (event-x e) (event-y e)))
          (show-mainhalo (event-x e) (event-y e))
          )
        ;else
        (begin
          (set! *halo_ref_bbox* (list (event-x e) (event-y e) (event-x e) (event-y e)))
          (show-objecthalo)
          )
        ;      (if (not (application-handles-metaclick obj x y))
        ;        (show-halo #f 'objmenu (send obj bbox) x y)
        ;      )
        )
    )
  )

(define (show-mainhalo x y)
  (hide-all-halo-buttons)
  (set! *halo_update_list* (get-main-halo))
  (set! *halo_mode* 'main)
  (update-halo-layout x y)
  )

(define (show-objecthalo)
  (hide-all-halo-buttons)
  (set! *halo_update_list* (get-object-halo))
  (set! *halo_mode* 'object)
  (update-halo-layout)
  )

(define (get-object-halo) *obj_update_list*)
(define (get-main-halo) *main_update_list*)

(define (hide-all-halo-buttons)
  (foreach *global_halo_button_list* (lambda (b) (send b layer hide_layer))) )

; change standard appearance of selection rectangles
;(send dynapad selectpenwidth 0)
;(send dynapad selectpen "red")

;=== create halo buttons =========================================

(set!-halo-button-size 12)

(define (scale-crds xylst) (map (lambda (xory) (* *halosize* xory)) xylst))

; right facing arrow
(define width_btn
  (ic (make-object polygon% dynapad (scale-crds '(
                                                  0.0  0.25
                                                  0.0  0.75
                                                  0.5  0.75
                                                  0.5  1.0
                                                  1.0  0.5
                                                  0.5  0.0
                                                  0.5  0.25
                                                  )))
      (layer hide_layer)
      (fill "yellow")(pen "white")(penwidth 0)))

; downward facing arrow
(define height_btn
  (ic (make-object polygon% dynapad (scale-crds '(
                                                  0.25 1.0
                                                  0.75 1.0
                                                  0.75 0.5
                                                  1.0  0.5
                                                  0.5  0.0
                                                  0.0  0.5
                                                  0.25 0.5
                                                  )))
      (layer hide_layer)
      (fill "yellow")(pen "white")(penwidth 0)))

(define resize_btn
  (ic (make-object oval% dynapad (scale-crds '(0.2 0.2 0.8 0.8)))
      ;  (layer hide_layer)
      (fill "yellow")(pen "white")))

; big X shape
(define delete_btn
  (ic (make-object polygon% dynapad (scale-crds '(
                                                  0.00 1.00
                                                  0.30 1.00
                                                  0.50 0.70
                                                  0.70 1.00
                                                  1.00 1.00
                                                  1.00 0.70
                                                  0.70 0.50
                                                  1.00 0.30
                                                  1.00 0.00
                                                  0.70 0.00
                                                  0.50 0.30
                                                  0.30 0.00
                                                  0.00 0.00
                                                  0.00 0.30
                                                  0.30 0.50
                                                  0.00 0.70
                                                  )))
      (layer hide_layer)
      (fill "black")(pen "white")))

; perspective of of two-headed arrow
(define zoom_btn
  (ic (make-object polygon% dynapad (scale-crds '(
                                                  0.37 0.96
                                                  0.14 0.96
                                                  0.5 1.08
                                                  0.89 0.96
                                                  0.637 0.96
                                                  0.84 0.35
                                                  1.08 0.35
                                                  0.5 -0.08
                                                  -0.08 0.35
                                                  0.14 0.35
                                                  )))
      (layer hide_layer)
      (fill "yellow")(pen "white")(penwidth 0)))

; perspective of of two-headed arrow
(define zoomview_btn
  (ic (make-object polygon% dynapad (scale-crds '(
                                                  0.37 0.96
                                                  0.14 0.96
                                                  0.5 1.08
                                                  0.89 0.96
                                                  0.637 0.96
                                                  0.84 0.35
                                                  1.08 0.35
                                                  0.5 -0.08
                                                  -0.08 0.35
                                                  0.14 0.35
                                                  )))
      (layer hide_layer)
      (fill "yellow")(pen "white")(penwidth 0)))

(define move_btn
  (ic (make-object polygon% dynapad (scale-crds '(
                                                  0.4 0.6 0.40 0.90 0.25 0.90 0.50 1.15 0.75 0.90 0.60 0.90
                                                  0.6 0.6 0.9 0.6 0.9 0.75 1.15 0.5 0.9 0.25 0.9 0.4 0.6 0.4
                                                  0.6 0.1 0.75 0.1 0.5 -0.15 0.25 0.1 0.4 0.1 0.4 0.4
                                                  0.1 0.4 0.1 0.25 -0.15 0.5 0.1 0.75 0.1 0.6 0.4 0.6
                                                  )))
      (layer hide_layer)
      (fill "blue")(pen "white")(penwidth 0)))

(define moveview_btn
  (ic (make-object polygon% dynapad (scale-crds '(
                                                  0.4 0.6 0.40 0.90 0.25 0.90 0.50 1.15 0.75 0.90 0.60 0.90
                                                  0.6 0.6 0.9 0.6 0.9 0.75 1.15 0.5 0.9 0.25 0.9 0.4 0.6 0.4
                                                  0.6 0.1 0.75 0.1 0.5 -0.15 0.25 0.1 0.4 0.1 0.4 0.4
                                                  0.1 0.4 0.1 0.25 -0.15 0.5 0.1 0.75 0.1 0.6 0.4 0.6
                                                  )))
      (layer hide_layer)
      (fill "blue")(pen "white")(penwidth 0)))

(define menu_btn (make-object group% dynapad (list
                                              (ic (make-object line% dynapad (scale-crds '(0.1 0.25 0.9 0.25)))
                                                  (pen "black")(penwidth 0))
                                              (ic (make-object line% dynapad (scale-crds '(0.1 0.5 0.9 0.5)))
                                                  (pen "black")(penwidth 0))
                                              (ic (make-object line% dynapad (scale-crds '(0.1 0.75 0.9 0.75)))
                                                  (pen "black")(penwidth 0))
                                              (ic (make-object rect% dynapad (scale-crds '(0.0 0.0 1.0 1.0)))
                                                  (layer hide_layer)
                                                  (fill "white")(pen "black"))
                                              )))
(send menu_btn layer hide_layer)
(send menu_btn divisible #f)

(define objmenu_btn (make-object group% dynapad (list
                                                 (ic (make-object line% dynapad (scale-crds '(0.1 0.25 0.9 0.25)))
                                                     (pen "black")(penwidth 0))
                                                 (ic (make-object line% dynapad (scale-crds '(0.1 0.5 0.9 0.5)))
                                                     (pen "black")(penwidth 0))
                                                 (ic (make-object line% dynapad (scale-crds '(0.1 0.75 0.9 0.75)))
                                                     (pen "black")(penwidth 0))
                                                 (ic (make-object rect% dynapad (scale-crds '(0.0 0.0 1.0 1.0)))
                                                     (layer hide_layer)
                                                     (fill "white")(pen "black"))
                                                 )))
(send objmenu_btn layer hide_layer)
(send objmenu_btn divisible #f)

;---

(make-into-halo-button width_btn)
(make-into-halo-button height_btn)
(make-into-halo-button delete_btn)
(make-into-halo-button zoom_btn)
(make-into-halo-button zoomview_btn)
(make-into-halo-button move_btn)
(make-into-halo-button moveview_btn)
(make-into-halo-button menu_btn)
(make-into-halo-button objmenu_btn)
(make-into-halo-button resize_btn)

;--------------------------------------------------
; define two basic halos,
; a general purpose halo and an object specific halo
;--------------------------------------------------

(define *obj_update_list* (list
                           (make-layout-function  resize_btn  b2  b1    b0  b3  0.2 -0.2 ) ;lower right
                           (make-layout-function  width_btn   b2  b1    b0  b3    0  1 ) ;lower right, then up
                           (make-layout-function  height_btn  b2  b1    b0  b3   -1  0 ) ;lower right, then left

                           (make-layout-function  delete_btn  b0  b3    b2  b1    0  0 ) ;upper left
                           (make-layout-function  move_btn    bxc b3    bxc b1    0  0 ) ;top center
                           (make-layout-function  zoom_btn    bxc b3    bxc b1    1  0 ) ;top center
                           (make-layout-function  objmenu_btn b0  byc   b2  byc   0  0 ) ;left middle
                           ))


(define *main_update_list* (list
                            (make-layout-function  moveview_btn     bxc b3    bxc b1    0  0 )  ;top center
                            (make-layout-function  zoomview_btn bxc byc   bxc byc   0  0 )  ;top center
                            (make-layout-function  menu_btn     b0  byc   b2  byc   0  0 )  ;left middle
                            ))

;=== bindings for halo buttons ===================================



;--- delete button -----------------------------------------------

(send delete_btn bind "<Select-ButtonRelease-1>" (lambda (w e)
                                                   (send-selected delete)
                                                   ))

(define (update-halo-layout-delete)
  (if (any-selected?)
      (update-halo-layout)
      ;else, last one deleted
      (begin
        (hide-all-halo-buttons)
        (run-mode!))))

;--- resize ------------------------------------------------------

(def _resize_btn_xoff)
(def _resize_btn_yoff)
(def _resize_btn_x0)
(def _resize_btn_y0)
(def _resize_btn_px0)
(def _resize_btn_py0)
(def _resize_btn_pz0)
(def _resize_btn_xul)
(def _resize_btn_yul)
(def _resize_btn_width0)
(def _resize_btn_height0)
(def _resize_btn_bbox0)
(def _resize_btn_layer)
(def _resize_btn_saveanchor)
(def _resize_btn_subject)

;--- maintain aspect ratio ------
(send resize_btn bind "<Select-ButtonPress-1>" (lambda (w e)
                                                 (set! _resize_btn_subject (car (reverse (send dynapad selected))))
                                                 (set! _resize_btn_bbox0 (send _resize_btn_subject bbox))
                                                 (set! _resize_btn_px0 (b0 _resize_btn_bbox0))
                                                 (set! _resize_btn_py0 (b3 _resize_btn_bbox0))
                                                 (set! _resize_btn_pz0 (caddr (send _resize_btn_subject position)))
                                                 (set! _resize_btn_saveanchor (send _resize_btn_subject anchor))
                                                 (send _resize_btn_subject re-anchor "nw")
                                                 (set! _resize_btn_width0 (bbwidth _resize_btn_bbox0))
                                                 (set! _resize_btn_height0 (bbheight _resize_btn_bbox0))
                                                 (set! _resize_btn_layer (send (event-obj e) layer))
                                                 (set! _resize_btn_x0 (event-x e))
                                                 (set! _resize_btn_y0 (event-y e))
                                                 (set! _resize_btn_xoff (- _resize_btn_x0 (b2 _resize_btn_bbox0)))
                                                 (set! _resize_btn_yoff (- _resize_btn_y0 (b0 _resize_btn_bbox0)))

                                                 #f
                                                 ))

(send resize_btn bind "<Select-ButtonRelease-1>" (lambda (w e)
                                                   (send _resize_btn_subject re-anchor _resize_btn_saveanchor) #f))

(send resize_btn bind "<B1-Motion>" (lambda (w e)
                                      (def dx (- (event-x e) _resize_btn_x0))
                                      (def dy (- _resize_btn_y0 (event-y e)))
                                      (def newwidth)
                                      (def newheight)
                                      (if (> dx (* dy (/ _resize_btn_width0 _resize_btn_height0)))
                                          (begin
                                            (set! newwidth (+ _resize_btn_width0 dx))
                                            (set! newheight (/ (* newwidth _resize_btn_height0) _resize_btn_width0))
                                            )
                                          ;else
                                          (begin
                                            (set! newheight (+ _resize_btn_height0 dy))
                                            (set! newwidth (/ (* newheight _resize_btn_width0) _resize_btn_height0))
                                            )
                                          )
                                      (when (and (> newwidth 0) (> newheight 0))
                                        (send _resize_btn_subject height newheight)
                                        (send _resize_btn_subject width newwidth))
                                      ;(send _resize_btn_subject position
                                      ;  (list _resize_btn_px0 _resize_btn_py0 _resize_btn_pz0))
                                      #f
                                      ))

;--- free aspect ratio (with shift) ------
(send resize_btn bind "<Shift-Select-ButtonPress-1>" (lambda (w e)
                                                       (set! _resize_btn_subject (car (reverse (send dynapad selected))))
                                                       (set! _resize_btn_bbox0 (send _resize_btn_subject bbox))
                                                       (set! _resize_btn_px0 (b0 _resize_btn_bbox0))
                                                       (set! _resize_btn_py0 (b3 _resize_btn_bbox0))
                                                       (set! _resize_btn_pz0 (caddr (send _resize_btn_subject position)))
                                                       (set! _resize_btn_saveanchor (send _resize_btn_subject anchor))
                                                       (send _resize_btn_subject re-anchor "nw")
                                                       (send _resize_btn_subject position (list _resize_btn_px0 _resize_btn_py0 _resize_btn_pz0))
                                                       (set! _resize_btn_width0 (bbwidth _resize_btn_bbox0))
                                                       (set! _resize_btn_height0 (bbheight _resize_btn_bbox0))
                                                       (set! _resize_btn_layer (send (event-obj e) layer))
                                                       (set! _resize_btn_x0 (event-x e))
                                                       (set! _resize_btn_y0 (event-y e))

                                                       #f
                                                       ))
(send resize_btn bind "<Shift-Select-ButtonRelease-1>" (lambda (w e)
                                                         (send _resize_btn_subject re-anchor _resize_btn_saveanchor) #f))

(send resize_btn bind "<Shift-B1-Motion>" (lambda (w e)
                                            (def dx (- (event-x e) _resize_btn_x0))
                                            (def dy (- _resize_btn_y0 (event-y e)))
                                            (def newwidth (+ _resize_btn_width0 dx))
                                            (def newheight (+ _resize_btn_height0 dy))
                                            (if (> newwidth 0) (send _resize_btn_subject width newwidth))
                                            (if (> newheight 0) (send _resize_btn_subject height newheight))
                                            (send _resize_btn_subject position (list _resize_btn_px0 _resize_btn_py0 _resize_btn_pz0))
                                            #f
                                            ))

;-----------------------------------------------------------------
(def _width_btn_x0)
(def _width_btn_y0)
(def _width_btn_px0)
(def _width_btn_py0)
(def _width_btn_pz0)
(def _width_btn_xul)
(def _width_btn_yul)
(def _width_btn_width0)
(def _width_btn_height0)
(def _width_btn_bbox0)
(def _width_btn_layer)
(def _width_btn_saveanchor)
(def _width_btn_subject)

(send width_btn bind "<Select-ButtonPress-1>" (lambda (w e)
                                                (set! _width_btn_subject (car (reverse (send dynapad selected))))
                                                (set! _width_btn_bbox0 (send _width_btn_subject bbox))
                                                (set! _width_btn_px0 (b0 _width_btn_bbox0))
                                                (set! _width_btn_py0 (b3 _width_btn_bbox0))
                                                (set! _width_btn_pz0 (caddr (send _width_btn_subject position)))
                                                (set! _width_btn_saveanchor (send _width_btn_subject anchor))
                                                (send _width_btn_subject re-anchor "nw")
                                                (send _width_btn_subject position (list _width_btn_px0 _width_btn_py0 _width_btn_pz0))
                                                (set! _width_btn_width0 (bbwidth _width_btn_bbox0))
                                                (set! _width_btn_height0 (bbheight _width_btn_bbox0))
                                                (set! _width_btn_layer (send (event-obj e) layer))
                                                (set! _width_btn_x0 (event-x e))
                                                (set! _width_btn_y0 (event-y e))

                                                #f
                                                ))
(send width_btn bind "<Select-ButtonRelease-1>" (lambda (w e)
                                                  (send _width_btn_subject re-anchor _width_btn_saveanchor) ))

(send width_btn bind "<B1-Motion>" (lambda (w e)
                                     (define dx (- (event-x e) _width_btn_x0))
                                     (def dy)
                                     (define newwidth (+ _width_btn_width0 dx))
                                     (define newheight (+ _width_btn_height0 dy))
                                     (if (> newwidth 0) (send _width_btn_subject width newwidth))
                                     (if (> newheight 0) (send _width_btn_subject height newheight))
                                     (send _width_btn_subject position (list _width_btn_px0 _width_btn_py0 _width_btn_pz0))
                                     #f
                                     ))

;-----------------------------------------------------------------
(def _height_btn_x0)
(def _height_btn_y0)
(def _height_btn_px0)
(def _height_btn_py0)
(def _height_btn_pz0)
(def _height_btn_xul)
(def _height_btn_yul)
(def _height_btn_width0)
(def _height_btn_height0)
(def _height_btn_bbox0)
(def _height_btn_layer)
(def _height_btn_saveanchor)
(def _height_btn_subject)

(send height_btn bind "<Select-ButtonPress-1>" (lambda (w e)
                                                 (set! _height_btn_subject (car (reverse (send dynapad selected))))
                                                 (set! _height_btn_bbox0 (send _height_btn_subject bbox))
                                                 ; force object to "nw" anchor
                                                 (set! _height_btn_px0 (b0 _height_btn_bbox0))
                                                 (set! _height_btn_py0 (b3 _height_btn_bbox0))
                                                 (set! _height_btn_pz0 (caddr (send _height_btn_subject position)))
                                                 (set! _height_btn_saveanchor (send _height_btn_subject anchor))
                                                 (send _height_btn_subject re-anchor "nw")
                                                 (send _height_btn_subject position (list _height_btn_px0 _height_btn_py0 _height_btn_pz0))
                                                 (set! _height_btn_width0 (bbwidth _height_btn_bbox0))
                                                 (set! _height_btn_height0 (bbheight _height_btn_bbox0))
                                                 (set! _height_btn_layer (send (event-obj e) layer))
                                                 (set! _height_btn_x0 (event-x e))
                                                 (set! _height_btn_y0 (event-y e))

                                                 #f
                                                 ))
(send height_btn bind "<Select-ButtonRelease-1>" (lambda (w e)
                                                   (send _height_btn_subject re-anchor _height_btn_saveanchor) #f))
(send height_btn bind "<Shift-Select-ButtonRelease-1>" (lambda (w e)
                                                         (send _height_btn_subject re-anchor _height_btn_saveanchor) #f))

(send height_btn bind "<B1-Motion>" (lambda (w e)
                                      (def dx)
                                      (define dy (- _height_btn_y0 (event-y e)))
                                      (define newwidth (+ _height_btn_width0 dx))
                                      (define newheight (+ _height_btn_height0 dy))
                                      (if (> newwidth 0) (send _height_btn_subject width newwidth))
                                      (if (> newheight 0) (send _height_btn_subject height newheight))
                                      (send _height_btn_subject position (list _height_btn_px0 _height_btn_py0 _height_btn_pz0))
                                      #f
                                      ))

;-----------------------------------------------------------------
(def _zoomview_btn_x0)
(def _zoomview_btn_y0)
(def _zoomview_btn_z0 1)
(def _zoomview_btn_xe)
(def _zoomview_btn_ye)
(def _zoomview_btn_lastx)
(def _zoomview_btn_lasty)
(def _zoomview_btn_sy)

(send zoomview_btn bind "<Select-ButtonPress-1>" (lambda (w e)
                                                   (let ((pos (send dynapad view)))
                                                     (set! _zoomview_btn_x0 (car pos))
                                                     (set! _zoomview_btn_y0 (cadr pos))
                                                     (set! _zoomview_btn_z0 (caddr pos))
                                                     )
                                                   (set! _zoomview_btn_xe (event-x e))
                                                   (set! _zoomview_btn_ye (event-y e))
                                                   (set! _zoomview_btn_lastx _zoomview_btn_x0)
                                                   (set! _zoomview_btn_lasty _zoomview_btn_y0)

                                                   (set! _zoomview_btn_sy (event-sy e))

                                                   #f
                                                   ))

(send zoomview_btn bind "<B1-Motion>" (lambda (w e)
                                        (let* ((newsy (event-sy e))
                                               (relz (expt 10.0 (/ (- _zoomview_btn_sy newsy) 100.0)))
                                               (newz (* _zoomview_btn_z0 relz))
                                               (xp 0)
                                               (yp 0)
                                               )
                                          (set! xp (+ _zoomview_btn_xe (/ (- _zoomview_btn_x0 _zoomview_btn_xe) relz)))
                                          (set! yp (+ _zoomview_btn_ye (/ (- _zoomview_btn_y0 _zoomview_btn_ye) relz)))
                                          (send dynapad moveto (list xp yp newz))
                                          )
                                        (update-halo-layout (event-x e) (event-y e))

                                        #f ; prevent further event calls
                                        ))

;-----------------------------------------------------------------
(def _zoom_btn_subject)
(def _zoom_btn_x0)
(def _zoom_btn_y0)
(def _zoom_btn_z0)
(def _zoom_btn_sy)
(def _zoom_btn_saveanchor)

(send zoom_btn bind "<Select-ButtonPress-1>" (lambda (w e)
                                               (set! _zoom_btn_subject (car (send dynapad selected)))
                                               (set! _zoom_btn_saveanchor (send _zoom_btn_subject anchor))
                                               (send _zoom_btn_subject re-anchor "center")
                                               (let* ((pos (send (car (send dynapad selected)) position)))
                                                 (set! _zoom_btn_x0 (car pos))
                                                 (set! _zoom_btn_y0 (cadr pos))
                                                 (set! _zoom_btn_z0 (caddr pos))
                                                 (set! _zoom_btn_sy (event-sy e))
                                                 (when (more-than-one-selected?)
                                                   (let ((bbox (bbunion-objects (send dynapad selected))))
                                                     (set! _zoom_btn_x0 (car (bbcenter bbox)))
                                                     (set! _zoom_btn_y0 (cadr (bbcenter bbox)))
                                                     )
                                                   )
                                                 #f
                                                 )
                                               ))
(send zoom_btn bind "<Select-ButtonRelease-1>" (lambda (w e)
                                                 (send _zoom_btn_subject re-anchor _zoom_btn_saveanchor)))

(send zoom_btn bind "<B1-Motion>" (lambda (w e)
                                    (let* ((newsy (event-sy e))
                                           (newz (expt 10.0 (/ (- _zoom_btn_sy newsy) 100.0)))
                                           (refz (caddr (send _zoom_btn_subject position)))
                                           (sclz (/ (* _zoom_btn_z0 newz) refz))
                                           )
                                      (send-selected scale sclz _zoom_btn_x0 _zoom_btn_y0)
                                      (update-halo-layout)
                                      )
                                    #f
                                    ))

;-----------------------------------------------------------------
(def _move_btn_x0)
(def _move_btn_y0)
(send move_btn bind "<Select-ButtonPress-1>" (lambda (w e)
                                               (set! _move_btn_x0 (event-x e))
                                               (set! _move_btn_y0 (event-y e))
                                               #f
                                               ))

(send move_btn bind "<B1-Motion>" (lambda (w e)
                                    (def newx (event-x e))
                                    (def newy (event-y e))
                                    (send-selected slide (- newx _move_btn_x0) (- newy _move_btn_y0))
                                    (set! _move_btn_x0 (event-x e))
                                    (set! _move_btn_y0 (event-y e))
                                    ))

(send move_btn bind "<ButtonRelease-1>" (lambda (w e)
                                          (def newx (event-x e))
                                          (def newy (event-y e))
                                          (send-selected slide (- newx _move_btn_x0) (- newy _move_btn_y0))
                                          (set! _move_btn_x0 (event-x e))
                                          (set! _move_btn_y0 (event-y e))
                                          ))

;-----------------------------------------------------------------
(def _moveview_btn_sx0)
(def _moveview_btn_sy0)
(def _moveview_btn_vx0)
(def _moveview_btn_vy0)
(def _moveview_btn_vz0)
(send moveview_btn bind "<Select-ButtonPress-1>" (lambda (w e)
                                                   (set! _moveview_btn_sx0 (event-sx e))
                                                   (set! _moveview_btn_sy0 (event-sy e))
                                                   (let ((vu (send dynapad view)))
                                                     (set! _moveview_btn_vx0 (car vu))
                                                     (set! _moveview_btn_vy0 (cadr vu))
                                                     (set! _moveview_btn_vz0 (caddr vu))
                                                     )
                                                   #f
                                                   ))

(send moveview_btn bind "<B1-Motion>" (lambda (w e)
                                        (send dynapad moveto (list
                                                              (- _moveview_btn_vx0 (/ (- (event-sx e) _moveview_btn_sx0) _moveview_btn_vz0))
                                                              (- _moveview_btn_vy0 (/ (- (event-sy e) _moveview_btn_sy0) _moveview_btn_vz0))
                                                              _moveview_btn_vz0
                                                              ))
                                        ))

;--- menu and object menu buttons --------------------------------

(dynaload "menu_popup.rkt")
(def show-popup-function (show-popup-lambda dynapad (make-popup-server)))

(send menu_btn bind "<Select-ButtonRelease-1>" (lambda (w e)
                                                 (show-popup-function (make-popup-menu #f) (event-sx e)(event-sy e))
                                                 ))

(def _objmenu_btn_subject #f)
(send objmenu_btn bind "<Select-ButtonRelease-1>" (lambda (w e)
                                                    (set! _objmenu_btn_subject (car (reverse (send dynapad selected))))
                                                    (show-popup-function (make-popup-menu _objmenu_btn_subject) (event-sx e)(event-sy e))
                                                    ))

