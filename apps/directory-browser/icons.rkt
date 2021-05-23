(require (lib "file.rkt"))  ; needed for "file-name-from-path"

(define icon-container-form%
  (class base-formation%
    (init dynaptr)

    (inherit dynaclass)
    (super-instantiate (dynaptr))
    (dynaclass 'icon-container%)

    (name-part _image_obj image-obj)
    (name-part _title_obj title-obj)
    ))


(define bounded-icon%
  (class icon-container-form%
    (init dynaptr
          _init-path  ;;; must point to an image file
          (_init-title #f)
          (_init-position #f))
    (inherit-field _dynapad)

    (inherit image-obj title-obj dynaclass)
    (super-instantiate (dynaptr))
    (dynaclass 'bounded-icon%)

    (name-part _boundary boundary)

    ;; Store the title-obj max width
    (field (_text-width 0))

    ;; Imagedata fields
    (field (_icon-data #f)     ;; Icon imagedata
           (_preview-data #f)) ;; Preview imagedata (actual graphic if it exists)

    ;; Event handling placeholders
    (field (_enter (lambda() #f))
           (_leave (lambda() #f))
           (_double-click (lambda() #f)))

    ;; Get icon imagedata%
    (set! _icon-data (get-id (file-name-from-path _init-path)))
    (image-obj (make-object baseimage% _dynapad _icon-data _init-position))

    ;; Make the title
    (let ((bb (send (image-obj) bbox)))
      (title-obj (make-object text% dynaptr _init-title
                              (list (bxc bb) (b1 bb) (* (send this z) 0.75))
                              "Helvetica" "n")))

    ;; Set title properties
    (set! _text-width (send (title-obj) width))
    (if (> _text-width 64) (send (title-obj) width 64))

    ;; Create a boundary around icon
    (let* ((bb (send (image-obj) bbox))
           (xc (bxc bb)) (yc (byc bb))
           (dx 32) (dy1 36) (dy2 21))
      (boundary
       (make-object rect% dynaptr (list (- xc dx) (- yc dy1)
                                        (+ xc dx) (+ yc dy2)))))


    (send (boundary) lower)

    ;; Add bindings
    (send this divisible #f)
    (bind this "<Enter>" (lambda (o e) (enter)))
    (bind this "<Leave>" (lambda (o e) (leave)))
    (bind this "<Double-ButtonPress-1>" (lambda (o e) (double-click)))


    ;; -- Event handling methods --
    (define/public enter
      (case-lambda
        (() (_enter))
        ((m) (set! _enter m))))

    (define/public leave
      (case-lambda
        (() (_leave))
        ((m) (set! _leave m))))

    (define/public double-click
      (case-lambda
        (() (_double-click))
        ((m) (set! _double-click m))))


    ;; -- Public Methods --
    (define/public (draw-icon)
      (if (not (image-obj))
          (let ((bbx (bxc (send (title-obj) bbox)))
                (bby (b3 (send (title-obj) bbox))))
            (image-obj (make-object baseimage% _dynapad _icon-data
                                    (list bbx (+ bby 16) (send this z)))))))


    (define/public (draw-preview)
      (when _preview-data
        (erase-icon)
        (image-obj (make-object baseimage% _dynapad _preview-data (send this position)))))

    (define/public (erase-icon)
      (let ((_icon (image-obj)))
        (image-obj #f)
        (if _icon (send _icon delete))))

    (define/public (title-only)
      (erase-icon)
      (send (title-obj) width (* _text-width (send this z)))
      (send (boundary) coords (bbwiden (send (title-obj) bbox) (* 2 (send this z)))))

    ; -------------
    ;(draw-icon)

    ))


;;Do not use make-object... instead use the make-file-icon constructor method below
(define file-icon%
  (class bounded-icon%
    (init dynaptr
          _init_file_path ;;can point to any existing file
          _init_path  ;;; must point to an image file
          (_init_title #f)
          (_init_position #f))

    (field (_path _init_file_path))

    (define/public (path) _path)

    (inherit image-obj title-obj boundary dynaclass)
    (super-instantiate (dynaptr _init_path _init_title _init_position))
    (dynaclass 'file-icon%)

    ;; Set title if necessary
    (if (not  _init_title)
        (set! _init_title (file-name-from-path _init_file_path)))

    ))

;; ---------- imagedata% objects ------------
(define icon-path (build-path->string *dynapad-directory* "apps/directory-browser/icons"))

(define id_image (make-object imagedata% dynapad (build-path->string icon-path "image.png")))
(define id_pdf (make-object imagedata% dynapad (build-path->string icon-path "pdf.png")))
(define id_txt (make-object imagedata% dynapad (build-path->string icon-path "txt.png")))
(define id_source_cpp (make-object imagedata% dynapad (build-path->string icon-path "source_cpp.png")))
(define id_source_c (make-object imagedata% dynapad (build-path->string icon-path "source_c.png")))
(define id_source_h (make-object imagedata% dynapad (build-path->string icon-path "source_h.png")))
(define id_source_o (make-object imagedata% dynapad (build-path->string icon-path "source_o.png")))
(define id_unknown (make-object imagedata% dynapad (build-path->string icon-path "unknown.png")))
(define id_folder (make-object imagedata% dynapad (build-path->string icon-path "folder.png")))
(define id_up (make-object imagedata% dynapad (build-path->string icon-path "up.png")))
(define id_top (make-object imagedata% dynapad (build-path->string icon-path "top.png")))

(define name-id-alist
  (list (list "image.png" id_image)
        (list "pdf.png" id_pdf)
        (list "txt.png" id_txt)
        (list "source_cpp.png" id_source_cpp)
        (list "source_c.png" id_source_c)
        (list "source_h.png" id_source_h)
        (list "source_o.png" id_source_o)
        (list "unknown.png" id_unknown)
        (list "folder.png" id_folder)
        (list "up.png" id_up)
        (list "top.png" id_top)
        ))

(define (get-id name)
  (let ((a (assoc name name-id-alist)))
    (if a (cadr a) #f)))

;; ---------- extension assoc. list  ------------
(define ext-path-alist
  '(("jpg" "image.png")
    ("gif" "image.png")
    ("png" "image.png")
    ("pdf" "pdf.png")
    ("txt" "txt.png")
    ("ss"  "txt.png")
    ("scm" "txt.png")
    ("cpp" "source_cpp.png")
    ("c" "source_c.png")
    ("h" "source_h.png")
    ("o" "source_o.png")))

;; ---------- file-icon constructor ------------
(define (make-file-icon file_path . arg)
  (let* ((pos (if (null? arg) #f (car arg)))
         (name (file-name-from-path file_path))
         (ext (filename-extension file_path))
         (a (assoc ext ext-path-alist))
         (img_path (if a (build-path->string icon-path (cadr a))
                       (build-path->string icon-path "unknown.png")))
         )
    (when (not ext)
      (if (directory-exists? file_path)
          (set! img_path (build-path->string icon-path "folder.png"))
          (set! img_path (build-path->string icon-path "unknown.png"))))
    (make-object file-icon% currentPAD file_path img_path name pos)))
