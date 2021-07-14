#lang racket/base
(require (only-in racket/class
                  class
                  init
                  inherit
                  super-instantiate
                  make-object
                  define/override
                  ; things needed when name-part expands
                  field
                  define/public
                  send
                  this
                  )
         (only-in racket/path file-name-from-path filename-extension)
         dynapad/utils/formation
         dynapad/layout/bbox
         dynapad/misc/misc
         dynapad/bind
         dynapad/pad-state
         (only-in dynapad/base rect% polygon% group%)
         (only-in dynapad/container container-form%)
         (only-in dynapad/image baseimage% image% imagedata% pdf%)
         (only-in dynapad/events/text text%)
         collects/misc/pathhack
         )

(define icon-container-form%
  (class container-form%
    (init dynaptr)

    (inherit dynaclass)
    (super-instantiate (dynaptr))
    (dynaclass 'icon-container%)

    (name-part _image_obj image-obj)
    (name-part _title_obj title-obj)
    ))


(define icon%
  (class icon-container-form%
    (init dynaptr
          _init-path  ;;; must point to an image file
          (_init-title #f)
          (_init-position #f))

    (super-instantiate (dynaptr))
    (send this dynaclass 'icon%)
    (inherit image-obj title-obj)

    ;; Do some checking of title.
    (when (not  _init-title)
      (set! _init-title (file-name-from-path _init-path)))

    ;; Make the image object
    (if (file-exists? _init-path)
        (image-obj (make-object image% dynaptr _init-path))
        (say "ERROR PATH IS DEAD"))
    (send (image-obj) position (send dynaptr view))

    ;; Make the text object
    (title-obj (make-object text% dynaptr _init-title))
    (send (title-obj) width (send (image-obj) width))
    (send (title-obj) anchor "nw")
    (let ((bb (send (image-obj) bbox)))
      (send (title-obj) xy (b0 bb) (+ (b1 bb) 3)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;; METHODS ;;;;;;;;;;;;;;;;;
    (define/public title
      (case-lambda
        (()    (send (title-obj) text))
        ((arg) (send (title-obj) text arg))))

    (define/public (hires)
      (send (image-obj) hires))

    (define/public (thumb)
      (send (image-obj) thumb))
    ))


(define fancy-icon%
  (class icon%
    (init dynaptr
          _init-path
          (_init-title #f)
          (_init-position #f))

    (inherit image-obj title-obj)

    (inherit dynaclass)
    (super-instantiate (dynaptr _init-path _init-title _init-position))
    (dynaclass 'fancy-icon%)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Methods to manipulate stuff
    ;;
    ;; Hide parts of the icon
    (define/public (hide-icon)
      (send (image-obj) transparency 0))

    (define/public (hide-title)
      (send (title-obj) transparency 0))

    (define/public (hide-all)
      (hide-icon)
      (hide-title))

    ;; Show parts of the icon
    (define/public (show-icon)
      (send (image-obj) transparency 1))

    (define/public (show-title)
      (send (title-obj) transparency 1))

    (define/public (show-all)
      (move-title-under-image)
      (show-icon)
      (show-title))

    (define/public (show-title-only)
      (define xyc (send (image-obj) xy))
      (hide-icon)
      (show-title)
      (send (title-obj) anchor "c")
      (send (title-obj) xy xyc))

    ;; Move parts of the icon
    (define/public (move-title-under-image)
      (define bb (send (image-obj) bbox))
      (send (title-obj) anchor "n")
      (send (title-obj) xy (bxc bb) (+ (b1 bb) 3)))

    (define/public (move-title-to-right-of-image)
      (define bb (send (image-obj) bbox))
      (define zfac (send this z))
      (send (title-obj) anchor "w")
      (send (title-obj) xy (+ (b2 bb) zfac) (byc bb)))

    ;; Change mode of parts of the icon
    (define/public (small-mode)
      (render-icon-small-mode)
      (move-title-to-right-of-image))

    (define/public (tile-mode)
      (render-icon-tile-mode)
      (move-title-under-image))

    (define/public (render-icon-tile-mode . args)
      (define scl (if (null? args) 125 (car args)))
      (define zfac (send this z))
      (define maxsize (max (bbheight (send (image-obj) bbox))
                           (bbwidth (send (image-obj) bbox))))
      (send (image-obj) scale (* (/ scl maxsize) zfac)))

    (define/public (render-icon-small-mode)
      (define height (bbheight (send (image-obj) bbox)))
      (define zfac (send this z))
      (send (image-obj) scale (* (/ 21 height) zfac) ))

    )
  )

;;------- Etienne's buttons ----------------------------

(define base-button%
  (class icon%
    (init dynaptr
          _init-path
          (_init-title #f)
          (_init-position #f))

    (inherit image-obj title-obj)

    (field (_execute (lambda () #f)))

    (inherit dynaclass)
    (super-instantiate (dynaptr _init-path _init-title _init-position))
    (dynaclass 'base-button%)

    ;; Set the bindings for the object
    (send this divisible #f)
    (bind this "<Enter>"                  (lambda (o e) (enter)))
    (bind this "<Leave>"                  (lambda (o e) (leave)))
    (bind this "<Run-ButtonPress-1>"      (lambda (o e) (press)   #t))
    (bind this "<Run-ButtonRelease-1>"    (lambda (o e) (release) #t))

    ;; The execute placeholder method
    (define/public execute
      (case-lambda
        ((l) (set! _execute l))
        (()  (_execute))))

    (define/public (enter) #t)

    (define/public (leave) #t)

    (define/public (press) #t)

    (define/public (release)
      (send this execute))
    ))

(define fancy-button%
  (class base-button%
    (init dynaptr
          _init-path
          (_init-title #f)
          (_init-position #f))

    (inherit dynaclass)
    (super-instantiate (dynaptr _init-path _init-title _init-position))
    (dynaclass 'fancy-button%)

    (name-part _button-obj button-obj)

    ;; Make the button object, set it to transparent for now
    (button-obj (make-object rect% dynaptr (send this bbox)))
    (send (button-obj) transparency 0)
    (send (button-obj) fill "none")

    (define/override (enter)
      (send (button-obj) pen "blue")
      (send (button-obj) transparency 1))

    (define/override (leave)
      (send (button-obj) transparency 0))

    (define/override (press)
      (send (button-obj) pen "red"))

    (define/override (release)
      (send (button-obj) pen "blue")
      (send this execute))
    ))




;;-------- file-icons for directory browser ---------------
(define bounded-icon%
  (class icon-container-form%
    (init dynaptr
          _init_path  ;;; must point to an image file
          (_init_title #f)
          (_init_position #f))

    (inherit image-obj title-obj dynaclass)
    (super-instantiate (dynaptr))
    (dynaclass 'bounded-icon%)

    (name-part _boundary boundary)

    ;; Imagedata may exist globally..
    (let ;((id (make-object imagedata% dynaptr _init_path)))
        ((id (get-id (file-name-from-path _init_path))))
      (image-obj (make-object baseimage% dynaptr id _init_position)))

    ;; Make the title
    (let ((bb (send (image-obj) bbox)))
      (title-obj (make-object text% dynaptr _init_title
                              (list (bxc bb) (b1 bb) (* (send this z) 0.75))
                              "Helvetica" "n")))

    ;; Set title properties
    (let ((w (send (title-obj) width))
          ;(maxw (* (send (image-obj) width) 1.25)))
          (maxw 64))
      (when (> w maxw) (send (title-obj) width maxw)))

    ;; Creates a boundary around icon
    (let* ((bb (send (image-obj) bbox))
           (xc (bxc bb)) (yc (byc bb))
           (dx 32) (dy1 36) (dy2 21))
      (boundary (ic (make-object rect% dynaptr
                                 (list (- xc dx) (- yc dy1)
                                       (+ xc dx) (+ yc dy2)))
                    ;(transparency 0)
                    )))
    (send (boundary) lower)

    ;; Add bindings
    (send this divisible #f)
    (bind this "<Enter>" (lambda (o e) (enter)))
    (bind this "<Leave>" (lambda (o e) (leave)))
    ;   (bind this "<ButtonPress-1>" (lambda (o e) (buttonpress-1)))
    ;   (bind this "<ButtonRelease-1>" (lambda (o e) (buttonrelease-1)))
    ;   (bind this "<Select-ButtonPress-1>" (lambda (o e) (buttonpress-1)))
    ;   (bind this "<Select-ButtonRelease-1>" (lambda (o e) (buttonrelease-1)))

    (define/public (enter) #f)

    (define/public (leave) #f)

    (define/public (buttonpress-1) #f)

    (define/public (buttonrelease-1) #f)

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
    (when (not  _init_title)
      (set! _init_title (file-name-from-path _init_file_path)))

    ))

;; --Button icons for control panel
(define button-icon%
  (class bounded-icon%
    (init dynaptr
          _init_path  ;;; must point to an image file
          (_init_title #f)
          (_init_position #f))

    (field (_btn_fnc (lambda() #f))) ;; button function

    (inherit image-obj title-obj boundary dynaclass)
    (super-instantiate (dynaptr _init_path _init_title _init_position))
    (dynaclass 'button-icon%)

    (define/public btn-fnc (get/set _btn_fnc))

    (define/override (enter) (send (boundary) transparency .25))
    (define/override (leave) (send (boundary) transparency 0))
    (define/override (buttonpress-1) (send (boundary) fill "red"))
    (define/override (buttonrelease-1) (send (boundary) fill "black")(_btn_fnc))

    ;;-- Temporary hack for configuration ---
    (send (title-obj) delete)

    (let ((bb (send (boundary) bbox)))
      (send (boundary) bbox (list (+ (b0 bb) 12)  (+ (b1 bb) 16)
                                  (- (b2 bb) 12) (b3 bb))))

    (bind this "<ButtonPress-1>" (lambda (o e) (buttonpress-1)))
    (bind this "<ButtonRelease-1>" (lambda (o e) (buttonrelease-1)))

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




;;===================================================================
;;-- Global functions (obsolete?) for icons -------------------------

(define (is-directory? fullpath) (directory-exists? fullpath))

(define (has-extension filename ext)
  (regexp-match (regexp (string-append ".*" "\\." ext "$")) filename))

(define regexp_dotdot (regexp ".*\\.\\.$"))

(define (get-filetype fullpath)
  (cond
    ((is-directory? fullpath)
     (if (regexp-match regexp_dotdot fullpath)
         'dotdot
         'directory))
    ((has-extension fullpath "jpg") 'jpg)
    ((has-extension fullpath "gif") 'gif)
    ((has-extension fullpath "png") 'png)
    ((has-extension fullpath "pdf") 'pdf)
    ((has-extension fullpath "txt") 'txt)
    ((has-extension fullpath "ss") 'scheme)
    ((has-extension fullpath "scm") 'scheme)
    ((has-extension fullpath "cpp") 'cpp)
    ((has-extension fullpath "h") 'h)
    (else (get-mime-type fullpath)) ))

(define (get-mime-type fullpath)
  'unknown
  )

(define (build-thumbnail path pos . args)
  (define small? (if (null? args) #f (car args)))
  (define filetype (get-filetype path))
  (cond
    ((equal? filetype 'dotdot) (generate-dotdot-icon pos))
    ((equal? filetype 'directory) (generate-directory-icon pos))
    ((equal? filetype 'jpg) (generate-jpg-icon path pos small?))
    ((equal? filetype 'gif) (generate-gif-icon path pos small?))
    ((equal? filetype 'png) (generate-png-icon path pos small?))
    ((equal? filetype 'pdf) (generate-pdf-icon path pos small?))
    ((equal? filetype 'txt) (generate-txt-icon pos))
    ((equal? filetype 'scheme) (generate-code-icon pos))
    ((equal? filetype 'cpp) (generate-code-icon pos))
    ((equal? filetype 'h) (generate-code-icon pos))
    (else (generate-default-icon pos 'unknown)))
  )

(define (generate-image-icon path pos)
  (if (file-exists? path)
      (make-object image% currentPAD path pos) #f))

(define (generate-jpg-icon path pos small?)
  (if small?
      (generate-default-icon pos 'jpg)
      (generate-image-icon path pos)))

(define (generate-gif-icon path pos small?)
  (if small?
      (generate-default-icon pos 'gif)
      (generate-image-icon path pos)))

(define (generate-png-icon path pos small?)
  (if small?
      (generate-default-icon pos 'png)
      (generate-image-icon path pos)))

(define (generate-pdf-icon path pos small?)
  ; (if small? (generate-default-icon pos 'pdf))
  (unless small?
    (let ((obj #f))
      (cond
        ((file-exists? path)
         (set! obj (make-object pdf% currentPAD path pos))
         (if (send obj hirespath) obj
             (begin
               (send obj delete)
               #f)))
        (else #f))))
  )

(define (generate-txt-icon pos)
  (define bb (bbslide '(-62 -62 62 62) (car pos) (cadr pos)))
  (ic (make-object polygon% currentPAD
                   (fitpoly bb '(0.11 1 0.11 0 0.89 0 0.89 0.89 0.78 1)))
      (fill (get-filetype-color 'txt))))

(define (generate-code-icon pos)
  (define bb (bbslide '(-62 -62 62 62) (car pos) (cadr pos)))
  (ic (make-object polygon% currentPAD
                   (fitpoly bb '(0.11 1 0.11 0 0.89 0 0.89 0.89 0.78 1)))
      (fill (get-filetype-color 'code))))

(define (generate-default-icon pos filetype)
  (define bb (bbslide '(-62 -62 62 62) (car pos) (cadr pos)))
  (ic (make-object rect%  currentPAD bb)
      (fill (get-filetype-color filetype))))

(define (generate-dotdot-icon pos)
  (define bb (bbslide '(-62 -62 62 62) (car pos) (cadr pos)))
  (ic
   (make-object group% currentPAD
                (list
                 (ic (make-object polygon% currentPAD
                                  (fitpoly bb '(0.49 0.18 0.6 0.26 0.63 0.39
                                                     0.64 0.5 0.59 0.61 0.56
                                                     0.68 0.65 0.72 0.43 0.81
                                                     0.36 0.58 0.46 0.62 0.5
                                                     0.51 0.52 0.36)))
                     (fill (get-filetype-color 'dotdot)))
                 (ic (make-object polygon% currentPAD
                                  (fitpoly bb '(0 0.92  0.08 1  0.32 1  0.4 0.92
                                                  0.96 0.92  1 0.88  1 0  5f0 0)))
                     (fill (get-filetype-color 'directory))) ))
   (divisible #f) ))

(define (generate-directory-icon pos)
  (define bb (bbslide '(-62 -62 62 62) (car pos) (cadr pos)))
  (ic (make-object polygon% currentPAD
                   (fitpoly bb '(0 0.92  0.08 1  0.32 1  0.4 0.92
                                   0.96 0.92  1 0.88  1 0  0 0)))
      (fill (get-filetype-color 'directory))))


(define (fitpoly bb coords)
  (let ((x0 (b0 bb)) (y0 (b1 bb))
                     (xs (bbwidth bb)) (ys (bbheight bb))
                     (newcrds '()))
    (while (> (length coords) 1)
      (endpush! newcrds (+ x0 (* xs (car coords))))
      (endpush! newcrds (+ y0 (* ys (cadr coords))))
      (set! coords (cddr coords)))
    newcrds))

(define filetype-color-alist '((directory "#fff888") (dotdot "#44bb00")
                                                     (jpg "#2222ff") (gif "#2222ff") (png "#2222ff")
                                                     (pdf "#ff2200") (txt "#eeeeff") (code "gray")
                                                     (unknown "black")))

(define (get-filetype-color filetype)
  (let ((a (assoc filetype filetype-color-alist)))
    (if a (cadr a) "black")))


