#lang racket/base

;(require (lib "process.ss")) ; this was mzlib/process, maybe racket/system has what we need?
(require racket/class
         racket/system
         dynapad/pdf
         dynapad/image
         dynapad/misc/progress
         dynapad/utils/handle
         dynapad/image-utils/metadata
         collects/misc/pathhack
         collects/imagemagick/imagemagick
         collects/pdfrip/pdfrip
         collects/pdfrip/misc
         collects/thumbify/thumbify
         )

(provide image-composite%)

(announce-module-loading "PDF features...")

(define *max-pdf-images* 8)
(define *max-pdf-images-shown* 1)



; never mind; use (make-object imagedata%..)  instead
#|
(define *image-size-regexp* (regexp "^[^ ]+ [A-Z]+ ([0-9]+)x([0-9]+) "))
(define (find-image-size imagepath)
  (let* ((strng (read-line (system (format "identify ~a" imagepath))))
         (match (regexp-match *image-size-regexp* strng)))
    (and match (list (string->number (cadr match))
                     (string->number (caddr match))))))
|#


(define (build-composite-cmd bbox scale img old-file new-file)
  ; constructs system command to composite two images together:
  ;  superimposes img atop old-file, assuming old-file fills bbox
  ;  saves merged image as new-file
  ; (list "-geometry" "<NXN>" "<filename>" ...)
  (list "-geometry"
        (bb-geometry->string
         (bb-geometry 'down scale bbox img) "!")
        ;potentially insert borders/etc here
        (send img hirespath)
        old-file
        new-file))

(define (limit-drag-to-bbox obj bbox-fn union?)
  ;prevents obj from being dragged outside of bbox
  ;  bbox-fn is (lambda (obj) ...) which returns bbox enclosure
  ;  (can change during drag if needed)
  (let ((n-fn (if union? b1 b3))
        (s-fn (if union? b3 b1))
        (e-fn (if union? b0 b2))
        (w-fn (if union? b2 b0)))
    (send obj bind "<B1-Motion>"
          (lambda (eventPAD e)
            (let* ((last_x (sendf eventPAD evs lastx))
                   (last_y (sendf eventPAD evs lasty))
                   (x (event-x e))
                   (y (event-y e)))
              (if (and last_x last_y)
                  (let* ((bb-now (send obj bbox))
                         (bb-limit (bbox-fn obj))
                         (max-up (- (b3 bb-limit) (n-fn bb-now)))
                         (max-lf (- (b0 bb-limit) (w-fn bb-now)))
                         (max-rt (- (b2 bb-limit) (e-fn bb-now)))
                         (max-dn (- (b1 bb-limit) (s-fn bb-now)))
                         (dx (- x last_x))
                         (dy (- y last_y)))
                    (if (negative? dx)
                        (set! dx (max max-lf dx))
                        (set! dx (min max-rt dx)))
                    (if (negative? dy)
                        (set! dy (max max-dn dy))
                        (set! dy (min max-up dy)))
                    (send eventPAD do-duringdrag e)
                    (for-each
                     (lambda (o) (send o slide dx dy))
                     (send eventPAD getvar 'dragset))
                    (sendf eventPAD evs set-last-xy (+ last_x dx) (+ last_y dy)) )
                  (sendf eventPAD evs set-last-xy x y))
              #f ;override existing <Drag-B1-Motion> binding
              )))
    (send obj afterposition-callbacks 'add
          (lambda (obj xyz)
            (let* ((bb (send obj bbox))
                   (cage (bbox-fn obj))
                   (n (b3 cage))
                   (s (b1 cage))
                   (e (b2 cage))
                   (w (b0 cage)))
              (when (< n (n-fn bb)) (send obj bbox (list #f n #f #f))) ;snap to n
              (when (< e (e-fn bb)) (send obj bbox (list e #f #f #f))) ;snap to e
              (when (> s (s-fn bb)) (send obj bbox (list #f #f #f s))) ;snap to s
              (when (> w (w-fn bb)) (send obj bbox (list #f #f w #f))) ;snap to w
              ))
          )))

#|
(define (make-object-resizable obj)
  (let ((border (ic (make-object rect% dynapad (send obj bbox))
                    (fill "none")
                    (pen "white")
                    (transparency .5))))
    (send obj afterslide-callbacks 'add
          (lambda (o dx dy) (send border slide dx dy)))
    (resizer-bindings (send obj dynapad)
                      border
                      (lambda (frame bb) (send obj bbox bb)))))

(define draglimit-container%
  (class frame-container%
    (init dynaptr)
    (init (frame-arg #f))
    (inherit dynaclass)
    (super-instantiate (dynaptr frame-arg))
    (dynaclass 'draglimit-container%)

    (define (get-my-frame-bbox obj)  ;doesnt use obj arg
      (send (send this frame) bbox))

    (define/override (add obj)
      (limit-drag-to-bbox obj get-my-frame-bbox)
      (if (is-a? obj base-formation%)
          (foreach (send obj named-parts)
                   (lambda (prt)
                     (limit-drag-to-bbox prt get-my-frame-bbox))))
      (super add obj))

    (define/override (remove obj)
      (send obj bind "<Drag-B1-Motion>" #f)
      (super remove obj))

    (define/override contents
      (case-lambda
        (() (super contents))
        ((lst) (foreach lst (lambda (o) (limit-drag-to-bbox o get-my-frame-bbox))))))
    ))
|#

#|
(define thumbify-via-mogrify
  ;assumes dir/thumbs/size exists already
  (case-lambda
    ((filepath) (thumbify-via-mogrify filepath 125))
    ((filepath maxsize)
     (set! maxsize (round-to-int maxsize))
     (let-values (((dir name junk) (split-path->string filepath)))
       (let* ((newpath (format "~athumbs/~a/~a" dir maxsize
                               (insert-filenamebase-suffix
                                name (format "-~a" maxsize))))
              (copy-cmd (format "cp ~a ~a" filepath newpath))
              (mogrify-cmd (format "mogrify -format jpg -geometry ~ax~a ~a"
                                   maxsize maxsize newpath)))
         (system copy-cmd)
         (system mogrify-cmd))))))

|#

; no need for above; use build-in split path
; (or: replace with (file-name-from-path...)?)
(define (end-of-path path)
  (let-values (((path end result) (split-path->string path)))
    end))

(define *pdf-subimage-base* "image")
(define *pdf-subimage-rexp* (regexp (format "^(~a.*)" *pdf-subimage-base*)))

(define (imagefile-aspect-ratio file)
  (let* ((w+h (im/identify file))
         (w (car w+h))
         (h (cadr w+h))
         (aspect (if (zero? h)
                     0
                     (/ w h))))
    aspect))

(define (reasonable-aspect? file)
  (let ((aspect (imagefile-aspect-ratio file)))
    (and (< aspect 5)
         (> aspect .2))))

(define (gather-pdf-subimages dir)
  (and (directory-exists? dir)
       (filter (lambda (filename) (regexp-match *pdf-subimage-rexp* filename))
               (map
                path->string
                (filter-and-sort-directory dir file-size >
                                           sch_imagep
                                           (lambda (f) (>= (file-size f) 5000)) ;file must be >5k
                                           reasonable-aspect?
                                           )))
       ;(sort-image-files-by-descending-size dir)))
       ))

(define 1cell-layout (list '(0.2 #f 0.8 0.75))) ;centered below 3/4 hline
(define 2cell-layout (list '(0.3 0.5 0.7 #f) '(0.3 #f 0.7 0.45))) ;N,S
(define 3cell-layout (list '(0 0.4 #f 0.75) '(#f 0.2 1 0.6) '(0 0 #f 0.4))) ;NW,E,SW
(define 4cell-layout (list '(0 0.4 #f 0.75) '(#f 0.4 1 0.75) ;NW,NE,
                           '(0 0 #f 0.4)   '(#f 0 1 0.4)))   ;SW,SE
(define 5cell-layout (cons '(0.3 #f 0.7 0.7) 4cell-layout))
;(define 6cell-layout (list-append (list '() '())
;                  4cell-layout))
(define 1cell-surplus (list '(#f 1 0 1.5)))
(define 2cell-surplus (cons '(1 1 #f 1.5) 1cell-surplus))
;(define 3cell-surplus (cons '(-0.5 1 #f 0) 2cell-surplus))
(define 3cell-surplus (cons '(1 -0.5 #f 0) 2cell-surplus))
(define 4cell-surplus (cons '(#f -0.5 0 0) 3cell-surplus))

(define *subimg-grab* #f)

(define (ensure-pdf obj)
  (cond ((is-a? obj pdf-portrait%) obj)
        ((is-a? obj resizable-image%) (send (get-container obj) getgroup))
        (else #f)))


(define (save-pdf-config pdf) ;pdf is a pdf-portrait
  (let ((dir (send pdf dir)))
    (and (directory-exists? dir)
         (let* ((path (build-path->string dir "config.ss"))
                (file (open-output-file path #:mode 'text #:exists 'truncate))
                (cmd `(ic (make-object pdf-portrait% dynapad
                                       (relpath ,(build-path->string "../"
                                                                     (file-name-from-path (send pdf url)))))
                          (subimages (list ,@(map (lambda (lst) (cons 'list lst))
                                                  (send pdf subimages)))))))
           (write cmd file)
           (close-output-port file)))))

(define make-metadata-menu-for-pdf
  (case-lambda
    ((obj) (make-metadata-menu-for-pdf (new-popup "Document Details")))
    ((obj menu)
     (date-display-format (pad-date-format))
     (make-submenu-DateFormat menu obj)
     (add-menu-item menu (format "File: ~a" (send obj url)) void #f)
     (add-menu-item menu (format "Created: ~a"
                                 (let* ((pair (get-pdf-date obj))
                                        (date (pair->date pair)))
                                   (if date
                                       (date->string date (show-metadata-time?))
                                       "(unknown)"))) void #f)
     (add-menu-item menu (format "Acquired: ~a"
                                 (let* ((pair (get-pdf-filedate obj))
                                        (date (pair->date pair)))
                                   (if date
                                       (date->string date (show-metadata-time?))
                                       "(unknown)"))) void #f)
     (add-menu-item menu "Author" void #f)
     (add-menu-item menu "Title" void #f)
     menu)))

(define (make-menu-for-pdf obj)
  (let ((menu (new-popup "Document Details"))
        (pdf  (ensure-pdf obj))) ;obj itself or pdf containing it
    ;    (when (send obj findable)
    (send pdf select)
    (make-submenu-Edit menu pdf)
    ;(make-submenu-Arrange menu obj) ;dubious...
    (unless (eq? obj pdf)
      (add-menu-item menu "Raise" (lambda () (send obj raise)))
      (add-menu-item menu "Lower" (lambda () (send obj lower))))
    (add-checkable-menu-item menu "Lock Arrangement"
                             (lambda (i) (if (send pdf expanded?)
                                             (send pdf condense)
                                             (send pdf expand)))
                             (not (send pdf expanded?)))
    ;      (add-menu-item menu "Rearrange..."
    ;             (lambda () (send obj expand))
    ;             (not (send obj expanded?)))
    ;      (add-menu-item menu "Flatten..."
    ;             (lambda () (send obj condense))
    ;             (send obj expanded?)))
    ;)
    (add-menu-item menu "View Document..." (lambda () (send pdf view-document)))
    (make-submenu-Select-Highlighted menu pdf)
    (add-menu-separator menu) ;------------------------------
    (make-metadata-menu-for-pdf pdf menu)
    menu))

(update-progress 1)
