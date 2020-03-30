#lang racket/base

(require (only-in racket/class make-object)
         mzlib/etc
         ;dynapad/image
         dynapad/pad-state
         dynapad/layout/composite  ; alte might be dynapad/image-utils/arrangeimages.rkt depending on load order
         collects/misc/pathhack
         )

;============= file/subdir filters ======
;these 3 taken from arrangeimages.ss:
(define pdf_rexp (regexp "(.*[/\\])?(.*)(\\.([pP][dD][fF]))$"))
(define *pdf_rexp* pdf_rexp)
(define *image_rexp* (regexp "\\.((jpe?g)|(JPE?G)|(gif)|(GIF))$"))


(define *import-file-filters* (list *image_rexp* *pdf_rexp*))
(define *thumbs_dir_rexp* (regexp "^thumbs$"))
(define *hidden_dir_rexp* (regexp "^[.]"))
(define *dont-import-subdir-rexps* (list *thumbs_dir_rexp* *hidden_dir_rexp*))
(define *import-subdir-rexp* #f) ;set to either #f or *dont-import-subdir-rexps*
; list of rexps excludes them


(define (regexp-match-or-dont rexp str)
  ; if rexp is a regexp, tries to match str
  ; else if list, tries *not* to match str to any rexp in list;
  ;   matching any returns #f
  (define (match-fn rexp)
    (regexp-match rexp str))
  (if (list? rexp)
      (not (ormap match-fn rexp))
      (match-fn rexp)))
;      (not (regexp-match-or-dont (car rexp) str))
;      (regexp-match rexp str)))

(define (gather-dirs dir subdir-rexp . file-rexps)
  ; Returns nested lists of files:
  ; car of any list is full directory name,
  ; cdr elements are filenames *relative* to that dir
  ; EX: ("dir" ("dir/subdir" "file0" "file1") "file2" "file3")
  ;  full filenames for a list may be found with
  ;  (map (lambda (name) (build-path->string (car lst) name)) (cdr lst))
  ; Results are filtered by regexps:
  ;  subdirs must match subdir-rexp (no recursion if subdor-rexp is #f);
  ;  files must match one of file-rexps (but always match if no file-rexps)
  ; If any rexp is a (list rexp) instead, negates it
  (let* ((local-names (directory-list->string dir))
         ;     (subdir-mask (map (lambda (name)
         ;                 (let ((fullname (build-path->string dir name)))
         ;                   (directory-exists? fullname)))
         ;               local-names))
         (keep (map (lambda (name)
                      (let ((fullname (build-path->string dir name)))
                        (if (directory-exists? fullname)
                            ; potential subdirectory
                            (and subdir-rexp
                                 (regexp-match-or-dont subdir-rexp name)
                                 (apply gather-dirs fullname subdir-rexp file-rexps))
                            ; else file
                            (if (or (null? file-rexps)
                                    (ormap (lambda (rexp) (regexp-match-or-dont rexp name)) file-rexps))
                                name
                                #f))))
                    local-names)))
    (cons dir (filter identity keep))))



;(define (import-and-prearrange-tree subdirs arrange-fn make-one-fn)
; subdirs is nested lists of filenames (as generated by gather-dirs)
; (flatten-fn files) possible recstructures subdirs into new list
; (arrange-fn files) creates sublists of (x y z) coords isomorphic to (flatten..)
; (make-one-fn file coords) creates one obj from file
; Returns nested sublists of objs

(define (flatten-list lst)
  (if (list? lst)
      (apply append (map flatten-list lst))
      (list lst)))

(define (cdr-mass lst)
  ;counts total # of non-first atoms within lst
  (if (list? lst)
      (apply + (map cdr-mass (cdr lst)))
      1))


;these may be redefined for alternative wrappings
(define (wrap-directory dir . args)
  dir)

(define (add-to-wrapper parent obj)
  #f)

(define (parent-wrapper-dir parent)
  parent)

(define (dummy-obj . arg)
  #f)

(define file->obj #f)
(let ((img-rexp *image_rexp*) ;might want local defn
      (pdf-rexp *pdf_rexp*))
  (set! file->obj
        (lambda (path xyz)
          (and
           (file-exists? path)
           (cond ((regexp-match img-rexp path)
                  (make-object image% dynapad path xyz))
                 ((regexp-match pdf-rexp path)
                  (apply make-pdf-at-position path xyz))
                 (else (dummy-obj path xyz)))))))

(define (maybe-zoom-out-until-fits argPAD dx dy)
  (let* ((min-width  (* 1.5 dx))
         (min-height (* 1.5 dy))
         (bb  (send argPAD bbox))
         (bbw (bbwidth bb))
         (bbh (bbheight bb))
         (xzoom (/ min-width bbw))
         (yzoom (/ min-height bbh))
         (zoom (max xzoom yzoom 1)))
    (send argPAD zoom (/ 1 zoom) 500)))

(define (ask-for-rigid-grid-and-do len dx dy grid-callback . args)
  ; grid-callback is (lambda (bbox cols rows . args) ...)
  ;   where dx, dy are grid spacing
  (let* ((cols (round (sqrt len)))
         (rows (ceiling (/ len cols)))
         (width (* dx (- cols 1)))
         (height (* dy (- rows 1))))
    (maybe-zoom-out-until-fits dynapad width height)
    (ask-user-for-bbox dynapad (lambda (bb) (apply grid-callback bb cols rows args))
                       width height)))

(define (flatten-and-arrange-gathered-dirs dirs)
  (let* ((len (cdr-mass dirs))
         ;     (coord-queue (precompute-grid-coords len 200 200))
         (dx 200) ;for now...
         (dy 200))
    ; ask user for bbox of layout:
    (unless (zero? len)
      (ask-for-rigid-grid-and-do len dx dy
                                 flatten-and-arrange-gathered-dirs-in-grid dx dy dirs)
      ; alternative: automatically center around default origin (0,0)
      ;(let ((coord-queue (precompute-grid-coords len dx dy)))
      ;  (flatten-and-arrange-gathered-dirs-using-coords dirs coord-queue))
      )))

(define (flatten-and-arrange-gathered-dirs-using-coords dirs coord-queue)
  (define wrap-subdir #f)
  (set! wrap-subdir
        (lambda (dirs . args)
          (if (list? dirs)
              ; directory
              (let* ((wrapper (apply wrap-directory (car dirs) args))
                     (leaves (map (lambda (f) (wrap-subdir f wrapper))
                                  (cdr dirs))))
                (cons wrapper leaves))
              ; file
              (let* ((parent (and (not (null? args)) (car args)))
                     (name dirs)
                     (fullname (build-path->string (parent-wrapper-dir parent) name))
                     (coords (pop! coord-queue))
                     (new (file->obj fullname coords)))
                (add-to-wrapper parent new)
                ;(push! new new-objs)
                new))))
  (undoify-fresh-objs (flatten-list (wrap-subdir dirs))))

(define (flatten-and-arrange-gathered-dirs-in-grid bbox cols rows dx dy dirs)
  (thread
   (lambda ()
     (show-possible-delay dynapad
                          (let* ((x0 (b0 bbox))
                                 (y0 (b3 bbox))
                                 (z 1)
                                 (coord-queue (precompute-NxM-grid-coords-at-xyz cols rows dx dy x0 y0 z)))
                            (flatten-and-arrange-gathered-dirs-using-coords dirs coord-queue))))))

;=============== GUI/Menus ==============
(define (Select-Directory)
  (let ((dir (get-directory (if *import-subdir-rexp*
                                "Import Directories Below..."
                                "Import Directory...") #f *default_directory* null)))
    (if (path? dir) (path->string dir)
        dir)))

(define (Import-Flatten-and-Arrange-Dirs)
  (let* ((dir (Select-Directory)))
    (and dir
         (show-possible-delay dynapad
                              (let ((subdirs (apply gather-dirs dir *import-subdir-rexp* *import-file-filters*)))
                                (flatten-and-arrange-gathered-dirs subdirs)))
         )))

(define (checkable-filetype-item sb title rexp)
  (add-checkable-menu-item sb title
                           (lambda (i) (if (send i is-checked?)
                                           (push! rexp *import-file-filters*)
                                           (set! *import-file-filters*
                                                 (remq rexp *import-file-filters*))
                                           ))
                           (memq rexp *import-file-filters*)))

(define (checkable-subdirtype-item sb title rexp)
  (add-checkable-menu-item sb title
                           (lambda (i) (if (not (send i is-checked?))
                                           (push! rexp *dont-import-subdir-rexps*)
                                           (set! *dont-import-subdir-rexps*
                                                 (remq rexp *dont-import-subdir-rexps*))
                                           ))
                           (not (memq rexp *dont-import-subdir-rexps*))))


(define (make-submenu-Import mb obj)
  (let* ((sb (add-submenu mb "Import")))
    (add-menu-item sb "Import Workspace..."  Select-and-Import-File)
    (add-menu-item sb "Import Directory..."
                   (lambda () (set! *import-subdir-rexp* #f)
                           (Import-Flatten-and-Arrange-Dirs)))
    (add-menu-item sb "Import Dirs Below..."
                   (lambda () (set! *import-subdir-rexp*
                                    *dont-import-subdir-rexps*)
                           (Import-Flatten-and-Arrange-Dirs)))

    ;    (add-menu-item sb "Import Directory..."
    ;           Import-Flatten-and-Arrange-Dirs)
    ;    (add-checkable-menu-item sb "Include Subdirs"
    ;         (lambda (i) (if (send i is-checked?)
    ;                 (set! *import-subdir-rexp* *not-thumbs-rexp*)
    ;                    ;recurse unless "thumbs" subdir
    ;                 (set! *import-subdir-rexp* #f) ;dont recurse
    ;                 ))
    ;         *import-subdir-rexp*)
    (add-menu-separator sb) ;------------------------------
    (checkable-filetype-item sb "Include *.jpg/gif" *image_rexp*)
    (checkable-filetype-item sb "Include *.pdf" *pdf_rexp*)
    (add-menu-separator sb) ;------------------------------
    (checkable-subdirtype-item sb  "Include hidden dirs" *hidden_dir_rexp*)
    (checkable-subdirtype-item sb  "Include thumb dirs" *thumbs_dir_rexp*)
    ))

; ========= Modified from arrangeimages.ss:
;  This is obsolete but still called by menubar.
; So as a temp hack, set file filters to include only pdfs
; and redirect it to Import-Flatten-and-Arrange-Dirs

(define (Arrange-Pdfs)
  (set! *import-file-filters* (list *pdf_rexp*))
  (set! *dont-import-subdir-rexps*
        (list *hidden_dir_rexp* *thumbs_dir_rexp*))
  (Import-Flatten-and-Arrange-Dirs)
  )