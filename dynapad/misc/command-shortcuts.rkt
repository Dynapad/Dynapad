#lang racket/base

(require (only-in racket/class
                  send
                  interface->method-names
                  object-interface
                  )
         (only-in racket/path path-only file-name-from-path)
         (only-in racket/system process*)
         racket/list
         compatibility/defmacro
         (only-in mzlib/file build-absolute-path)
         dynapad/pad-state
         dynapad/misc/misc
         collects/misc/pathhack
         (for-syntax racket/base setup/dirs)
         )

(provide ls-dir
         get-username
         get-hostname
         fs
         )  ; really (all-defined-out)

;-----------------------------------------------------------------
; shortcuts for command line interface
; not for use in dynapad code

(define (say . args)
  (for-each (lambda (x) (printf "~a " x)) args)
  (printf "~%")
  (flush-output))

; find selected
(define (fs) (send currentPAD selected))
(define (selected) (send currentPAD selected))

; find first selected
(define (fs1) (let ((l (send currentPAD selected))) (if (not (null? l)) (car l) #f)))

; find the selected object, if only one
(define (it)
  (define them (send dynapad selected))
  (if (= 1 (length them)) (car them) #f))

(define (all) (send currentPAD objects))

;---
(define (updir path)
  (let-values (((base name dir?) (split-path->string path)))
    base))

(define *onedot-path-rexp* (regexp "^[.]/(.*)"))
(define *twodot-path-rexp* (regexp "^[.][.]/(.*)"))
(define (undot-rel-path base-path rel-path)
  (let ((m1 (regexp-match *onedot-path-rexp* rel-path))
        (m2 (regexp-match *twodot-path-rexp* rel-path)))
    (cond (m1 (undot-rel-path base-path (cadr m1)))
          (m2 (undot-rel-path (updir base-path) (cadr m2)))
          (else (path->complete-path rel-path base-path)))))
; use macro so that (this-expr...source-dir) is eval'd in local context
#; ; this was shadowed by the definition below
(define-macro (relpath path)
  `(undot-rel-path (this-expression-source-directory) ,path))
; perhaps re-implement using (simplify-path ...)?
; as in:
(define-macro (relpath path)
  `(simplify-path (build-path->string (this-expression-source-directory) ,path)))

(define *absolute-path-rexp* (regexp "^/"))
(define (absolute-path dir)
  (if (regexp-match *absolute-path-rexp* dir)
      dir
      (build-absolute-path (current-directory) dir)))

(define (pwd) (current-directory))
(define (cd dir) (current-directory dir))

(define (ls-dir dir . re)                    ; -- version with name matching
  (if (null? re)                             ;  eg. (ls "jpg" "gif" "2.*jpg")
      (directory-list->string dir)
      ;else
      (begin
        (set! re (apply string-append
                        (cons
                         (string-append ".*" (car re) ".*")
                         (map (lambda (r) (string-append "|.*" r ".*")) (cdr re)))))
        (filter (lambda (f) (regexp-match re f)) (directory-list->string dir)))))

(define (ls . res)
  (apply ls-dir (current-directory) res))

;an experiment, not ready for use:
(define (uber-ls . res)         ; -- includes non-local directories, pattern matches
  ;  eg. (ls "jpg" "gif" "2.*jpg")
  (if (null? res)
      (directory-list->string (current-directory))
      ;else
      (apply append
             (map (lambda (re)
                    (let* ((dir (path-only re))
                           (pattern (file-name-from-path re))
                           (absdir (absolute-path dir))
                           (found (directory-list->string absdir)))
                      (filter (lambda (f) (regexp-match pattern f)) found)))
                  res))))

;---

; show-methods that match "RE"
(define (show-method-names obj . arg)
  (let ((re (if (null? arg) #f (regexp (car arg))))
        (names (interface->method-names (object-interface obj))))
    (when re
      (set! names
            (filter
             (lambda (n) (if (regexp-match re (symbol->string n)) n #f))
             names)))
    (foreach (reverse names) (lambda (n) (say n)))))

; show-methods that match RE (no quotes needed)
(define-syntax meths
  (syntax-rules ()
    ((_ obj args ...)
     (if (null? (list 'args ...))
         (show-method-names obj)
         (let ((re (car (list 'args ...))))
           (show-method-names obj (string-append ".*" (symbol->string re) ".*")))))))

;---

(define-syntax getfield
  (syntax-rules ()
    ((_ object class field)
     ((class-field-accessor class field) object)) ))

(define-syntax setfield
  (syntax-rules ()
    ((_ object class field val)
     ((class-field-mutator class field) object val)) ))


;---

(define (get-system-reply cmd . args)
  (let* ((cmd-path (find-executable-path cmd #f))
         (ports (and cmd-path (file-exists? cmd-path)
                     (apply process* cmd-path args)))
         (input-port (and ports (car ports))))
    (and input-port
         (let ((try (read-line input-port)))
           (close-input-port input-port)
           (close-output-port (second ports))
           (close-input-port (fourth ports))
           (and (not (eq? eof try)) try)))
    ))

(define (get-username)
  (get-system-reply "whoami"))

(define (get-hostname)
  (get-system-reply "hostname"))

;  (let* ((hostname-cmd (find-executable-path "hostname" #f))
;         (ports (and hostname-cmd (file-exists? hostname-cmd)
;                     (process* hostname-cmd)))
;         (input-port (and ports (car ports))))
;    (and input-port
;         (let ((try (read-line input-port)))
;           (close-input-port input-port)
;           (close-output-port (second ports))
;           (close-input-port (fourth ports))
;           (and (not (eq? eof try)) try)))
;))

; TODO: this was "plthome.rkt", is this a safe choice of replacement..?
#; ; overwritten by the version below
(define-syntax (this-expression-source-path stx)
  (syntax-case stx ()
    [(_)
     (let* ([source (syntax-source stx)]
            [local (lambda ()
                     (or (current-load-relative-directory)
                         (current-directory)))]
            [dir (plthome-ify
                  (or (and source (string? source) (file-exists? source)
                           (let-values ([(base file dir?) (split-path->string source)])
                             (and (string? base)
                                  (path->complete-path
                                   base
                                   (or (current-load-relative-directory)
                                       (current-directory))))))
                      (local)))])
       (if (and (pair? dir) (eq? 'plthome (car dir)))
           (with-syntax ([d dir])
             (syntax (un-plthome-ify 'd)))
           (datum->syntax-object (quote-syntax here) source stx)))]))

;(define-syntax (this-expression-source-path stx)
;   (syntax-case stx ()
;     [(_) (let ((thispath (syntax-source stx)))
;            (datum->syntax-object (quote-syntax here) thispath stx))]))

;terser version:
(define-syntax (this-expression-source-path stx)
  (datum->syntax stx (syntax-source stx)))


