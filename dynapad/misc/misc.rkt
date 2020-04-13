#lang racket/base

(require compatibility/mlist
         (only-in racket/class
                  ; SO many cryptic and silent errors during
                  ; macro expansion if certain names are missing WOW
                  send
                  send*
                  method-in-interface?
                  object-interface
                  field
                  define/public
                  )
         dynapad/pad-state)

(provide (all-defined-out))

; "say" is defined here for use in debugging
(define (say . args) (for-each (lambda (x) (printf "~a " x)) args)(printf "~%"))

(define-syntax mpush!
  (syntax-rules ()
    ((_ val mlst)
     (set! mlst (mcons val mlst)))))

(define-syntax mpop!
  (syntax-rules ()
    ((_ mlst)
     (and (not (null? mlst))
          (let ((val (mcar mlst)))
            (set! mlst (mcdr mlst))
            val)))))

(define-syntax push!
  (syntax-rules ()
    ((_ val lst)
     (set! lst (cons val lst)))))

(define-syntax pop!
  (syntax-rules ()
    ((_ lst)
     (and (not (null? lst))
          (let ((val (car lst)))
            (set! lst (cdr lst))
            val)))))

(define (foreach elements body) (for-each body elements))
(define (mforeach elements body) (mfor-each body elements))

(define (false-fn . args) #f)
(define (true-fn . args) #t)

; "(def x)" or "(def x val)"
; used to declare variables (as syntactic alternative to define)
; provides #f as initial value
(define-syntax def
  (syntax-rules ()
    ((_ var) (define var #f))
    ((_ var initval) (define var initval))))

; get/set expands to our standard case-lambda:
; - method with no args returns current value,
; - method with one argument assigns a new value.
(define-syntax get/set
  (syntax-rules ()
    ((_ var_name)
     (case-lambda
       (() var_name)
       ((new_val) (set! var_name new_val))) )))


(define (find pred list)
  (if (null? list)
      #f
      (if (pred (car list))
          (car list)
          (find pred (cdr list)))))
(define (cadr-equal? lst match)
  (if (or (null? lst) (null? (cdr lst))) #f
      (equal? (cadr lst) match)))

;------- callbacks ------
; this macro provides a standard set of callback accessor functions
(define-syntax callback-accessor-functions
  (syntax-rules ()
    ((_ callback_list)
     (case-lambda
       ; 0
       (() callback_list)
       ; 1
       ((new_list)
        (set! callback_list new_list)
        (when (not (list? new_list))
          (set! callback_list '())
          (error "callback must be a list or null -->" new_list)))
       ; 2
       ((cmd new_fnc)
        (when (equal? cmd 'add)
          (when (not (member (list new_fnc) callback_list))
            (push! (list new_fnc) callback_list)))
        (when (equal? cmd 'add-last)
          (when (not (member (list new_fnc) callback_list))
            (endpush! callback_list (list new_fnc))))
        (when (equal? cmd 'remove)
          (set! callback_list (remove (list new_fnc) callback_list))) )
       ; 3
       ((cmd new_fnc name)
        (case cmd
          ((add)
           (push! (list new_fnc name) callback_list))
          ((add-last)
           (endpush! callback_list
                     (list new_fnc name)))
          ((find)
           (find (lambda (x) (cadr-equal? x name)) callback_list))
          ((remove)
           (set! callback_list (apply append
                                      (map
                                       (lambda (p)
                                         (if (and (not (null? (cdr p))) (equal? (cadr p) name))
                                             '() ;
                                             (list p)))
                                       callback_list))))
          ((remq)
           (set! callback_list (apply append
                                      (map
                                       (lambda (p)
                                         (if (and (not (null? (cdr p))) (eq? (cadr p) name))
                                             '() ;
                                             (list p)))
                                       callback_list))))
          )
        )))))

(define-syntax exec-any-callbacks
  (syntax-rules ()
    ((_ callback_list obj ...)
     (when (not (null? callback_list))
       (for-each
        (lambda (callback-fn-pair) ((car callback-fn-pair) obj ...))
        callback_list)) )))
;--- end callbacks ------

; send msg to friend class
(define-syntax sendf
  (syntax-rules ()
    ((_ obj friend msg args ...)
     (send (send obj friend) msg args ...))))
;see also senda (for actors) in actor.ss

(define-syntax friend
  (syntax-rules ()
    ((_ method-name field-name) (friend method-name field-name #f))
    ((_ method-name field-name init)
     (begin
       (field (field-name init))
       (define/public method-name (get/set field-name))))))


(define-syntax public-field
  ; WARNING if field or define/public are not defined
  ; this macro will produce extremely cryptic errors
  ; and say things like method-name not defined
  (syntax-rules ()
    ((_ method-name field-name) (public-field method-name field-name #f))
    ((_ method-name field-name init)
     (begin
       (field (field-name init))
       (define/public method-name (get/set field-name))))))

(define (has-method? obj m)
  (cond ((method-in-interface? m (object-interface obj)) obj) (else #f)))

(define-syntax send-selected
  (syntax-rules ()
    ((_ msg args ...)
     (for-each (lambda (obj) (send obj msg args ...))
               (send currentPAD selected)))))

(define-syntax send-tag
  (syntax-rules ()
    ((_ tag msg args ...)
     (for-each (lambda (obj) (send obj msg args ...))
               (send currentPAD find 'withtag tag)))))

; send if C++ object is not deleted
(define-syntax send-nd
  (syntax-rules ()
    ((_ obj msg args ...)
     (if (not (send obj deleted?))
         (send obj msg args ...)) )))

; send to object list, but only those with non-deleted C++ objects
(define-syntax send-nd-objs
  (syntax-rules ()
    ((_ lst msg args ...)
     (send-nd-objs-aux lst 'msg args ...))))
(define (send-nd-objs-aux lst msg . args)
  (for-each
   (lambda (x)
     (when (not (send x deleted?))
       (eval `(send/apply ,x ,msg ',args))))
   lst))

; cset! returns the value, in the fashion of C assignments  x = (y=42);
(define-syntax cset!
  (syntax-rules ()
    ((_ var val)
     ; in case var is an expression, using another var prevents eval'ing twice
     (let ((id val)) (set! var id) id))))

(define-syntax mlet
  (syntax-rules ()
    ((_ ((vars lst)) body ...)
     (let-values ((vars (apply values lst))) body ...))
    ))

;some helper functions
(define (any-objects?)  (not (null? (send currentPAD objects))))
(define (any-selected?) (not (null? (send currentPAD selected))))
(define (one-selected?) (= (length (send currentPAD selected)) 1))
(define (more-than-one-selected?) (> (length (send currentPAD selected)) 1))
(define many-selected? more-than-one-selected?)

;append a list to the end of a list variable
(define-syntax set-append!
  (syntax-rules ()
    ((_ var lst)
     (set! var (append var lst)))))

;push one atom to the end of a list variable
(define-syntax endpush!
  (syntax-rules ()
    ((_ var val)
     (set! var (append var (list val))))))

; ic = "item config"
; usage:
;       (ic (make-object line% currentPAD)
;           (coords (list x0 y0 x1 y1))
;           (penwidth width)
;           (pen color)
;       )
; returns the object
(define-syntax ic
  (syntax-rules ()
    ((_ obj) obj)
    ((_ obj msgs ...)
     (let ((id obj)) (send* id msgs ...) id))))

; with is like a simplified let:
;  bind the second arg to the symbol in the first, for use within the third,
;  and return the symbols value
;Usage:
;  (with obj (ic (make-object blah% ...) (msg val) (msg val))
;         (do-more obj)
;         (do-still-more obj))   --> obj
(define-syntax with
  (syntax-rules ()
    ((_ obj-label ic-clause also ...)
     (let ((obj-label ic-clause))
       also ... ;ops which may refer to obj-label
       obj-label))))  ;return obj

(define-syntax dotimes
  (syntax-rules ()
    ((_ (index maxval) body ...)
     (do ((index 0 (+ index 1)))
         ((= index maxval))
       body ...))))

(define-syntax while (syntax-rules ()
                       ((_ test body ...)
                        (do ()
                            ((not test))
                          body ...))))

; (define-macro (while cnd . dos)
;   `(letrec
;      ([loop
;        (lambda ()
;      (if ,cnd
;          (begin ,@dos (loop))))])
;      (loop)))

;(define-syntax unless (syntax-rules ()
;  ((_ cnd act) (if (not cnd) act))))

;this should probably move to bbox.ss:
(define center-of-view (case-lambda
                         ((PAD) (let ((l (send PAD view))) (list (car l)(cadr l)(/ 1.0 (caddr l)))))
                         ((PAD x y) (list x y (/ 1.0 (send PAD getzoom))))))


(define-syntax !=
  (syntax-rules ()
    ((_ x y) (not (= x y)))))

; pre-increment/decrement
; future work: consider making these polymorphic, to work for chars, etc
(define-syntax ++
  (syntax-rules ()
    ((_ var)
     (cset! var (+ var 1)))))
(define-syntax ++_
  (syntax-rules ()
    ((_ var)
     (begin (set! var (+ var 1)) var))))
(define-syntax --_
  (syntax-rules ()
    ((_ var)
     (begin (set! var (- var 1)) var))))

(define-syntax +=
  (syntax-rules ()
    ((_ var amount)
     (begin (set! var (+ var amount)) var)) ))

(define-syntax -=
  (syntax-rules ()
    ((_ var amount)
     (begin (set! var (- var amount)) var)) ))

; post-increment/decrement
(define-syntax _++
  (syntax-rules ()
    ((_ var)
     (let ((old var)) (set! var (+ var 1)) old))))

(define-syntax _--
  (syntax-rules ()
    ((_ var)
     (let ((old var)) (set! var (- var 1)) old))))

; #f-safe versions of min and max; treat #f as minimal value
(define (safemax . args)
  (let ((non#f (filter (lambda (x) x) args)))
    (if (null? non#f)
        #f
        (apply max non#f))))

(define (safemin . args)
  (let ((alltrue? (andmap (lambda (x) x) args)))
    (if alltrue?
        (apply min args)
        #f)))

(define (ensure-number str)
  (cond ((not str) 0)
        ((equal? "" str) 0)
        ((string? str) (string->number str))
        (else str)))

(define (ensure-string str)
  (cond ((string? str) str)
        ((number? str) (number->string str))
        ((path? str) (path->string str))
        (else str)))

;use this instead of regexp-match if there's any chance that path is a
; #path obj (plt-299 and later) instead of a string
(define (regexp-match-path rexp path)
  (regexp-match rexp (ensure-string path)))

(define (list-intersect l1 l2 mem-fn)
  (filter (lambda (o) (mem-fn o l2)) l1))
(define (list-intersectq l1 l2)
  (list-intersect l1 l2 memq))
; return members from L that are not in l2
(define (list-diff L l2 mem-fn) (filter (lambda (o) (not (mem-fn o l2))) L))
(define (list-diffq L l2) (filter (lambda (o) (not (memq o l2))) L))

(define (list-head? a b eq-fn)
  ;if a matches head of b, returns tail of b after a
  ;when a=b, returns null
  (cond ((null? a) b)
        ((null? b) #f)
        ((not (eq-fn (car a) (car b))) #f)
        (else (list-head? (cdr a) (cdr b) eq-fn))))

(define (find-layer argPAD name)
  (let ((ret (filter (lambda (lyr) (if (equal? name (send lyr name)) lyr #f))
                     (send argPAD layers))))
    (if (null? ret) #f (car ret))))

(define-syntax getfield
  (syntax-rules ()
    ((_ object class field)
     ((class-field-accessor class field) object)) ))

(define-syntax setfield
  (syntax-rules ()
    ((_ object class field val)
     ((class-field-mutator class field) object val)) ))


; OBSOLETE: subsumed by (send dynapad order objs)
;(define (objects-sorted-back2front dynapad objs . usebb)
;  (let* ((bb (if (null? usebb)
;                 (bbunion-objects objs)
;                 (car usebb)))
;         (local-objs (send dynapad find 'groupmembers 'overlapping bb))
;         (targets (filter (lambda (o) (memq o objs)) local-objs)))
;    targets))

;; moved in from undo.rkt
(define (current-error-ports) ;may be overridden to cc errors elsewhere
  (list (current-error-port)))

;; dynapad-c-api and menu_functions candidate to move to `dynapad/misc/misc'
(define (write-set objs)
  (if (null? objs)
      null
      (append
       (let ((obj (car objs)))
         (and obj
              (send obj write-all)))
       (write-set (cdr objs)))))
