#lang racket/base

(require racket/class
         compatibility/mlist
         dynapad/ffs
         dynapad/misc/misc
         dynapad/misc/alist
         )

(provide actor%
         named-actor%
         send-actor
         get-actor-named
         )

; Consider using surrogate.ss to replace actor construction

;--- actor baseclass ---------------------------------------------

(define actor%
  (class linkable-obj%
    (field (_object #f))
    (field (_alist null))
    (public object attach-to remove-from)
    (define object (case-lambda (() _object) ((newval) (set! _object newval))))
    (define (attach-to padobject)
      (send padobject add-actor this)
      (send this object padobject))
    (define (remove-from padobject)
      (send padobject actors (remove this (send padobject actors))))

    (define/public alist (get/set _alist))
    (super-instantiate ())))

;--- msg broadcast function --------------------------------------

;result convention:
; #f if no actors respond, or
; list of results from e. responding actor [e.g. (()), (#f), (blah #f)...]
(define (send-actor obj msg . args)
  (let ((result null))
    (for-each
     (lambda (actor)
       (when (method-in-interface? msg (object-interface actor))
         (if (null? args)
             (set! result (cons (eval `(send ,actor ,msg)) result))
             (set! result
                   (cons
                    (eval `(send/apply ,actor ,msg ',args))
                    result)))))
     (send obj actors))
    (if (null? result)
        #f
        result)))


;--- Named actors ------

;A named-actor can be used when client knows that an obj
; has one or more actors which accept a certain message.
;Each actor is bound to its object by one or more names,
; and multiple actors can share a name.
;Named actors are stored in the object's alist (by name), not actor-list,
; and called directly with (send-actor-named obj name msg...)
; instead of (send obj msg...).
;All named-actors bound under that name will respond.
;Names must be SYMBOLS (or otherwise eq? compatible)

(define named-actor%
  (class actor%
    (super-instantiate ())
    (field (_names null)
           (_delete-callbacks null))

    (define/public delete-callbacks (callback-accessor-functions _delete-callbacks))

    (define/override (attach-to padobject name)
      (let ((curr-obj (send this object)))
        (push! name _names)
        (when (not curr-obj)
          (begin
            (send this object padobject)
            (send padobject delete-callbacks 'add  ;auto-attach delete callback
                  (lambda (obj) (send this delete))
                  name)))
        (pushq-onto-malist-val-always! name this padobject alist)))
    ;         (if (not (pushq-onto-malist-val!
    ;               name this padobject alist))
    ;         (remote-push! (list name this) padobject alist))))

    (define/public (link-to padobject name)
      ;linking is "light attachment": obj refers to actor, but
      ;  actor does not refer to obj (and therefore does not get callbacks)
      (pushq-onto-malist-val-always! name this padobject alist))

    (define/override (remove-from padobject name)
      (remq-from-malist-val! name this padobject alist))

    (define/override (delete)
      ;removes references to actor for GCing
      (let ((obj (send this object)))
        (for-each (lambda (name) (remove-from obj name))
                  _names)
        (set! _names null)
        (exec-any-callbacks _delete-callbacks this)
        ))
    ))

(define (get-actors-named obj name)
  ; changed 10/05: should always return list --> null if no such actors
  (let ((results (assq name (send obj alist))))
    (if results (cdr results)
        null)))
;    (if (and results (not (null? (cdr results))))
;    (cdr results)
;    #f)))

(define (get-actor-named obj name)
  (let ((results (assq name (send obj alist))))
    (if (and results (not (null? (cdr results))))
        (cadr results)
        #f)))

;(define (send-actors-named obj name msg . args)
;  (let ((them (get-actors-named obj name)))
;    (if them
;    (if (null? args)
;        (map (lambda (it) (eval `(send ,it ,msg))) them)
;        (map (lambda (it) (eval `(send/apply ,it ,msg ',args))) them))
;    #f))) ;none with that name

(define-syntax send-actors-named
  ;always returns list: results of msg from each actor; null if no such actors
  (syntax-rules ()
    ((_ obj name msg args ...)
     (let ((actors (get-actors-named obj name)))
       (map (lambda (a) (send a msg args ...))
            actors)))))


;(define (send-actor-named obj name msg . args)
;  (let ((result (apply send-actors-named obj name msg args)))
;    (if result
;    (car result)
;    #f)))

(define-syntax send-actor-named
  ; blindly assumes only one actor; sends msg to first;
  ; returns result, and #f if no such actor
  (syntax-rules ()
    ((_ obj name msg args ...)
     (let ((actor (get-actor-named obj name)))
       (and actor
            (send actor msg args ...))))))

(define-syntax senda ;parallels sendf, defined in misc.ss
  ; (sendf obj <msg> msg ...) vs (senda obj <name-symbol> msg...)
  (syntax-rules ()
    ((_ obj name msg args ...)
     (send-actor-named obj name msg args ...))))

;--- Expiring actors -----

; Expiring actors automatically act (e.g. die) if not "kicked"
;  for some time.

(define expiring-actor%
  (class named-actor%
    (super-instantiate ())
    (field (_expiration #f))

    (define/public expiration
      (case-lambda
        (() _expiration)
        ((val) (set! _expiration val))))

    (define/public (kick until rel-time)
      ;One of until, rel-time is #f; use the other
      ;When actor is kicked, will run until *at least* _expiration
      ; (after which it might choose to expire, depending on subclass)
      (when (not until)
        (set! until (+ (current-milliseconds) rel-time)))
      (when (or (not _expiration) (> until _expiration))
        (set! _expiration until)))

    (define/public maybe-expire
      (case-lambda
        ;when this is checked, takes action if past _expiration
        ((action) ;use current time
         (maybe-expire action (current-milliseconds)))
        ((action now)
         (if (and _expiration (> now _expiration))
             (eval action)
             #f))))
    ))


