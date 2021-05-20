#lang racket/base

; a note here about the cryptic error problem
; override field inherit-field super
; the kind of errors that occurs when one of those names
; is not bound is quite cryptic because they are treated
; differently during class definition, this happens in
; other situations as well, but these are the clearest
; they usually show up as a name error when the missing
; name fundamentally changes how some later part of the
; program is interpreted -- this is similar to the issue
; that I asked Matthew about wrt syntax-parse, so I'm not
; sure that there is a good solution to cases like this
; other than perhaps checking whether a form is on a list
; of "you probably meant to require this ..." which is
; undesireable, but might work from a usability prespective

(require racket/class
         compatibility/defmacro
         dynapad/base
         dynapad/dynapad-c-api
         dynapad/misc/misc
         dynapad/misc/alist
         dynapad/misc/filenames
         dynapad/layout/bbox
         (for-syntax racket/base)
         (only-in dynapad/libdynapad-wrapper
                  sch_removemember
                  sch_addmember
                  sch_divisible 
                  sch_members
                  )
 )

(provide base-formation%
         name-part
         )

(define-macro (name-part field-name method-name)
  `(begin
     (field (,field-name #f))
     (define/public ,method-name
       (case-lambda
         (() ,field-name)
         ((new) (when ,field-name (send this remove-part ,field-name))
                (when new (send this add-part new))
                (set! ,field-name new))))
     (send this part-names
           (append (send this part-names)
                   (list ',method-name))) ))

(define (part? obj)
  (assq 'part (send obj alist)))

(define base-formation%
  (class base-group%
    ;    (inherit-field _dynapad)
    (init dynaptr);  (set! _dynapad dynaptr)
    (inherit dynaclass)
    (super-instantiate (dynaptr)) ; (_dynapad (sch_makegroup (send _dynapad get-cptr) this)))
    (sch_divisible cptr #t)
    (dynaclass 'base-formation%)
    (inherit-field cptr)
    (field (_part-names null))
    (field (_highlight_object #f))
    (override writeoptions delete)

    (define/public (highlight bool) #f) ; override this

    (field (_within_fnc #f))
    (define/public set-within-fnc (get/set _within_fnc))
    (define/public (xy-within? x y)
      (if _within_fnc
          (_within_fnc x y)
          (bbenclosed x y (send this bbox))))

    (define/public part-names (get/set _part-names))
    (define/public (named-parts)
      (map (lambda (meth) (eval `(send ,this ,meth))) _part-names))

    (define (mark-part obj)
      (get-else-push-onto-malist! assq (list 'part) obj alist))
    (define (unmark-part obj)
      (get-and-rem-from-malist! assq remq 'part obj alist))

    (define/public (add-part obj)
      (mark-part obj)
      (sch_addmember cptr (send obj get-cptr)))
    (define/public (remove-part obj)
      (unmark-part obj)
      (when (not (send obj deleted?))
        (sch_removemember cptr (send obj get-cptr))))
    (define delete
      (case-lambda
        (()
         ;delete callbacks may remove members before they're deleted,
         ; so need to apply callbacks (normally triggered in (super delete))
         ; BEFORE deleting members:
         (for-each (lambda (cb-fn-pair) ((car cb-fn-pair) this))
                   (send this delete-callbacks))
         ;already done, so clear them
         (send this delete-callbacks null)
         ;finally, delete remaining members + this
         (for-each (lambda (o) (send o delete)) (sch_members cptr))
         (super delete))
        ((partname)
         (let ((part (send this partname)))
           (send this partname #f)
           (send part delete)))))

    (define/public (disband)
      (sch_members cptr null)
      (delete)  ; FIXME this might be (super delete) someone missed super-delete -> delete during the v299 diff
      #t)

    (define (writeoptions)
      `(,@(super writeoptions)
        ,@(map (lambda (o) (send this write-part o)) (send this part-names))))

    (define/public (write-part name)
      (let ((obj (eval `(send ,this ,name))))
        `(,name ,(and obj (send obj write)))))

    (define/public (member? o)
      (eq? this (send o getgroup)))

    (define/public divisible
      (case-lambda
        (() (sch_divisible cptr))
        ((bool) (sch_divisible cptr bool))))

    (define/public separable
      (case-lambda
        ((memb) (and (send this member? memb)
                     (send memb takegroupevents)))
        ((memb bool) (when (send this member? memb)
                       (send memb takegroupevents bool)))))

    (field (_rigid-parts null)) ;subset of parts which ignore reshape: bbox, width, height, etc
    (define/public rigid
      (case-lambda
        ((part) (memq part _rigid-parts))
        ((part bool)
         (let ((already (rigid part)))
           (if bool
               (unless already
                 (push! part _rigid-parts))
               (when already
                 (set! _rigid-parts (remq part _rigid-parts))))))))

    ; THIS MAY BE TOO BOLD...
    ;redefs of reshaping methods:
    (define/override bbox
      (case-lambda
        (() (super bbox))
        ((bb)
         (let ((nonrigids (list-diff (named-parts) _rigid-parts memq)))
           (foreach nonrigids
                    (lambda (part) (and part (send part bbox bb))))))))
    (define/override width
      (case-lambda
        (() (super width))
        ((w)
         (let ((nonrigids (list-diff (named-parts) _rigid-parts memq)))
           (foreach nonrigids
                    (lambda (part) (and part (send part width w))))))))
    (define/override height
      (case-lambda
        (() (super height))
        ((h)
         (let ((nonrigids (list-diff (named-parts) _rigid-parts memq)))
           (foreach nonrigids
                    (lambda (part) (and part (send part height h))))))))
    ))
