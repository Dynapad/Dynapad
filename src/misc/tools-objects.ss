(require (lib "defmacro.ss"))

;remote-get, -set, -push
; are "remote" in that they can operate through an object interface
; where they can't set! the field/variable directly.
;They work on either lvals (settable variables)
;  or object fields accessed via message:
;   (send obj msg) for getting;
;   (send obj msg val) for setting
(define-macro remote-get
  (case-lambda
   ((var) var)
   ((obj msg) `(send ,obj ,msg))))

(define-macro remote-set!
  (case-lambda
   ((var val)
    `(set! ,var ,val))
   ((obj msg val)
    `(send ,obj ,msg ,val))))

(define-macro (remote-push! atom . lst)
  `(remote-set! ,@lst (cons ,atom (remote-get ,@lst))))

(define-macro remote-append!
  (case-lambda
   ((var-lst tail-lst)
    `(set-append! ,var-lst ,tail-lst))
   ((obj msg lst)
    `(send ,obj ,msg (append (send ,obj ,msg) ,lst)))))

;Accessing fields from outside object (bypassing methods)
(define-macro (getf object class field)    ;access fields of any object
  `((class-field-accessor ,class ,field) ,object))

(define-macro (setf! object class field val) ;set fields of any object
  `((class-field-mutator ,class ,field) ,object ,val))

;Accessing fields from within object
(define-macro (access-1field f1)
  `(case-lambda
     (() ,f1)
     ((n1) (set! ,f1 n1))))

(define-macro (access-2fields f1 f2)
  `(case-lambda
;     (() (values ,f1 ,f2))
    (() (list ,f1 ,f2))
    ((n1 n2)
      (set! ,f1 n1)
      (set! ,f2 n2))))
