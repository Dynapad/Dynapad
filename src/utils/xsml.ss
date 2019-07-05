(dynaload "alist.ss")

;=================================================================
;  xsml = extendable scheme markup language
;  - a general purpose data structure
;    that is like XML in form but like scheme in syntax

; xtree := (tagname attributes
;             element
;             element
;             element
;             ...
;          )
;
; attributes := ((attr-name value) (attr-name value) (attr-name value) ...)
;
; element := xtree     ; an xtree is an element.


(define (attr-list xtree) (cadr xtree))
(define (elem-list xtree) (cddr xtree))

(define (has-tag xtree tagname)
  (equal? (car xtree) tagname)
)

(define (get-xtree-attr xtree attr)
  (assq attr (attr-list xtree))
)
(define (get-xtree-attr-value xtree attr . fail)
  (set! fail (if (pair? fail) (car fail) #f))
  (let ((val (assq attr (attr-list xtree))))
    (if val (cadr val) fail)))

(define (has-xtree-attrval xtree attr val)
  (let ((p (assq attr (attr-list xtree))))
    (if (and p (equal? val (cadr p))) p #f)
  )
)

(define (clear-xtree-attrlist xtree)
  (set-car! (cdr xtree) ()))

(define (set-xtree-attrval xtree attr val)
  (set-car! (cdr xtree) (set-alist-keyval (attr-list xtree) attr val)))

(define (rem-xtree-attr xtree attr)
  (set-car! (cdr xtree) (rem-alist-key (attr-list xtree) attr)))

(define (add-xtree-element xtree element)
  (set-mcdr! (cdr xtree) (reverse (cons element (reverse (cddr xtree))))))

(define (rem-xtree-element xtree element)
  (set-mcdr! (cdr xtree) (remove element (cddr xtree))))

(define (get-xtree-elements-by-fnc xtree match-fnc)
  (if (not xtree)
    ()
    (filter (lambda (elt) (match-fnc elt)) (elem-list xtree))
  )
)

(define (get-xtree-elements-by-tag xtree tagname)
  (if (not xtree)
    ()
    (filter (lambda (elt) (eq? (car elt) tagname)) (elem-list xtree))
  )
)

(define (get-xtree-elements-by-tag-attr xtree tagname attr)
  (if (not xtree)
    ()
    (filter (lambda (elt) (and (eq? (car elt) tagname) (get-xtree-attr elt attr))) (elem-list xtree))
  )
)

(define (get-xtree-elements-by-tag-attrval xtree tagname attr val)
  (let ((ret ()))
    (foreach (elem-list xtree) (lambda (elt)
      (if (and (eq? (car elt) tagname) (has-xtree-attrval elt attr val))
	(set! ret (append ret (list elt))))))
    ret
  )
)

(define (has-xtree-element xtree fnc)
  (call/cc (lambda (return)
    (foreach (elem-list xtree) (lambda (elt)
      (if (fnc elt) (return elt))))
    #f
    )))

(define (make-xtree tagname . attr-val-pairs)
  (define xtree (list tagname ()))
  (foreach attr-val-pairs (lambda (av)
    (set-xtree-attrval xtree (car av) (cadr av))))
  xtree)

;--- functions for xtrees in situ on dynaobjects -----------------

(define (get-xtree obj)
  (let ((xtree (assq 'xatree (send obj alist))))
    (if xtree (cadr xtree) #f)))

(define (xadd-object-attrval obj attr val)
  (define xtree (get-xtree obj))
  (if (not xtree) (set! xtree (list 'xatree ())))
  (set-xtree-attrval xtree attr val)
  (set-object-keyval obj 'xatree xtree)
)

(define (xrem-object-attrval obj attr)
  (define xtree (get-xtree obj))
  (when xtree
    (rem-xtree-attr xtree attr)
    (set-object-keyval obj 'xatree xtree)
  )
)

(define (xget-object-attrval obj attr . fail_value)
  (define xtree (get-xtree obj))
  (if xtree
    (let ((val (get-xtree-attr xtree attr)))
      (if val (cadr val) (if (null? fail_value) #f (car fail_value))))
    ;else
    #f
  )
)

(define (xadd-object-element obj element)
  (define xtree (get-xtree obj))
  (if (not xtree) (set! xtree (list 'xatree ())))
  (add-xtree-element xtree element)
  (set-object-keyval obj 'xatree xtree)
  element
)

(define (xget-object-element-attrval obj tagname attr val)
  (define xtree (get-xtree obj))
  (if (not xtree)
    #f
    (let ((e 0)(elts ())(found #f))
      (set! elts (elem-list xtree))
      (if (null? elts) #f
	(do ((i 0 (+ 1 i))) ((or found (>= i (length elts))) (if found e #f))
	  (set! e (list-ref elts i))
	  (when (and (eq? (car e) tagname)
		   (has-xtree-attrval e attr val))
	    (set! found #t))
	)
      )
    )
  )
)

(define (xrem-object-element obj elem)
  (define xtree (get-xtree obj))
  (if xtree 
    (rem-xtree-element xtree elem))
)

(define (x-removematching-object-element-by-fnc obj proc)
  (define xtree (get-xtree obj))
  (if (not xtree)
    #f
    (let ((e 0)(elts ())(remlist ())(ret #f))
      (set! elts (elem-list xtree))
      (if (null? elts)
        #f
        (begin
          (foreach elts
            (lambda (e)
              (if (proc e) (set! remlist (cons e remlist)))))
          (foreach remlist
            (lambda (e)
              (set! elts (remove e elts))))
          (set-mcdr! (cdr xtree) elts)
          (not (null? remlist))
	)
      )
    )
  )
)

;-----

(define (prettyprint-xsml tree filename)
  (if (file-exists? filename) (delete-file filename))
  (call-with-output-file filename
    (lambda (fp) (prettyprint-xsml-aux fp "" tree))
  )
)
(define (prettyprint-xsml-aux fp indent tree )
  (let ((newindent (string-append "  " indent))(needindent #f))
      (display indent fp)
      (display "(" fp)

      ; write the tag of this tree
      (write (car tree) fp)

      ; write the attributes of this tree
      (if (<= (length (cadr tree)) 3)
	(begin (display " " fp) (write (cadr tree) fp))
	;else put attributes in a column
	(begin
	  (newline fp)
	  (display newindent fp)
	  (display "(" fp)
	  (newline fp)
	    (foreach (cadr tree)
	      (lambda (att)
		(display newindent fp)
		(display "  " fp)
		(write att fp)(newline fp)
	      )
	    )

	  (display newindent fp)
	  (display ")" fp)
	  (newline fp)
	  (set! needindent #t)
	)
      )

      ; write the elements of this tree
      (if (not (null? (cddr tree)))
	(begin
	  (newline fp)  ; conclude attributes line
	  (foreach (cddr tree)
	    (lambda (element)
	      (prettyprint-xsml-aux fp newindent element)))
	  (set! needindent #t)
	)
      )
      (if needindent (display indent fp))
      (display ")" fp)
      (newline fp)
  )
)

;-----
(define (xhelp)
  (printf "(attr-list xtree)\n")
  (printf "(elem-list xtree)\n")
  (printf "(has-tag xtree tagname)\n")
  (printf "(get-xtree-attr xtree attr)\n")
  (printf "(has-xtree-attrval xtree attr val)\n")
  (printf "(clear-xtree-attrlist xtree)\n")
  (printf "(set-xtree-attrval xtree attr val)\n")
  (printf "(rem-xtree-attr xtree attr)\n")
  (printf "(add-xtree-element xtree element)\n")
  (printf "(rem-xtree-element xtree element)\n")
  (printf "(get-xtree-elements-by-fnc xtree match-fnc)\n")
  (printf "(get-xtree-elements-by-tag xtree tagname)\n")
  (printf "(get-xtree-elements-by-tag-attr xtree tagname attr)\n")
  (printf "(get-xtree-elements-by-tag-attrval xtree tagname attr val)\n")
  (printf "(has-xtree-element xtree fnc)\n")
  (printf "(make-xtree tagname . attr-val-pairs)\n")
  (printf "--------------------------------\n")
  (printf "(set-object-keyval obj key val)\n")
  (printf "(rem-object-keyval obj key)\n")
  (printf "(set-alist-keyval alist key val)\n")
  (printf "(rem-alist-key alist key)\n")
  (printf "(get-xtree obj)\n")
  (printf "(xadd-object-attrval obj attr val)\n")
  (printf "(xadd-object-element obj element)\n")
  (printf "(xget-object-element-attrval obj tagname attr val)\n")
  (printf "(xrem-object-element obj elem)\n")
  (printf "(x-removematching-object-element-by-fnc obj proc)\n")
  (printf "--------------------------------\n")
  (printf "(prettyprint-xsml tree filename)\n")
  (printf "(prettyprint-xsml-aux fp indent tree )\n")
  (printf "--------------------------------\n")
  (printf "(make-feature ftype . args)\n")
  (printf "(add-feature-to-obj obj feature)\n")
  (printf "(get-features-of-obj obj match-fnc)\n")
  (printf "(has-object-feature obj match-fnc)\n")
  (printf "(show-features-of-obj obj)\n")
  (printf "(remove-feature-from-obj fptr obj)\n")
  (printf "(add-object-to-category obj categname)\n")
  (printf "(object-in-category? obj categname)\n")
  (printf "(rem-object-from-category obj categname)\n")
  (printf "(find-objects-in-category categname object_list)\n")
)

;--- element collections -----------------------------------------

(define (clear-element-collection obj typename)
  (x-removematching-object-element-by-fnc obj (lambda (e) (eq? (car e) typename))))

(define (make-element-collection obj typename)
  (clear-element-collection obj typename)
  (xadd-object-element obj (list typename ())))

(define (add-element-to-collection pcollection element)
  (add-xtree-element pcollection element)
  element)

(define (get-element-collection obj typename)
  (define xtree (get-xtree obj))
  (and xtree (get-xtree-elements-by-tag xtree typename)))

;--- end element collections -------------------------------------

;=================================================================

; example usage

; (make-feature 'categ '(name book) `(time ,(current-seconds)))
(define (make-feature ftype . args)
  (list 'feature (cons `(type ,ftype) args)))

; (add-feature-to-obj obj (make-feature 'anno '(text "good picture.")))
(define (add-feature-to-obj obj feature)
  (xadd-object-element obj feature)
  feature
)

; (get-features-of-obj obj (lambda (elt) (and (eq? (car elt) 'feature)
;                                             (has-xtree-attrval elt 'type 'anno))))
(define (get-features-of-obj obj match-fnc)
  (let ((xtree (get-xtree obj)))
    (if (not xtree)
        ()
	;else
	(begin
	  (set! xtree xtree)
          (get-xtree-elements-by-fnc xtree match-fnc)
	)
    )
  )
)

; (has-object-feature obj (lambda (elt) (and (eq? (car elt) 'feature)
;                                             (has-xtree-attrval elt 'type 'anno))))
(define (has-object-feature obj match-fnc)
  (let ((xtree (get-xtree obj)))
    (if xtree
      (has-xtree-element xtree match-fnc)
      ;else
      #f
    )
  )
)

(define (show-features-of-obj obj)
  (let ((flist (get-features-of-obj obj (lambda (elt) (eq? (car elt) 'feature)))))
    (foreach flist
      (lambda (f) (printf "~a~%" f))
    )
  )
)

(define (remove-feature-from-obj fptr obj)
  (xrem-object-element obj fptr)
)

; build category functions on top of feature functions

; (add-object-to-category obj 'mostly_white)
(define (add-object-to-category obj categname)
  (if (string? categname) (set! categname (string->symbol categname)))
  (if (not (object-in-category? obj categname))
    (add-feature-to-obj obj (make-feature 'categ `(name ,categname))))
)

; (object-in-category? obj 'mostly_white)
(define (object-in-category? obj categname)
  (if (string? categname) (set! categname (string->symbol categname)))
  (has-object-feature obj (lambda (elt) (and (eq? (car elt) 'feature)
                                              (has-xtree-attrval elt 'type 'categ)
                                              (has-xtree-attrval elt 'name categname))))
)

(define (get-categories-of-obj obj)
  (get-features-of-obj obj
    (lambda (elt) (and (eq? (car elt) 'feature)
		       (has-xtree-attrval elt 'type 'categ))))
)

(define (show-categories-of-obj obj)
  (define flist (get-categories-of-obj obj))
  (foreach flist (lambda (f) (printf "~a~%" f)))
)

; (rem-object-from-category obj 'mostly_white)
(define (rem-object-from-category obj categname)
  (if (string? categname) (set! categname (string->symbol categname)))
  (let ((fptr (object-in-category? obj categname)))
    (remove-feature-from-obj fptr obj)
  )
)

; (find-objects-in-category 'mostly_white (send dynapad objects))
(define (find-objects-in-category categname object_list)
  (filter
    (lambda (obj)
      (has-object-feature obj
         (lambda (elt) (and (eq? (car elt) 'feature)
                            (has-xtree-attrval elt 'type 'categ)
                            (has-xtree-attrval elt 'name categname))))
    )
    object_list
  )
)







