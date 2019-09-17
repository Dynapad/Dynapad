(require (lib "defmacro.ss"))
(load-relative "tools-objects.ss")

(define (multiple? lst)
  (and (list? lst)
       (not (null? lst))
       (not (null? (cdr lst)))))

;these complement built-in append!:
;(define-macro (set-append! var lst)
;  `(set! ,var (append ,var ,lst)))

;(define-macro (push! atom lst)
;  `(begin
;     (set! ,lst (cons ,atom ,lst))
;     ,lst))

(define-macro (prepend! pre lst)
  `(cset! ,lst (append ,pre ,lst)))

;(define-macro pop!
;pops first (n) item(s) off list and returns it
;  (case-lambda
;   ((lst)
;    `(let ((it (car ,lst)))
;       (set! ,lst (cdr ,lst))
;       it))
;   ((n lst)
;    `(let ((them (list-head ,lst ,n)))
;       (set! ,lst (list-tail ,lst ,n))
;       them))))

; (define-macro pop!
;   (case-lambda
;    ((lst)
;     `(let ((it (car ,lst)))
;        (set! ,lst (cadr ,lst))
;        it))
;    ((n lst)
;     `(if (zero? ,n)
;      null
;      (let ((brk (list-tail ,lst (- ,n 1)))
;            (head ,lst))
;        (set! ,lst (cdr brk))
;        (set-mcdr! brk null)
;        head)))))

(define-macro chop!
;chops last (n) item(s) off list and returns it/them
  (case-lambda
   ((lst)  ;return single item
    `(let* ((len (- (length ,lst) 1))
        (it (list-ref ,lst len)))
       (set! ,lst (list-head ,lst len))
       it))
   ((n lst)  ;return list (singleton if n=1)
    `(let* ((len (- (length ,lst) ,n))
        (them (list-tail ,lst len)))
       (set! ,lst (list-head ,lst len))
       them))))

(define (list-diff l1 l2 mem-fn)
  (filter (lambda (it) (not (mem-fn it l2))) l1))
(define (list-diffq l1 l2) (list-diff l1 l2 memq))
(define (list-diffv l1 l2) (list-diff l1 l2 memv))

;(define (also-in-list l1 l2 mem-fn)
;  (map (lambda (it) (mem-fn it l2)) l1))
;(define (list-intersect+diff l1 l2 mem-fn)
;  (let ((also-ins (also-in-list l1 l2 mem-fn)))
;    (list
;     (filter (lambda (l1-item in-l2?) in-l2?)
;         l1 also-ins)
;     (filter (lambda (l1-item in-l2?) (not in-l2?))
;         l1 also-ins))))

(define (make-list n atom)
  (vector->list (make-vector n atom)))

(define-macro (auto-extend-list-tail lst n fill)
;acts like normal list-tail if n<=(length lst)
; else appends copies of fill onto list to reach length n
  `(let ((tooshort (- ,n (length ,lst) -1)))
     (when (positive? tooshort)
     (set! ,lst (append ,lst (make-list tooshort ,fill))))
     (list-tail ,lst ,n)))

(define (prior end lst)
;returns cons-cell in lst just prior to end
; or #f if fails
  (cond ((null? lst) #f)
    ((eq? (cdr lst) end) lst)
    (else (prior end (cdr lst)))))


(define (first-valid lst fn)
;returns either
;-> (tail val) where (car tail) is first tail s.t. val=(fn (car tail) is not #f
;-> #f if (fn item)=#f for all items in lst
  (if (null? lst)
      #f
      (let ((result (fn (car lst))))
    (if result
        (list lst result)
        (first-valid (cdr lst) fn)))))

(define (inserted-list key-fn cmp-fn newval newkey lst)
  (cond ((null? lst) (list newval))
    ((positive? (cmp-fn newkey (key-fn (car lst)))) ;keep going
       (cons (car lst) (inserted-list key-fn cmp-fn newval newkey (cdr lst))))
    (else ;insert here
       (cons newval lst))))

(define-macro (insert-into-sorted-list key-fn cmp-fn newval . lst)
  `(let* ((newkey (,key-fn ,newval))
      (lst-val (remote-get ,@lst)))
     (remote-set! ,@lst (inserted-list ,key-fn ,cmp-fn ,newval newkey lst-val))))

(define-macro (get-else-insert-into-sorted-list key-fn cmp-fn newval . lst)
  `(let* ((newkey (,key-fn ,newval))
      (lst-val (remote-get ,@lst))
      (found (memf (lambda (cel) (not (positive? (,cmp-fn newkey (,key-fn cel)))))
              lst-val)))
     (cond ((not found) ;append to end
        (let ((return (list ,newval)))
          (remote-append! ,@lst return)
          return))
       ((zero? (,cmp-fn newkey (,key-fn (car found)))) ;found match
        found)
       ((eq? found lst-val) ;insert as first
        (let ((return (cons ,newval lst-val)))
          (remote-set! ,@lst (cons ,newval lst-val))
          return))
       (else  ;insert mid-list
        (let ((pr (prior found lst-val))
          (return (cons ,newval found)))
          (set-mcdr! pr return)
          return)))))

(define (list-delete-nth lst n)
  (if (zero? n)
      (cdr lst)
      (cons (car lst) (list-delete-nth (cdr lst) (- n 1)))))

;OBSOLETE; should replace with list-overlap
;returns ((A intersect B) (A subtract B))
(define (list-intersect+diff l1 l2 mem-fn)
  (if (null? l1)
      (list null null)
      (let* ((this (car l1))
         (rest (list-intersect+diff (cdr l1) l2 mem-fn))
         (ints (car rest))
         (diffs (cadr rest)))
    (if (mem-fn this l2)
        (list (cons this ints)
          diffs)
        (list ints
          (cons this diffs))))))

(define (list-overlap l1 l2 mem-fn rem-fn)
;return three-part list:
; ((l1-l2) (l1 intersect l2) (l2-l1))
; \------- l1 -------------/
;          \------------l2 ---------/
  (cond ((null? l1) (list null null l2))
    ((null? l2) (list l1 null null))
    (else
     (let* ((this (car l1))
        (trio (list-overlap (cdr l1) l2 mem-fn rem-fn))
        (l1-l2 (car trio))
        (int (cadr trio))
        (l2-l1 (rem-fn this (caddr trio))))
       (if (mem-fn this l2)
           (list l1-l2 (cons this int) l2-l1)
           (list (cons this l1-l2) int l2-l1))))))
(define (list-overlapq l1 l2) (list-overlap l1 l2 memq remq))
(define (list-overlapv l1 l2) (list-overlap l1 l2 memv remv))

(define (list-position-general lst target match-fn)
    (let loop ((i 0) (lst lst))
      (if (null? lst) #f
      (if (match-fn target (car lst)) i
          (loop (+ i 1) (cdr lst))))))
(define (list-position lst target)
  (list-position-general lst target equal?))
(define (list-positionv lst target)
  (list-position-general lst target eqv?))
(define (list-positionq lst target)
  (list-position-general lst target eq?))

;---
(define (car2 lst)
  (list (car lst) (cadr lst)))

(define (cdr2 lst)
  (list-tail lst 2))

(define (make-pairs lst)
; takes list (x1 y1 x2 y2...) and returns ((x1 y1) (x2 y2)...)
  (if (null? lst)
      '()
      (cons (car2 lst) (make-pairs (cdr2 lst)))))

(define (remove-duplicates lst mem-fn)
  (let ((newones null))
    (filter (lambda (it)
          (let ((seen? (mem-fn it newones)))
        (if (not seen?)
            (begin
              (push! it newones)
              #t)
            #f)))
        lst)))

;---
(define cross-map
;returns list of results of fn applied to every pair in
; lst1 X lst2
  (case-lambda
   ((fn lst1 lst2)
    (apply append
       (map (lambda (c)
          (map (lambda (l) (fn c l)) lst2))
        lst1)))))

(define (filtered-cross-map filter-fn map-fn l1 l2)
  (filter filter-fn (cross-map map-fn l1 l2)))

(define cross-filter
  (case-lambda
   ((fn lst1 lst2)
    (apply append
       (map (lambda (c)
          (filter (lambda (l) (fn c l)) lst2))
        lst1)))))

;---
(define (list-head lst n)
  (if (or (null? lst) (zero? n))
      null
      (cons (car lst) (list-head (cdr lst) (- n 1)))))

(define (rotate-list lst n)
  ;moves n elements from head of list onto tail
  ; i.e. slides list leftward by n
  (cond
   ((zero? n) lst)
   ((positive? n) (append (list-tail lst n) (list-head lst n)))
   (else (let* ((l (length lst))
        (end (+ l n))) ;n is negative
       (append (list-tail lst end) (list-head lst end))))))

(define (left-rotate-list lst)
  (append (list-tail lst 1) (list (car lst))))


;----
;given length l of some list and index i, returns index of next/prev
; element wrapping around as needed
(define (next-item i l)
  (modulo (+ i 1) l))
(define (prev-item i l)
  (modulo (- i 1) l))

(define list-slice-wrap
  (case-lambda
   ((lst offset runlen) (list-slice-wrap lst offset runlen (length lst)))
   ((lst offset runlen listlen)
    (if (zero? runlen)
    null
    (cons (list-ref-wrap lst offset listlen)
          (list-slice-wrap lst (+ offset 1) (- runlen 1) listlen))))))

(define list-ref-wrap
  (case-lambda
   ((lst ref) (list-ref-wrap lst ref (length lst)))
   ((lst ref len)
    (list-ref lst (modulo ref len)))))
    
(define (mmap width shift  skip fn lst) ;"Multi-map"            
; generalized map where lambda fn can operate on multiple adjacent elements
; Special case: (mmap 1 0 1 fn lst) = (map fn lst)
; width is # of args to fn, each set a series of successive lst elements
; shift is offset (relative to (current" lst item) of first arg to fn
; skip (>0) is how many lst elements to offset before next fn call
; wraps around as necessary
;EXs:
; (mmap 1 0 2 (lmb(n) (sqr n)) '(1 2 3 4 5 6))
;                                ^
;                                    ^
;                                        ^     --> (1 9 25)
; (mmap 3 -1 2 (lmb(nop) (str-append n o p)) '(a b c d e f))
;                                              ^ ^       ^
;                                                ^ ^ ^
;                                                    ^ ^ ^ --> (fab bcd def)
; (mmap 2 1 1 (lmb(no) (str-append n o)) '(a b c d e f))
;                                            ^ ^
;                                              ^ ^
;                                                ^ ^...   --> (bc cd de ef fa ab)
  (let* ((result '())
     (l (length lst)))
    (let loop ((i 0))
      (when (< i l)
        (set! result (cons (apply fn (list-slice-wrap
                      lst
                      (+ i shift)
                      width
                      l))
                   result))
        (loop (+ i skip))))
    (reverse result)))

;---
(define (list-depth lst) ;returns 0 if lst is atom,
                         ;1 if (car lst) is atom
                         ;2 if (caar lst) is atom, etc
  (if (list? lst)
      (+ 1 (list-depth (car lst)))
      0))

(define counting-list
  (case-lambda
   ((len) (counting-list len 0))
   ((len from)
    (if (zero? len)
    null
    (cons from (counting-list (- len 1) (+ from 1)))))))

(define (count-between-list from to step)
  (if (>= from to)
      '()
      (cons from (counting-list (+ from step) to step))))
;(define (counting-list lo len)
; builds list (lo lo+1 ... lo+len-1)
;  (if (positive? len)
;      (cons lo (counting-list (+ 1 lo) (- len 1)))
;      null))

(define (wrapped-counting-list from to step len)
  (when (>= to len)
      (set! to (modulo to len)))
  (if (= from to)
      '()
      (cons from (wrapped-counting-list (modulo (+ from step) len) to step len))))

(define (index-list lst)
  (counting-list 0 (length lst) 1))


;(define-macro (bind-list-elements vars vals)
;  (cons `(full-list ,vals)
;     (map (lambda (var) `(,var (pop! ,full-list)))
;          vars)))
