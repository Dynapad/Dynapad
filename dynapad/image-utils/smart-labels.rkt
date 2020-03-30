#lang racket/base

(require racket/date
         dynapad/misc/misc
         dynapad/utils/parsedate
         dynapad/misc/tools-lists
         )

(define (list-offset lst fn)
  (cond ((null? lst) #f)
        ((fn (car lst)) 0)
        (else (let ((lo (list-offset (cdr lst) fn)))
                (and lo (+ lo 1))))))

(define (cross-slice matrix)
  (map (lambda (l) (sublist-slice-at-level matrix l))
       (counting-list (length (car matrix)))))

(define (find-list-changes lst eq-fn . last-val-arg)
  (if (null? lst)
      null
      (let* ((this-val (car lst))
             (diff (cond ((null? last-val-arg) #t)
                         ((eq-fn (car last-val-arg) this-val) #f)
                         (else #t))))
        (cons diff (find-list-changes (cdr lst) eq-fn this-val)))))

(define (sublist-slice-at-level sublists lvl)
  (map (lambda (subl) (list-ref subl lvl)) sublists))

(define (sublist-slice-until-fn sublists stop-fn eq-fn)
  ; slices sublists, starting at lvl0 and and descending,
  ; and accumulating change-slices until (stop-fn change-slice) is not #f
  ; Returns list of slices produced (reverse order), starting w last
  (let ((slices null)
        (last-slice #f)
        (lvl 0))
    (while (not (stop-fn
                 (cset! last-slice (find-list-changes
                                    (sublist-slice-at-level sublists (_++ lvl)) eq-fn))))
      (push! last-slice slices))
    (cons last-slice slices)))

(define (boolean->bit bool)
  (if bool 1 0))

(define (diversity changelst)
  (apply + (map boolean->bit changelst)))
(define (diverse? changelst)
  (> (diversity changelst) 1))

#; ; broken missing some-changes, also not used
(define (sublist-slice-until-changes sublists eq-fn)
  (sublist-slice-until-fn sublists some-changes eq-fn))


(define (all-change-slices sublists eq-fn)
  (map (lambda (lvl)
         (find-list-changes (sublist-slice-at-level sublists lvl)
                            eq-fn))
       (counting-list (length (car sublists)))))

(define (make-mask-between lo hi)
  (map (lambda (p) (and (>= p lo) (<= p hi))) (counting-list 6)))


; EX:
; sublist:
; -->Lvl 0-5
; [(1  2  3  4  5  6)
;  (3  3  3  3  3  3)
;  (3  4  3  4  3  3)]

; change-slices:
; [(1 (1 (1 (1 (1
;   1  1  0  1  1
;...
;   0) 1) 0) 1) 0)]

(define (or-mask m1 m2)
  (if (null? m1)
      null
      (cons (or (car m1) (car m2))
            (or-mask (cdr m1) (cdr m2)))))

(define (and-mask m1 m2)
  (if (null? m1)
      null
      (cons (and (car m1) (car m2))
            (and-mask (cdr m1) (cdr m2)))))

(define build-label-masks #f)
(let ((year-lvl 0)
      (month-lvl 1)
      (day-lvl 2)
      (hour-lvl 3)
      (min-lvl 4)
      (sec-lvl 5))

  (set! build-label-masks
        (lambda (sublists)  ;assumes non-null sublists
          (let* ((change-slices (all-change-slices sublists eq?))
                 (first-diverse-lvl (or (list-offset change-slices diverse?) sec-lvl))
                 (widely-diverse?
                  (lambda (lvl) (let ((slice (list-ref change-slices lvl)))
                                  (> (diversity slice) 2))))
                 (time-vals '(#f #f #f #t #t #t)) ; a mask
                 (last-lvl-kept
                  (case first-diverse-lvl
                    ((0)   (if (widely-diverse? month-lvl) month-lvl day-lvl))
                    ((1)   (if (widely-diverse? day-lvl) day-lvl min-lvl))
                    ((2)   (if (widely-diverse? hour-lvl) hour-lvl min-lvl))
                    ((3 4) (if (widely-diverse? min-lvl) min-lvl sec-lvl))
                    ((5) sec-lvl)))
                 (shared-mask (make-mask-between 0 (- first-diverse-lvl 1)))
                 (base-mask   (make-mask-between first-diverse-lvl last-lvl-kept))
                 (private-masks
                  (map (lambda (mymask) (and-mask base-mask
                                                  (or-mask time-vals mymask)))
                       (cross-slice change-slices))))
            (cons shared-mask private-masks)))))

;  Date labels

(define (subdivide-date date) (reverse (date->list date)))

(define (dates->labels datelist)
  (date-display-format 'iso-8601)
  (if (null? datelist)
      null
      ;else
      (let* ((sublists (map subdivide-date datelist))
             (masks    (build-label-masks sublists))
             (shared-mask (car masks))
             (priv-masks (cdr masks))
             (raw-strs (map (lambda (d) (date->string d #t)) datelist))
             (trim-strs (map (lambda (str mask)
                               (prune-date-string mask *iso-date-format-rexp* str))
                             raw-strs priv-masks))
             (shared-str (if (null? datelist)
                             ""
                             (prune-date-string shared-mask
                                                *iso-date-format-rexp*
                                                (car raw-strs)))))
        (list trim-strs shared-str))))
; trim-strs))))
