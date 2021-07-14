(load-extension (build-path->string *dynapad-directory* "apps/dt/mzdt/mzdt.so"))

(define (table->root coords)
  (if (null? coords) null
      (let* ((table-width 128.0)
             (table-height 96.0)
             (winfo (send dynapad winfo))
             (root-width (fifth winfo))
             (root-height (sixth winfo))
             (table-x (first coords))
             (table-y (second coords))
             (root-x (* table-x (/ root-width table-width)))
             (root-y (* table-y (/ root-height table-height))))
        (cons root-x (cons root-y
                           (table->root (list-tail coords 2)))))))

(define (root->dynapad coords)
  (if (null? coords) null
      (let* ((zoom (send dynapad getzoom))
             (winfo (send dynapad winfo))
             (window-origin-x (first winfo))
             (window-origin-y (second winfo))
             (dynapad-bbox (send dynapad bbox))
             (dynapad-west (first dynapad-bbox))
             (dynapad-north (fourth dynapad-bbox))
             (root-x (first coords))
             (root-y (second coords))
             (dynapad-x (+ dynapad-west
                           (/ (- root-x window-origin-x) zoom)))
             (dynapad-y (- dynapad-north
                           (/ (- root-y window-origin-y) zoom))))
        (cons dynapad-x (cons dynapad-y
                              (root->dynapad (list-tail coords 2)))))))

(define (table->dynapad coords)
  (root->dynapad (table->root coords)))

(define (dt-touch-id touch) (list-ref touch 0))
(define (dt-user-id touch) (list-ref touch 1))
(define (dt-x touch) (list-ref touch 2))
(define (dt-y touch) (list-ref touch 3))
(define (dt-w touch) (list-ref touch 4))
(define (dt-s touch) (list-ref touch 5))
(define (dt-e touch) (list-ref touch 6))
(define (dt-n touch) (list-ref touch 7))
(define (dt-center touch) (list (dt-x touch) (dt-y touch)))
(define (dt-bbox touch)
  (list (dt-w touch) (dt-s touch) (dt-e touch) (dt-n touch)))
