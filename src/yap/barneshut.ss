(define QuadTreeNode%
  (class object%
    (field
      (_hasChildren #f)
      (_mass 0)
      (_com (list 0 0))
      (_value #f)
      (_children (make-vector 4 #f)))
    (super-instantiate ())

    (define/public hasChildren
      (case-lambda
        (() _hasChildren)
	((v) (set! _hasChildren v))))
    (define/public mass
      (case-lambda
        (() _mass)
	((v) (set! _mass v))))
    (define/public com
      (case-lambda
        (() _com)
	((v) (set! _com v))))
    (define/public xcom
      (case-lambda
        (() (car _com))
	((v) (set! _com (list v (cadr _com))))))
    (define/public ycom
      (case-lambda
        (() (cadr _com))
	((v) (set! _com (list (car _com) v)))))
    (define/public value
      (case-lambda
        (() _value)
	((v) (set! _value v))))
    (define/public children
      (case-lambda
        (() _children)
        ((i) (vector-ref _children i))
	((i v) (vector-set! _children i v))))))
    
(define NBodyForce%
  (class object%
    (field
      (_root #f)
      (l ())
      ;(_G -0.4)
      (_G -4)
      (_minDistance -1)
      (_BHTheta 0.9)
      (_xMin 0)
      (_xMax 0)
      (_yMin 0)
      (_yMax 0)
      (factory #f))

    (define/public G
      (case-lambda
        (() _G)
	((v) (set! _G v))))
    (define/public minDistance
      (case-lambda
        (() _minDistance)
	((v) (set! _minDistance v))))
    (define/public BHTheta
      (case-lambda
        (() _BHTheta)
	((v) (set! _BHTheta v))))
    (define/public (bbox) (list _xMin _yMin _xMax _yMax))
    (define/public (root) _root)

    (define insert
      (case-lambda
        ((p) (insert p _root _xMin _yMin _xMax _yMax))
	((p n x1 y1 x2 y2)
	  (cond
	    ((send n hasChildren)
	      (insertHelper p n x1 y1 x2 y2))
	    ((and (send n value) (isSameLocation (send n value) p))
	      (insertHelper p n x1 y1 x2 y2))
	    ((and (send n value) (not (isSameLocation (send n value) p)))
	      (let ((v (send n value)))
	        (send n value #f)
	        (insertHelper v n x1 y1 x2 y2)
	        (insertHelper p n x1 y1 x2 y2)))
	    (else
	      (send n value p))))))

    (define (isSameLocation f1 f2)
      (let*
        ((xy1 (send f1 xy))
	 (xy2 (send f2 xy))
	 (dx (abs (- (car xy1) (car xy2))))
	 (dy (abs (- (cadr xy1) (cadr xy2)))))
	(and (< dx 0.01) (< dy 0.01))))

    (define (insertHelper p n x1 y1 x2 y2)
      (let*
        ((x (car (send p xy)))
	 (y (cadr (send p xy)))
	 (splitx (+ x1 (/ (- x2 x1) 2)))
	 (splity (+ y1 (/ (- y2 y1) 2)))
	 (i (+ (if (> x splitx) 1 0) (if (> y splity) 2 0))))
	
	(when (not (send n children i))
	  (send n children i (send factory getQuadTreeNode))
	  (send n hasChildren #t))
	(if (or (= i 1) (= i 3))
	  (set! x1 splitx)
	  (set! x2 splitx))
	(if (> i 1)
	  (set! y1 splity)
	  (set! y2 splity))
	(insert p (send n children i) x1 y1 x2 y2)))

    (define (_walk qtn quad depth)
      (cond
        ((send qtn value)
          (printf "~a ~a~%"
  	    (make-string (* depth 2) #\space)
	    (list (send qtn xcom) (send qtn ycom) (send qtn mass) quad depth)))
        ((send qtn hasChildren)
	  (printf "~a ~a hasChildren~%"
	    (make-string (* depth 2) #\space)
	    (list (send qtn xcom) (send qtn ycom) (send qtn mass) quad depth))
	  (do
	    ((i 0 (+ i 1)))
            ((>= i 4))
	    (let ((n (vector-ref (send qtn children) i)))
	      (if n (_walk n i (+ depth 1))))))
        (else
          (printf "empty qtn~%"))))

  (define/public walk
    (case-lambda
      (() (_walk _root 0 0))
      ((n) (_walk n 0 0))))

    (define (calcMass n)
      (let ((xcom 0) (ycom 0))
        (send n mass 0)
	(if (send n hasChildren)
	  (for-each
	    (lambda(o)
	      (when o
	        (calcMass o)
		(send n mass (+ (send n mass) (send o mass)))
		(set! xcom (+ xcom (* (send o mass) (send o xcom))))
		(set! ycom (+ ycom (* (send o mass) (send o ycom))))))
	    (vector->list (send n children))))
        (when (send n value)
	  (send n mass (+ (send n mass) (send (send n value) mass)))
	  (set! xcom (+ xcom
	      (* (send (send n value) mass) (car (send (send n value) xy)))))
	  (set! ycom (+ ycom
	      (* (send (send n value) mass) (cadr (send (send n value) xy))))))
        (send n xcom (/ xcom (send n mass)))
        (send n ycom (/ ycom (send n mass)))))

    (define/public (force p)
      (forceHelper p _root _xMin _yMin _xMax _yMax))

    (define (forceHelper p n x1 y1 x2 y2)
      (let/ec return
        (let*
          ((dx (- (send n xcom) (car (send p xy))))
           (dy (- (send n ycom) (cadr (send p xy))))
           (r (sqrt (+ (* dx dx) (* dy dy))))
           (same #f)
           (minDist #f))
          (when (= r 0)
            ;; gotta wonder about this, assuming dx between -0.020 and +0.020
            ;; rather than chose a range based on the current zoom
            (set! dx (/ (- (random 100) 50) 1000))
            (set! dy (/ (- (random 100) 50) 1000))
            (set! r (sqrt (+ (* dx dx) (* dy dy))))
            (set! same #t))
          (set! minDist (and (> _minDistance 0) (> r _minDistance)))
	  (cond
	    ((or
	        (and (not (send n hasChildren)) (not (eq? (send n value) p)))
	        (and (not same) (< (/(- x2 x1) r) _BHTheta)))
	      (if minDist (return (list 0 0)))
	      (let
	        ((v (/ (* _G (send p mass) (send n mass)) (* r r)))
	         (forcex (car (send p force)))
	         (forcey (cadr (send p force))))
	        (set! forcex (+ forcex (* v dx)))
	        (set! forcey (+ forcey (* v dy)))
	        (send p force (list forcex forcey))))
	    ((send n hasChildren)
	      (let
	        ((splitx (+ x1 (/ (- x2 x1) 2)))
	         (splity (+ y1 (/ (- y2 y1) 2))))
		(do
		  ((i 0 (+ i 1)))
		  ((>= i 4))
		  (let ((qtn (send n children i)))
		    (when qtn
		      (forceHelper p qtn
		        (if (or (= i 1) (= i 3)) splitx x1)
			(if (> i 1) splity y1)
			(if (or (= i 1) (= i 3)) x1 splitx)
			(if (> i 1) y2 splity)))))
		(if minDist (return (list 0 0)))
		(when (and (send n value) (not (eq? (send n value) p)))
		  (let
		    ((v (/ (* _G (send p mass) (send (send n value) mass))
		      (* r r)))
	              (forcex (car (send p force)))
	              (forcey (cadr (send p force))))
		    (set! forcex (+ forcex (* v dx)))
		    (set! forcey (+ forcey (* v dy)))
		    (send p force (list forcex forcey))))))))))

    (define (bbox-of-list l)
      (let ((lx +inf.0) (ly +inf.0) (ux -inf.0) (uy -inf.0))
        (for-each
          (lambda (o)
	     (let ((bb (send o bbox)))
               (if (< (car bb) lx) (set! lx (car bb)))
               (if (< (cadr bb) ly) (set! ly (cadr bb)))
               (if (> (caddr bb) ux) (set! ux (caddr bb)))
               (if (> (cadddr bb) uy) (set! uy (cadddr bb)))))
           l)
        (list lx ly ux uy)))

    (define QuadTreeNodeFactory%
      (class object%
        (field
	  (maxNodes 10000)
	  (nodes ()))
	(define/public (getQuadTreeNode)
	  (cond
	    ((not (null? nodes))
	      (let ((x (car nodes)))
	        (set! nodes (cdr nodes))
		x))
            (else
	      (make-object QuadTreeNode%))))
	(define/public (reclaim n)
	  (send n mass 0)
	  (send n com (list 0 0))
	  (send n value #f)
	  (send n hasChildren #f)
	  (do
	    ((i 0 (+ i 1)))
	    ((>= i 4))
	    (send n children i #f))
	  (if (< (length nodes) maxNodes)
	    (set! nodes (cons n nodes))))
	(super-instantiate())))

    (define (clearHelper qtn)
      (do
        ((i 0 (+ i 1)))
	((>= i 4))
	(let ((n (send qtn children i)))
	  (when n
	    (clearHelper n))))
      (send factory reclaim qtn))

    (define (clear)
      (clearHelper _root)
      (set! _root (send factory getQuadTreeNode)))

    (define/public (init l)
      (clear)
      (let ((bb (bbox-of-list l)))
        (set! _xMin (car bb))
        (set! _yMin (cadr bb))
        (set! _xMax (caddr bb))
        (set! _yMax (cadddr bb)))
      (for-each insert l)
      (calcMass _root))

    (super-instantiate ())
    (set! factory (make-object QuadTreeNodeFactory%))
    (set! _root (send factory getQuadTreeNode))
    ))

(define (physic l)
  (for-each
    (lambda(x)
      (send x physics #t)
      (send x velocity (list 0 0)))
    l)
  (let ((nb (make-object NBodyForce%)))
    (thread
      (lambda()
        (let loop ()
          ;(sleep 0.020)
	    (send nb init l)
	    (for-each (lambda(x) (send x force (list 0 0))) l)
	    (for-each (lambda(x) (send nb force x)) l)
	    (for-each
	      (lambda(x)
	        (let
	          ((force (send x force))
		   (velocity (send x velocity))
		   (position (send x position)))
		  (set! velocity (list
		    (+ (car velocity) (car force))
		    (+ (cadr velocity) (cadr force))))
		  (send x position (list
		    (+ (car position) (car velocity))
		    (+ (cadr position) (cadr velocity))))))
	      l)
	    (loop))))))

(define (physic2 l)
  (for-each
    (lambda(x)
      (send x physics #t)
      (send x velocity (list 0 0)))
    l)
  (let*
    ((nb (make-object NBodyForce%))
     (t
       (make-object timer%
         (lambda()
	   (send nb init l)
	   (for-each (lambda(x) (send x force (list 0 0))) l)
	   (for-each (lambda(x) (send nb force x)) l)
	   (for-each
	     (lambda(x)
	       (let
	         ((force (send x force))
		  (velocity (send x velocity))
		  (position (send x position)))
		 (set! velocity (list
		   (+ (car velocity) (car force))
		   (+ (cadr velocity) (cadr force))))
		 (send x position (list
		   (+ (car position) (car velocity))
		   (+ (cadr position) (cadr velocity))))))
	     l)
	   (send t start 20 #t)))))
    (send t start 20 #t)
    t))
