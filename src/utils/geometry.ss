(require (lib "defmacro.ss"))
(require (lib "math.ss"))
(dynaload "actor.ss")
;(dynaload "misc.ss")
;(dynaload "alist.ss")
(dynaload "alist.ss")
;(dynaload "tools-objects.ss")
(dynaload "tools-misc.ss")
(dynaload "tools-lists.ss")

;if debugging used:
;(dynaload "profile.ss")

(define stale 'stale)

(define-macro (stale? res)
  `(eq? ,res stale))

(define-macro (fresh? res)
  `(not (stale? ,res)))

(define-macro (ensure-fresh var refresh-fn . args)
  `(begin
     (when (stale? ,var)
     (set! ,var (,refresh-fn ,@args)))
     ,var))

(define-macro (invalidate-vars . vars)
  (cons 'begin (map (lambda (var) `(set! ,var stale)) vars)))

;Converts three parameter syntaxes to normal form:
; (dxy-fn obj)    |
; (dxy-fn '(x y)) |-> (dxy-fn x y)
; (dxy-fn x y)    |
(define-macro (vector-op-template dxy-fn)
  `(case-lambda
    ((vc) (if (list? vc)
          (send/apply this ,dxy-fn vc)
          ;else assume vc is vector-like object
          (let ((dxy (send vc dxy)))
        (send/apply this ,dxy-fn dxy))))
    ((dx dy) (send this ,dxy-fn dx dy))))

(define-macro (ensure-point-object pt)
  `(cond
     ((list? ,pt) (cset! ,pt (make-object geo-point% ,pt)))
     ((is-a? ,pt geo-point%) ,pt)
     (else (error "Expected geo-point% or coord pair, given" ,pt))))

(define-macro (ensure-angle-object ang)
  `(cond ((number? ,ang) (set! ,ang (make-object geo-angle% ,ang)))
     ((list? ,ang)   (set! ,ang (make-object geo-angle% ,ang)))))

(define-macro (ensure-vector-object vect)
  `(when (list? ,vect)
      (set! ,vect (make-object geo-vector% ,vect))))

(define-macro (ensure-segment-object seg)
  `(when (list? ,seg)
      (set! ,seg (make-object geo-segment% ,seg))))  

;============ debugging tools ================
(define next-obj-ID 0)  ;any obj ref'd in a desc is given unique ID#,
                    ; assigned sequentially from 0
(define obj-ID-alist null) ;              alist of (obj ID) pairs
(define ID-obj-alist null) ;complementary alist of (ID obj) pairs

(define (describe obj)
; obj may be actual object or ID of previously desc'd obj
  (when (number? obj)
      (let ((id-pair (assoc obj ID-obj-alist)))
    (set! obj (if id-pair
              (cadr id-pair)
              #f))))
  (if (stale? obj)
      obj
      (if obj
      (display (send obj describe-fields))
      (display "no such ID\n"))))

(define (grab id)
  (let ((id-pair (assoc id ID-obj-alist)))
    (if id-pair
    (cadr id-pair)
    (display "no such ID\n"))))

(define (name-of-obj obj)
; reuse or create ID for obj
  (cond ((stale? obj) obj)
    ((not obj) obj)
    ((list? obj) (map name-of-obj obj))
    (else
     (let* ((newpair (list obj next-obj-ID))
        (result (get-else-push-onto-malist! assq newpair obj-ID-alist))
        (ID (cadr result)))
       (when (eq? result newpair) ;not found, use new
           (begin
         (push! (list ID obj) ID-obj-alist)
         (+= next-obj-ID 1)))
       (format "<~a>" ID)))))

; Variations on y=mx+b:
(define (linear-y x m b)
  (if (infinite? m)
      infinity
      (+ (* m x) b)))
(define (linear-x y m b)
  (cond ((infinite? m) b)
    ((zero? m) infinity)
    (else (/ (- y b) m))))
(define (linear-b m x y)
  (if (infinite? m)
      x
      (- y (* m x))))
(define (linear-m dx dy)
  (cond ((not (zero? dx)) (/ dy dx))
    ((zero? dy) 0)
    ((negative? dy) -infinity)
    (else infinity)))

(define (ordered-in-direction? pt1 pt2 ang)
  (let ((x1 (send pt1 x))
    (x2 (send pt2 x)))
    (cond
     ((send ang rightward?)   (<= x1 x2))
     ((send ang leftward?)    (>= x1 x2))
     (else ;vertical
      (let ((y1 (send pt1 y))
        (y2 (send pt2 y)))
    (if (send ang upward?)
        (<= y1 y2)
        (>= y1 y2)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;       /\ |    /| /~ /~   /~ | | |\ /| |\ /|  /| /~\ \ /
;      /   |   /-| \_ \_   \_ | | | V | | V | /-| |_/  Y
;      \_/ |_ /  | _/ _/   _/ |_/ |   | |   |/  | | \  |
;
; geo-actor
;  geo-point
;   geo-intersect-point
;  geo-angle
;   geo-vector
;  geo-line
;   geo-ray
;    geo-segment
;     geo-poly-segment  <.
;      geo-polygon-edge  : <.
;  geo-polyline  ........:  :
;  geo-polygon .............:
;                    comprises                    

#|
(define (attach-geo-callbacks obj this)
;  (set! obj (get-top-group obj))
  (let* ((grp (get-topmost-group obj))
     (targets (if (eq? grp obj)
              (list obj)
              (list obj grp))))
    (foreach targets
      (lambda (obj)
    (send this attach-callbacks-to-obj obj)))
))
|#

(define (attach-geo-callbacks obj this)
  (send this attach-callbacks-to-obj obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;      /\ /~~ /~\     /|  /\ ~T~ /~\ /~\
;     / _ |-- | | -- /-| /    |  | | |_/
;     \_/ |__ |_/   /  | \_/  |  |_/ | \
;              (BASE CLASS)

(define geo-actor%
  (class named-actor%
    (super-instantiate ())
    
    ;FOR ALL (descendants must override)
    (define/public (clone) 'undefined)     
    (define/public (slide-by-dxy dx dy) 'undefined)
    (define/public slide-copy (vector-op-template slide-by-dxy))
    (define/public (slide-by-dxy! dx dy) 'undefined)            
    (define/public slide! (vector-op-template slide-by-dxy!))
    (define/public slide (vector-op-template slide-by-dxy!))
    (define/public (describe-fields) (format "Object ~a:~%" (name-of-obj this)))
    (define/public (describe) (describe this))

#|
    (define/public (attach-callbacks-to-obj obj)
      ;(say "adding callbacks")
      (send obj delete-callbacks 'add
        (lambda (obj)
          (send this delete)) this)
      (send obj scale-callbacks 'add
        (lambda (obj fact x y)
          (send this rescale-around-xy fact fact (list x y))) this)
      (send obj width-callbacks 'add
        (lambda (obj new-w)
          (let* ((old-w (send obj width))  ;requires pre-callback
             (xy (send obj xy))
             (fact (/ new-w old-w)))
        (send this rescale-around-xy fact 1 xy))) this)
      (send obj height-callbacks 'add
        (lambda (obj new-h)
          (let* ((old-h (send obj height)) ;requires pre-callback
             (xy (send obj xy))
             (fact (/ new-h old-h)))
        (send this rescale-around-xy 1 fact xy))) this)
      
      (send obj position-callbacks 'add
        (lambda (obj new-xyz)
          (let* ((new-xy (list (car new-xyz) (cadr new-xyz)))
             (new-z  (cddr new-xyz))
             (old-xy (send obj xy)))  ;requires pre-callback
        (send this slide (map - new-xy old-xy))
        (if (not (null? new-z))
            (let ((factor (/ (car new-z) (send obj z))))
              (send this rescale-around-xy factor factor new-xy))))) this)
      
      (send obj slide-callbacks 'add
        (lambda (obj dx dy) ;(display (format "sliding ~a~%" obj))
            (send this slide dx dy)) this)
      )

    (define/public (detach-callbacks-from-obj obj)
      (send obj delete-callbacks 'remove #f this)
      (send obj scale-callbacks 'remove #f this)
      (send obj width-callbacks 'remove #f this)
      (send obj height-callbacks 'remove #f this)
      (send obj position-callbacks 'remove #f this)
      (send obj slide-callbacks 'remove #f this)
      )
|#
    (define/public (attach-callbacks-to-obj obj)
      (send obj delete-callbacks 'add
        (lambda (obj) (send this delete)) this)
      (send obj reposition-callbacks 'add
        (lambda (o dx dy) (send this slide dx dy)) this)
      (send obj resize-callbacks 'add
        (case-lambda
         ((o fac x y) (send this rescale-around-xy fac fac (list x y)))
         ((o xfac yfac x y) (send this rescale-around-xy xfac yfac (list x y))))
        this))

    (define/public (detach-callbacks-from-obj obj)
      (send obj delete-callbacks 'remove #f this)
      (send obj reposition-callbacks 'remove #f this)
      (send obj resize-callbacks 'remove #f this)
      )
      
;    (define (rescale-around-xy xfact yfact xy-src)
;      (
))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;     /~\ /~\ | |\ | ~T~
;     |_/ | | | | \|  |
;     |   |_/ | |  V  |

;At times a simple list (x y) may substitute for a point-object
;For points, using (send-point ...) instead of (send...)
; will work for (x y) pairs also
(define-macro (send-point pt msg . args)
  `(when (list? ,pt)
       (cond
     ((eq? ,msg 'xy) (list (car ,pt) (cadr ,pt)))
     ((eq? ,msg 'x)  (car ,pt))
     ((eq? ,msg 'y)  (cadr ,pt))
     (else  (send (make-object geo-point% ,pt) ,msg ,@args)))
    ;else actual object
       (send ,pt ,msg ,@args)))

;(make-object geo-point% { x y | (x y)})
(define geo-point%
  (class geo-actor%
    (init-field _x (_y 0))
    (when (list? _x) (set! _y (cadr _x)) (set! _x (car _x)))
    (super-instantiate ())

    ;REDEFS
    ;(define/override (attach-to obj name) (error "Can't attach geo-point% to a dynaobject."))

    (define/public (copy-as-point) (make-object geo-point% _x _y))
    (define/override (clone) (copy-as-point))
    (define/override (describe-fields)
      (string-append (super describe-fields) (format "geo-point%: x:~a y:~a~%" _x _y)))

    (define/override (slide-by-dxy! dx dy) ;slide this
      (send this xy (+ _x dx) (+ _y dy)))
        
    (define/override (slide-by-dxy dx dy)  ;slide copy
      (make-object geo-point% (+ _x dx) (+ _y dy)))
        
    ;LOCAL
    (define/public x (get/set _x))
    (define/public y (get/set _y))
    (define/public xy
      (case-lambda
        (() (list _x _y))
        ((n1 n2)
          (set! _x n1)
          (set! _y n2))))

    (define/public (betweenish? p1 p2 exclude-p2)
    ; Assumes this is collinear with p1, p2
    ; Faster than between? b/c needs to check only x (or y is vertical)
    ; Returns #f if no qualifying intersection, or else:
    ; 0 if this is strictly between p1,p2 (wrt x)
    ; -1 if this x is lowest x of p1,p2 (relative y unknown)
    ;  1 if this x is highest x of p1,p2 (relative y unknown)
    ; -2 if this y is lowest y of p1,p2 (x=x1=x2)
    ;  2 if this y is highest y of p1,p2 (x=x1=x2)
      (let* ((xy1 (send p1 xy))
         (x1 (car xy1))
         (y1 (cadr xy1))
         (xy2 (send p2 xy))
         (x2 (car xy2))
         (y2 (cadr xy2)))
    (if (!= x1 x2)
        ;compare x's if different
        (let* ((x1-hi? (> x1 x2))
           (hi (if x1-hi? x1 x2))
           (lo (if x1-hi? x2 x1)))
          (or (and (< lo _x) (< _x hi) 0) ;strictly between, return 0
          (and (= _x x1) (if x1-hi? 1 -1))
          (and (not exclude-p2) (= _x x2) (if x1-hi? -1 1))))
        ;otherwise compare y's:
        (let* ((y1-hi? (> y1 y2))
           (hi (if y1-hi? y1 y2))
           (lo (if y1-hi? y2 y1)))
          (or (and (< lo _y) (< _y hi) 0) ;strictly between
          (and (= _y y1) (if y1-hi? 2 -2))
          (and (not exclude-p2) (= _y y2) (if y1-hi? -2 2)))))))
    
    (define/public segment-to
      (case-lambda
       ((pt)    (make-object geo-segment% this pt))
       ((x2 y2) (make-object geo-segment% this x2 y2))))

    (define/public vector-to
      (case-lambda
       ((pt) (let ((xy (if (list? pt) pt (send pt xy))))
           (apply make-object geo-vector%
              (map - xy (list _x _y)))))
       ((x2 y2) (make-object geo-vector% (- x2 _x) (- y2 _y)))))
))

(define geo-intersect-point%
  (class geo-point%
    (init _x _y)
    (init-field _parents)
    (super-instantiate (_x _y))
    
    (define/public parents (get/set _parents))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;       /| |\ |  /\ |   /~~
;      /-| | \| / _ |   |--
;     /  | |  V |_/ |__ |__

(define -pi (- pi))
(define pi/4 (/ pi 4))
(define pi/2 (/ pi 2))
(define 3pi/2 (* pi 1.5))
(define 2pi (* pi 2))
(define infinite-slope (tan 3pi/2))

(define (parallel-slope? s1 s2)
  (or (equal? s1 s2)
      (and (infinite? s1) (infinite? s2))))

(define (my-angle-class this)
  (if (is-a? this geo-vector%)
      geo-vector%
      geo-angle%))

;(make-object geo-angle% {theta | (dx dy) | dx dy})
(define geo-angle%
  (class geo-actor%
    (init (arg0 0)
      (arg1 #f))
    (field (_theta stale)  ;in radians, positive for counter-clockwise
           (_net-theta stale)
       (_pos-theta stale)
       (_dx stale)
       (_dy stale)
       (_length stale) ; used only when _dx _dy specified;
                ; non-vector angle has no length, used internally only
       (_slope stale))
;Either _dx & _dy or _theta will be known at all times.
; Use fresh-theta? or fresh-dxy? to test which is avail;
;   if both, use whichever is faster for operation.
; If comparing angles where fresh params are different
;  faster to convert to theta (arctan alone faster than sin+cos)
; BUT-- _dx _dy more useful to have fresh, so consider investing in _dx _dy
;  if operation likely to be repeated.
    (super-instantiate ())
; Interpret args:
    (cond ((list? arg0) (set! _dx (car arg0)) (set! _dy (cadr arg0))) ; (x y)
      (arg1         (set! _dx arg0) (set! _dy arg1))              ;  x y
      (else         (set! _theta arg0)))                          ; theta

    ;==== REDEFS ====
    (define/override (attach-to obj name)
      (error "Can't attach geo-angle% to a dynaobject."))

    (define/override (clone) (if (fresh? _dx)
                 (make-object geo-angle% _dx _dy)
                 (make-object geo-angle% _theta)))
    ; Consider copying more fields...

    (define/override (describe-fields)
      (string-append (super describe-fields)
             (format "geo-angle%: theta:~a net:~a pos:~a dx:~a dy:~a slope:~a~%"
                 _theta _net-theta _pos-theta _dx _dy _slope)))
    ;although _length is a field, omitted b/c not "public";
    ;save it for geo-vector% description
    ;==== LOCAL ====

    ;THETA etc.
    (define/public (fresh-theta?)
      (fresh? _theta))
    (define (refresh-theta) ;requires fresh _dx,_dy
      (if (and (zero? _dx) (zero? _dy))
      0                ; define theta of null vector to be 0
      (atan _dy _dx)))
    (define/public (invalidate-theta)
      (invalidate-vars _theta _net-theta _pos-theta))
    (define/public theta 
      (case-lambda
       (() (ensure-fresh _theta refresh-theta))
       ((val) (begin
        (set! _theta val)
        (invalidate-vars _dx _dy _net-theta _pos-theta _slope)))))
    (define/public (cycles)
      (/ (theta) 2pi))
    (define/public net-theta ;always betw. -pi and pi
      (case-lambda
       (() (ensure-fresh _net-theta net-theta (theta)))
       ((theta) (if (<= (abs theta) pi)
            theta
            (- theta (* 2pi (round (/ theta 2pi))))))))
    (define/public pos-theta ;always between 0 and 2pi
      (case-lambda
       (() (ensure-fresh _pos-theta pos-theta (theta)))
       ((theta) (let ((net (net-theta theta)))
          (if (negative? net)
              (+ net 2pi)
              net)))))
    ;DX/DY
    (define/public (fresh-dxy?)
      (fresh? _dx)) ;assume _dx, _dy always co-fresh
    (define (refresh-dxy) ;requires _theta to be fresh
      (list (cos _theta) (sin _theta)))
    (define/public (refresh-length)
      (hypotenuse _dx _dy))
    (define/public dxy
      (case-lambda
       (() (if (fresh? _dx) ;assume _dx, _dy always co-fresh
           (list _dx _dy)
           (let ((pair (refresh-dxy)))
         (set! _dx (car pair))
         (set! _dy (cadr pair))
         (set! _length 1)
         (invalidate-vars _slope))))
       ((dx dy) (set! _dx dx)
            (set! _dy dy)
        (invalidate-vars _length _theta _net-theta _pos-theta _slope))))
    
    ;SLOPE
    (define (refresh-slope)
      (if (fresh? _dx) ;use _dxy if available
      (linear-m _dx _dy)
      ;else use theta
      (let ((slp (tan _theta)))
        (if (>= (abs slp) infinite-slope) ;if >threshold, round to infinity
        infinity
        slp))))
    (define/public (invalidate-slope) (invalidate-vars _slope))
    (define/public (slope) (ensure-fresh _slope refresh-slope))

    (define/public (l-normal)
      (if (fresh? _dx)
      (make-object (my-angle-class this) (- _dy) _dx)
      (make-object (my-angle-class this) (+ _theta pi/2))))
    (define/public (r-normal)
      (if (fresh? _dx)
      (make-object (my-angle-class this) _dy (- _dx))
      (make-object (my-angle-class this) (- _theta pi/2))))

    ;;UNIT-CROSS/DOT are complex b/c must decide most efficient computation
    ; depending on freshness of _theta&_dxy of this&other
    ;       |    Other
    ;       | th dxy both  <-- which are fresh?
    ; T ----+------------
    ; h th  | th  th  th
    ; i dxy | th dxy dxy   <-- used in each condition
    ; s both| th dxy dxy
    (define (unit-crossdot-via-theta dot? arg)
      (let* ((theta2 (cond ((number? arg) arg)
               ((list? arg) (atan (cadr arg) (car arg)))
               (else (send arg theta))))
         (sweep (- theta2 (theta))))
    (if dot?
        (cos sweep)
        (sin sweep))))
    (define unit-crossdot-via-dxy
      ;equals sin of angle this->other; assumes fresh _dx, _dy
      (case-lambda
       ((dot? obj) (if (is-a? obj geo-vector%)
               (apply unit-crossdot-via-dxy dot? (send obj length) (send obj dxy))
               (apply unit-crossdot-via-dxy dot? (send obj dxy))))
       ((dot? dx2 dy2)     (unit-crossdot-via-dxy dot? (hypotenuse dx2 dy2) dx2 dy2))
       ((dot? len2 dx2 dy2) (/ (if dot?
                   (+ (* _dx dx2) (* _dy dy2))
                   (- (* _dx dy2) (* _dy dx2)))
                   (* (ensure-fresh _length refresh-length) len2)))))
    (define unit-crossdot
      (case-lambda
       ((dot? arg) (cond ((not (fresh-dxy?))    (unit-crossdot-via-theta dot? arg))
            ((list? arg)           (apply unit-crossdot-via-dxy dot? arg))
            ((send arg fresh-dxy?) (unit-crossdot-via-dxy dot? arg))
            (else                  (unit-crossdot-via-theta dot? arg))))
       ((dot? dx2 dy2) (unit-crossdot dot? (list dx2 dy2)))))
    (define/public (unit-cross . args)
      (apply unit-crossdot #f args))
    (define/public (unit-dot . args)
      (apply unit-crossdot #t args))

    ;MISC UNARY
    (define/public (upward?)
      (if (fresh? _dy)
      (positive? _dy)
      (let ((theta (send this net-theta)))
        (positive? theta))))
    (define/public (downward?)
      (if (fresh? _dy)
      (negative? _dy)
      (let ((theta (send this net-theta)))
        (negative? theta))))
    (define/public (rightward?)
      (if (fresh? _dx)
      (or (positive? _dx) (and (zero? _dx) (zero? _dy))) ;special case for null vect
      (let ((theta (send this net-theta)))
        (< (abs theta) pi/2))))
    (define/public (leftward?)
      (if (fresh? _dx)
      (negative? _dx)
      (let ((theta (send this net-theta)))
        (> (abs theta) pi/2))))

    ;MISC BINARY
    (define/public (plus-angle ang2)
      (make-object geo-angle% (+ (theta) (if (number? ang2) ang2
                         (send ang2 theta)))))
    (define/public (minus-angle ang2)
      (make-object geo-angle% (- (theta) (if (number? ang2) ang2
                         (send ang2 theta)))))
    (define/public (sweep-to-angle ang2)
      (make-object geo-angle% (- (if (number? ang2) ang2
                     (send ang2 theta)) (theta))))
    (define/public (same-dir? ang2)
      (cond ((and (fresh-theta?) (send ang2 fresh-theta?)) ;prefer theta
         (equal? (net-theta) (send ang2 net-theta)))
        ;else use slope
        ((not (equal? (slope) (send ang2 slope))) #f)
          ;equal slope; compare signs of dx,dy
        (else (let* ((dxy1 (dxy))
             (dxy2 (send ang2 dxy))
             (sx1 (sign (car dxy1)))
             (sx2 (sign (car dxy2))))
            (cond ((not (equal? sx1 sx2)) #f)
              ((not (zero? sx1)) #t)
                        ; dxs both 0, use dys
              (else (equal? (sign (cadr dxy1)) (sign (cadr dxy2)))))))))
    (define (parallel-pair? ang2)
      (if (or (fresh-dxy?) (send ang2 fresh-dxy?)) ;prefer slope
      (parallel-slope? (slope) (send ang2 slope))
    ;else use theta
      (let ((th1 (net-theta))
        (th2 (send ang2 net-theta)))
        (or (equal? th1 th2)
        (equal? (+ th1 pi) th2)
        (equal? (- th1 pi) th2)))))

    ;MISC N-NARY
    (define/public (parallel? . angs)
      (cond ((null? angs) #t)
        ((not (parallel-pair? (car angs))) #f)
        (else (send/apply this parallel? (cdr angs)))))

    (define/public (between? a b)
    ;returns #t if ccw rotation from A hits this before B
      (let ((my-pos (send this pos-theta))
            (a-pos (send a pos-theta))
            (b-pos (send b pos-theta)))
    (<= (pos-theta (- my-pos a-pos)) (pos-theta (- b-pos a-pos)))))

    (define/public (within? a b)
    ;returns #t iff this is in smaller of a->b, b->a
      (let* ((my-pos (send this pos-theta))
             (a-pos (send a pos-theta))
             (b-pos (send b pos-theta))
             (b-a (pos-theta (- b-pos a-pos)))
             (me-a (pos-theta (- my-pos a-pos))))
        (if (<= b-a pi)
            (<= me-a b-a) ;normal between?, like above
            (or (>= me-a b-a)
                (zero? me-a)))))

;    (define/public (splits? a b)
      ;returns #t iff this or its opposite is in smaller of a->b, b->a
      ; i.e. a and b on opposite sides of this line
;      (let* ((neta (net-theta (- (send a theta) (theta))))
;         (netb (net-theta (- (send b theta) (theta)))))
;    (!= (2val-sign neta) (2val-sign netb))))
    (define/public (splits? a b)
      ;returns #t iff this or its opposite is in smaller of a->b, b->a
      ; i.e. a and b on opposite sides of this line
      (let* ((neta (net-theta (- (theta) (send a theta))))
         (netb (net-theta (- (theta) (send b theta)))))
    (!= (2val-sign neta) (2val-sign netb))))
))

(define (scale-dxy fact dx dy)
  (list (* fact dx) (* fact dy)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;     | | /~~  /\ ~T~ /~\ /~\
;     | | |-- /    |  | | |_/
;      V  |__ \_/  |  |_/ | \

;(make-object geo-vector% {(dx dy) | dx dy | angle length } )
(define geo-vector%
;a geo-vector is a direction with length, but no location/origin
; (therefore does not specify a line, cannot be slid)
; While _dx _dy are optional for angle, always required for vector
  (class geo-angle%
    (init (arg0 '(0 0)) (arg1 #f))
    (inherit-field _dx _dy _length)
    ;interpret args
    (cond ((list? arg0) (super-instantiate (arg0)))  ; (dx dy)
      ((is-a? arg0 geo-angle%)                   ; angle length
       (if (send arg0 fresh-dxy?)          ;use angle dxy
           (let ((dxy (send arg0 dxy)))
         (super-instantiate (dxy))
         (send this length arg1))
           (let ((thet (send arg0 theta))) ;use angle theta
         (super-instantiate (thet))
         (set! _length arg1)
         (set! _dx (* _length (cos thet)))
         (set! _dy (* _length (sin thet))))))
      (else (super-instantiate (arg0 arg1)))) ;dx dy

    ;REDEFS
    (define/override (describe-fields)
      (string-append (super describe-fields)
             (format "geo-vector%: length:~a~%" _length)))

    (define/public (copy-as-vector)
      (make-object geo-vector% _dx _dy))
    (define/override (clone) (copy-as-vector))
    
    (define/override dxy
      (case-lambda
        (() (list _dx _dy))
    ((vect) (let ((dxy-pair (if (list? vect)
                   vect
                   (send vect dxy))))
          (send/apply this dxy dxy-pair)))
    ((xval yval) (set! _dx xval)
                 (set! _dy yval)
             (send this invalidate-length)
             (send this invalidate-theta)
             (send this invalidate-slope))))

    ;LOCAL
    (define/public dx
      (case-lambda
        (() _dx)
        ((val)    (set! _dx val)
                  (send this invalidate-length)
                  (send this invalidate-theta)
          (send this invalidate-slope))))

    (define/public dy
      (case-lambda
        (() _dy)
        ((val)    (set! _dy val)
                  (send this invalidate-length)
                  (send this invalidate-theta)
          (send this invalidate-slope))))

    (define (refresh-length)
      (hypotenuse _dx _dy))
    (define/public (invalidate-length) (invalidate-vars _length))
    (define/public length
      (case-lambda
       (() (ensure-fresh _length refresh-length))
       ((newlen) (if (zero?  (ensure-fresh _length refresh-length))
             (set! _dx newlen) ;null vector -> assume angle 0
                                   ; (_dy should be 0 already)
                 ;else
             (let ((scale (/ newlen _length)))
               (set! _dx (* _dx scale))
               (set! _dy (* _dy scale))))
             (set! _length (abs newlen)))))

    (define/public (scale! scl)
      (let ((len (length)))
    (when (not (zero? len))
        (begin
          (set! _dx (* _dx scl))
          (set! _dy (* _dy scl))
          (set! _length (abs (* len scl)))))))
    
    (define/public (add-dxy! dx dy)
       (send this dxy (+ _dx dx) (+ _dy dy)))
    (define/public (subtract-dxy! dx dy)
       (send this dxy (- _dx dx) (- _dy dy)))
    (define/public add!
      (vector-op-template add-dxy!))
    (define/public subtract!
      (vector-op-template subtract-dxy!))    

    (define/public (add-dxy dx dy)
      (let ((new (send this clone)))
    (send new add-dxy! dx dy)
    new))
    (define/public (subtract-dxy dx dy)
      (let ((new (send this clone)))
    (send new subtract-dxy! dx dy)
    new))
    (define/public add
      (vector-op-template add-dxy))
    (define/public subtract
      (vector-op-template subtract-dxy))

    (define/public (cross-dxy dx dy)
      (- (* _dx dy) (* _dy dx)))
    (define/public cross
      (vector-op-template cross-dxy))

    (define/public (dot-dxy dx dy)
      (+ (* _dx dx) (* _dy dy)))
    (define/public dot
      (vector-op-template dot-dxy))

    (define/public (convert-to-basis basis)
      ;basis must be a geo-vector%
      (let* ((basis-len-sqr (sqr (send basis length)))
         (u (/ (send this dot basis)
           basis-len-sqr))
         (v (/ (- (send this cross basis)) ; = (this dot (send basis r-normal))
           basis-len-sqr)))
    (make-object geo-vector% u v)))

    (define/public (convert-from-basis basis)
      (let* ((basis-len-sqr (sqr (send basis length)))
         (bxy (send basis dxy))
         (bx (car bxy))
         (by (cadr bxy)))
    (make-object geo-vector%
             (- (* (dx) bx) (* (dy) by))
             (+ (* (dx) by) (* (dy) bx)))))         
))

(define (null-vector) (make-object geo-vector% 0 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;     |   | |\ | /~~
;     |   | | \| |--
;     |__ | |  V |__

;(make-object geo-line% slope intercept)
(define geo-line%
  (class geo-actor%
    (init-field (_slope 0) (_intercept 0))
    ;intercept is normally y-intercept, but is x-intercept when slope=infinite
    (super-instantiate ())

    ;REDEFS
    (define/override (attach-to obj name)
      (if (is-a? this geo-segment%)
      (super attach-to obj name)
      (error "Can't attach geo-line% or geo-ray% to a dynaobject.")))

    (define/public (copy-as-line)
      (make-object geo-line% (slope) (intercept)))
    (define/override (clone) (copy-as-line))
    (define/override (describe-fields)
      (string-append (super describe-fields)
             (format "geo-line%: slope:~a intercept:~a~%"
                 _slope _intercept)))

    ;LOCAL
    (define/public slope
      (case-lambda
       (()  (ensure-fresh _slope refresh-slope))
       ((m) (set! _slope m))))
    (define/public intercept
      (case-lambda
       (()  (ensure-fresh _intercept refresh-intercept))
       ((b) (set! _intercept b))))
    ; line cannot refresh these if stale;
    ; subclasses (e.g. ray, segment) must redefine refresh-slope/intercept
    (define/public (refresh-slope) _slope)
    (define/public (refresh-intercept) _intercept)

    (define/public slope-intercept 
      (case-lambda
        (() (list (ensure-fresh _slope refresh-slope) (ensure-fresh _intercept refresh-intercept)))
        ((m b)
         (set! _slope m)
         (set! _intercept b))))

    (define/public (invalidate-slope) (invalidate-vars _slope))
    (define/public (invalidate-intercept) (invalidate-vars _intercept))

    (define/public (vertical?) (infinite? (ensure-fresh _slope refresh-slope)))

    (define (go-thru-xy x y)
      (ensure-fresh _slope refresh-slope)
      (set! _intercept (linear-b _slope x y))
      _intercept)

    (define/public go-thru-point
      (case-lambda
        ((pt)  (apply go-thru-xy (send pt xy)))
        ((x y) (go-thru-xy x y))))

    (define/public (slide-ul dist) ;slides upward (or left if vert) to separation dist
      (intercept) ;refresh
      (if (vertical?)
      (-= _intercept dist)
      (+= _intercept (* dist (sqrt (+ 1 (sqr (slope))))))))
    (define/public (slide-dr dist) ;slides downward (or right if vert)
      (intercept) ;refresh
      (if (vertical?)
      (+= _intercept dist)
      (-= _intercept (* dist (sqrt (+ 1 (sqr (slope))))))))

    (define (origin-xy)
      (if (vertical?)
      (list (intercept) 0)
      (list 0 (intercept))))

    (define/override (slide-by-dxy! dx dy)
      (apply go-thru-xy (map + (origin-xy) (list dx dy))))

    (define (same-slope? s1 s2)
      (or (eqv? s1 s2)
          (and (infinite? s1)
               (infinite? s2))))
    (define/public (parallel? line2)
      (same-slope? (slope) (send line2 slope)))
    (define/public (equal? line2)
      (and (parallel? line2)
           (eqv? (intercept) (send line2 intercept))))
    (define/public normal-slope
      (case-lambda
        (() (normal-slope (send this slope)))
        ((slp) 
     (cond ((infinite? slp) 0)
           ((zero? slp) infinity)
           (else (- (/ 1 slp)))))))
    (define/public (normal? line2)
      (same-slope? (normal-slope) (send line2 slope)))
    
    (define/public (y-at-x x)
      (linear-y x (slope) (intercept)))

    (define/public (x-at-y y)
      (linear-x y (slope) (intercept)))

    (define/public (intersect-line-of l2)
      (if (parallel? l2)
      null
      (let ((parents (list this l2)))
        (ensure-fresh _intercept refresh-intercept)
        (if (vertical?)
          (list (make-object geo-intersect-point%
                 _intercept (send l2 y-at-x _intercept) ;new x,y
                  parents))
          (let* ((m2  (send l2 slope))
             (b2  (send l2 intercept)))
        (if (send l2 vertical?)
            (list (make-object geo-intersect-point%
                       b2 (send this y-at-x b2);new x, y
                       parents))
            (let* ((x (/ (- b2 _intercept) (- _slope m2)))
               (y (+ (* _slope x) _intercept)))
              (list (make-object geo-intersect-point% x y parents)))))))))

    (define (intersect-oval) 'undefined) ;FINISH...

    ; If objs of diff types are intersected, responsibility goes to most specific:
    ;       line ray  seg  poly
    ;      +------------------
    ; line |line ray  seg  poly
    ; ray  |ray  ray  seg  poly
    ; seg  |seg  seg  seg  poly
    ; poly |poly poly poly poly

    ;returns list of points of intersection with obj (or null if none)
    (define/public (intersects obj)
      ;must check classes from specific->general
      (cond
       ((is-a? obj geo-segment%)  (send obj intersects this)) ; seg's prob
       ((is-a? obj geo-ray%)      (send obj intersects this)) ; ray's prob
       ((is-a? obj geo-line%)     (intersect-line-of obj))
       ((is-a? obj geo-polyline%) (send obj intersects this)) ; poly's prob
       ((is-a? obj geo-polygon%)  (send obj intersects this))
       (else null)))

    (define/public (closest-pt-to pt)
      (let* ((normal-line (ic
               (make-object geo-line% (normal-slope) 0)
               (go-thru-point pt))))
    (car (intersect-line-of normal-line))))
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;     /~\  /| \ /
;     |_/ /-|  Y
;     | \/  |  |

;(make-object geo-ray {pt-obj | (x y)}
;                     {ang-obj | theta | (x y) | thru-pt-obj | dx dy})
(define geo-ray%
  (class geo-line%
    (init-field _origin)
    (init arg1 (arg2 #f))
    (ensure-point-object _origin)
    (field (_angle stale))  ; for geo-segments, angle is a vector
    (inherit-field _slope _intercept)
    (super-instantiate (stale stale))

    ;interpret args:
    (cond ((list? arg1)             ;as thru-pt (x y)
         (send this go-thru-point arg1))
      ((is-a? arg1 geo-point%)  ;as thru-pt obj
         (send this go-thru-point arg1))
      ((and (number? arg1) arg2) ;as dx dy
         (send this dxy arg1 arg2))
      (else (send this angle arg1))) ;as ang-obj or theta

    ;REDFES
    (define/override (describe-fields)
      (string-append (super describe-fields)
             (format "geo-ray%: origin:~a angle/vector:~a~%"
                 (name-of-obj _origin)
                 (name-of-obj _angle))))

    (define/public (copy-as-ray)
      (make-object geo-ray% (send _origin clone) (send _angle clone)))
    (define/override (clone) (copy-as-ray))

    (define/override (go-thru-point pt)
      (let ((xy (if (list? pt) pt (send pt xy))))
    (send/apply this dxy (map - xy (send _origin xy)))))

    (define/override (slope)  ;eliminate 1-arg case
       (ensure-fresh _slope refresh-slope))

    (define/override (intercept)  ;eliminate 1-arg case
       (ensure-fresh _intercept refresh-intercept))

    (define/override (refresh-slope)
      (send (angle) slope))
          
    (define/override (refresh-intercept)
      (super go-thru-point _origin))

    (define/override (slide-by-dxy! dx dy)
      (send this origin slide dx dy)
      (invalidate-vars _intercept))

    (define/override (slide-by-dxy dx dy)
      (let ((clone (send this clone)))
    (send clone slide-by-dxy! dx dy)
    clone))
      
    (define/override (intersects obj)
      ;must check classes from specific->general
      (cond
       ((is-a? obj geo-segment%)  (send obj intersects this)) ; seg's prob
       ((is-a? obj geo-polyline%) (send obj intersects this)) ; poly's prob
       ((is-a? obj geo-polygon%)  (send obj intersects this))
       (else ; ray or line; my problem...
    (let ((ints (send this intersect-line-of obj))) ;intersect as line first
      (if (null? ints)
          null
          (let ((int (car ints)))
        (cond ((not (send this in-my-direction? int))
                 null)
              ((and (is-a? obj geo-ray%)
                (not (send obj in-my-direction? int)))
                 null)
              (else ints))))))))

    (define/override (closest-pt-to pt)
      (let ((int (super closest-pt-to pt)))
    (if (send this in-my-direction? int)
        int
        _origin)))

    ;LOCAL
    (define/public (in-my-direction? pt)
      ; assuming pt is collinear w. this, is it after _origin in direction _angle?
      (ordered-in-direction? _origin pt _angle))

    (define/public origin 
      (case-lambda
        (() _origin)
        ((pt) (ensure-point-object pt)
          (invalidate-vars _intercept)
              (set! _origin pt))))

    (define/public angle
      (case-lambda
        (() _angle)
        ((ang) (ensure-angle-object ang)
           (invalidate-vars _slope _intercept)
               (set! _angle ang))))
      
    (define/public (dxy . args)
      (if (null? args)
      (send _angle dxy)
      (begin
        (when (stale? _angle) ;create _angle if needed
        (if (is-a? this geo-segment%)
            (set! _angle (make-object geo-vector%))
            (set! _angle (make-object geo-angle%))))
        (send/apply _angle dxy args)
        (invalidate-vars _slope _intercept))))
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;     /~ /~~  /\ |\ /| /~~ |\ | ~T~
;     \_ |-- / _ | V | |-- | \|  |
;     _/ |__ |_/ |   | |__ |  V  |

;(make-object geo-segment% {from-pt-obj | (x y)}
;                          {  to-pt-obj | (x y) | vector-obj | dx dy} )
(define geo-segment%
 ; A segment is a fixed vector (origin + vector) or line-segment (origin->endpoint)
  (class geo-ray%
    ;ALWAYS FRESH: any two of (_origin | _angle | _endpoint)
    (init arg0)
    (init arg1 (arg2 0))
    (inherit-field _origin)
    (inherit-field _angle) ;the segment's vector
    (inherit-field _slope _intercept)
    (field (_bbox stale)
           (_endpoint stale)
       (_exclude-endpt? #f) ;if #t, segment is open-ended in intersections
       (_anchor-endpt? #f)) ;if #f, length/dxy/angle/slide measured from origin
                ;if #t,                        measured from endpt 
    ; ...
    ;SEE INIT CODE AT END OF CLASS DEFN

    ;REDEFS
    (define/override (describe-fields)
      (string-append (super describe-fields)
             (format "geo-segment%: endpoint:~a bbox:~a anchor-endpt?~a~%"
                 (name-of-obj _endpoint) _bbox _anchor-endpt?)))

    (define/override (intersects obj)
      (def ints)
      (cond
       ((is-a? obj geo-polyline%) (send obj intersects this)) ; poly's prob
       ((is-a? obj geo-polygon%)  (send obj intersects this))
       ; seg, ray, or line; my problem...
       ((is-a? obj geo-segment%)  ;segment:
      (cond ((not (bboverlap? (send this bbox) (send obj bbox))) null)
        ((null? (cset! ints (send this intersect-line-of obj))) null)
        ((let ((int (car ints)))
           (and (send this in-my-range? int)
            (send obj  in-my-range? int))) ints)
        (else null)))
       ((is-a? obj geo-ray%)   ;ray:
      (cond ((null? (cset! ints (send this intersect-line-of obj))) null)
        ((let ((int (car ints)))
           (and (send obj in-my-direction? int)
            (send this in-my-range? int))) ints)
        (else null)))
       ;line:
       ((null? (cset! ints (send this intersect-line-of obj))) null)
       ((let ((int (car ints)))
      (send this in-my-range? int)) ints)
       (else null)))

    (define/override (closest-pt-to pt)
      (let ((int (super closest-pt-to pt)))
    ;already >= origin, but test if within endpt
    (if (ordered-in-direction? (endpoint) int (angle))
        _endpoint
        int)))

    (define/override (slide-by-dxy! dx dy)
      (send this invalidate-intercept)
      (when (fresh? _bbox)
      (set! _bbox (bbslide _bbox dx dy)))
      (when (fresh? _origin)
      (send _origin slide-by-dxy! dx dy))
      (when (fresh? _endpoint)
      (send _endpoint slide-by-dxy! dx dy)))

    (define/override (slide-by-dxy dx dy)
      (let ((clone (send this clone)))
    (send clone slide-by-dxy! dx dy)
    clone))

    (define/override (go-thru-point pt) #f) ;eliminate this msg

    (define/public (copy-as-segment)
      (make-object geo-segment% (send _origin clone)
        (list (send this dx) (send this dy))))
    (define/override (clone) (copy-as-segment))

    (define-macro (anchor-end-template msg . args)
      `(if _anchor-endpt?
       (begin
         (ensure-fresh _endpoint refresh-endpoint)
         (,msg ,@args)
         (invalidate-origin))
       (begin
         (ensure-fresh _origin refresh-origin)
         (,msg ,@args)
         (invalidate-endpoint))))

    (define/override dxy
      (case-lambda
       (() (super dxy))
       ((arg1) (anchor-end-template super dxy arg1))
       ((arg1 arg2) (anchor-end-template super dxy arg1 arg2))))

    (define/override angle
      (case-lambda
       (()   (super angle))
       ((val) (ensure-vector-object val)  ;needs vector inst. of angle
          (anchor-end-template super angle val))))
    ;LOCAL
    (define (set-angle-dx val) (send _angle dx val))
    (define (set-angle-dy val) (send _angle dy val))
    (define (set-angle-length val) (send _angle length val))

    (define/public dx
      (case-lambda
       (()   (send _angle dx))
       ((val) (anchor-end-template set-angle-dx val))))

    (define/public dy
      (case-lambda
       (()   (send _angle dy))
       ((val) (anchor-end-template set-angle-dy val))))

    (define/public length
      (case-lambda
       (()   (send _angle length))
       ((val) (anchor-end-template set-angle-length val))))

    (define/public (in-my-range? pt)
      (send pt betweenish? (origin) (endpoint) _exclude-endpt?))

    (define/public exclude-endpt? (get/set _exclude-endpt?))
    (define/public anchor-endpt? (get/set _anchor-endpt?))

    (define (refresh-origin)
      (make-object geo-point% (map - (send _endpoint xy) (send _angle dxy))))

    (define (refresh-endpoint)
      (send _origin slide-copy this)) ;treats this as a vector
    
    (define/public (invalidate-origin) 
      (invalidate-vars _origin _bbox))

    (define/public (invalidate-endpoint) 
      (invalidate-vars _endpoint _bbox))

    (define (refresh-bbox)
      (def lox) (def loy) (def hix) (def hiy)
      (let ((ox (send (origin) x))
        (oy (send _origin y))
        (ex (send (endpoint) x))
        (ey (send _endpoint y)))
    (if (< ox ex)
        (begin
          (set! lox ox)
          (set! hix ex))
        (begin
          (set! lox ex)
          (set! hix ox)))
    (if (< oy ey)
        (begin
          (set! loy oy)
          (set! hiy ey))
        (begin
          (set! loy ey)
          (set! hiy oy)))
    (list lox loy hix hiy)))
    (define/public (bbox)
      (ensure-fresh _bbox refresh-bbox))
      
    (define local-endpoint
      (case-lambda
       (()   (ensure-fresh _endpoint refresh-endpoint))
       ((pt) (begin  ;keep origin, stretch dxy
        (ensure-point-object pt)
        (set! _angle (send (send this origin) vector-to pt))
        (invalidate-vars _slope _intercept _bbox)
        (set! _endpoint pt)))
        ((x y)
     (send this endpoint (make-object geo-point% x y)))))

    (define/public (endpoint . args) (apply local-endpoint args))

    (define local-origin
      ;moves origin, leaving endpoint fixed
      (case-lambda
       (() (ensure-fresh _origin refresh-origin))
       ((pt) (begin
           (ensure-point-object pt)
           (set! _angle (send pt vector-to (send this endpoint)))
           (invalidate-vars _slope _intercept _bbox)
           (set! _origin pt)))
       ((x y) (send this origin (make-object geo-point% x y)))))

    (define/override (origin . args) (apply local-origin args))

    (define/public slide-to
      (case-lambda
       ((pt) (ensure-point-object pt)
         (send this invalidate-intercept)
         (if _anchor-endpt?
         (begin
           (set! _endpoint pt)
           (invalidate-origin))
         (begin
           (set! _origin pt)
           (invalidate-endpoint))))))

    ;returns point at fraction (default .5) of translation from origin to endpt
    (define/public midpoint
      (case-lambda
       (() (midpoint .5))
       ((frac)
    (apply make-object geo-point% (map (lambda (o d) (+ o (* frac d)))
                       (send _origin xy)
                       (send this dxy))))))
    
;     (define/public (slide-left dist)
;       (let ((l-normal-vect (send (send this angle) l-normal)))
;     (send l-normal-vect length dist)
;     (send this slide-copy l-normal-vect)))
;     (define/public (slide-right dist)
;       (let ((r-normal-vect (send (send this angle) r-normal)))
;     (send r-normal-vect length dist)
;     (send this slide-copy r-normal-vect)))

    (define/public (l-normal-segment)
      (let ((newme (send this clone)))
        (send newme angle (send (send this angle) l-normal))
        newme))
    (define/public (l-normal-angle)
      (send (send this angle) l-normal))

    ;INIT continued...
    (ensure-point-object arg0)
    ;interpret second arg...
    (cond ((is-a? arg1 geo-vector%) (super-instantiate (arg0 arg1)))
      ((is-a? arg1 geo-point%)  (begin
                      (super-instantiate (arg0 0 0))
                      (local-endpoint arg1)))
      ((list? arg1)             (begin
                      (super-instantiate (arg0 0 0))
                      (local-endpoint arg1)))
      (else                       (super-instantiate (arg0 arg1 arg2)))) ;dx dy args
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;     
;     /~\ /~\ |  \ /   /~ /~~  /\   / /~~ |~\  /\ /~~
;     |_/ | | |   Y -- \_ |-- / _  /  |-- |  >/ _ |--
;     |   |_/ |__ |    _/ |__ \_/ /   |__ |_/ \_/ |__

(define-macro (my-seg-type)
  '(if (is-a? this geo-polygon-edge%)
       geo-polygon-edge%
       geo-poly-segment%))

;(make-object geo-segment% parent-obj
;                          {from-pt-obj | (x y)}
;                          {  to-pt-obj | (x y) | vector-obj | dx dy} )
(define geo-poly-segment%
;segment which is part of a series, as in poly-line or polyon
; circular RING in polygons;  bounded PATH in polylines
  (class geo-segment%
    (init-field _parent) ;polygon/polyline containing this seg
    (init arg1 arg2 (arg3 #f))
    (inherit-field _bbox _origin _endpoint)
    (field (_next #f) (_prev #f))  ;next/prev segment in path/ring
    (super-instantiate (arg1 arg2 arg3))

    (send this exclude-endpt? #t) ;ensures that lines/rays/etc thru
        ;vertices will trigger intersection on only one of two adjacent poly-segs

    (field (_already-moved #f))  ;stops recursion when moving origin/endpt

    ;REDEFS
    (define/override origin
      (case-lambda
       (() (super origin))
       ((pt) (when (not _already-moved)
         (begin
           (send _parent invalidate-vertices)
           (ensure-point-object pt)
           (super origin pt)   ;move this origin
           (when _prev
               (begin
             (set! _already-moved #t)
             (send _prev endpoint pt)
             (set! _already-moved #f))))))))
    (define/override endpoint
      (case-lambda
       (() (super endpoint))
       ((pt) ;(display "running poly-seg endpoint...\n")
    (when (not _already-moved)
          (begin
            (send _parent invalidate-vertices)
            (ensure-point-object pt)  ;move endpoint to pt
            (super endpoint pt)
            (when _next
            (begin
              (set! _already-moved #t)
              (send _next origin pt)
              (set! _already-moved #f))))))))

    (define/override (slide-to pt) #f) ;eliminate this

    (define/override (describe-fields)
      (string-append (super describe-fields)
             (format "geo-poly-segment%: parent:~a next:~a prev:~a~%"
                 (name-of-obj _parent) (name-of-obj _next) (name-of-obj _prev))))
    
    (define/public (slide-alone-by-dxy! dx dy)
      ;Parent polygon/polyline will call this for each segment
      (send this invalidate-intercept)
      (when (fresh? _bbox)
      (set! _bbox (bbslide _bbox dx dy)))
      (when (fresh? _origin)
      (send _origin slide-by-dxy! dx dy))
      ; _endpt is shared, and will be slid as _origin of next segment
      ;   slide _endpt only if this is last in path
      (when (and (not _next) (fresh? _endpoint))
      (send _endpoint slide-by-dxy! dx dy)))

    (define/override (slide-by-dxy! dx dy)
      ;must be slid as polygon/polyline, not indiv. segs
      (send _parent slide-by-dxy! dx dy))

    ;LOCAL
    (define/public (parent) _parent)

    ;these should not be used alone; use insert../cut.. instead
    (define/public next (get/set _next))
    (define/public prev (get/set _prev))

    (define/public (cut-origin)
      ;     --->a===>b~~~>  >> ------->b~~~>
      ;delete this seg, adjust prev endpoint
      (if _prev
      (begin
        (send _prev next _next)
        (send _prev endpoint (send this endpoint)))
      (send _parent first-seg _next))
      (when _next
      (send _next prev _prev))
      (when (eq? this (send _parent first-seg))
      (send _parent first-seg _next))
      ;ideally should delete this segment, but settle for isolating it:
      (set! _prev #f)
      (set! _next #f)
      (send _parent invalidate-segs-and-vertices))

    (define/public (cut-endpoint)
      ;     --->a===>b~~~>  >> --->a~~~~~~~>
      ;delete this seg, adjust next origin
      (if _prev
      (send _prev next _next)
      (send _parent first-seg _next))
      (if _next
      (send _next prev _prev)
      (send _next origin (send this origin)))
      (when (eq? this (send _parent first-seg))
      (send _parent first-seg _next))
      (set! _prev #f)
      (set! _next #f)
      (send _parent invalidate-segs-and-vertices))

    (define/public (insert-segment-start pt)
      ;     a----->b  >>     a==>pt-->b
      (let* ((new-seg (make-object (my-seg-type) _parent (send this origin) pt)))
    (if _prev
        (send _prev next new-seg)         ;relink prev seg
        (send _parent first-seg new-seg)) ;else new first seg; update parent
    (send new-seg prev _prev)
    (send new-seg next this)
    (set! _prev new-seg)
    (send this origin pt)
    (send _parent invalidate-segs-and-vertices)
    new-seg))

    (define/public (insert-segment-end pt)
      ;     a----->b  >>     a-->pt==>b
      (let* ((new-seg (make-object (my-seg-type) _parent pt (send this endpoint))))
    (when _next
        (send _next prev new-seg))
    (send new-seg next _next)
    (send new-seg prev this)
    (set! _next new-seg)
    (send this endpoint pt)
    (send _parent invalidate-segs-and-vertices)
    new-seg))

    (define/public (insert-segment-before pt)
      ;     a----->b  >> pt==>a----->b 
      (let* ((new-seg (make-object (my-seg-type) _parent pt (send this origin))))
    (if _prev
        (begin
          (send _prev next new-seg)       ;relink, move prev seg
          (send _prev endpoint pt))
        (send _parent first-seg new-seg)) ;else new first seg; update parent
    (send new-seg prev _prev)
    (send new-seg next this)
    (set! _prev new-seg)
    (send _parent invalidate-segs-and-vertices)
    new-seg))

    (define/public (insert-segment-after pt)
      ;     a----->b  >>     a----->b==>pt
      (let* ((new-seg (make-object (my-seg-type) _parent (send this endpoint) pt)))
    (when _next
        (begin
          (send _next prev new-seg)
          (send _next origin pt)))
    (send new-seg next _next)
    (send new-seg prev this)
    (set! _next new-seg)
    (send _parent invalidate-segs-and-vertices)
    new-seg))
))

; A polygon edge also encodes the triangle between its 2 vertices and
; the poly's anchor point.   These triangles radially partition the polygon.
; Edge N follows vertex N in a ccw direction;
; spoke N is vector from anchor to vertex N (= origin of edge).
;
;          edge
;       <--------* vtx     |
; next /.       ,          | inward-normal
; edge/  . ang ,           V
;    /    .<--, spoke
;   /      . ,
;  / . . . .* 
;   \        anchor
;    \

;(make-object geo-polygon-edge% parent-obj
;                          {from-pt-obj | (x y)}
;                          {  to-pt-obj | (x y) | vector-obj | dx dy} )
(define geo-polygon-edge%
  (class geo-poly-segment%
    (init parent) ;polygon containing this seg
    (init arg1 arg2 (arg3 #f))
    (super-instantiate (parent arg1 arg2 arg3))

    (field (_spoke stale)
           (_area stale)  ;area of triangle
       (_sector-angle stale) ;sweep angle around anchor from spoke1->spoke2
       (_median stale) ;median of triangle
       (_inward-normal stale))

    ;REDEFS
    (define/override (describe-fields)
      (string-append (super describe-fields)
             (format "geo-polygon-edge%: spoke:~a area:~a sector-angle:~a median:~a~%"
                 (name-of-obj _spoke) _area
                 (name-of-obj _sector-angle) (name-of-obj _median))))

    (define/override (slide-alone-by-dxy! dx dy)
      (super slide-alone-by-dxy! dx dy)
      (when (fresh? _median)
      (send _median slide-by-dxy! dx dy)))

    (define/public (set-anchor pt)
    ;should be called for all sectors if at all
    ;(send _spoke origin pt)
      (invalidate-vars _spoke _median _area _sector-angle))
    
    (define (refresh-spoke)
      (send (send parent anchor) vector-to (send this origin)))
    (define/public (spoke) (ensure-fresh _spoke refresh-spoke))

    (define/override origin
      (case-lambda
       (() (super origin))
       ((pt) (super origin pt)
    ;(send _spoke endpoint (send this origin))
    (invalidate-vars _spoke _median _area _sector-angle _inward-normal))))
    (define/override endpoint
      (case-lambda
       (() (super endpoint))
       ((pt) (super endpoint pt))
       (invalidate-vars _median _area _sector-angle _inward-normal)))
    
    (define/public (inward-normal)
    ;returns segment: rim rotated pi/2 ccw around vtx
      (when (stale? _inward-normal)
      (set! _inward-normal (send this left-normal)))
      _inward-normal)
    
    (define (refresh-median)
    ;finds median of sector triangle (= area centroid)
      (let* ((rim-bisector (make-object geo-segment%
                    (send this midpoint)
                    (send (send this parent) anchor))))
    (send rim-bisector midpoint)))
    (define/public (median)
      (ensure-fresh _median refresh-median))
    
    (define (refresh-angle-area)
    ;almost same computation to find either, so find both together
      (let* ((my-spoke (spoke))
         (next-spoke (send (send this next) spoke))
         (l1 (send my-spoke length)) 
         (l2 (send next-spoke length))
         (z  (send my-spoke cross next-spoke))
         (s  (2val-sign z))
         (t1 (* l1 l2)))
    (let-values (((ang area)
              (if (zero? t1)
              (values 0 0)
              (let* ((dot (send my-spoke dot next-spoke))
                 (t2 (/ dot t1)))
                (if (> (abs t2) 1)
                (values 0 0)
                (let ((theta (* s (acos t2))))
                  (values theta
                      (* (/ t1 2) (sin theta)))))))))
      (set! _sector-angle (make-object geo-angle% ang))
      (set! _area area))))
    (define/public (sector-angle)
      (when (stale? _sector-angle)
      (refresh-angle-area))
      _sector-angle)
    (define/public (area)
      (when (stale? _area)
      (refresh-angle-area))
      _area)
    
    (define/public (contains-line-of-ang? ang)
      (let* ((ang1 (spoke))
         (ang2 (send (send this next) spoke)))
    ;always reject if ang, ang1, ang2 collinear
    (if (send ang parallel? ang1 ang2)
        #f
        (send ang splits? ang1 ang2))))
))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;     /~\ /~\ |  \ / |   | |\ | /~~
;     |_/ | | |   Y  |   | | \| |--
;     |   |_/ |__ |  |__ | |  V |__

; (make-object geo-polyline% (pt0 pt1 ...))
(define geo-polyline%
;polylines use PATH _seg-path as primary structure;
; lists _vertices and _segments are refreshed only as needed
  (class geo-actor%
    (init init-points) ;list of >=2 pts 
    (set! init-points (map (lambda (pt) (ensure-point-object pt)) init-points))
    (super-instantiate ())
    (field (_vertices stale)  ;ready-made lists of vtxs/segs; refresh on demand
           (_segments stale))
    (field (_seg-path  ;always valid, start of PATH
        (make-object geo-poly-segment% this (car init-points) (cadr init-points))))
    (let ((last-seg _seg-path))
      (for-each (lambda (vtx) (set! last-seg
                    (send last-seg insert-segment-after vtx)))
        (cddr init-points)))

    (define (refresh-segments) (path->list _seg-path))
    (define/public (invalidate-segs-and-vertices)
      (set! _segments stale)
      (set! _vertices stale))
    (define/public (segments) (ensure-fresh _segments refresh-segments))

    (define (refresh-vertices)
      (cons (send _seg-path origin)
        (path-map (lambda (seg) (send seg endpoint)) _seg-path)))
    (define/public (invalidate-vertices) (set! _vertices stale))
    (define/public (vertices) (ensure-fresh _vertices refresh-vertices))

    (define/override (slide-by-dxy! dx dy)
      ;also called by any segment receiving slide!...
      (path-foreach (lambda (seg) (send seg slide-alone-by-dxy! dx dy))
            _seg-path))

    (define/public (intersects obj)
      (apply append (path-map (lambda (s) (send s intersects obj)) _seg-path)))

    (define/public first-seg (get/set _seg-path))

    (define/public (segment-n n)
      (path-ref _seg-path n)) ;could also refresh segments, use list-ref
    (define/public (vertex-n n)
      (list-ref (vertices) n))
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;     /~\ /~\ |  \ / /\ /~\ |\ |
;     |_/ | | |   Y / _ | | | \|
;     |   |_/ |__ | \_/ |_/ |  V

; (make-object geo-polygon% (pt0 pt1 ...))
(define geo-polygon%
;polygons use RING _edge-ring as primary structure;
; lists _vertices and _edges are refreshed only as needed
  (class geo-actor%
    (init init-points) ;list of >=1 pts, assumed in ccw circuit order
    (set! init-points (map (lambda (pt) (ensure-point-object pt)) init-points))
    (super-instantiate ())
    (field (_edges stale)  ;ready-made lists of edges/vtxs; refresh on demand
           (_vertices stale)) ; don't set _vertices now; clobbered by (insert...) below 
    (field (_anchor stale)
       (_orientation stale) ;1 for ccw (positive area), -1 for cw
       (_pt-centroid stale)
       (_area-centroid stale)
       (_area stale)
       (_perimeter stale)
       (_bbox stale)) ;_bbox needed only for stand-alone geo-polys; never used unless called
    (field (_edge-ring #f)) ;always valid, start of RING = first edge

    ;(define/public (force-refresh) (invalidate-vars _edge-ring _anchor
;                            _area-centroid _bbox))

    (define (rebuild-from-coords crds)
      ;crds is list of geo-point%s
    ;default anchor is pt-centroid
      (set! _anchor (send (send this point-centroid crds) clone))

      (set! _edge-ring
        (let ((vtx0 (car crds)))
          (make-object geo-polygon-edge% this vtx0 vtx0)))
      (send _edge-ring next _edge-ring)  ;link to self
      (send _edge-ring prev _edge-ring)
    ;now _edge-ring is valid one-point polygon
    ; add remaining points
      (let ((last-edge _edge-ring))
    (for-each (lambda (vtx) (set! last-edge
                      (send last-edge insert-segment-end vtx)))
        (cdr crds)))

      (invalidate-vars _edges _vertices _orientation _area-centroid
               _area _bbox _perimeter)
      _edge-ring
      )

    (rebuild-from-coords init-points)  ;initialize

    (define/public (refresh-edge-ring)
      (let* ((it (send this object))) ;this better work, else stuck
    (send this coords
          (if (or (is-a? it rect%)
              (is-a? it image%)
              (is-a? it text%)
              (is-a? it panel%))
          (coords-from-bbox (send it bbox)) ;only 2pts; must expand
          (send obj coords)))))

    ;REDEFS
    (define/public (rescale-around-xy xfact yfact xy-src)
      (for-each (lambda (edge)
          (let* ((dxy (map - (send (send edge origin) xy) xy-src))
             (dx (* xfact (car dxy)))
             (dy (* yfact (cadr dxy))))
            (send edge origin (map + xy-src (list dx dy)))))
        (edges)))
      
    (define/override (attach-to obj name)
      (super attach-to obj name)
      (super attach-to obj 'reshape-sensitive))

    (define/override (describe-fields)
      (string-append (super describe-fields)
             (format "geo-polygon%: 1st-edge:~a edges:~a vertices:~a
       anchr:~a orient:~a pt-cntrd:~a area-cntrd:~a area:~a perimtr:~a bbox:~a~%"
                 (name-of-obj _edge-ring)
                 (name-of-obj _edges)
                 (name-of-obj _vertices)
                 (name-of-obj _anchor)
                 _orientation
                 (name-of-obj _pt-centroid)
                 (name-of-obj _area-centroid)
                 _area _perimeter _bbox)))
    (define/override (clone) (make-object geo-polygon% (vertices)))

    (define/override (slide-by-dxy! dx dy)
      ;also called by any edge receiving slide...
      (send _anchor slide dx dy)
      (ring-foreach (lambda (edge) (send edge slide-alone-by-dxy! dx dy))
            (ensure-fresh _edge-ring refresh-edge-ring))
      (when (fresh? _bbox)
      (set! _bbox (bbslide _bbox dx dy)))
      (when (fresh? _pt-centroid)  ;danger if _pt-cent = _anchor
      (send _pt-centroid slide dx dy))
      (when (fresh? _area-centroid)
      (send _area-centroid slide dx dy)))

    ;LOCAL
    (define/public (invert)
      (let ((inverse (make-object geo-polygon% (reverse (vertices)))))
    inverse))

    (define (refresh-orientation)  ;there are faster algoritms for this; revisit someday
      (sign (send this area)))     ;currently this never goes stale; need to figure out when that happens
    (define/public orientation
      (case-lambda
       (() (ensure-fresh _orientation refresh-orientation))))
;       ((o) (if (not (eq? (orientation) o))
;        (invert)))))

    (define/public first-seg (get/set _edge-ring))

    (define (refresh-edges) (ring->list (ensure-fresh _edge-ring refresh-edge-ring)))
    (define/public (invalidate-segs-and-vertices)
      (invalidate-vars _edges _vertices _area _perimeter
               _bbox _pt-centroid _area-centroid))
    (define/public (edges) (ensure-fresh _edges refresh-edges))

    (define (refresh-vertices)
      (ring-map (lambda (edge) (send edge origin)) (ensure-fresh _edge-ring refresh-edge-ring)))
    (define/public (invalidate-vertices)
      (invalidate-vars _vertices _area _perimeter
               _bbox _pt-centroid _area-centroid))
    (define/public (vertices) (ensure-fresh _vertices refresh-vertices))

    (define/public (intersects obj)
      (apply append (ring-map (lambda (e) (send e intersects obj)) (ensure-fresh _edge-ring refresh-edge-ring))))

    (define/public (edge-n n)
      (ring-ref (ensure-fresh _edge-ring refresh-edge-ring) n))
    (define/public (vertex-n n)
      (send (edge-n n) origin))
    (define/public (spoke-n n)
      (send (edge-n n) spoke))

    (define/public coords
      (case-lambda
       (()
      (apply append (map (lambda (pt) (send pt xy)) (vertices))))
       ((lst)
    (when (not (list? (car lst)))
        (set! lst (make-pairs lst)))
    (set! lst (map (lambda (pt) (ensure-point-object pt)) lst))
    (rebuild-from-coords lst))))

    (define/public anchor
      (case-lambda
       (() _anchor)
       ((pt) (begin
           (ensure-point-object pt)
           (set! _anchor pt)
           (ring-foreach (lambda (edge) (send edge set-anchor pt))
                 (ensure-fresh _edge-ring refresh-edge-ring))))))

    (define/public point-centroid
      (case-lambda
       (() (point-centroid _vertices))
       ((pts)
    (when (stale? _pt-centroid)
        (let ((xs (map (lambda (vtx) (send vtx x)) pts))
          (ys (map (lambda (vtx) (send vtx y)) pts)))
          (set! _pt-centroid
            (make-object geo-point% (apply mean xs) (apply mean ys)))))
    _pt-centroid)))

    (define/public (area-centroid)
      (when (stale? _area-centroid)
      (let* ((pts    (ring-map (lambda (edge) (send edge median)) (ensure-fresh _edge-ring refresh-edge-ring)))
         (masses (ring-map (lambda (edge) (send edge area))   (ensure-fresh _edge-ring refresh-edge-ring)))
         (xs (map (lambda (pt) (send pt x)) pts))
         (ys (map (lambda (pt) (send pt y)) pts)))
        (set! _area-centroid
          (make-object geo-point%
                   (weighted-mean xs masses)
                   (weighted-mean ys masses)))))
      _area-centroid)

    (define (refresh-area)
      (apply + (ring-map (lambda (edge) (send edge area))
             (ensure-fresh _edge-ring refresh-edge-ring))))
    (define/public (area) (ensure-fresh _area refresh-area))

    (define (refresh-perimeter)
      (apply + (ring-map (lambda (edge) (send edge length))
            (ensure-fresh _edge-ring refresh-edge-ring))))
    (define/public (perimeter)
      (ensure-fresh _perimeter refresh-perimeter))

    (define (refresh-bbox)
      (let* ((xs (map (lambda (vtx) (send vtx x)) (vertices)))
         (ys (map (lambda (vtx) (send vtx y)) (vertices))))
    (list
     (apply min xs) (apply min ys) (apply max xs) (apply max ys))))
    (define/public (bbox) (ensure-fresh _bbox refresh-bbox))
    (define/public (width) (bbwidth (bbox)))
    (define/public (height) (bbheight (bbox)))

    (define/public (contains-pt? pt)
      (let* ((ray (make-object geo-ray% pt _anchor))
         (ray-ang (send ray angle))
         (relevant-edges
          (ring-filter (lambda (edge) (send edge contains-line-of-ang? ray-ang))
               (ensure-fresh _edge-ring refresh-edge-ring)))
         (crossed-edges
          (filter (lambda (edge) (let ((int (send ray intersect-line-of edge)))
                        (and (not (null? int))
                         (send ray in-my-direction? (car int)))))
                    ; (send ray intersects edge))))
              relevant-edges)))
    (odd? (length crossed-edges))))

    (define/public (margin dist)
      (let ((lines
         (ring-map (lambda (edge)
             (let* ((this-line (send edge copy-as-line))
                (shift  (send (send edge angle) r-normal)))
               (send shift length dist)
               (send this-line slide! shift)
               this-line))
               (ensure-fresh _edge-ring refresh-edge-ring))))
    (make-object geo-polygon%
                     (apply append
             (mmap 2 0 1 (lambda (l1 l2)
                   (send l1 intersects l2)) lines)))))

    (define/public (smoothed-margin2 dist)
      (let ((new-vtxs
          (apply append
             (ring-map (lambda (edge)
                (let* ((side-shift (send (send edge angle) r-normal))
                   (myvect (send (send edge angle) clone))
                   (junk (send myvect length 1))
                   (prevvect (send (send (send edge prev) angle) clone))
                   (junk2 (send prevvect length 1))
                   (corner-shift (send prevvect subtract myvect))
;                   (corner-shift (send (send edge spoke) clone))
                   (start (send edge origin))
                   (end (send edge endpoint)))
              (send side-shift length dist)
              (send corner-shift length dist)
              (list (send start slide-copy corner-shift)
                (send start slide-copy side-shift)
                (send end slide-copy side-shift))))
                   (ensure-fresh _edge-ring refresh-edge-ring)))))
    (make-object geo-polygon% new-vtxs)))

    (define (margin-corner-points edge dist)
      (let ((side-shift (send (send edge angle) r-normal)))
    (send side-shift length dist)
    (list (send (send edge origin) slide-copy side-shift)
          (send (send edge endpoint) slide-copy side-shift))))

#|    (define (margin-bisect-point edge dist)
      (let* ((myvect (send (send edge angle) clone))
         (junk   (send myvect length 1))
         (prevvect (send (send (send edge prev) angle) clone))
         (junk2  (send prevvect length 1))
         (bisect-shift (send prevvect add myvect)))
    (set! bisect-shift (send bisect-shift r-normal))
    (send bisect-shift length dist)
    (list (send (send edge origin) slide-copy bisect-shift))))
|#

    (define (margin-bisect-point edge dist)
      (let* ((myvect (send (send edge angle) r-normal))
         (junk   (send myvect length 1))
         (prevvect (send (send (send edge prev) angle) r-normal))
         (junk2  (send prevvect length 1))
         (bisect-shift (send prevvect add myvect)))
    (send bisect-shift length dist)
    (list (send (send edge origin) slide-copy bisect-shift))))

    (define/public (smoothed-margin dist)
      (let ((new-vtxs
         (apply append
           (ring-map (lambda (edge)
         (let* ((myvect (send edge angle))
            (nextvect (send (send edge next) angle))
            (dullness (send myvect unit-dot nextvect)))
           (cond 
            ((< dullness 0) (append (margin-bisect-point edge dist)
                        (margin-corner-points edge dist))) ;3 pts
            ; dullness 0 = interior right angle
            ((< dullness .7) (margin-corner-points edge dist)) ;2 pts
            ; dullness .707 = interior angle 3pi/4
            (else            (margin-bisect-point edge dist)))  ;1 pt
           ))
          (ensure-fresh _edge-ring refresh-edge-ring)))))
    (make-object geo-polygon% new-vtxs)))

    ; not reliable:
    (define/public (prune threshold)
      ;thins out vertices: if (A dot B) > threshold for adjacent A,B, kills A
      (ring-foreach
       (lambda (e)
     (when e
         (let ((unitdot (send (send e spoke) unit-dot (send (send e next) spoke))))
           (when (> unitdot threshold)
           (send e cut-origin)))))
;       (send this edges)))
       (ensure-fresh _edge-ring refresh-edge-ring)))
          

    (define/public (closest-pt-to pt)
      (ensure-point-object pt)
      (let* ((near-pts (map (lambda (edge) (send edge closest-pt-to pt))
                (edges)))
         (vects    (map (lambda (near-pt) (send pt vector-to near-pt)) near-pts))
         (dists    (map (lambda (vect) (send vect length)) vects))
         (min-dist (apply min dists))
         (dist-idx (list-position-general dists min-dist =))
         (nearest  (list-ref near-pts dist-idx)))
    nearest))

    (define/public (provide-reshape-handle-drag-callback i)
    ;Returns function which is called every time the reshape handle (hdl) moves
      (let ((my-edge (edge-n i)))
    (lambda (xy) ;receives one arg: xy = new hdl position
      (send my-edge origin xy))))

    (define/public (provide-reshape-handle-delete-callback i)
      (let ((my-edge (edge-n i)))
    (lambda ()
      (send my-edge cut-origin))))

    (define/public (add-vertex-n n xy)
      (send (edge-n n) insert-segment-end xy))
))

;polygon which is the result of a intersection/union/etc operation--
;  remembers the source of each of its sides
(define geo-polygon-child%
  (class geo-polygon%
     (init _vtx-donor-list) ;list of pairs (vtx source-polygon-of-edge-after-vtx)...)
     (super-instantiate ((map car _vtx-donor-list)))
     (field (_donors (map cadr _vtx-donor-list)))
     (field (_parents (remove-duplicates _donors memq)))
                      
     (define/public (donor-of-edge-n n)
       (list-ref _parents n))

     (define (accumulate-edge-force sector donor-parent other-parent)
       (let* ((rim (send sector edge))
          (midrim (send rim midpoint))
          (inward-ang (send rim left-normal-angle))
          (inward-vect (make-object geo-vector%))
          (junk (send inward-vect angle inward-ang))
          (force (make-object geo-segment% midrim inward-vect)))
         (send force length (send rim length))
         (send force scale! (/ 1 (send this perimeter)))
         (send-actor-named (send donor-parent object) motion-actor-name
             accumulate-force force)
         (send force scale! -1)
         (send-actor-named (send other-parent object) motion-actor-name
             accumulate-force force)
         ))

     (define/public (accumulate-forces-on-parents)
       (for-each (lambda (sect donor)
               (accumulate-edge-force sect donor
                     (apply other donor eq? _parents)))
             (send this sectors)
             _donors))
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


                                             ;;;;;      ;;;;   ;;;;  ;;   ;;
                                             ;;  ;;    ;;     ;;  ;; ;;;  ;;
                                             ;;;;   ;; ;; ;;; ;;  ;; ;; ; ;;
                                             ;; ;;     ;;  ;; ;;  ;; ;;  ;;;
                                             ;;  ;;     ;;;;   ;;;;  ;;   ;;

; (define geo-rectangle%
;   (class geo-polygon%
;         (init-field _vertices)
;     (set! _vertices (map (lambda (vtx) (ensure-point-object vtx)) _vertices))
;     ;Assumes vertices in ccw circuit order

;     (field (_anchor stale)
;        (_pt-centroid stale)
;        (_area-centroid stale)
;        (_area stale)
;        (_perimeter stale)
;        (_bbox stale))
;     ;_bbox needed only for stand-alone geo-polys; never used unless called

;     ;REDEFS
;     (define/override (slide-by-dxy! dx dy)
;     (define/override (intersects obj)
;     (define/public anchor
;     (define/public (centroid-of-points)
;     (define/public (centroid-of-area)
;     (define/public (vertex-n n)
;     (define/public (edge-n n)
;     (define/public (vertices) _vertices)
;     (define/public (sectors) _sectors)
;     (define/public (area) (ensure-fresh _area refresh-area))
;     (define/public (perimeter)
;     (define/public (contains-pt? pt)
;     (define/public (insert-vertex n pt)
;     (define/public (cut-vertex n))
;     (define/public (move-vertex n to-pt)
;     (define/public (bbox) (ensure-fresh _bbox refresh-bbox))


; (define geo-rectigon%
;   ; a rectigon is a polygon composed only of rectangles where
;   ;  all edges are horiz or vertical, and all vertices are right angles.
;   ; Most polygon operations are greatly simplified.
; #f)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;INTERSECTION/UNION TOOLS

(define (edge-pairs p-edges q-edges)
  (cross-map
   (lambda (p-edge q-edge) (list p-edge q-edge)) p-edges q-edges))

(define (edge-pair-intersections pairs)
;returns list of all intersections of pairs
  (apply append (map (lambda (pair) (send (car pair) intersects (cadr pair)))
             pairs)))

(define (edge-pair-intersections? pairs)
;#t iff any edge pairs intersect; stops after first intsct found
  (cond ((null? pairs) #f)
    ((not (null? (send (caar pairs) intersects (cadar pairs)))) #t)
    (else (edge-pair-intersections? (cdr pairs)))))

(define (nested? P Q)
  (or (send P contains-pt? (send Q vertex-n 0))
      (send Q contains-pt? (send P vertex-n 0))))

;returns either P or Q, whichever is: outermost if outer?, innermost else
; if neither contains other, returns #f
; If already known whether Q contains P0, passed in as PinQ? 
(define (inner/outer-polygon outer? P Q . PinQ?)
  (if (null? PinQ?)
      (set! PinQ? (send Q contains-pt? (send P vertex-n 0)))
      (set! PinQ? (car PinQ?)))
  (cond (PinQ?  ;P inside Q
     (if outer? Q P))
    ((send P contains-pt? (send Q vertex-n 0)) ;Q inside P
     (if outer? P Q))
    (else #f)))

(define (sort-points-along-direction pts ang)
;assumes all pts are collinear (therefore uses monotonic x or y)
; and sorts them in direction of ang
  (let ((sort-fn
      (cond ((send ang leftward?)  (lambda (pt1 pt2)
                     (let ((x1 (send pt1 x))
                           (x2 (send pt2 x)))
                       (> x1 x2))))
        ((send ang rightward?) (lambda (pt1 pt2)
                     (let ((x1 (send pt1 x))
                           (x2 (send pt2 x)))
                       (< x1 x2))))
    ;else ang is vertical:
        ((send ang upward?)    (lambda (pt1 pt2)
                     (let ((y1 (send pt1 y))
                           (y2 (send pt2 y)))
                       (< y1 y2))))
        (else                  (lambda (pt1 pt2)
                     (let ((y1 (send pt1 y))
                           (y2 (send pt2 y)))
                       (> y1 y2)))))))
    (sort sort-fn pts)))

(define (index-intersections-by-parent pts)
;returns alist of intersection-pts keyed by both parent edges/segs
  (let ((alist null))
    (for-each (lambda (pt)
        (let* ((parents (send pt parents))
               (p1 (car parents))
               (p2 (cadr parents)))
          (pushq-onto-malist-val-always! p1 pt alist)
          (pushq-onto-malist-val-always! p2 pt alist)))
          pts)
    (for-each (lambda (edge-pts)
        (set-mcdr! edge-pts
              (sort-points-along-direction
               (cdr edge-pts)
               (send (car edge-pts) angle))))
          alist)
    alist))

(define (complete-circuit alist this-pt is-int? poly edge stop-pt)
;traverses edge-network from this-pt to next
; this-pt is an intersection pt if is-int?, am edge end/vtx if not
;  which-poly is 0 for P, 1 for Q
;  which-edge is the edge (of which-poly) to follow next
; stop-pt is the pt at end of this circuit
  (let ((edge-pairs (assq edge alist))
    (next-pt-slot null))
    (when edge-pairs  ;look for intersct-pt after this-pt
    (set! next-pt-slot (cdr
                (if (not is-int?)
                edge-pairs ;don't bother looking for this-pt, use first int-pt
                (memq this-pt edge-pairs)))))
    (if (and edge-pairs (not (null? next-pt-slot)))
    ;found next intersection
    (let ((next-pt (car next-pt-slot)))
      (if (eq? next-pt stop-pt)
          (list stop-pt)
          (cons next-pt
            (let ((new-poly (other poly eq? 0 1)))
              (complete-circuit alist next-pt #t new-poly
                    (list-ref (send next-pt parents) new-poly)
                    stop-pt)))))
    ;no more intersections on this edge, proceed to endpoint
    (let ((next-pt (send edge endpoint)))
      (cons next-pt
        (complete-circuit alist next-pt #f poly (send edge next) stop-pt))))))

(define (new-circuit int-alist op)
  (if (null? int-alist)
      null
      (let* ((start-pt (cadar int-alist))  ;random int pt
         (parents (send start-pt parents))
         (p-edge (car parents))
         (q-edge (cadr parents))
         (cross-prod (send (send p-edge angle)
                   cross
                   (send q-edge angle)))
         (poly-to-follow
          (if (positive? cross-prod)
          (if (eq? op 'intersect) 1 0)
          (if (eq? op 'intersect) 0 1)))
         (edge-to-follow (list-ref parents poly-to-follow))
         (this-circuit (complete-circuit int-alist
                         start-pt #t
                         poly-to-follow
                         edge-to-follow start-pt)))
    (for-each (lambda (pt)
            (when (is-a? pt geo-intersect-point%)
            (let ((edge-keys (send pt parents)))
              (remq-clean-from-malist-val! (car edge-keys) pt int-alist)
              (remq-clean-from-malist-val! (cadr edge-keys) pt int-alist))))
          this-circuit)
    (cons this-circuit (new-circuit int-alist op)))))

(define (polygon-combine P Q op)
  (let* ((ints (edge-pair-intersections (edge-pairs (send P edges) (send Q edges))))
     (int-alist (index-intersections-by-parent ints))
     (pointset (new-circuit int-alist op)))
     (if (null? pointset) ;no edges intersect-- could be completely nested
     (let ((which (inner/outer-polygon
              (eq? op 'union) P Q
              (send Q contains-pt? (send P vertex-n 0)))))
       (if which
          (list (send which clone))
;           (copy-as-child-polygon which)
           null))
    (map (lambda (circuit) (make-object geo-polygon% circuit)) pointset))))

(define (render-polygon p) (make-object polygon% dynapad (apply append (map (lambda (vtx) (send vtx xy)) (send p vertices)))))

(define (AreaSubtractArea p q)
  (polygon-combine p (send q invert) 'intersect))
(define (AreaIntersectArea p q)
  (polygon-combine p q 'intersect))
(define (AreaUnionArea p q)
  (polygon-combine p q 'union))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (geo-polygon-from-bbox bbox)
  (make-object geo-polygon%
           (make-lazy-pairs (coords-from-bbox bbox))))

(define (geo-polygon-from-oval-bb bbox)
  (set! bbox (bbstretch bbox 0.01166))  ; 0.02695 for numpoints=8, 0.01166 for 12
  (make-object geo-polygon%
    (let*((xc (car (bbcenter bbox)))
          (yc (cadr (bbcenter bbox)))
          (xr (- (b2 bbox) xc))
          (yr (- (b3 bbox) yc))
          (coords '())
          (numpoints 12)
          (da (/ (* 8 (atan 1)) numpoints))
         )
      (dotimes (i numpoints)
        (push!
          (list (+ xc (* xr (cos (* i da)))) (+ yc (* yr (sin (* i da)))))
          coords))
      coords)))

(define geo-actor-name 'geon)
(define (geodize it)  ;creates, attaches, and returns geo-actor
  (def geon)
  (cond ((is-a? it geo-actor%)
     it) ;if it IS geo-actor, return it
    ((cset! geon (get-actor-named it geo-actor-name))
     geon) ;if already has geo-actor, return that
    (else
     (if (and (is-a? it base-formation%)
          (cset! geon (send it main)))
         (geodize geon) ;recursive case
         (begin  ;else make new geo-actor
           (set! geon
             (cond
              ((or ;(is-a? it base-formation%)
               (is-a? it rect%)
               (is-a? it image%)
               (is-a? it text%)
               (is-a? it panel%))
               (geo-polygon-from-bbox (send it bbox)))
              ((is-a? it oval%)
               (geo-polygon-from-oval-bb (send it bbox)))
              ((is-a? it line%)
               (make-object geo-polyline%
                    (make-lazy-pairs (send it coords))))
              ((is-a? it polygon%)
               (make-object geo-polygon%
                    (make-lazy-pairs (send it coords))))
              ))
           (send geon attach-to it geo-actor-name)
           (attach-geo-callbacks it geon)
           geon)))))

(define (copy-as-child-polygon poly)
  (map (lambda (vtx) (list (send vtx clone) poly))
       (send poly vertices)))

(define (polygon-pairwise-intersection P Q)
  (let ((pts (polygon-combine P Q #f)))
    ;each element of pts is pair: (<point> <poly>)
    ;point is vertex of intersection polygon; poly is owner (P or Q)
    ;  of edge trailing (CCW) that vertex
    ;Returns #f if no intersection
    (if (null? pts)
    #f
    (make-object geo-polygon-child% pts))))

(define (polygon-pairwise-union P Q)
  (let ((pts (polygon-combine P Q #t)))
    (make-object geo-polygon-child% pts)))

(define (polygon-intersection . polys)
  (let ((len (length polys)))
    (case len
      ((0 1) #f)
      ((2) (apply polygon-pairwise-intersection polys))
      (else (polygon-pairwise-intersection
         (car polys)
         (polygon-intersection (cdr polys)))))))
 

(define (intersection . objs)
  (let* ((geo-objs (map geodize objs)))
    (apply polygon-intersection geo-objs)))

(define (pairwise-intersection? P Q)
  (or (edge-pair-intersections? (edge-pairs (send P edges) (send Q edges)))
      (nested? P Q)))

(define intersection? pairwise-intersection?)

(define (intersecting-objects obj)
  (let* ((bbox (send obj bbox))
     (intruders (send dynapad find 'overlapping bbox)))
    (filter (lambda (it) (and (not (eq? it obj)) ;exclude obj itself
                  (intersection? it obj)))
        intruders)))

(define (ensure-bbox obj)
  (if (list? obj)
      obj
      (send obj bbox)))

(define contains-pt?
  (case-lambda
   ((container xy)  (if (list? xy)
            (apply contains-pt? container xy)
            (error ("Expects xy-pair or x, y args\n"))))
   ((container x y)
      (if (list? container)
      (bbenclosedoron x y container)
      (and
       (bbenclosedoron x y (send container bbox))
       (if (is-a? container geo-actor%)
           (send container contains-pt? (list x y))
           (send-actor-named container
                 geo-actor-name contains-pt? (list x y))))))))

;assumes container has geo-actor
; obj may or may not have geo-actor
(define (centroid-in? obj container)
  (let ((center-pt
     (or
      (if (is-a? obj geo-actor%)
          (send obj area-centroid)
          (send-actor-named obj geo-actor-name area-centroid))
      (bbcenter (send obj bbox)))))
    (if (list? container)
    (bbenclosedoron (car center-pt) (cadr center-pt) container)
    (let ((cntr-geon
           (if (is-a? container geo-actor%)
           container
           (get-actor-named container geo-actor-name))))
      (send cntr-geon contains-pt? center-pt)))))

; Containment means:
; 1) centroid of obj is inside boundary of container, and
; 2) bbox of container not strictly within bbox of obj
; (this excludes objs which dwarf container)
(define (contained-in? obj container)
  (and (centroid-in? obj container)
       (not (bbsurrounds? (ensure-bbox obj) (ensure-bbox container)))))

(define (intersects? obj container)
  (let ((cnt-geon (get-actor-named container geo-actor-name))
    (obj-geon (get-actor-named obj geo-actor-name)))
    (when (not cnt-geon)
    (set! cnt-geon (geo-polygon-from-bbox (send container bbox))))
    (when (not obj-geon)
    (set! obj-geon (geo-polygon-from-bbox (send obj bbox))))
    (intersection? cnt-geon obj-geon)))

(define (intersects-but-not-surrounds? obj container)
  (and (intersects? obj container)
       (not (bbsurrounds? (send obj bbox) (send container bbox)))))

;returns list of dynaobjs whose centroid lies within dynaobj container
(define (contained-objects container . fn)
;container may be: 1) bbox 2) geo-actor% 3) dynaobject%
  (set! fn (if (null? fn) contained-in? (car fn)))      
  (if (list? container)
      (send dynapad find 'groupmembers 'overlapping container)     ;assume bbox
      (let* ((bbox (send container bbox))
         (intruders (send dynapad find 'groupmembers 'overlapping bbox)))
    (filter (lambda (it) (and (not (eq? it container))
                  (fn it container)))
        intruders))))

