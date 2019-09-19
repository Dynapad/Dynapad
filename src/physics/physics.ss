(require (lib "class.ss"))
(require (lib "defmacro.ss"))
(require (lib "math.ss"))
;(dynaload "geometry.ss") ;includes actor.ss, alist.ss
;(dynaload "bbox.ss")
;(dynaload "actor.ss")
;(dynaload "actortimer.ss")


;relation of actors:
;
;       ( dynaobject )--- awakener
;      /   |     |    \_           _ (another actor team)
;  geo  motion lookout  \         /
; actor  actor  actor.....repulsor


(define visible-state? #t) ;turn on to show sleep/waking, braking

(define (reposition obj absx absy dx dy) ;called when obj receives (slide x y)
  (send-actors-named obj 'shadows reposition absx absy dx dy))

;  (let ((shadows (assq 'shadows (send obj alist))))
;    (if shadows
;    (for-each (lambda (shadow)
;            (send shadow reposition-echo obj x y dx dy)
;            ;(display shadow)(newline)
;            )
;          (cdr shadows)))))

(define (resize obj absx absy relx rely)
  (send-actors-named obj 'shadows refresh-boundary))

(define (add-slide-shadow obj new-shadow)
  (set-object-callbacks obj)
  (send new-shadow attach-to obj 'shadows))

;(define (add-slide-shadow obj new-shadow)
;make sure obj uses slide-echo as slide-callback
;  (if (not (send obj reposition-callback))
;      (send obj reposition-callback reposition-echo))
;add new-shadow to slide-shadows list
;    (send new-shadow attach-to obj 'shadows))


;---- TIMERS ----

(define motion-tick 150)
(define repulsion-tick 250)  ; should be >motion, <lookout
(define repulsion-kick-duration (+ repulsion-tick 100))
(define lookout-tick 750)
(define lookout-kick-duration (+ lookout-tick 200))
(define hibernation-tick 60000) ;for garbage collection of idle actors

(define motion-timer (make-object auto-unsubscribe-timer% dynapad motion-tick))
(send motion-timer start)

(define repulsion-timer (make-object auto-unsubscribe-timer% dynapad repulsion-tick))
;(send repulsion-timer start)

(define lookout-timer (make-object auto-unsubscribe-timer% dynapad lookout-tick))
;(send lookout-timer start)

(define hibernation-timer (make-object subscription-timer% dynapad hibernation-tick))
(send hibernation-timer start)

(define (stop-all)
  (send motion-timer stop)
  (send repulsion-timer stop)
  (send lookout-timer stop))

;---- PHYSICS ACTORS ----
(define homing-actor-name 'homer)
(define homing-actor
  (class named-actor%
    (super-instantiate ())
    (field (_home #f))

    (send homing-timer subscribe this)
    ;     (define/public (update abstime dtime)
    ;       ;try to go home if
    ;       )
    ))

;(define delay-to-sleep 5000)
;(define nobounce #t)
(define motion-actor-name 'motor)
(define motion-actor%
  (class expiring-actor%
    ;metric standard used for callibration:
    ; assume 1 pad unit is 1 mm
    (init-field (_velocity (null-vector))) ;units: mm/msec=units/msec
    (super-instantiate ())

    (field    (_asleep? #t)
              (_coast-until #f))
    (field (_slow-limit 0) ;either may be #f
           (_fast-limit .01))

    (define/override (delete)
      (send motion-timer unsubscribe this)
      (super delete))

    (define/override (kick until rel-time)
      (if _asleep?
          (send motion-timer subscribe this))
      (super kick until rel-time))

    (define/public velocity (get/set _velocity)) ;vector
    (define/public speed ;scalar
      (case-lambda
        (() (send _velocity length))
        ((newspeed) (send _velocity length newspeed))))

    (define/public (change-speed dspeed)
      (let* ((newspeed (+ (speed) dspeed)))
        (if (and _slow-limit (< newspeed _slow-limit))
            (set! newspeed _slow-limit)
            (if (and _fast-limit (> newspeed _fast-limit))
                (set! newspeed _fast-limit)))
        (display (format "speed:~a~%" newspeed))
        (send _velocity length newspeed)))

    (define/public (update abstime dtime)
      (if (positive? (speed))
          ;IF MOVING:
          (let* ((obj (send this object))
                 (dxy (apply scale-dxy dtime (send _velocity dxy))))
            (send/apply obj slide dxy)) ;slide dynaobj; iattached geo-actor also slides
          ;ELSE NOT MOVING: fall asleep
          (begin
            (set! _asleep? #t)
            'unsubscribe)))
    ;go to sleep (i.e. unsubscribe from timer)
    ; will awaken when next kicked
    ))

(define default-mass 100)
(define default-friction 0)
(define particle-actor%
  ;0D (pointlike) massed particle w. linear momentum, translation forces, friction
  (class motion-actor%
    (init (_velocity (null-vector)))
    (super-instantiate (_velocity))
    (init-field (_mass default-mass) ;grams
                (_friction default-friction))
    ;drag force in newtons,
    ; probably proportional to mass
    (field (_force (null-vector))) ;vector, in newtons

    (define/public (mass) (get/set _mass))
    (define/public (friction) (get/set _friction))

    (define/public (accumulate-force frc) ;frc is geo-vector
      (send _force add! frc))
    (define/public (clear-force)
      (set! _force (null-vector)))
    (define/public (apply-force duration coast-time)
      (send this kick #f coast-time) ;use relative time
      ;dv= f*t/m
      (send _force scale! (/ duration _mass))
      ;(display (apply format "force: ~a ~a~%" (send _force dxy)))
      (send (send this velocity) add! _force)
      (clear-force))

    (define/override (update abstime dtime)
      ;apply friction
      (if (not (zero? _friction))
          (let* ((decel (/ _friction _mass))
                 (dspeed (- (* dtime decel))))
            (send this change-speed dspeed)))
      (super update abstime dtime))

    ))

(define slab-actor%
  ;particle with 2D extent (thus with rotational inertia/friction, torque)
  (class particle-actor%
    (init (_velocity (null-vector))
          (_mass default-mass)
          (_friction default-friction))
    (init-field (_moment 1)
                (_ang-friction 1))
    (super-instantiate (_velocity _mass _friction))
    (field (_torque 0)) ;scalar (+ means ccw)

    ;      (define (refresh-geo-object)
    ;        (let* ((myobj (send this object))
    ;           (geo-obj (send-single-actor myobj 'geo-object)))
    ;          (set! _geo-object geo-obj)
    ;          geo-obj))

    ;      (define/public geo-object
    ;        (case-lambda
    ;         (() (if _geo-object
    ;             _geo-object
    ;             (refresh-geo-object)))
    ;         ((it) (set! _geo-object it))))

    (define/override (accumulate-force frc)
      ;frc is a geo-segment: dxy is force vector, xy is point of application
      (let* ((centroid (send-actor-named (send this object) geo-actor-name centroid-of-area))
             (radial (send (send frc origin) vector-to centroid))
             ;           (if centroid
             ;               (send (send frc origin) vector-to centroid)
             ;               (send/apply (send frc origin) vector-to
             ;                   (bbcenter (send (send this object) bbox)))))
             ;     (send-actor-named (send this object) geo-actor-name centroid-of-area)))
             ;(tork (send frc cross radial))
             (junk (send radial length 1))
             (shove (send frc dot radial)))
        (send radial scale! shove) ;translation force
        ;(display (send radial length))(newline)
        (super accumulate-force radial)
        ;(+= _torque tork)
        ))
    ))

(define braking-slab-actor%
  (class slab-actor%
    (init (_velocity (null-vector))
          (_mass default-mass)
          (_friction 0)
          (_moment 1)
          (_ang-friction 1))
    (super-instantiate (_velocity _mass _friction _moment _ang-friction))
    (field    (_moving-friction 0)
              (_braking-friction 100)
              (_brakes-on? #f)) ;start w brakes off

    (define/public brakes-on
      (case-lambda
        (()
         ;(display "trying...")(newline)
         (if (not _brakes-on?)
             (begin
               ;(display "braking...")(newline)
               ;(if visible-state? (send (send this object) fill "red"))
               (send this friction _braking-friction)
               (set! _brakes-on? #t))))
        ((frict) (begin
                   (set! _braking-friction frict)
                   (set! _brakes-on? #f)
                   (brakes-on)))))
    (define/public (brakes-off)
      (if _brakes-on?
          (begin
            ;(if visible-state? (send (send this object) fill "blue"))
            (send this friction _moving-friction)
            (set! _brakes-on? #f))))
    (define/override (apply-force duration coast-time)
      (brakes-off)
      (super apply-force duration coast-time))
    ))

(define self-braking-slab-actor%
  (class braking-slab-actor%
    (init (_velocity (null-vector))
          (_mass default-mass)
          (_friction default-friction)
          (_moment 1)
          (_ang-friction 1))
    (super-instantiate (_velocity _mass _friction _moment _ang-friction))

    (define/override (update abstime dtime)
      (super update abstime dtime)
      ;apply brakes if not kicked recently
      (send this maybe-expire `(send ,this brakes-on)))
    ))


; (define momentum-actor%
;   (class actor%
;      (init-field (_velocity (send null-vector clone)))
;      ;(ensure-vector-object _velocity) ;displacement per second
;      ;(init-field (_ang-velocity 0)) ;radians per second
;      (field    (asleep? #t)
;         (time-force-last-applied (current-milliseconds)))
;      (super-instantiate ())
;      ;(send motion-timer subscribe this)

;      (define/public (speed) (send _velocity length))

;      (define/public velocity (get/set _velocity))

;      (define/public (update abstime dtime)
;        (if (> (speed) 0)
;            (let ((fract (/ dtime 1000))
;              (obj (send this object))
;              (move (send _velocity clone)))
;          (send move scale! fract)
;          (let-values (((dx dy) (send move dxy)))
;            (send obj slide dx dy) ;slide visible dynaobj
;            ;(send (send-actor obj 'geo-object)
;             ; slide! move)) ;slide matching geo-obj
;            )
;          ;(send obj spin! (* fract _ang-velocity))
;          ;apply friction:
;          ;(if (method-in-interface? friction obj)

;          (if friction-on?
;              (let* ((frict (send-single-actor obj 'friction))
;         ;(ang-frict (send obj ang-friction))
;                 (mass (send-single-actor obj 'mass))
;                ;(moment (send obj moment))
;                 (decel (/ frict mass))
;                 (dspeed (* fract decel))
;                 (newspeed (- (send _velocity length) dspeed))
;                 )

;                (if (negative? newspeed)
;                (set! newspeed 0))
; ;           (send _velocity scale! .9)
;                (send _velocity length newspeed)
;                ))))
;        (if (> (- abstime time-force-last-applied) delay-to-sleep)
;            (begin
;          (set! asleep? #t)
;          'unsubscribe)) ;go to sleep
;        )


;      (define/public (apply-force frc dtime)
;        (if asleep?
;            (send motion-timer subscribe this))
;        (set! time-force-last-applied (current-milliseconds))
;        (let ((accel (send frc clone)))
;          (send accel scale! (/ dtime
;                    (send-single-actor (send this object) 'mass)))
;          (send _velocity add! accel)))

;      ;(define/public (apply-torque tork)
;      ;  )

; ))

;THIS VERSION MANAGES REPULSORS LOCALLY
; (define lookout-actor%
;   (class named-actor%
;      (super-instantiate ('lookout))
;      (field (_repulsors-alist null))

;      (send lookout-timer subscribe this) ;activate

;      (define (delete-watch watch)
;         (send (cadr watch) delete) ;kill repulsor
;         (send-actor (car watch) 'lookout-cut-watch this)) ;tell partner to stop watching me

;      (define/override (delete)
;        (send lookout-timer unsubscribe this)
;        (for-each delete-watch _repulsors-alist)
;        (super delete))

;      (define/public (update abstime dtime)
;         ;check for overlap of owner's bbox with others
;        (let* ((me (send this object))
;           (bbox (send me bbox))
;           (intruders (send dynapad find 'overlapping bbox))
;           (keep-alist null))
;         ; update keep-alist: keep existing, add new
;          (for-each
;           (lambda (intruder)
;         (if (not (eq? intruder me)) ;exclude oneself
;             (let ((watch (assq intruder _repulsors-alist)))
;               (if (not watch) ;not already included
;               (let* ((repulsor (make-object repulsor% me intruder))
;                  (handshake
;                   (send-actor-named intruder 'lookout lookout-create-watch me repulsor))) ;tell intruder to watch me
;                 (if (not handshake) ;intruder is passive obj (no lookout actor), so vitalize it
;                 (begin
;                   (awaken intruder)
;                   (send-actor-named intruder 'lookout lookout-create-watch me repulsor))) ;assume it works this time
;                 (set! watch (list intruder repulsor)))) ; watch intruder
;               (push! watch keep-alist))))
;           intruders)
;          ;clean up stale repulsors (those not in keep-alist)
;          (for-each
;           (lambda (watch)
;         ; watch is pair (partner-object repulsor)
;         (let ((partner (car watch)))
;           (if (not (assq partner keep-alist)) ;if not in keep list, kill it
;               (delete-watch watch))))
;           _repulsors-alist)
;       (set! _repulsors-alist keep-alist)))

(define lookout-actor-name 'lookout)

;THIS VERSION MANAGES REPULSORS USING DYNAOBJ ALIST
(define lookout-actor%
  (class named-actor%
    (super-instantiate ())
    (send lookout-timer subscribe this) ;activate

    ;      (define (delete-watch watch)
    ;         (send (cadr watch) delete) ;kill repulsor
    ;         (send-actor (car watch) 'lookout-cut-watch this)) ;tell partner to stop watching me

    (define/override (delete)
      (send lookout-timer unsubscribe this)
      ;        (for-each delete-watch _repulsors-alist)
      (super delete))

    (define/public (update abstime dtime)
      ;check for overlap of my bbox with others
      (let* ((me (send this object))
             (my-watches (get-else-push-onto-malist!
                          assq (list 'repulsors) me alist))
             (bbox (send me bbox))
             (intruders (send dynapad find 'overlapping bbox)))
        (for-each
         (lambda (intruder)
           (if (and
                (not (eq? intruder me)) ;exclude oneself
                (send-actor-named intruder 'awakener alive?))
               (let ((watch (assq intruder (cdr my-watches))))
                 (if (not watch) ;not already included
                     (let* ((repulsor (make-object repulsor% me intruder)))
                       ;make sure intruder is awake (may be already)
                       (awaken intruder)
                       ;put me on intruder's watch list
                       (pushq-onto-malist-val-always!
                        'repulsors
                        (list me repulsor)
                        intruder alist)
                       ;put intruder on my watch list
                       (set! watch (list intruder repulsor))
                       (set-mcdr! my-watches
                                  (cons watch (cdr my-watches)))))
                 ;In either case; kick the resulting repulsor
                 (send (cadr watch) kick #f lookout-kick-duration))))
         intruders)
        ;stale repulsors will expire on their own if not kicked
        ))


    ;  (define/public (lookout-create-watch whom repulsor)
    ;    (push-onto-alist! whom repulsor _repulsors-alist assq))
    ;  (define/public (lookout-cut-watch whom)
    ;    (remv-from-alist! whom _repulsors-alist assq))
    ))

(define *dxes* null)
(define homing-actor%
  (class named-actor%
    (init-field (_target-x 0) (_target-y 0))
    (field (_target-fn #f))
    (field (_duedate 0))
    ;(field (_where-am-i-fn (lambda () (send (send this object) xy))))
    ;(field (_force (null-vector)))
    (super-instantiate ())

    (define/public (go)
      (send motion-timer subscribe this))
    (define/public (stop)
      (send motion-timer unsubscribe this))

    (define/public duedate (get/set _duedate))

    (define/public target
      (case-lambda
        (() (list _target-x _target-y)) ;return momentary target coords
        ((targ) (if (list? targ)
                    (target (car targ) (cadr targ)) ;fixed coord pair
                    (set! _target-fn targ)))        ;else lambda
        ((x y) (set! _target-fn #f)
               (set! _target-x x)
               (set! _target-y y))))

    (define/public (update abstime dtime)
      (if _target-fn   ; handle moving target
          (let ((newxy (_target-fn)))
            (set! _target-x (car newxy))
            (set! _target-y (cadr newxy))))
      (let* ((timeleft (- _duedate abstime)))
        (if (positive? timeleft)
            (let* ((rawfraction (/ motion-tick timeleft))
                   (fraction (if (> rawfraction 1)
                                 1
                                 rawfraction))
                   (xynow (send (send this object) xy))
                   (dx-total (- _target-x (car xynow)))
                   (dy-total (- _target-y (cadr xynow)))
                   (dx (* fraction dx-total))
                   (dy (* fraction dy-total)))
              (push! dx *dxes*)
              (send (send this object) slide dx dy))
            ; else jump directly there
            (begin
              (send (send this object) xy _target-x _target-y)
              (send this stop)
              (send this delete)))))

    ;     (define/public (update abstime dtime)
    ;       (let* ((motor (get-actor-named (send this object) motion-actor-name))
    ;          (xy (_where-am-i-fn))
    ;          (x (car xy))
    ;          (y (cadr xy))
    ;          (dx (- _target-x x))
    ;          (dy (- _target-y y))
    ;          (r (hypotenuse dx dy)))
    ;     (send _force dxy dx dy)
    ;     (send _force length (/ 1 (sqr r)))
    ;     (send motor accumulate-force _force)
    ;     (send motor apply-force 1 repulsion-kick-duration)))
    ))

(define animate-to
  (case-lambda
    ((obj target-fn) (animate-to obj target-fn 1000)) ;1000ms by default
    ((obj target-fn howlong)
     (let ((homer (get-actor-named obj 'homer)))
       (if (not homer)
           (begin
             (set! homer (make-object homing-actor%)); target-fn))
             (send homer attach-to obj 'homer)))
       (send homer target target-fn)
       (send homer duedate (+ (current-milliseconds) howlong))
       (send homer go)
       homer))))


#|EXAMPLES:
(animate-to obj '(0 0))        take 1 sec
(animate-to obj '(0 0) 500)    take 500 ms
(animate-to obj
            (lambda () (let ((xy (send target position)))
                         (list (+ (car xy) xoffset)
                               (+ (cadr xy) yoffset)))))
|#


(define repulsor%
  ; A repulsor represents a relation between two objects
  ;It only exists while their bboxes overlap (as managed by lookout actor)
  (class expiring-actor%
    (init-field obj1 obj2) ;parent objects
    (super-instantiate ())
    ;temp:
    ;     (field (birthdate (let ((now (current-milliseconds)))
    ;                 (- now (modulo now repulsion-tick)))))

    (send repulsion-timer subscribe this) ;activate

    (define/override (object) (list obj1 obj2))

    (define/override (delete) ;shut down (should be GC'd)
      (send repulsion-timer unsubscribe this)
      (delete-from-malist-val! assq remove 'repulsors
                               (list obj1 this) obj2 alist)
      (delete-from-malist-val! assq remove 'repulsors
                               (list obj2 this) obj1 alist))


    (define/public (overlap-or-die)
      (if (bbintersection (send obj1 bbox) (send obj2 bbox))
          (begin
            (send this kick #f lookout-tick)
            ; extend expiration for awhile (exact time not important)
            #f) ; and continue
          (begin
            (delete)
            #t)))

    (define/public (update abstime dtime)
      ;       (define time1 (/ (- (current-milliseconds) birthdate) 10))
      (if (not (send this maybe-expire `(send ,this overlap-or-die)))
          (let ((overlap (intersection obj1 obj2))) ;see geometry.ss
            ;       (let ((overlap
            ;          (measure 'itrsct intersection obj1 obj2)))
            (if overlap
                (begin
                  (send overlap accumulate-forces-on-parents)
                  (send-actor-named obj1 motion-actor-name apply-force 1 repulsion-kick-duration)
                  (send-actor-named obj2 motion-actor-name apply-force 1 repulsion-kick-duration)
                  )
                ;TEMPORARY:
                (delete)))
          ;       (plot-tick this
          ;              time1
          ;              (/ (- (current-milliseconds) birthdate) 10)
          ;              "red")

          ))
    ))

(define awakener%
  ; An awakener is a lightweight "pilot light" which
  ;  stays permanently bound to its obj once created
  ;  and "awakens" it (i.e. makes actor team) when moved.
  ; "Sleeping" kills off the other actors to save space,
  ;   and awakener recreates them as needed
  (class named-actor%
    (init object)
    (field (_awake? #f))
    (super-instantiate ())
    (send this attach-to object 'awakener)
    (send object slide-callbacks 'add
          (lambda (obj dx dy) (send this slide)))
    ;listen for future motion of object obj
    (if visible-state? (send object pen "pink")) ;show I'm alive

    (define/public (alive?)
      #t) ;acknowledge awakener presence

    (define/public (awake?) _awake?)

    (define/public (awaken)
      (if (not _awake?)
          (let* ((me (send this object))
                 (geoactor (geodize me))
                 (lookoutactor (make-object lookout-actor%))
                 (momentum-actor (make-object self-braking-slab-actor%)))
            (send lookoutactor attach-to me lookout-actor-name)
            (send momentum-actor attach-to me motion-actor-name)
            (set! _awake? #t)
            (send hibernation-timer subscribe this) ;maybe sleep in awhile
            (if visible-state? (send me pen "red"))
            )))

    (define/public (sleep)
      (let ((me (send this object)))
        (for-each (lambda (name)
                    (send-actor-named me name delete))
                  (list lookout-actor-name motion-actor-name))
        (if visible-state? (send me pen "pink"))
        (send hibernation-timer unsubscribe this)
        (set! _awake? #f)))

    (define/override (delete)
      (sleep)
      (if visible-state? (send (send this object) pen "white"))
      (super delete))

    (define/public (update . args)
      ; if awake, infrequently receives update from hiberation timer
      ; Then: go to sleep if not moving and not selected
      (let ((me (send this object)))
        (if (and (not (send me selected?))
                 (zero? (send-actor-named me motion-actor-name speed)))
            (sleep))))

    (define/public (slide . args) (awaken)) ;respond to future motion

    (awaken) ;act when created

    ))


; (define vitalize
;   (case-lambda
; ;   (() (vitalize (send dynapad find
;    ((it)   (if (list? it)
;            (for-each vitalize it)
;         ;else
;            (let ((geoactor (geodize it))
;              (lookoutactor (make-object lookout-actor%))
;              (momentum-actor (make-object self-braking-slab-actor%)))
;          (send lookoutactor attach-to it)
;          (send momentum-actor attach-to it)
;          (send it pen "red")
; )))))

(define (awaken sth)
  ;vitalize one object,
  ; which then spreads life to nearby objects (via lookout actor)
  (if (list? sth)
      (for-each awaken sth)
      ;else single item; attach awakener, or use existing
      (if (not (send-actor-named sth 'awakener awaken)) ;if has awakener, use it
          (make-object awakener% sth)))) ;else make new

(define (sleep sth)
  (if (list? sth)
      (for-each sleep sth)
      ;else
      (send-actor-named sth 'awakener sleep)))


(define physics-region%
  (class region%
    (init _obj)
    (super-instantiate (_obj))
    (send this enter-action awaken)
    (send this leave-action (lambda (obj) (send-actor-named obj 'awakener delete)))
    (send this refresh-contents)
    ))

