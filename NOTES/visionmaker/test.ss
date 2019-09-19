(define tablet (cadr (assoc "Tablet" (send dynapad xinputdevices))))
(define mouse (cadr (assoc "Mouse0" (send dynapad xinputdevices))))

(define o (make-object oval% dynapad '(0 0 100 100)))
(send o anchor "center")
(send o fill "red")
(send o bind "<MotionType>"
      (lambda(w e)
        (printf "~a ~a~%" (tablet-event-xid e) (tablet-event-p e))
        (send (event-obj e) width (+ 100 (tablet-event-p e)))))

(define r (make-object rect% dynapad '(-100 -100 -50 -50)))
(send r anchor "center")
(send r fill "white")

(define l (make-object line% dynapad '(-75 -75 0 0)))
(send l anchor "center")
(send l penwidth 10)
(send l pen "blue")

(define g (make-object group% dynapad (list l r)))
(send g divisible #f)

(send g bind "<MotionType>"
      (lambda(w e)
        (printf "~a ~a ~a~%" (tablet-event-xid e) (tablet-event-tiltx e) (tablet-event-tilty e))
        (let
            ((rx (car (send r xy)))
             (ry (cadr (send r xy))))
          (cond
            ((= (tablet-event-xid e) tablet)
             (printf "~a ~a~%" (event-x e) (event-y e))
             (send l coords (list
                             rx
                             ry
                             (- rx (* 2 (tablet-event-tiltx e)))
                             (- ry (* 2 (tablet-event-tilty e))))))
            ((= (tablet-event-xid e) mouse)
             (printf "~a ~a~%" (event-x e) (event-y e))
             (when (> (tablet-event-state e) 0)
               (send l coords (list
                               rx
                               ry
                               (event-x e)
                               (event-y e)))))))))

(send g bind "<ButtonPressType>"
      (lambda(w e)
        (let
            ((rx (car (send r xy)))
             (ry (cadr (send r xy))))
          (cond
            ((= (tablet-event-xid e) tablet)
             (printf "tablet ~a~%" (tablet-event-button e)))
            ((= (tablet-event-xid e) mouse)
             (printf "mouse ~a ~a ~a~%" (tablet-event-button e) (event-x e) (event-y e))
             (send l coords (list
                             rx
                             ry
                             (event-x e)
                             (event-y e))))
            (else
             (printf "unknown ~a~%" (tablet-event-xid e)))))))
