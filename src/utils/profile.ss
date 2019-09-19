(define profile-enter-times null)
(define profile-total-times null)
(define (enter msgname)
  ;  (let* ((time (ensure-value-in-alist msgname (current-milliseconds)
  ;                      enter-times assv)))
  (push! (list msgname (current-milliseconds)) profile-enter-times))

(define (leave msgname)
  (let* ((exit-time (current-milliseconds))
         (enter-pair (get-and-rem-from-malist! assv remv msgname profile-enter-times))
         (enter-time (cadr enter-pair))
         (total-pair (get-and-rem-from-malist! assv remv msgname profile-total-times))
         ;     (total (if total-pair
         ;            (cadr total-pair)
         ;            0))
         )
    (get-else-push-onto-malist! assv (list msgname 0) profile-total-times)
    (modify-malist-val! assv
                        (lambda (val) (list (+ (car val) (- exit-time enter-time))))
                        msgname
                        profile-total-times)))
;    (push! (list msgname (+ total (- exit-time enter-time)))
;       profile-total-times)))


;FUNCTION USAGE:
;(function args...) --> (measure <name> function args...)
;                        ^^^^^^^ ^^^^^^
(define (measure name cmd . args)
  (enter name)
  (let ((result (eval (apply cmd args))))
    (leave name)
    result))

;MESSAGE USAGE:
;(send obj msg args...)  --> (measure-send <name> obj msg args...)
;                             ^^^^^^^^^^^^ ^^^^^^
(define (measure-send name obj msg . args)
  (enter name)
  (let ((result
         (if (null? args)
             (eval `(send ,obj ,msg))
             (eval `(send/apply ,obj ,msg ',args)))))
    (leave name)
    result))

(define echo-indent 0)

(define-syntax echo-send
  (syntax-rules ()
    ((_ obj msg ...)
     (begin
       (display (format "~a~a send: " (make-string echo-indent #\space) (current-milliseconds)))
       (display (list 'msg ...))(newline)
       (+= echo-indent 2)
       (let ((result (send obj msg ...)))
         (-= echo-indent 2)
         result)))))

(define-syntax echo-send/apply
  (syntax-rules ()
    ((_ obj msg ...)
     (begin
       (display (format "~a~a send: " (make-string echo-indent #\space) (current-milliseconds)))
       (display (list 'msg ...))(newline)
       (+= echo-indent 2)
       (let ((result (send/apply obj msg ...)))
         (-= echo-indent 2)
         result)))))
