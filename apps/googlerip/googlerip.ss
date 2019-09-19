(require (lib "process.ss"))

(define dir "/tmp/googlerip/")
(if (not (directory-exists? dir))
    (make-directory dir))

(define (google-images . args)
  (define cmd (string-append *dynapad-directory* "apps/googlerip/google.pl"))
  (delete-google-images)
  (for-each (lambda (a) (set! cmd (string-append cmd " " a))) args)
  (set! cmd (string-append cmd " 2> /dev/null"))
  (system cmd)
  (arrange-google-images)
  )


;; Use object-oriented design to distinguish google images
;; from normal dynapad images
(define googleimage%
  (class image%
    (init initpad)
    (init (_hirespath #f))
    (init (initposition #f))
    (super-instantiate (initpad _hirespath initposition))
    )
  )


;; Delete images from last google query if they exist
(define (delete-google-images)
  ;; Delete images from Dynapad
  (for-each (lambda (o)
              (if (is-a? o googleimage%)
                  (send o delete-all)))
            (send currentPAD objects))

  ;; Remove images from file system
  (for-each
   (lambda(x)
     (delete-file (build-path->string dir x)))
   (directory-list->string dir))
  )

;; Make and arrange images from recent google query
(define (arrange-google-images . returnflag)
  (let ((image-list (arrangedir-google)))
    (if image-list (send dynapad center image-list 1000 #t))
    ;(if (null? returnflag) (length image-list) image-list)
    image-list
    )
  )

(define (arrangedir-google)
  (if (not (directory-exists? dir))
      (begin (error "arrangedir-google: directory doesn't exist " dir) #f)
      (let ((filelist (hires-list dir))
            (images ()))
        (set! images (arrangegoogleimages (map (lambda (file) (build-path->string dir file)) filelist)))
        (for-each (lambda (fn) (fn dir images)) *image-dir-callbacks*)

        images))
  )

(define (arrangegoogleimages files)
  (set! files
        (filter
         (lambda (file) (and (regexp-match gifjpg_rexp file) (file-exists? file)))
         files))
  (let* ((images null)
         (vu (send dynapad view))
         (view_x (car vu))
         (view_y (cadr vu))
         (zfac (/ 1.0 (caddr vu)))
         (x view_x)
         (y view_y)
         (numrows (sqrt (length files)))
         )
    (if *current-z-plane* (set! zfac *current-z-plane*))
    (for-each
     (lambda (file)
       (set! images
             (append images (list
                             (make-object googleimage% dynapad file (list x y zfac)))))
       (set! x (+ x (* 200 zfac)))
       (when (> (- x view_x) (* (* 200 numrows) zfac))
         (set! x view_x)
         (set! y (- y (* 200 zfac)))))
     files)
    ;undo hook replaces: images))
    (undoify-fresh-objs images)))
