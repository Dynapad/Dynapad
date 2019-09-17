(require (lib "process.ss"))
(define break-on-last-dot-regexp (regexp "(.*)([.][a-zA-Z]+)"))
; parses xxxxx.yyy into xxxxx and .yyy (where xxxxx may contain .)

(define (break-on-last-dot filename)
  (let ((match (regexp-match break-on-last-dot-regexp filename)))
    (if match
    (cdr match)  ;found dot, return '("before" ".after")
    (list filename "")))) ;no dot, return '("all" "")

(define (insert-filenamebase-suffix name suffix)
;  name = base.dot --> base<suffix>.dot
  (let ((matches (regexp-match break-on-last-dot-regexp name)))
    (if matches
    (string-append (cadr matches) suffix (caddr matches))
    (string-append name suffix))))


(define (file-or-dir-exists? path)
  (or (file-exists? path)
      (directory-exists? path)))

(define (construct-unique-filename+seed construct-fn exists-fn increment-fn seed)
  ; tries filename (construct-fn seed)
  ;   successively replacing seed with (increment-fn seed)
  ;   until
  ; 1) (not (exists-fn filename)): returns (list <construction> seed)
  ; 2) or (construct-fn seed) returns #f: returns #f

  (let ((tryname (construct-fn seed)))
    (cond ((not tryname) #f) ;give up
      ((not (exists-fn tryname)) (list tryname seed)) ;use this name
      (else (construct-unique-filename+seed construct-fn exists-fn increment-fn
                        (increment-fn seed))))))

(define (construct-unique-filename construct-fn exists-fn increment-fn seed)
  (let ((pair (construct-unique-filename+seed construct-fn exists-fn increment-fn seed)))
    (and pair (car pair))))

(define (construct-unique-filename-seed construct-fn exists-fn increment-fn seed)
  (let ((pair (construct-unique-filename+seed construct-fn exists-fn increment-fn seed)))
    (and pair (cadr pair))))
  

#|
; Examples:
; --Generates "dir/tryN" for first unique N>=0:
(construct-unique-filename (lambda (n) (format "dir/try~a" n))
            file-exists?
            (lambda (n) (+ n 1))  0)
=
(construct-unique-filename-append-number "dir/try")
|#

(define (construct-unique-filename-append-number basestr)
; returns "name-0", "name-1"...
  (construct-unique-filename (lambda (n) (format "~a-~a" basestr n))
                 file-or-dir-exists?
                 (lambda (n) (+ n 1))
                 0))
(define (construct-unique-filename-insert-number>1 basestr)
; returns "name.xyz", "name#2.xyz"...
  (construct-unique-filename (lambda (n)
                   (if (< n 2)
                   basestr
                   (let* ((pair (break-on-last-dot basestr))
                      (pre  (car pair))
                      (post (cadr pair)))
                     (format "~a#~a~a" pre n post))))
                 file-or-dir-exists?
                 (lambda (n) (+ n 1))
                 1))

#|
; --Generates "try123", "try123a", "try123b"...
;   (and fails if runs out of letters):
(let ((alphabet (list "" "a" "b" "c"...)))
  (ensure-unique-filename (lambda (a) (and (not (null? a))
                       (format "try123~a" (car a))))
              file-exists?
              cdr alphabet))

|#

#|
  (case-lambda
   ((obj dir)
    (let* ((names (send-actor obj 'title))
       (usename (ormap identity names)))
      (if (not usename)
      (let* ((clumpword (or (ormap (lambda (w) w) (send-actor obj 'clumpword))
                "group")))
        (set! usename
          (construct-unique-filename-append-number (buildpath dir clumpword)))
        ))
      (export-container obj dir usename)))      
   ((obj dir filename)
|#

(define (export-container-generic-name dir namebase)
  (let ((path (construct-unique-filename-append-number (build-path->string dir namebase))))
    (make-directory path)
    path))

(define (export-container-custom-name dir filename)
    (let ((path (build-path->string dir filename)))
      (if (file-or-dir-exists? path)
      (begin (error "File exists:" path) #f)
      (begin (make-directory path) path))))

;creates an empty file in dir uniquely named "namebaseN" (usually "dynaclass%N")
(define (export-stub dir namebase)
  (let ((filename (construct-unique-filename-insert-number>1
           (build-path->string dir (format "~a" namebase)))))
    (if (file-exists? filename)
    (begin (error "File exists:" filename) #f)
    (begin (system (format "touch ~a" filename)) #t))))

;creates a symbolic link to target named filename
(define (export-link target filename)
  (set! filename (construct-unique-filename-insert-number>1 filename))
  (if (file-exists? filename)
      (begin (error "File exists:" filename) #f)
      (begin (system* (find-executable-path "ln" #f) "-s" target filename)
         #t)))
;      (begin (system (format "ln -s ~a ~a" target filename)) #t)))


(define (filter-and-sort-directory dir extract-fn lessthan-fn . filter-fns)
(say "filtering...")
  (let* ((names (directory-list->string dir))
     (fullnames (map (lambda (f) (build-path->string dir f)) names))
     (vals null)
     (tuples null))
    (foreach (cons file-exists?  filter-fns)
         (lambda (fn)
           (set! fullnames
             (filter fn fullnames))))
    (set! names (map file-name-from-path fullnames))
    (set! vals (map extract-fn fullnames))
    (set! tuples (map list names vals))
    (set! tuples (sort (lambda (a b)
             (lessthan-fn (cadr a) (cadr b))) tuples))
(say "end filtering...")
    (map car tuples)
))

; Example of usage:
(define (sort-image-files-by-descending-size dir)
  (filter-and-sort-directory dir file-size >
                 sch_imagep))
