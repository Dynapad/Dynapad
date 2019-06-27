(module dynaload mzscheme
  (provide
    dynaload)
    
(require (lib "pathhack.ss" "misc"))

;--- dynaload ----------------------------------------------------

(define *dynapad-directory* (current-load-relative-directory))
(define *dynaload-list* ())

(define (dynaload file)
  (if (not (member file *dynaload-list*))
    (dynaload-force file) ))

(define (dynaload-force file)
  (define fullpath (dynaload-find-file file))
  (if fullpath
    (begin
      (load fullpath)
      (set! *dynaload-list* (cons file *dynaload-list*)) )
    ;else
    (error "dynaload can't find file:" file 'in *dynapad-directory*) ))

(define (file-if-exists? filename)
  (if (file-exists? filename)
    filename
    #f))

(define (dynaload-find-file file)
  (cond
    ((file-if-exists? (build-path->string *dynapad-directory* "src" "utils" file)))
    ((file-if-exists? (build-path->string *dynapad-directory* "src" "misc" file)))
    ((file-if-exists? (build-path->string *dynapad-directory* "src" "layout" file)))
    ((file-if-exists? (build-path->string *dynapad-directory* "src" "events" file)))
    ((file-if-exists? (build-path->string *dynapad-directory* "src" "menu" file)))
    ((file-if-exists? (build-path->string *dynapad-directory* "src" "image-utils" file)))
    ((file-if-exists? (build-path->string *dynapad-directory* "src" "history" file)))
    ((file-if-exists? (build-path->string *dynapad-directory* "src" "physics" file)))
    ((file-if-exists? (build-path->string *dynapad-directory* "src" file)))
    ((file-if-exists? (build-path->string *dynapad-directory* file)))
    (else #f)))

;--- end dynaload ------------------------------------------------

; appload assumes application name matches application directory
; usage: (appload "foo") --> apps/foo/foo.ss
; usage: (appload "foo" "bar.ss") --> apps/foo/bar.ss
(define (appload dirname . startfile)
  (define appname (if (not (null? startfile))
                      (car startfile)
                      (string-append dirname ".ss")))
  (define filename (build-path->string *dynapad-directory* "apps" dirname appname ))
  (if (file-exists? filename)
    (begin (load filename) #t)
    (error "can't find app:" filename)))

)