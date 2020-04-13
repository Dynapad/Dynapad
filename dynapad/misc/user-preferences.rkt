#lang racket/base

(require (only-in racket/class send)
         (only-in racket/file make-directory*)
         (only-in racket/gui/base message-box)
         collects/misc/pathhack
         )

(provide setup-dynapad-subdirectory
         *use-menubar*
         *home-directory*
         load-and-apply-user-preferences
         *draw-menu-x*
         *draw-menu-y*
         *cowboy-zoom-on-objects*
         *window-geometry*
         )

; default user preferences
(define *window-geometry* #f)
(define *draw-menu-x* #f)
(define *draw-menu-y* #f)
(define *saved-fill* "none")
(define *saved-pen* "#ffffff")
(define *saved-penwidth* 1)
(define *saved-background* "#000000")
(define *pad-selection-mode* 'show)
(define *cowboy-zoom-on-objects* #t)
(define *use-menubar* #t)

(define *home-directory* #f)

; load .padsetup.ss to get user preferences
(define (load-user-preferences)
  (let ((home (find-system-path->string 'home-dir))
        (padsetup #f))
    (cond
      ((not home) (error "no home directory"))
      ((string-ci=? home "C:\\")
       (message-box "HOME" "home not set, Using C:\\" #f '(ok-cancel))))
    (when home
      (set! *home-directory* home))
    (when home
      (set! padsetup (build-path->string home ".padsetup.ss"))
      (when (file-exists? padsetup) (load padsetup)))))

(define (setup-dynapad-subdirectory . args)
  (unless *home-directory*
    (error "No *home-directory* in which to create" args))
  (let ((path (apply build-path->string *home-directory* ".dynapad" args)))
    (make-directory* path)
    path))

(define (load-and-apply-user-preferences argPAD)
  (load-user-preferences)

  ;tell the dynapad about stored user preferences
  (send argPAD defaultfill *saved-fill*)
  (send argPAD defaultpen *saved-pen*)
  (send argPAD defaultpenwidth *saved-penwidth*)
  (send argPAD background *saved-background*)
  )

