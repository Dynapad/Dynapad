# -*- mode: org; -*-
* Setup

Install [[https://www.racket-mode.com/][racket-mode]]
([[https://github.com/greghendershott/racket-mode/][GitHub]]).

If you use the built in package manager you can run the following block.

#+begin_src elisp
(require 'package)
(dolist (pair '(("melpa" . "https://melpa.org/packages/")))
  (add-to-list 'package-archives pair t))
(unless package--initialized
  (package-initialize))

(package-install 'racket-mode)
(require 'racket-mode)
(racket-mode-start-faster)
#+end_src

Put the following in [[~/.emacs.d/init.el][init.el]] for the following useful features.
- Run =raco make= to compile the current file (defaults to =F5=).
- Switch which Racket implementation you are using (invoke =racket-start-back-end= to reload).
- Enable semantic =xref-find-definition= (=M-,=) support (and more!) via
  [[https://www.racket-mode.com/#racket_002dxp_002dmode][=racket-xp-mode=]].
- 

#+begin_src elisp
(require 'racket-xp)

(defun recompile-quietly ()
  "Re-compile without changing the window configuration."
  (interactive)
  (save-window-excursion
    (recompile)))

(defun rcc ()
    "set racket compile command for racket buffers when there is a file to compile"
    ;; FIXME this is broken if the file is renamed via dired
    ;; `compilation-arguments' exists ... but I think this also has to go in
    ;; under rename hook? there is not rename hook so this would require advice
    ;; or something
    (when buffer-file-name
      (setq-local compile-command
           ;; change to "racocgc make %s" if using racketcgc directly
           (format "raco make %s" (file-name-nondirectory (buffer-file-name))))))

;; uncomment this if you want to use racketcgc directly
;; (setq racket-program "~/git/racket/racket/bin/racketcgc")
;; (racket-memory-limit 0) ; 0 required to get racketcgc to start

(add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode))

(define-key racket-mode-map (kbd "<f5>") #'recompile-quietly)

(add-hook 'racket-mode-hook #'rcc)
(add-hook 'racket-mode-hook #'racket-xp-mode)
#+end_src

* Workflow
A workflow that I have found effective for developing large racket
programs is to use =f5= to compile modules and then =C-c C-c= to run
them in the Racket repl. This is generally faster than just using =C-c
C-c= directly.

If you are using =racketcgc= directly (see comments in the =init.el=
block above) then you can also evaluate forms from a file using =C-x
C-e= from a =racket-mode= buffer.

Example. =M-x find-file= =~/git/dynapad/apps/paddraw/paddraw.rkt= =f5=
=C-c C-c=.

You might want to do this in a dedicated Emacs instance since dynapad
is known to crash on exit/restart, and even though the racket process
is separate, something could get messed up with =racket-mode='s state.
