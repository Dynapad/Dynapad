; usage: thumbifydir [directory [res]]

; mzc --exe thumbifydir thumbifydir.ss
; requires thumbify.ss

(module thumbifydir mzscheme
  (require "thumbify.ss")

  (thumbify/verbose #t)

  (let*
    ((l (vector->list (current-command-line-arguments)))
     (dir (if (null? l) (current-directory) (car l)))
     (res (if (> (length l) 1) (string->number (cadr l)) 125)))

    (printf "THIS WILL CHANGE ~s~%" dir)
    (printf "ARE YOU SURE? (yes) ")
    (if (not (string-ci=? (read-line) "yes"))
      (error (format "Not doing anything, you weren't sure.~%")))

    (thumbifydir dir res)))
