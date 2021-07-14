; usage: pdfriptree [directory]

; mzc --exe pdfriptree pdfriptree.ss
; requires pdfrip.ss

(module pdfriptree mzscheme
  (require "pdfrip.rkt")

  (pdfrip/verbose #t)

  (let*
      ((l (vector->list (current-command-line-arguments)))
       (dir (if (null? l) (current-directory) (car l)))
       (res (if (> (length l) 1) (string->number (cadr l)) 125)))

    (printf "THIS WILL CHANGE ~s~%" dir)
    (printf "ARE YOU SURE? (yes) ")
    (if (not (string-ci=? (read-line) "yes"))
        (error (format "Not doing anything, you weren't sure.~%")))

    (pdfriptree dir)))
