; usage: pdfripdir [directory]

; mzc --exe pdfripdir pdfripdir.ss
; requires pdfrip.ss

(module pdfripdir mzscheme
  (require "pdfrip.ss")

  (pdfrip/verbose #t)

  (let*
    ((l (vector->list (current-command-line-arguments)))
     (dir (if (null? l) "." (car l)))
     (res (if (> (length l) 1) (string->number (cadr l)) 125)))

    (printf "THIS WILL CHANGE ~s~%" dir)
    (printf "ARE YOU SURE? (yes) ")
    (if (not (string-ci=? (read-line) "yes"))
      (error (format "Not doing anything, you weren't sure.~%")))

    (pdfripdir dir)))
