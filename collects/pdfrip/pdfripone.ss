; usage: pdfrip file

; mzc --exe pdfrip pdfripone.ss
; requires pdfrip.ss

; yes, I know the naming is confusing, compiling pdfripone creates pdfrip
; but I like the name pdfrip for both the base module and the command

(module pdfripone mzscheme
  (require "pdfrip.ss")

  (let*
    ((l (vector->list (current-command-line-arguments)))
     (file (if (null? l) null (car l))))
    (pdfrip file)))
