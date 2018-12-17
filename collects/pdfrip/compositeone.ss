; usage: composite file

; mzc --exe composite compositeone.ss
; requires composite.ss

; yes, I know the naming is confusing, compiling compositeone creates composite
; but I like the name composite for both the base module and the command

(module compositeone mzscheme
  (require "composite.ss")

  (let*
    ((l (vector->list (current-command-line-arguments)))
     (file (if (null? l) null (car l))))
    (composite file)))
