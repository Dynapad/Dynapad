; usage: thumbify file [res]

; mzc --exe thumbify thumbifyone.ss
; requires thumbify.ss

; yes, I know the naming is confusing, compiling thumbifyone creates thumbify
; but I like the name thumbify for both the base module and the command

(module thumbifyone mzscheme
  (require "thumbify.ss")

  (let*
      ((l (vector->list (current-command-line-arguments)))
       (file (if (null? l) null (car l)))
       (res (if (> (length l) 1) (string->number (cadr l)) 125)))
    (thumbify file res)))
