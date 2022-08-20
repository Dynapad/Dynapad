; usage: movebadtree [directory]

; mzc --exe movebadtree movebadtree.ss
; requires movebad.ss

(module movebadtree mzscheme
  (require "../pdfrip/movebad.rkt")

  (let*
      ((args (vector->list (current-command-line-arguments)))
       (dir (if (null? args) (current-directory) (car args))))
    (if (not (directory-exists? dir))
        (error (format "~s is not a directory" dir)))

    (printf "THIS WILL CHANGE ~s~%" dir)
    (printf "ARE YOU SURE? (yes) ")
    (if (not (string-ci=? (read-line) "yes"))
        (error (format "Not doing anything, you weren't sure.~%")))

    (movebadtree dir)))
