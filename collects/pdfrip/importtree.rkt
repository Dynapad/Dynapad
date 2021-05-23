; usage: importtree [directory]

; mzc --exe importtree importtree.ss
; requires import.ss

(module importtree mzscheme
  (require "../pdfrip/import.ss")

  (let*
      ((args (vector->list (current-command-line-arguments)))
       (dir (if (null? args) (current-directory) (car args))))
    (if (not (directory-exists? dir))
        (error (format "~s is not a directory" dir)))

    (printf "THIS WILL CHANGE ~s~%" dir)
    (printf "ARE YOU SURE? (yes) ")
    (if (not (string-ci=? (read-line) "yes"))
        (error (format "Not doing anything, you weren't sure.~%")))

    (importtree dir)))
