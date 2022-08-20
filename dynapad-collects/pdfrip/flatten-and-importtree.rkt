(module flatten-and-importtree mzscheme
  (require "../pdfrip/import.rkt")

  (let*
      ((args (vector->list (current-command-line-arguments)))
       (dir (if (null? args) (current-directory) (car args))))
    (if (not (directory-exists? dir))
        (error (format "~s is not a directory" dir)))

    (printf "THIS WILL CHANGE ~s~%" dir)
    (printf "ARE YOU SURE? (yes) ")
    (if (not (string-ci=? (read-line) "yes"))
        (error (format "Not doing anything, you weren't sure.~%")))

    (flatten-and-importtree dir)))
