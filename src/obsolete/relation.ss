;THIS SHOULD ALL BE OBSOLETE, replaced by newrelation.ss
;  keep for back-compat

(dynaload "brush.ss")

(define relation%
  (class linkable-obj%
     (super-instantiate ())
     (init-field (_name #f))

     (field (_myclass 'relation%))
     (define/public myclass (get/set _myclass))

     (field (_members null))

     (define/public name (get/set _name))

     (define/public members
       (case-lambda
    (() _members)
        ((new-members)
         (for-each remove-member _members)
         (for-each add-member new-members))
    ((cmd . args)
         (case cmd
           ('add (for-each add-member args))
           ('remove (for-each remove-member args))))))

     (define (add-member memb)
       (unless (memq memb _members)
           (activate-member memb)
           (send memb delete-callbacks 'add
             (lambda (o)
               (send this members 'remove o)) this)
           (send memb post-build-ops 'add
             (lambda (obj label) ;obj=memb;
               `(defer-send '(,(export-objs this) members 'add ,(export-objs memb)))))
;               `(send obj refer-when-ready
;                  ,(send this id) 'members ''add obj)))
           (push! memb _members)))

    (define (remove-member memb)
      (deactivate-member memb)
      (send memb delete-callbacks 'remove #f this)
      (send memb post-build-ops 'remove #f this)
      (set! _members (remq memb _members)))

    (define/public (delete-all) (send this delete))

    (define/public (write)  ;at the moment: don't keep track of members;
                            ;  let them include selves in relation
      `(ic (make-object ,(myclass))
       ,@(writeoptions)))

    (define/public (write-all)
      (list (write)))

     (define/public (writeoptions)
       (filter identity
           `(
         (id ,(send this id))
         ,(if _name `(name ,_name) #f))))
     ;these will probably be overridden by subclass:
     (define/public (activate-member memb) #t)
     (define/public (deactivate-member memb) #t)

))

(define exhaustive-brushed-relation%
  (class relation%
     (init (_name #f))
     (super-instantiate (_name))
     (send this myclass 'exhaustive-brushed-relation%)
     (field (_brush-set (make-object exhaustive-brush-set%)))

     (define/public brush-set (get/set _brush-set))

     (define/public enable
       (case-lambda
    (() (enable #t))
    ((bool) (send _brush-set enable bool))))
     (define/public (disable) (enable #f))


     (define/override (activate-member memb)
       (send (brush-set) members 'add memb))
     (define/override (deactivate-member memb)
       (send (brush-set) members 'remove memb))

))


;(define (in-relation

;(define same-image-relation%
;  (class relation%
;     (init _name)
;     (super-instantiate (_name))

