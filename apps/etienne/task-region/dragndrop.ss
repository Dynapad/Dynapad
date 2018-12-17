
(define (find-first-not-like-me me l)
  (if (null? l)
      #f
      (if (eq? me (car l))
	  (find-first-not-like-me me (cdr l))
	  (car l))))

(define (handle-drag-release o e)
  (define overlapped   (send dynapad find 'overlapping
			     (list (event-x e) (event-y e)
				   (event-x e) (event-y e))))
  (define selected     (send dynapad selected))
  (define regions
    (filter
     (lambda (r)
       (if (is-a? r task-region%)
	   (send r xy-within? (event-x e) (event-y e))))
     overlapped))

  ;; For each item that was dropped
  (for-each
   (lambda (object)
     ;; Get the region the object belongs to, if any.
     (define oldr (get-object-keyval object 'region #f))

     ;; make sure no overlap of regions -- can't get regioned to self
     (define newr (find-first-not-like-me object regions))

     (cond ((and newr oldr)
	    (if (eq? newr oldr)
                (send newr drag-drop-move object)
                (begin
                  (send oldr drag-drop-leave object)
                  (send newr drag-drop-enter object))))
	   ((and newr (not oldr))
            (send newr drag-drop-enter object))
	   ((and (not newr) oldr)
	    (send oldr drag-drop-leave object))
	   ((and (not newr) (not oldr))
	    '()))
     (set-object-keyval object 'region newr))
   selected))