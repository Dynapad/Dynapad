
(define named-binding%
  (class object%
    (init __name __function)
    (field (_name     __name)
	   (_function __function))
    (super-instantiate ())
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Methods
    (define/public (name)     _name)
    (define/public (function) _function)
    )
  )

(define region-bindbox%
  (class object%
    (field (_object-bindings null))
    (field (_region-bindings null))
    (field (_region          null))

    (field (_launcher  (make-object launcher%)))
    (field (_mysql     (make-object mysql-history%)))

    (super-instantiate ())

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Pile the binbox belongs to
    (define/public region
      (case-lambda
       (()  _region)
       ((r) (set! _region r))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Shortcut methods
    (define/public (get-region-bindings event)
      (get-bindings "region" event))

    (define/public (get-object-bindings event)
      (get-bindings "object" event))

    (define/public (add-region-binding event binding)
      (add-binding "region" event binding))

    (define/public (add-object-binding event binding)
      (add-binding "object" event binding))

    (define/public (remove-region-binding event name)
      (remove-binding "region" event name))

    (define/public (remove-object-binding event name)
      (remove-binding "object" event name))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Helper methods
    (define/private (update-event-bindings type event bindings)
      ;; Replace the list of pile-object-bindings for this event
      ;;  to the list of bindings for this bindbox%
      (cond ((string=? type "region")
	     (replace-else-push-onto-malist!
	      assoc event bindings _region-bindings))
	    ((string=? type "object")
	     (replace-else-push-onto-malist!
	      assoc event bindings _object-bindings))
	    (else null)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Get the binding list for an event
    (define/public (get-bindings type event)
      (define w (cond ((string=? type "region") _region-bindings)
		      ((string=? type "object") _object-bindings)
		      (else null)))
      (define x (assoc event w))
      (if x (cdr x) null))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Remove a binding object from a list of 
    ;;  region or object bindings
    (define/public (remove-binding type event name)
      ;; Get the bindings for this event.
      (define event-bindings (get-bindings type event))
      ;;
      ;; Remove the binding object
      (for-each
       (lambda (event-binding)
	 (if (string=? (send event-binding name) name)
	     (set! event-bindings (remq event-binding event-bindings))))
       event-bindings)
      ;;
      ;; Update the bindings for this event
      (update-event-bindings type event event-bindings))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Add binding to the bindbox
    (define/public (add-binding type event binding)
      ;; Remove existing bindings by that name first
      (remove-binding type event (send binding name))
      ;;
      ;; With the new event-bindings
      (let ((event-bindings (get-bindings type event)))
	;;
	;; Add the new named-binding% to the list
	(push! binding event-bindings)
	;;
	;; Update the bindings for this event
	(update-event-bindings type event event-bindings)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Set the bindings of a given object
    (define/public (set-bindings object)
      (say "set-bindings object")

      ;; First set the object bindings.
      (for-each
       (lambda (event)
	 (define event-list (get-object-bindings event))
	 (for-each
	  (lambda (binding)
	    (send object bind event (send binding function)))
	  event-list))
       (map car _object-bindings))

      (say "set-bindings object")

      ;; Then set the region bindings.
      (if (not (null? _region))
	  (for-each
	   (lambda (event)
	     (define event-list (get-region-bindings event))
	     (for-each
	      (lambda (binding)
		(cond ((string=? event "<Abandon>")
		       (send _region abandon-actions 'add
			     (send binding function)))
		      ((string=? event "<Enter>")
		       (send _region enter-actions 'add
			     (send binding function)))
		      ((string=? event "<Leave>")
		       (send _region leave-actions 'add
			     (send binding function)))
		      ((string=? event "<Remove>")
		       (send _region remove-actions 'add
			     (send binding function)))))
	      event-list))
	   (map car _region-bindings))))
    ))


(define web-region-bindbox%
  (class region-bindbox%
    (inherit add-object-binding add-region-binding)
    (inherit-field _launcher _mysql _region)

    (super-instantiate ())

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Object bindings
    (define go-to-url
      (make-object
       named-binding%
       "go-to-url"
       (lambda (d e)
	 (define object (event-obj e))
	 (define event  (get-object-keyval object 'event -1))
	 (if (> event 0)
	     (let ((url (send _mysql get-meta-value event 2)))
	       (send _launcher web-browser url)
	     null)))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Region bindings
    (define make-new-pile
      (make-object
       named-binding%
       "make-new-pile"
       (lambda (obj)
	 (define coordinates (send obj xy))
	 (define pile (make-pile event-pile% "etienne" ()))
	 (define polygon (send pile poly))
	 (define duplicate (clone-object obj))
	 (say "one")
	 ;; place new pile and add object to the new object
	 (send polygon xy coordinates)
	 (say "two")
	 (send pile receive duplicate)
	 (say "three")
	 (send pile finish)
	 (say "four")
	 ;; now delete the original
	 (send _region remove obj #t)
	 (say "five"))
       ))


    ;; Set object bindings
    (add-object-binding "<Double-ButtonPress-1>" go-to-url)

    ;; Set region bindings
    (add-region-binding "<Remove>" make-new-pile)
	     
    )
  )

;(define bindbox (make-object web-pile-object-bindbox%))

;(define poly (make-object polygon% dynapad '(0 0 0 50 50 50 50 0)))
;(send poly fill "blue")

;(send bindbox set-bindings poly)

