; A handle% associates multiple dynaobjects (renderings)
;  with a common abstract object such that they all share
;  a level of metadata.
; Furthermore, different handles share URLs (e.g. filenames, directories, etc)
;      URL
;     / | \
;    /  |  \
;   H0  H1 H2      <= metadata on URL is shared within each H group
;  / |  |  | \
; R0 R1 R2 R2 R4

(define *url-handle-alist* null)
(define (get-handles url)
  (let ((tuple (assoc url *url-handle-alist*)))
    (if tuple (cdr tuple) null)))
(define (assign-handle url choose-fn hndl-class . args)
; if >1 existing handle for url, (choose-fn list) returns one choice
;   (may be simply car)
; if none existing, make new using handle-class% and args
  (let* ((existing-hndls (get-handles url)))
    (if (null? existing-hndls)
	(apply make-object hndl-class url args) ;make new
	(choose-fn existing-hndls))))   ; reuse one


;URL redirection
(define (replace-path-head? oldhead newhead path)
; if oldhead heads path, returns new path substituting newhead for oldhead
; else return #f
  (let* ((oldheadlist (explode-path->string oldhead))
	 (pathlist (explode-path->string path))
	 (tail	  (list-head? oldheadlist pathlist equal?)))
    (and tail
	 (apply build-path->string newhead tail))))

; defs of *url-alias-alist* and alias-url
;  moved to alias.ss


(define url%
  (class object%
    (init _original)
    (super-instantiate ())
    (field (_url (send this url _original)))
    
    (define/public url
      (case-lambda
       (() (ensure-string _url))  ;#path issues
       ((path) ;maybe replace path head
	(if (and path (relative-path? path))
	    (error "URL is relative path"))
	(cset! _url
	       (and path
		    (let ((replacement
			   (first-valid 
			    *url-alias-alist*
			    (lambda (tuple)
			      (let ((old (car tuple))
				    (new (cadr tuple)))
				(replace-path-head? old new path))))))
		      (if replacement
			  (cadr replacement)
			  path)))))))
))


(define base-handle%
  (class object%
    (init (_url-init #f))
    (field (_url (make-object url% _url-init)))
    (field (_instances null))
    (super-instantiate ())

    (push-onto-alist-val-always! assoc (url) this *url-handle-alist*)

    (define/public (url) (send _url url))

    (define/public (instances)
      (filter (lambda (o) (not (send o deleted?))) _instances))
    (define/public (instances-except . objs)
      (filter (lambda (i) (not (memq i objs))) (instances)))

    (define/public (add inst) ;inst is a dynaobject
      (if (memq inst _instances)
	  #t ;already there
	  (begin ;newly added
	    (push! inst _instances)
	    (set-object-keyval inst 'handle this)
	    #f)))

    (define/public (remove inst)
      (rem-object-keyval inst 'handle)
      (remq inst _instances))

    (define/public (delete)
      (foreach (lambda (i) (rem-object-keyval i 'handle)) (instances))
      (delete-clean-from-alist-val! assoc delete (url) this *url-handle-alist*))
))

(define (get-hndl obj)  ;needed for back-compatibility (e.g. brush.ss)
  (get-object-keyval obj 'handle))

(define (get-pdf-metadata-dir pdfpath)
  (let ((url (send (make-object url% pdfpath)
		   url)))
    (let-values (((pdfdir pdffile junk) (split-path->string url)))
		(string-append pdfdir "." pdffile))))

(define image-handle%
  (class base-handle%
    (init url)
    (super-instantiate (url))
))
(define (spawn-process cmd . args)
  (apply process* cmd args))
;  (apply process* cmd (append args (list "&"))))

(define pdf-handle%
  (class base-handle%
    (init url)
    (super-instantiate (url))

    (field (_dir ;.name.pdf subdir where metadata/images are stored
	    (get-pdf-metadata-dir url)))
	  
    (field (_title-file "title")
	   (_title #f)
	   (_baseimage-file "firstpage.jpg") ; filename of base image
	   (_composite-file "composite.jpg") ; filename of merged image
	   (_subimages null)) ;configuration of subimages:
			      ; list of tuples (filename b0 b1 b2 b3)
                              ; where b0..b3 constitute relative bbox
			      ; within portrait bbox
;    (field (_year 1970))

    (define/public (title)
      (unless _title
	;look up in title-file and cache
	      (let* ((filename (build-path->string _dir _title-file))
		     (readport (and (file-exists? filename)
				    (open-input-file filename 'text)))
		     (line (and readport (read-line readport))))
		(if readport (close-input-port readport))
		(set! _title (or line ""))))
      _title)

    (define/public dir (get/set _dir))
    (define/public subimages (get/set _subimages))
    (define/public baseimage-file (get/set _baseimage-file))
    (define/public composite-file (get/set _composite-file))
    ;(define/public year
    ;  (case-lambda
    ;   (() _year)
    ;   ((y) (set! _year y)
;	    (foreach (send this instances)
;	       (lambda (inst)
;		 (replace-else-push-onto-alist! assq 'timestamp (list y) inst alist))))))
;			(list y "" "" "" "" "" "") inst alist))))))

    (define/public (start-viewer)
      (let ((useapp (find-executable-path *pdf-viewer-app* #f)))
	;this still works even if path is already found
	(spawn-process useapp
		       (ensure-string (send this url)))))

    (define/public (refresh-all-instances-except . excpts)
      (let ((valids ;(list-diff (send this instances) excpts memq)))
	        (send/apply this instances-except excpts)))
	; this first loop is a hack to solve the problem
	; that shared image urls are cached:
	; must first destroy all copies of old composite
	;  before installing new one
	(foreach valids
	    (lambda (o)
	      (let* ((old-pict (send o graphic))
		     (old-pos  (and old-pict (send old-pict position))))
		(when old-pos
		      (send old-pict delete)
		      (send o graphic (ic (make-object rect% (send o dynapad))
					  (position old-pos)))))))
	(foreach valids
		 (lambda (i) 
		   (unless (memq i excpts)
			   (send i refresh-graphic))))))

))
