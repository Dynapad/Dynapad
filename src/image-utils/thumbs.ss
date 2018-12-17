(require (lib "process.ss" "mzlib"))

(dynaload "strings.ss")

(define (thumbify hires-path thumb-path size)
  (system (string-append "thumbify thumb_max_size=" size  
			 " " hires-path " " thumb-path)))

(define (create-thumbs hires-dir size)
   (let* ((hires (hires-list hires-dir))
	(size-string (if (number? size) (number->string size) size))
	(thumb-dir (build-path->string hires-dir "thumbs" size-string)))
     (when (not (null? hires))
	   (unless (file-exists? thumb-dir)
		   (system (string-append "mkdir -p " thumb-dir)))
	   (map (lambda (h)
		  (let* ((hires-path (build-path->string hires-dir h))
			(dot (string-index h #\.))
			(name (substring h 0 dot))
			(type (substring h dot (string-length h)))
			(thumb-name (string-append name "-" size-string type))
			(thumb-path (build-path->string thumb-dir thumb-name)))
		    (thumbify hires-path thumb-path size-string)))
		  hires))))

	

