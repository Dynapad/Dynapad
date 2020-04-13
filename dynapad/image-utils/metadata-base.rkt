#lang racket/base

(require (only-in racket/class send)
         (only-in racket/list
                  second
                  fourth)
         (only-in racket/system process)
         (only-in dynapad/misc/misc
                  while
                  cset!
                  ensure-number
                  )
         (only-in dynapad/misc/alist
                  remote-get
                  remote-push!
                  remote-set!)

         (only-in dynapad/utils/parsedate
                  infer-complete-date
                  list->date
                  string->date
                  date->seconds
                  )
         )
(provide (all-defined-out))

; this define...let...set! construction lets regexps be pre-compiled but scoped locally:
(define (match-metadata-line-for-tag line tag) #f)
; given a tag and line of output from a metadata-generating process
; (e.g. jhead, pdfinfo),
; parses line appropriately to tag and returns non-#f if match, #f otherwise
(let (; jhead entries:
      (photo-datetime-rexp (regexp "^Date/Time[ \t]*: +(....):(..):(..) +(..):(..):(..)$"))
      (focus-dist-rexp     (regexp "^Focus Dist.[ \t]+: +([0-9.]+)([a-z]+)$"))
      (file-datetime-rexp  (regexp "^File date[ \t]*: +(....):(..):(..) +(..):(..):(..)$"))
      (aperture-rexp       (regexp "^Aperture[ \t]+: +f/(.*)$"))
      ; pdfinfo entries:
      (publish-date-rexp   (regexp "^CreationDate:[ \t]*(.+)$"))
      ; other services/entries may be added here...
      )
  (set! match-metadata-line-for-tag
        (lambda (line tag)
          (case tag
            ((photo-datetime)
             (let ((match (regexp-match photo-datetime-rexp line)))
               (and match
                    (infer-complete-date
                     (apply list->date
                            (map ensure-number (reverse (cdr match))))))))

            ((file-datetime)
             (let ((match (regexp-match file-datetime-rexp line)))
               (and match
                    (infer-complete-date
                     (apply list->date
                            (map ensure-number (reverse (cdr match))))))))

            ((fstop)
             (let ((match (regexp-match aperture-rexp line)))
               (and match (ensure-number (cadr match)))))

            ((focus-dist)
             (let ((match (regexp-match focus-dist-rexp line)))
               (and match (ensure-number (cadr match)))))

            ((publish-date)
             (let ((match (regexp-match publish-date-rexp line)))
               (and match
                    (infer-complete-date
                     (string->date (cadr match))))))

            (else #f)))))

;generalize generate-and-cache process:
(define (get-obj-metadata obj tag generate-fn)
  (let* ((keyval (assq tag (send obj alist)))
         (val (if keyval
                  (cadr keyval) ;reuse cached val
                  (generate-fn obj)))) ;unknown, generate anew
    (when (not keyval) ;cache for next time
      (remote-push! (list tag val) obj alist))
    val))

(define (extract-metadata-with-cmd tag path cmd)
  ; jhead is a system app which extracts exif data from .jpgs;
  ;   pdfinfo extracts info from .pdfs
  ; This fn starts a metadata-extraction process (e.g. jhead, pdfinfo)
  ;  and reads its results on input-port;
  ;  if any line matches regexp (above) for tag, returns parsed value, else #f
  (let* ((ports (and path (file-exists? path)
                     (process (format "~a ~s" cmd path))))
         (input-port (and ports (car ports))))
    (and input-port
         (let ((line #f)
               (found #f))
           (while (and (not found)
                       (not (eq? eof (cset! line (read-line input-port)))))
             (set! found
                   (match-metadata-line-for-tag line tag)))
           (close-input-port input-port)
           (close-output-port (second ports))
           (close-input-port (fourth ports))
           found))))

(define (get-pdf-date obj)
  (let ((pair
         (get-obj-metadata
          obj 'publish-date
          (lambda (pdf) (let ((date
                               (extract-metadata-with-cmd
                                'publish-date (send pdf url) "pdfinfo ")))
                          (and date
                               (list (date->seconds date) date)))))))
    pair))

(define (get-filedate path)
  (and (file-exists? path)
       (let* ((secs (file-or-directory-modify-seconds path))
              (date (seconds->date secs)))
         (list secs date))))

(define (get-pdf-filedate pdf)
  (let ((pair
         (get-obj-metadata
          pdf 'filedate
          (lambda (pdf) (get-filedate (send pdf url))))))
    pair))

