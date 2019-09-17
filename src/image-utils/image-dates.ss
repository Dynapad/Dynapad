; images dumped from digital cameras sometime contain
; information in the file header.  The time/dates info
; can be extracted using the shellscript "exif_dates"
; which stores the results in a file called "./thumbs/dates.txt"

; image-dates.ss enables the dynapad to read that date
; information, if it exists.  This happens after the entire
; directory has been loaded (see arrangeimages.ss).

; The data is stored with the dynaobject in this format:
; (1046133339 "2003" "02" "24" "16" "35" "39")
; which has data fields:
;
;  total seconds since 1970
;  year
;  month
;  day
;  hour
;  minutes
;  seconds

(require (lib "date.ss"))
(dynaload "parsedate.ss")

(define (dir->date-alist dir)
  (define datefile (build-path->string dir "thumbs" "dates.txt"))
  (define dates_alist ())

  (if (file-exists? datefile)
    (let ((line 0)
          (fp (open-input-file datefile))
          (re (regexp "(.*):(..):(..) +(..):(..):(..) (.*)"))
          (done #f)
          (totalseconds 0)
         )
      (while (not done)
        (set! line (read-line fp))
        (if (eof-object? line)
          (set! done #t)
          ;else
          (mlet (((y m d hr min sec name) (cdr (regexp-match re line))))
            (set! totalseconds (list sec min hr d m y))
            (set! totalseconds (map (lambda (n) (string->number n)) totalseconds))
            (set! totalseconds (apply find-seconds totalseconds))
            (push!
              (list (build-path->string dir name)
                    (list totalseconds y m d hr min sec))
              dates_alist))))))

  dates_alist
)

(define (lookup-date-in-alist obj alist)
  (let* ((tuple (assoc (send obj hirespath) alist)))
    (if tuple (cadr tuple) #f)))

(define find-imagedates-after-loading-image-directory
  (lambda (dir imagelist)
    (let ((dates_alist (dir->date-alist dir)))
      (if (not (null? dates_alist))
          (foreach imagelist (lambda (obj)
            (let ((tuple (lookup-date-in-alist obj dates_alist)))
              (if tuple
                ;(replace-else-push-onto-malist! assq 'date.v0 tuple obj alist)
                (set-object-keyval obj 'date.v0 tuple) ))))))
    imagelist))

(define (get-date-tuple obj)
  (get-object-keyval obj 'date.v0))

(push-image-dir-callback  find-imagedates-after-loading-image-directory)


;--- simple utilities for using datestamp ------------------------

; (define (get-object-date-tuple obj)
;   (if (not (is-a? obj image%))
;       #f
;       ;else get date from alist or use file-modification date
;       (let* ((tuple (get-date-tuple obj))
;              (datestamp #f))
;         (if tuple (set! datestamp (car tuple)))
;         (if (not datestamp)
;           (set! datestamp (file-or-directory-modify-seconds (send obj hirespath))))
;     (set-object-keyval obj 'timestamp datestamp)
;         datestamp)))

; (define (get-obj-date obj)
;   (if (is-a? obj group%)
;       (get-obj-date (car (send obj members)))
;       (let* ((keyval (assq 'timestamp (send obj alist)))
;          (datestamp (if keyval (cadr keyval) (get-object-date-tuple obj))))
;     datestamp)))
; ;    (or datestamp 1023456789))))  ;-- default.  could use today's date.

;========
;generalize generate-and-cache process:
(define (get-obj-metadata obj tag generate-fn)
  (let* ((keyval (assq tag (send obj alist)))
     (val (if keyval
          (cadr keyval)
          (generate-fn obj)))) ;
    (if (not keyval) ;cache for next time
    (remote-push! (list tag val) obj alist))
    val))


; this construction forces regexps to be pre-compiled but scoped locally:
(define (match-jhead-line-for-tag line tag) #f)
(let ((photo-datetime-rexp (regexp "^Date/Time[ \t]*: +(....):(..):(..) +(..):(..):(..)$"))
      (focus-dist-rexp     (regexp "^Focus Dist.[ \t]+: +([0-9.]+[a-z]+)$"))
      (file-datetime-rexp  (regexp "^File date[ \t]*: +(....):(..):(..) +(..):(..):(..)$"))
      (aperture-rexp       (regexp "^Aperture[ \t]+: +f/(.*)$")))
  (set! match-jhead-line-for-tag
    (lambda (line tag)
      (cond
        ((eq? tag 'photo-datetime)
         (let ((match (regexp-match photo-datetime-rexp line)))
           (and match
            (infer-complete-date
             (apply list->date
              (map ensure-number (reverse (cdr match))))))))

        ((eq? tag 'file-datetime)
         (let ((match (regexp-match file-datetime-rexp line)))
           (and match
            (infer-complete-date
             (apply list->date
              (map ensure-number (reverse (cdr match))))))))

        ((eq? tag 'fstop)
         (let ((match (regexp-match aperture-rexp line)))
           (and match (ensure-number (cadr match)))))

        ((eq? tag 'focus-dist)
         (let ((match (regexp-match focus-dist-rexp line)))
           (and match (cadr match))))

        (else #f)))))

(define (match-pdfinfo-line-for-tag line tag) #f)
(let ((publish-date-rexp (regexp "^CreationDate:[ \t]*(.+)$")))
  (set! match-pdfinfo-line-for-tag
    (lambda (line tag)
      (cond
        ((eq? tag 'publish-date)
         (let ((match (regexp-match publish-date-rexp line)))
           (and match
            (string->date (cadr match)))))
        (else #f)))))

(define (extract-exif-metadata img tag)
; jhead is a system app which reads .jpgs and extracts any embedded exif data.
; This fn starts a jhead process and reads its results on input-port;
;  if any line matches regexp (above) for tag, returns parsed value, else #f
  (let* ((path (send img hirespath))
     (ok   (and path (file-exists? path)))
     (ports (and ok (process (format "jhead ~s" path))))
     (input-port (and ports (car ports))))
    (and input-port
     (let ((line #f)
           (found #f))
       (while (and (not found)
               (not (eq? eof (cset! line (read-line input-port)))))
          (set! found
            (match-jhead-line-for-tag line tag)))
       (close-input-port input-port)
       (close-output-port (cadr ports))
       (close-input-port (cadddr ports))
       found))))


(define (extract-pdf-metadata obj tag)
  (let* ((path (send obj url))
     (ok (and path (file-exists? path)))
     (ports (and ok (process (format "pdfinfo ~s" path))))
     (input-port (and ports (car ports))))
    (and input-port
     (let ((line #f)
           (found #f))
       (while (and (not found)
               (not (eq? eof (cset! line (read-line input-port)))))
          (set! found
            (match-pdfinfo-line-for-tag line tag)))
       (close-input-port input-port)
       (close-output-port (cadr ports))
       (close-input-port (cadddr ports))
       found))))

(define (get-image-date obj)
  (let ((pair
     (get-obj-metadata
      obj 'photo-datetime
      (lambda (o) (let ((date
                 (extract-exif-metadata o 'photo-datetime)))
            (list (date->seconds date) date))))))
    (and pair (car pair))))

(define (get-pdf-date obj)
  (let ((pair
     (get-obj-metadata
      obj 'publish-date
      (lambda (o) (let ((date
                 (extract-pdf-metadata o 'publish-date)))
            (list (date->seconds date) date))))))
    (and pair (car pair))))

(define (get-image-fstop obj)
  (get-obj-metadata obj 'fstop
            (lambda (o) (extract-exif-metadata o 'fstop))))

(define (get-obj-date obj)
  (cond ((is-a? obj group%)
       (get-obj-date (car (send obj members))))
    ((is-a? obj image%)
       (get-image-date obj))
    ((is-a? obj pdf-portrait%)
       (get-pdf-date obj))
    (else #f)))