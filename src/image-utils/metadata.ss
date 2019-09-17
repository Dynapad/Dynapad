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
(require (lib "etc.ss"))
(dynaload "parsedate.ss")
(dynaload "smart-labels.ss")


;==================== Parameters =============
;(define (*metadata-parameter-alist* null))

(define nominal-parameter%
  (class object%
    (init-field _name)
    (init-field _access-fn)   ;(fn obj) extracts parameter value
    (init-field _equal-fn)    ;(fn val-a val-b) returns #t if vals are equivalent
    (init-field _label-fn)    ;(fn val-list) returns corresponding list of string labels
    (super-instantiate ())
    (define/public name (get/set _name))
    (define/public access-fn (get/set _access-fn))
    (define/public equal-fn  (get/set _equal-fn))
    (define/public label-fn  (get/set _label-fn))
))

(define ordinal-parameter%
  (class nominal-parameter%
    (init _name _access-fn _label-fn)
    (init-field _compare-fn)  ;(fn val-a val-b) returns -1,0,1 for <,=,>
    (super-instantiate (_name _access-fn
                  (lambda (a b) (zero? (_compare-fn a b)))
                  _label-fn))

    (define/public compare-fn
      (case-lambda
       (() _compare-fn)
       ((fn) (set! _compare-fn fn)
         (send this equal-fn (lambda (a b) (zero? (_compare-fn a b)))))))
))

(define quantitative-parameter%
  (class ordinal-parameter%
    (init _name _access-fn _label-fn _compare-fn)
    (init-field _quantify-fn)  ;(fn val) maps val to number
    (super-instantiate (_name _access-fn _label-fn _compare-fn))

    (define/public quantify-fn (get/set _quantify-fn))
))

; Example:
(define zero-parameter
  (make-object quantitative-parameter%
           'zero-parameter
           (lambda (obj) 0)
           (lambda (nums) (map number->string nums))
           (lambda (a b) cmp-nums)
           (lambda (val) 0)))


;generalize generate-and-cache process:
(define (get-obj-metadata obj tag generate-fn)
  (let* ((keyval (assq tag (send obj alist)))
     (val (if keyval
          (cadr keyval) ;reuse cached val
          (generate-fn obj)))) ;unknown, generate anew
    (when (not keyval) ;cache for next time
    (remote-push! (list tag val) obj alist))
    val))
      

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

(define (get-filedate path)
  (and (file-exists? path)
       (let* ((secs (file-or-directory-modify-seconds path))
          (date (seconds->date secs)))
     (list secs date))))
    
(define (get-image-date obj)
  (let ((pair
     (get-obj-metadata
      obj 'photo-datetime
      (lambda (img) (let ((date
                 (extract-metadata-with-cmd
                  'photo-datetime (send img hirespath) "jhead ")))
              (and date
                   (list (date->seconds date) date)))))))
    pair))

(define (get-image-filedate img)
  (let ((pair
     (get-obj-metadata
      img 'filedate
      (lambda (img) (get-filedate (send img hirespath))))))
    pair))

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

(define (get-pdf-filedate pdf)
  (let ((pair
     (get-obj-metadata
      pdf 'filedate
      (lambda (pdf) (get-filedate (send pdf url))))))
    pair))

(define (pair->date pair)
  (and pair
       (or (cadr pair)
       (and (car pair)
        (seconds->date (car pair))))))

(define (get-image-fstop obj)
  (get-obj-metadata obj 'fstop
            (lambda (img) 
              (and (is-a? img image%)
               (extract-metadata-with-cmd
                'fstop (send img hirespath) "jhead ")))))

(define (get-image-focus-dist obj)
  (get-obj-metadata obj 'focus-dist
            (lambda (img) 
              (and (is-a? img image%)
               (extract-metadata-with-cmd
                'focus-dist (send img hirespath) "jhead ")))))

(define (get-obj-date obj)
  (cond ((is-a? obj group%)
       (get-obj-date (car (send obj members))))
    ((is-a? obj image%)
       (get-image-date obj))
    ((is-a? obj pdf-portrait%)
       (get-pdf-date obj))
    (else #f)))
(define (get-obj-filedate obj)
  (cond ((is-a? obj group%)
       (get-obj-filedate (car (send obj members))))
    ((is-a? obj image%)
       (get-image-filedate obj))
    ((is-a? obj pdf-portrait%)
       (get-pdf-filedate obj))
    (else #f)))

;====== Set alist modifiers so obj can write metadata vals ===
(alist-filters 'add (make-alist-modifier-function-general
             (lambda (key) (memq key (list 'filedate 'photo-datetime 'publish-date)))
             (lambda (vals) (let* ((pair (car vals)) ;assumes only one val
                       (sec (car pair)))
                      (list (list sec #f))))))

; ====== Metadata menu =========
(define *show-metadata-time?* #t)
(define (show-metadata-time?) *show-metadata-time?*)
(define *pad-date-format* 'american)
(define pad-date-format
;(date-display-format ...) needs this wrapper for some reason
;  or menu doesnt refresh  
  (case-lambda
   (() *pad-date-format*)
   ((frmt) (set! *pad-date-format* frmt)
           (date-display-format frmt))))

                         
(define-macro (time-format-item format)
  `(add-checkable-menu-item sb ,(symbol->string format)
       (lambda (i) 
         (pad-date-format ',format))
       (eq? ',format (pad-date-format))))

(define (make-submenu-DateFormat mb obj)
  (let* ((sb (add-submenu mb "Date Format...")))
    (add-checkable-menu-item sb "Show time"
       (lambda (i) (set! *show-metadata-time?*
             (not (show-metadata-time?))))
       (show-metadata-time?))
    (add-menu-separator sb) ;------------------------------
    (time-format-item american)
    (time-format-item chinese)
    (time-format-item german)
    (time-format-item indian)
    (time-format-item irish)
    (time-format-item julian)
    (time-format-item iso-8601)
    (time-format-item rfc2822)
    ))



;====== Parameters =======
; template:
; (def <parameter-name>                   <-_
;  (make-object quantitative-parameter%      \
;               '<parameter-name>        <-- same <parameter-name>
;               extract-value-fn
;               values->labels-fn
;               compare-vals-fn
;               quantify-val-fn

(define date-created-parameter
  (make-object quantitative-parameter%
           'date-created-parameter
           get-obj-date
           (lambda (prs) (dates->labels
                  (map (lambda (pr)
                     (or (cadr pr) (seconds->date (car pr))))
                   prs)))
           (lambda (pr1 pr2) (safe-cmp-nums (car pr1) (car pr2)))
           (lambda (pr) (ensure-number (car pr)))))

(define date-acquired-parameter
  (make-object quantitative-parameter%
           'date-acquired-parameter
           get-obj-filedate
           (lambda (prs) (dates->labels
                  (map (lambda (pr)
                     (or (cadr pr) (seconds->date (car pr))))
                   prs)))
           (lambda (pr1 pr2) (safe-cmp-nums (car pr1) (car pr2)))
           (lambda (pr) (ensure-number (car pr)))))


(define (extract-image-filename obj)
  (if (not (is-a? obj image%))
      #f
      (let* ((filename (send obj hirespath))
         (matches  (regexp-match gifjpg_rexp filename))
         (base     (caddr matches)))
    (replace-else-push-onto-malist! assq 'filename (list base) obj alist)
    base)))

(define (get-image-filename obj)
      (let* ((filename (assq 'filename (send obj alist))))
    (when (and (not filename)
         (extract-image-filename obj))
        (set! filename (assq 'filename (send obj alist))))
    (cadr filename)))

(define filename-parameter
  (make-object quantitative-parameter%
           'filename-parameter
           get-image-filename
           list
           safe-cmp-strs
           (lambda (str) (if str
                 (char->integer (car (string->list str)))
                 ""))
                 ))

(define focus-dist-parameter
  (make-object quantitative-parameter%
           'focus-dist-parameter
           get-image-focus-dist
           (lambda (nums) (list (map number->string nums)))
           safe-cmp-nums
           ensure-number))

(define fstop-parameter
  (make-object quantitative-parameter%
           'fstop-parameter
           get-image-fstop
           (lambda (nums) (list (map (lambda (num) (format "f/~a" num)) nums)))
           safe-cmp-nums
           ensure-number))

;  Date labels

(define (subdivide-date date) (reverse (date->list date)))


#| DEAD CODE:
(define (expand-levels-of-description subdivide-val-fn val-list)
  (map (lambda (val) (subdivide-val-fn val)) val-list))

(define (count-adjacent-inequalities lst eq-fn . last-val-arg)
; count # of times adjacent values in a list are unequal (via eq-fn)
  (cond ((null? lst) 0)
    ((null? last-val-arg) (count-adjacent-inequalities (cdr lst) eq-fn (car lst)))
    (else (let* ((last-val (car last-val-arg))
             (this-val (car lst))
             (diff (if (eq-fn last-val this-val) 0 1)))
        (+ diff (count-adjacent-inequalities (cdr lst) eq-fn this-val))))))

(define (count-adjacent-inequalities-of-sublists-at-level sublists lvl eq-fn)
; EX: sublists [(1 2 3 4 5)
;               (3 3 3 3 3)
;               (3 4 3 4 3)]
; result:   (lvl 0 ----> 4)
;       -------->1 2 0 2 1
  (let ((vals-at-lvl (map (lambda (sublist) (list-ref sublist lvl)) sublists)))
    (count-adjacent-inequalities vals-at-lvl eq-fn)))


(define (inequality-vector-of-sublists-at-level sublists lvl eq-fn)
  ;returns list of: 0 if (car sublist) is eq-fn last item, 1 otherwise


; (subdivide-val-fn val) -> list of subvalues hi->lo
; e.g. (subdivide-date date)-> (year month day hour min sec)

(define (choose-level-of-description sublists eq-fn ineq-ratio-fn)
  (let* ((len (length sublists))
;     (num-ineqs (map (lambda (lvl)
;               (count-adjacent-inequalities-of-sublists-at-level
;                expanded-vals-list lvl eq-fn))
;             (counting-list len)))
     (found (first-valid (counting-list len)
             (lambda (lvl)
            (let* ((ineqs (count-adjacent-inequalities-of-sublists-at-level
                       sublists lvl eq-fn))
                   (ineq-ratio (/ ineqs len)))
              (ineq-ratio-fn ineq-ratio))))))
    (and found (caar found))))

(define (choose-desc-lvl-range-for-dates datelist)
  (let* ((sublists (map subdivide-date datelist))
     (top-lvl-fn (lambda (ratio) (> ratio 0)))
;     (btm-lvl-fn (lambda (ratio) (> ratio .5)))
     (top-lvl (choose-level-of-description sublists eq? top-lvl-fn))
     (btm-lvl (min (+ top-lvl 1) 5)))
;     (btm-lvl (choose-level-of-description sublists eq? btm-lvl-fn)))
    (list (or top-lvl 5) (or btm-lvl 5))))

(define (convert-dates-to-labels datelist)
  (date-display-format 'iso-8601)
  (let* ((range (choose-desc-lvl-range-for-dates datelist))
     (from (car range))
     (to (max (cadr range) from))
     (template (map (lambda (n) (and (>= n from) (<= n to)))
            (counting-list 6))) ;--> e.g. (#f #f #t #t #f #f) for range (2 3)
     (shared-template
               (map (lambda (n) (< n from)) (counting-list 6)))
     (raw-strs (map (lambda (d) (date->string d #t)) datelist))
     (trim-strs (map (lambda (str)
               (prune-date-string template
                          *iso-date-format-rexp*
                          str)) raw-strs)))
    (list
     trim-strs
     (if (null? raw-strs)
     ""
     (prune-date-string shared-template *iso-date-format-rexp* (car raw-strs)))
     )))
|#
