#lang racket/base
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

(require racket/class
         racket/date
         racket/list
         mzlib/etc
         dynapad/base
         dynapad/image
         (only-in dynapad/pdf
                  pdf-portrait%
                  )
         (only-in dynapad/misc/misc
                  ensure-number)
         dynapad/misc/misc
         dynapad/misc/alist
         dynapad/misc/tools-cmp
         dynapad/image-utils/smart-labels
         dynapad/image-utils/metadata-base
         )

(provide get-image-date
         get-image-filedate
         get-image-focus-dist
         get-image-fstop
         )

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
