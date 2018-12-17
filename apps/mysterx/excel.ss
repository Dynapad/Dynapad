(require (lib "mysterx.ss" "mysterx"))

(define excel (cci/progid "Excel.Application"))
(com-set-property! excel "Visible" #t)

(define workbooks (com-get-property excel "Workbooks"))
(define book (com-invoke workbooks "Open" "E:/stanonik/clock.xls"))
(define sheet (com-get-property book "ActiveSheet"))

;(define book (com-invoke workbooks "Add"))

;(define range (com-get-property sheet '("Range" "A1")))
;(define value (com-get-property range "Value"))
;or
;(define value (com-property-get sheet '("Range" "A1") "Value"))
;(com-set-property! sheet '("Range" "A1") 2.3)

;(com-invoke book "Save")
;(com-invoke book "SaveAs" "E:/stanonik/blap.xls")
;(com-invoke book "Close")
