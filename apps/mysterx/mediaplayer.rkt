(require (lib "mysterx.ss" "mysterx"))

;make a browser (ie without controls) hosts the object
(define mx (make-object mx-browser%))

;get the current document (empty page)
(define mxdoc (send mx current-document))

;insert an object
(define mp (send mxdoc insert-object-from-progid "MediaPlayer.MediaPlayer" 300 300))

;another way to insert, then get the object
;(send mxdoc insert-html (progid->html "MediaPlayer.MediaPlayer" 300 300))
;(define mp (car (send mxdoc objects)))

;play it, sam
;(com-invoke mp "Open" "E:/stanonik/clock.avi")

;what methods
;(com-methods mp)

;what properties
;(com-get-properties mp)

;what events
;(com-events)

;get a property
;(com-get-property mp "CurrentPosition")

;set a property
;(com-set-property! mp "CurrentPosition" 3.3)
