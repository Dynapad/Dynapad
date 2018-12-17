; This file contains a five-part demo of Dynapad's generalized relation-brushing.
; It can be loaded all at once, but you'll understand things better by reading through
; and executing each part separately.


; ======= Part 0: Run this first ============
; Dramatis personae:
(define one
  (ic
   (make-object text% dynapad)
   (id 1)
   (anchor "nw")
   (position -60 10 1)
   (text "one")
   (font "Times")
   (pen "#ffffff")))

(define two
  (ic
   (make-object text% dynapad)
   (id 2)
   (anchor "nw")
   (position 00 55 1)
   (text "two")
   (font "Times")
   (pen "#ffffff")))

(define three
  (ic
   (make-object text% dynapad)
   (id 3)
   (anchor "nw")
   (position 60 10 1)
   (text "three")
   (font "Times")
   (pen "#ffffff")))

(define four
  (ic
   (make-object text% dynapad)
   (id 4)
   (anchor "nw")
   (position 40 -50 1)
   (text "four")
   (font "Times")
   (pen "#ffffff")))

(define five
  (ic
   (make-object text% dynapad)
   (id 5)
   (anchor "nw")
   (position -40 -50 1)
   (text "five")
   (font "Times")
   (pen "#ffffff")))


; And now, the magic...

; =========== Part 1 ===========

(define divisibility (make-object clique-relation%))
(send divisibility add-objs-with-key (list two four) "evens")
(send divisibility add-objs-with-key (list one three five) "odds")
; Creates a clique-relation and defines its two-clique structure

(send divisibility brush-style halo-brush% #t "green")
; Adds a brush to the relation    (and this^^ #t activates it)

(menuify-relation divisibility "Divisibility...")
; Makes that relation and its brush available to
;  "Display Relations..." popup menu

; =========== Part 2 ============
(send divisibility add-objs-with-key (list two three five) "primes")
(send (send divisibility find-clique "primes")
      brush-style box-brush% #t "purple")
; Creates another clique of all the primes
;  and gives it a private brush.
; Another way of doing this would be to create a new relation, as in:
#|
 (define primes (make-object clique-relation%))
 (send primes add-objs-with-key (list two three five) 'foo) ;<-- only one clique, so
                                                            ;  key could be anything
 (send primes brush-style box-brush% #t "purple")
|#
; These two methods behave the same when both brushes are on,
;  but differently when the primes brush is off:
; In the first case, the primes clique will inherit the shared brush.
; Try it:
#| (send (send divisibility find-clique "primes") brush deactivate)
|#

; ========== Part 3 ============
(define lessthan (make-object directed-relation%))
(send lessthan relate one (list two three four five))
(send lessthan relate two (list three four five))
(send lessthan relate three (list four five))
(send lessthan relate four five)
; Creates an asymmetric/directed relation and defines its structure

(send lessthan brush-style arc-brush%)
; Brush that relation with arrows
(sendf lessthan brush activate)
; An alternative means of activation
(menuify-relation lessthan "Less Than...")
; As before, add this to the menus


; =========== Part 4 ============
(send lessthan antibrush-style antiarc-brush%)
(sendf lessthan antibrush activate)
; We can also brush the "inflow", the opposite relation,
;  by giving the relation an anti-brush


; =========== Part 5 =============
(track-duplicates-of-objs (list one two three four five))
; Duplicate-tracking is a clique-relation like any other;
;  this simply creates a unique clique for each obj
;  which is inherited by any copies
; The default brush for duplicates is a yellow halo, already active
; Copy+paste some objects and observe the result...
