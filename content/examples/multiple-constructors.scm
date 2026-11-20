;; Multiple constructors
;;
;; A class can have multiple constructors that assign the fields in different ways.
;; Sometimes it's beneficial to specify every aspect of an object's data by assigning
;; parameters to the fields, but other times it might be appropriate to define only
;; one or a few.

(import (processing))

(define-record-type spot (make-spot-record x y radius)
  spot?
  (x spot-x set-spot-x!)
  (y spot-y set-spot-y!)
  (radius spot-radius set-spot-radius!))

; First version of the Spot constructor;
; the fields are assigned default values
(define (make-spot0)
  (let ((self (make-spot-record 0.0 0.0 0.0)))
    (set-spot-radius! self 40)
    (set-spot-x! self (* width 0.25))
    (set-spot-y! self (* height 0.5))
    self))

; Second version of the Spot constructor;
; the fields are assigned with parameters
(define (make-spot xpos ypos r)
  (let ((self (make-spot-record 0.0 0.0 0.0)))
    (set-spot-x! self xpos)
    (set-spot-y! self ypos)
    (set-spot-radius! self r)
    self))

(define (spot-display self)
  (ellipse (spot-x self)
           (spot-y self)
           (* (spot-radius self) 2)
           (* (spot-radius self) 2)))

(define sp1 #f)
(define sp2 #f)

(define (setup)
  (size 640 360)
  (background 204)
  (no-loop)
  ; Run the constructor without parameters
  (set! sp1 (make-spot0))
  ; Run the constructor with three parameters
  (set! sp2 (make-spot (* width 0.5) (* height 0.5) 120)))

(define (draw) (spot-display sp1) (spot-display sp2))
