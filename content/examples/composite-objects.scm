;; Composite Objects
;;
;; An object can include several other objects. Creating such composite objects
;; is a good way to use the principles of modularity and build higher levels of
;; abstraction within a program.

(import (processing))

(define-record-type egg (make-egg-record x y tilt angle scalar range)
  egg?
  (x egg-x set-egg-x!)
  (y egg-y set-egg-y!)
  (tilt egg-tilt set-egg-tilt!)
  (angle egg-angle set-egg-angle!)
  (scalar egg-scalar set-egg-scalar!)
  (range egg-range set-egg-range!))

; X-coordinate, y-coordinate
; Left and right angle offset
; Used to define the tilt
; Height of the egg
; Constructor
(define (make-egg xpos ypos r s)
  (let ((self (make-egg-record 0.0 0.0 0.0 0.0 0.0 0.0)))
    (set-egg-x! self xpos)
    (set-egg-y! self ypos)
    (set-egg-tilt! self 0)
    (set-egg-scalar! self (/ s 100.0))
    (set-egg-range! self r)
    self))

(define (egg-wobble self)
  (set-egg-tilt! self (/ (cos (egg-angle self)) (egg-range self)))
  (set-egg-angle! self (+ (egg-angle self) 0.1))
  ; print (angle + "-");
  )

(define (egg-display self)
  (no-stroke)
  (fill 255)
  (push-matrix)
  (translate (egg-x self) (egg-y self))
  (rotate (egg-tilt self))
  (scale (egg-scalar self))
  (begin-shape)
  (vertex 0 -100)
  (bezier-vertex 25 -100 40 -65 40 -40)
  (bezier-vertex 40 -15 25 0 0 0)
  (bezier-vertex -25 0 -40 -15 -40 -40)
  (bezier-vertex -40 -65 -25 -100 0 -100)
  (end-shape)
  (pop-matrix))

(define-record-type egg-ring (make-egg-ring-record ovoid circle)
  egg-ring?
  (ovoid egg-ring-ovoid set-egg-ring-ovoid!)
  (circle egg-ring-circle set-egg-ring-circle!))

(define (make-egg-ring x y t sp)
  (let ((self (make-egg-ring-record #f #f)))
    (set-egg-ring-circle! self (make-ring))
    (set-egg-ring-ovoid! self (make-egg x y t sp))
    (ring-start (egg-ring-circle self) x (- y (/ sp 2)))
    self))

(define (egg-ring-transmit self)
  (egg-wobble (egg-ring-ovoid self))
  (egg-display (egg-ring-ovoid self))
  (ring-grow (egg-ring-circle self))
  (ring-display (egg-ring-circle self))
  (when (not (ring-on (egg-ring-circle self))) (set-ring-on! (egg-ring-circle self) #t)))

(define-record-type ring (make-ring-record x y diameter on)
  ring?
  (x ring-x set-ring-x!)
  (y ring-y set-ring-y!)
  (diameter ring-diameter set-ring-diameter!)
  (on ring-on set-ring-on!))

; X-coordinate, y-coordinate
; Diameter of the ring
; Turns the display on and off
(define (ring-start self xpos ypos)
  (set-ring-x! self xpos)
  (set-ring-y! self ypos)
  (set-ring-on! self #t)
  (set-ring-diameter! self 1))

(define (ring-grow self)
  (when (eqv? (ring-on self) #t)
    (set-ring-diameter! self (+ (ring-diameter self) 0.5))
    (when (> (ring-diameter self) (* width 2)) (set-ring-diameter! self 0.0))))

(define (ring-display self)
  (when (eqv? (ring-on self) #t)
    (no-fill)
    (stroke-weight 4)
    (stroke 155 153)
    (ellipse (ring-x self) (ring-y self) (ring-diameter self) (ring-diameter self))))

(define (make-ring)
  (let ((self (make-ring-record 0.0 0.0 0.0 #f))) (set-ring-on! self #f) self))

(define er1 #f)
(define er2 #f)

(define (setup)
  (size 640 360)
  (set! er1 (make-egg-ring (* width 0.45) (* height 0.5) 2 120))
  (set! er2 (make-egg-ring (* width 0.65) (* height 0.8) 10 180)))

(define (draw)
  (background 0)
  (egg-ring-transmit er1)
  (egg-ring-transmit er2))
