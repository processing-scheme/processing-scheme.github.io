;; Acceleration with Vectors
;; by Daniel Shiffman.
;;
;; Demonstration of the basics of motion with vector.
;; A "Mover" object stores location, velocity, and
;; acceleration as vectors. The motion is controlled by
;; affecting the acceleration (in this case towards the mouse).
; A Mover object

(import (processing))

;; Acceleration with Vectors
;; by Daniel Shiffman.
;;
;; Demonstration of the basics of motion with vector.
;; A "Mover" object stores location, velocity, and acceleration as vectors
;; The motion is controlled by affecting the acceleration (in this case towards the mouse)

(define-record-type mover (make-mover-record location velocity acceleration topspeed)
  mover?
  (location mover-location set-mover-location!)
  (velocity mover-velocity set-mover-velocity!)
  (acceleration mover-acceleration set-mover-acceleration!)
  (topspeed mover-topspeed set-mover-topspeed!))

; The Mover tracks location, velocity, and acceleration
; The Mover's maximum speed
(define (make-mover)
  (let ((self (make-mover-record #f #f #f 0.0)))
    ; Start in the center
    (set-mover-location! self (PVector (/ width 2) (/ height 2)))
    (set-mover-velocity! self (PVector 0 0))
    (set-mover-topspeed! self 5)
    self))

(define (mover-update self)
  ; Compute a vector that points from location to mouse
  (define mouse (PVector mouse-x mouse-y))
  (define acceleration (sub (copy mouse) (mover-location self)))
  ; Set magnitude of acceleration
  (set-mag acceleration 0.2)

  ; Velocity changes according to acceleration
  (add (mover-velocity self) acceleration)
  ; Limit the velocity by topspeed
  (limit (mover-velocity self) (mover-topspeed self))
  ; Location changes by velocity
  (add (mover-location self) (mover-velocity self)))

(define (mover-display self)
  (stroke 255)
  (stroke-weight 2)
  (fill 127)
  (ellipse (vec-x (mover-location self)) (vec-y (mover-location self)) 48 48))

(define mover #f)

(define (setup) (size 640 360) (set! mover (make-mover)))

(define (draw)
  (background 0)

  ; Update the location
  (mover-update mover)
  ; Display the Mover
  (mover-display mover))
