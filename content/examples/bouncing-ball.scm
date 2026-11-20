;; Bouncing Ball with Vectors
;; by Daniel Shiffman.
;;
;; Demonstration of using vectors to control motion
;; of a body. This example is not object-oriented
;; See AccelerationWithVectors for an example of how
;; to simulate motion using vectors in an object.

(import (processing))

(define location #f)  ; Location of shape
(define velocity #f)  ; Velocity of shape
(define gravity #f)  ; Gravity acts at the shape's acceleration

(define (setup)
  (size 640 360)
  (set! location (PVector 100 100))
  (set! velocity (PVector 1.5 2.1))
  (set! gravity (PVector 0 0.2)))

(define (draw)
  (background 0)

  ; Add velocity to the location.
  (add location velocity)
  ; Add gravity to velocity
  (add velocity gravity)

  ; Bounce off edges
  (when (or (> (vec-x location) width) (< (vec-x location) 0))
    (set-vec-x! velocity (* (vec-x velocity) -1)))
  (when (> (vec-y location) height)
    ; We're reducing velocity ever so slightly
    ; when it hits the bottom of the window
    (set-vec-y! velocity (* (vec-y velocity) -0.95))
    (set-vec-y! location height))

  ; Display circle at location vector
  (stroke 255)
  (stroke-weight 2)
  (fill 127)
  (ellipse (vec-x location) (vec-y location) 48 48))
