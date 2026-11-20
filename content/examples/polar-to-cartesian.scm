;; Polar to Cartesian
;; by Daniel Shiffman.
;;
;; Convert a polar coordinate (r,theta) to cartesian (x,y).
;; The calculations are x=r*cos(theta) and y=r*sin(theta).

(import (processing))

(define r 0.0)

; Angle and angular velocity, accleration
(define theta 0.0)
(define theta-vel 0.0)
(define theta-acc 0.0)

(define (setup)
  (size 640 360)

  ; Initialize all values
  (set! r (* height 0.45))
  (set! theta 0)
  (set! theta-vel 0)
  (set! theta-acc 0.0001))

(define (draw)
  (background 0)

  ; Translate the origin point to the center of the screen
  (translate (/ width 2) (/ height 2))

  ; Convert polar to cartesian
  (define x (* r (cos theta)))
  (define y (* r (sin theta)))

  ; Draw the ellipse at the cartesian coordinate
  (ellipse-mode CENTER)
  (no-stroke)
  (fill 200)
  (ellipse x y 32 32)

  ; Apply acceleration and velocity to angle
  (set! theta-vel (+ theta-vel theta-acc))
  (set! theta (+ theta theta-vel)))
