;; Mixture Grid
;; modified from an example by Simon Greenwold.
;;
;; Display a 2D grid of boxes with three different kinds of lights.

(import (processing))

(define (setup) (size 640 360 P3D) (no-stroke))

(define (draw)
  (define-lights)
  (background 0)

  (do ((x 0 (+ x 60))) ((> x width))
    (do ((y 0 (+ y 60))) ((> y height))
      (push-matrix)
      (translate x y)
      (rotate-y (map mouse-x 0 width 0 PI))
      (rotate-x (map mouse-y 0 height 0 PI))
      (box 90)
      (pop-matrix))))

(define (define-lights)
  ; Orange point light on the right
  (point-light 150 100 0 200 -150 0)  ; Position

  ; Blue directional light from the left
  (directional-light 0 102 255 1 0 0)  ; The x-, y-, z-axis direction

  ; Yellow spotlight from the front
  ; Angle, concentration
  (spot-light 255 255 109 0 40 200 0 -0.5 -0.5 (/ PI 2) 2))
