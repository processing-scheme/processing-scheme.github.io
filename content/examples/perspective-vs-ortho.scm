;; Perspective vs. Ortho
;;
;; Move the mouse left to right to change the "far"
;; parameter for the perspective() and ortho() functions.
;; This parameter sets the maximum distance from the
;; origin away from the viewer and will clip the geometry.
;; Click a mouse button to switch between the perspective and
;; orthographic projections.

(import (processing))

(define show-perspective #f)

(define (setup)
  (size 600 360 P3D)
  (no-fill)
  (fill 255)
  (no-stroke))

(define (draw)
  (lights)
  (background 0)
  (define far (map mouse-x 0 width 120 400))
  (if (eqv? show-perspective #t)
    (perspective (/ PI 3.0) (/ (float width) (float height)) 10 far)
    (ortho (/ (- width) 2.0) (/ width 2.0) (/ (- height) 2.0) (/ height 2.0) 10 far))
  (translate (/ width 2) (/ height 2) 0)
  (rotate-x (/ (- PI) 6))
  (rotate-y (/ PI 3))
  (box 180))

(define (mouse-pressed) (set! show-perspective (not show-perspective)))
