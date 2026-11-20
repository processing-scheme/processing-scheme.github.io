;; Radial Gradient.
;;
;; Draws a series of concentric circles to create a gradient
;; from one color to another.

(import (processing))

(define dim 0)

(define (setup)
  (size 640 360)
  (set! dim (quotient width 2))
  (background 0)
  (color-mode HSB 360 100 100)
  (no-stroke)
  (ellipse-mode RADIUS)
  (frame-rate 1))

(define (draw)
  (background 0)
  (do ((x 0 (+ x dim))) ((> x width))
    (draw-gradient x (/ height 2))))

(define (draw-gradient x y)
  (define radius (quotient dim 2))
  (define h (random 0 360))
  (do ((r radius (- r 1))) ((<= r 0))
    (fill h 90 90)
    (ellipse x y r r)
    (set! h (modulo (+ h 1) 360))))
