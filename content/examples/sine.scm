;; Sine.
;;
;; Smoothly scaling size with the sin() function.

(import (processing))

(define diameter 0.0)
(define angle 0)

(define (setup)
  (size 640 360)
  (set! diameter (- height 10))
  (no-stroke)
  (fill 255 204 0))

(define (draw)
  (background 0)

  (define d1 (+ 10 (/ (* (sin angle) diameter) 2) (/ diameter 2)))
  (define d2 (+ 10 (/ (* (sin (+ angle (/ PI 2))) diameter) 2) (/ diameter 2)))
  (define d3 (+ 10 (/ (* (sin (+ angle PI)) diameter) 2) (/ diameter 2)))

  (ellipse 0 (/ height 2) d1 d1)
  (ellipse (/ width 2) (/ height 2) d2 d2)
  (ellipse width (/ height 2) d3 d3)

  (set! angle (+ angle 0.02)))
