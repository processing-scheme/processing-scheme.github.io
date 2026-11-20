;; Recursion.
;;
;; A demonstration of recursion, which means functions call themselves.
;; Notice how the drawCircle() function calls itself at the end of its block.
;; It continues to do this until the variable "level" is equal to 1.

(import (processing))

(define (setup) (size 640 360) (no-stroke) (no-loop))

(define (draw) (draw-circle (/ width 2) 280 6))

(define (draw-circle x radius level)
  (define tt (/ (* 126 level) 4.0))
  (fill tt)
  (ellipse x (/ height 2) (* radius 2) (* radius 2))
  (when (> level 1)
    (set! level (- level 1))
    (draw-circle (- x (/ radius 2)) (/ radius 2) level)
    (draw-circle (+ x (/ radius 2)) (/ radius 2) level)))
