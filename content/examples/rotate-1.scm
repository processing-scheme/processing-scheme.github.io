;; Rotate 1.
;;
;; Rotating simultaneously in the X and Y axis.
;; Transformation functions such as rotate() are additive.
;; Successively calling rotate(1.0) and rotate(2.0)
;; is equivalent to calling rotate(3.0).

(import (processing))

(define a 0.0)
(define r-size 0.0)  ; rectangle size

(define (setup)
  (size 640 360 P3D)
  (set! r-size (/ width 6))
  (no-stroke)
  (fill 204 204))

(define (draw)
  (background 126)

  (set! a (+ a 0.005))
  (when (> a TWO-PI) (set! a 0.0))

  (translate (/ width 2) (/ height 2))

  (rotate-x a)
  (rotate-y (* a 2.0))
  (fill 255)
  (rect (- r-size) (- r-size) (* r-size 2) (* r-size 2))

  (rotate-x (* a 1.001))
  (rotate-y (* a 2.002))
  (fill 0)
  (rect (- r-size) (- r-size) (* r-size 2) (* r-size 2)))
