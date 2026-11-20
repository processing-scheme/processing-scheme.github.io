;; Scale
;; by Denis Grutze.
;;
;; Paramenters for the scale() function are values specified
;; as decimal percentages. For example, the method call scale(2.0)
;; will increase the dimension of the shape by 200 percent.
;; Objects always scale from the origin.

(import (processing))

(define a 0.0)
(define s 0.0)

(define (setup)
  (size 640 360)
  (no-stroke)
  (rect-mode CENTER)
  (frame-rate 30))

(define (draw)
  (background 102)

  (set! a (+ a 0.04))
  (set! s (* (cos a) 2))

  (translate (/ width 2) (/ height 2))
  (scale s)
  (fill 51)
  (rect 0 0 50 50)

  (translate 75 0)
  (fill 255)
  (scale s)
  (rect 0 0 50 50))
