;; Milliseconds.
;;
;; A millisecond is 1/1000 of a second.
;; Processing keeps track of the number of milliseconds a program has run.
;; By modifying this number with the modulo(%) operator,
;; different patterns in time are created.

(import (processing))

(define scale-v 0.0)

(define (setup)
  (size 640 360)
  (no-stroke)
  (set! scale-v (/ width 20)))

(define (draw)
  (do ((i 0 (+ i 1))) ((>= i scale-v))
    (color-mode RGB (* (+ i 1) scale-v 10))
    (fill (modulo (millis) (* (+ i 1) scale-v 10)))
    (rect (* i scale-v) 0 scale-v height)))
