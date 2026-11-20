;; Double Random
;; by Ira Greenberg.
;;
;; Using two random() calls and the point() function
;; to create an irregular sawtooth line.

(import (processing))

(define total-pts 300)
(define steps (+ total-pts 1))

(define (setup) (size 640 360) (stroke 255) (frame-rate 1))

(define (draw)
  (background 0)
  (define rand 0)
  (do ((i 1 (+ i 1))) ((>= i steps))
    (point (* (/ width steps) i) (+ (/ height 2) (random (- rand) rand)))
    (set! rand (+ rand (random -5 5)))))
