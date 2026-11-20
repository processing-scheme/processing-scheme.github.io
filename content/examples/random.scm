;; Random.
;;
;; Random numbers create the basis of this image.
;; Each time the program is loaded the result is different.

(import (processing))

(define (setup)
  (size 640 360)
  (background 0)
  (stroke-weight 20)
  (frame-rate 2))

(define (draw)
  (do ((i 0 (+ i 1))) ((>= i width))
    (define r (random 255))
    (stroke r)
    (line i 0 i height)))
