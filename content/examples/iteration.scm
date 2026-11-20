;; Iteration.
;;
;; Iteration with a "for" structure to construct repetitive forms.

(import (processing))

(define (setup)
  (define y 0)
  (define num 14)

  (size 640 360)
  (background 102)
  (no-stroke)

  ; White bars
  (fill 255)
  (set! y 60)
  (do ((i 0 (+ i 1))) ((>= i (/ num 3)))
    (rect 50 y 475 10)
    (set! y (+ y 20)))

  ; Gray bars
  (fill 51)
  (set! y 40)
  (do ((i 0 (+ i 1))) ((>= i num))
    (rect 405 y 30 10)
    (set! y (+ y 20)))
  (set! y 50)
  (do ((i 0 (+ i 1))) ((>= i num))
    (rect 425 y 30 10)
    (set! y (+ y 20)))

  ; Thin lines
  (set! y 45)
  (fill 0)
  (do ((i 0 (+ i 1))) ((>= i (- num 1)))
    (rect 120 y 40 1)
    (set! y (+ y 20))))
