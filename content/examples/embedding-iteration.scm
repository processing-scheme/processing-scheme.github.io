;; Embedding Iteration.
;;
;; Embedding "for" structures allows repetition in two dimensions.

(import (processing))

(define (setup)
  (size 640 360)
  (background 0)

  (define grid-size 40)

  (do ((x grid-size (+ x grid-size))) ((> x (- width grid-size)))
    (do ((y grid-size (+ y grid-size))) ((> y (- height grid-size)))
      (no-stroke)
      (fill 255)
      (rect (- x 1) (- y 1) 3 3)
      (stroke 255 100)
      (line x y (/ width 2) (/ height 2)))))
