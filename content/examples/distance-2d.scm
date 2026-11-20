;; Distance 2D.
;;
;; Move the mouse across the image to obscure and reveal the matrix.
;; Measures the distance from the mouse to each square and sets the
;; size proportionally.

(import (processing))

(define max-distance 0.0)

(define (setup)
  (size 640 360)
  (no-stroke)
  (set! max-distance (dist 0 0 width height)))

(define (draw)
  (background 0)

  (do ((i 0 (+ i 20))) ((> i width))
    (do ((j 0 (+ j 20))) ((> j height))
      (define size-v (dist mouse-x mouse-y i j))
      (set! size-v (* (/ size-v max-distance) 66))
      (ellipse i j size-v size-v))))
