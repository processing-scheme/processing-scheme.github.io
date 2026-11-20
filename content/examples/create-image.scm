;; Create Image.
;;
;; The createImage() function provides a fresh buffer of pixels to play with.
;; This example creates an image gradient.

(import (processing))

(define img #f)

(define (setup)
  (size 640 360)
  (set! img (create-image 230 230 ARGB))
  (do ((i 0 (+ i 1))) ((>= i (vector-length (pixels img))))
    (define a (map i 0 (vector-length (pixels img)) 255 0))
    (vector-set! (pixels img) i (color 0 153 204 a))))

(define (draw)
  (background 0)
  (image img 90 80)
  (image img (- mouse-x (/ (width img) 2)) (- mouse-y (/ (height img) 2))))
