;; Loading Images.
;;
;; Processing applications can load images from the network.

(import (processing))

(define img #f)

(define (setup)
  (size 640 360)
  (set! img (load-image "https://processing.org/img/processing-web.png"))
  (no-loop))

(define (draw)
  (background 0)
  (when img (do ((i 0 (+ i 1))) ((>= i 5)) (image img 0 (* (height img) i)))))
