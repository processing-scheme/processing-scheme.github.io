;; Pointillism
;; by Daniel Shiffman.
;;
;; Mouse horizontal location controls size of dots.
;; Creates a simple pointillist effect using ellipses colored
;; according to pixels in an image.

(import (processing))

(define img #f)
(define small-point 0)
(define large-point 0)

(define (setup)
  (size 640 360)
  (set! img (load-image "moonwalk.jpg"))
  (set! small-point 4)
  (set! large-point 40)
  (image-mode CENTER)
  (no-stroke)
  (background 255))

(define (draw)
  (define pointillize (map mouse-x 0 width small-point large-point))
  (define x (int (random (width img))))
  (define y (int (random (height img))))
  (define pix (get img x y))
  (fill pix 128)
  (ellipse x y pointillize pointillize))
