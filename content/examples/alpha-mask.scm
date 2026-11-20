;; Alpha Mask.
;;
;; Loads a "mask" for an image to specify the transparency
;; in different parts of the image. The two images are blended
;; together using the mask() method of PImage.

(import (processing))

(define img #f)
(define img-mask #f)

(define (setup)
  (size 640 360)
  (set! img (load-image "moonwalk.jpg"))
  (set! img-mask (load-image "mask.jpg"))
  (mask img img-mask)
  (image-mode CENTER))

(define (draw)
  (background 0 102 153)
  (image img (/ width 2) (/ height 2))
  (image img mouse-x mouse-y))
