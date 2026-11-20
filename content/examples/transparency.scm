;; Transparency.
;;
;; Move the pointer left and right across the image to change
;; its position. This program overlays one image over another
;; by modifying the alpha value of the image with the tint() function.

(import (processing))

(define img #f)
(define offset 0)
(define easing 0.05)

(define (setup)
  (size 640 360)
  ; Load an image into the program
  (set! img (load-image "moonwalk.jpg")))

(define (draw)
  (image img 0 0)  ; Display at full opacity
  (define dx (- mouse-x (/ (width img) 2) offset))
  (set! offset (+ offset (* dx easing)))
  (tint 255 127)  ; Display at half opacity
  (image img offset 0))
