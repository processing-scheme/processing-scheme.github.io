;; Background Image.
;;
;; This example presents the fastest way to load a background image
;; into Processing. To load an image as the background, it must be
;; the same width and height as the program.

(import (processing))

(define bg #f)
(define y 0)

(define (setup)
  (size 640 360)
  ; The background image must be the same size as the parameters
  ; into the size() method. In this program, the size of the image
  ; is 640 x 360 pixels.
  (set! bg (load-image "moonwalk.jpg")))

(define (draw)
  (background bg)

  (stroke 226 204 0)
  (line 0 y width y)

  (set! y (+ y 1))
  (when (> y height) (set! y 0)))
