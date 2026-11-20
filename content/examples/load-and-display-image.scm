;; Load and Display
;;
;; Images can be loaded and displayed to the screen at their actual size
;; or any other size.

(import (processing))

(define img #f)  ; Declare variable "a" of type PImage

(define (setup)
  (size 640 360)
  ; The image file must be in the data folder of the current sketch
  ; to load successfully
  ; Load the image into the program
  (set! img (load-image "moonwalk.jpg")))

(define (draw)
  ; Displays the image at its actual size at point (0,0)
  (image img 0 0)
  ; Displays the image at point (0, height/2) at half of its size
  (image img 0 (/ height 2) (/ (width img) 2) (/ (height img) 2)))
