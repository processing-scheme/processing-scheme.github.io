;; Save One Image
;;
;; The save() function allows you to save an image from the
;; display window. In this example, save() is run when a mouse
;; button is pressed. The image "line.tif" is saved to the
;; same folder as the sketch's program file.

(import (processing))

(define (setup) (size 640 360))

(define (draw)
  (background 204)
  (line 0 0 mouse-x height)
  (line width 0 0 mouse-y))

(define (mouse-pressed) (save "line.tif"))
