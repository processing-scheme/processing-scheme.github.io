;; Mouse Press.
;;
;; Move the mouse to position the shape.
;; Press the mouse button to invert the color.

(import (processing))

(define (setup)
  (size 640 360)
  (no-smooth)
  (fill 126)
  (background 102))

(define (draw)
  (if mouse-is-pressed (stroke 255) (stroke 0))
  (line (- mouse-x 66) mouse-y (+ mouse-x 66) mouse-y)
  (line mouse-x (- mouse-y 66) mouse-x (+ mouse-y 66)))
