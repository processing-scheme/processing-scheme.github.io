;; Linear Motion.
;;
;; Changing a variable to create a moving line.
;; When the line moves off the edge of the window,
;; the variable is set to 0, which places the line
;; back at the bottom of the screen.

(import (processing))

(define a 0.0)

(define (setup)
  (size 640 360)
  (stroke 255)
  (set! a (/ height 2)))

(define (draw)
  (background 51)
  (line 0 a width a)
  (set! a (- a 0.5))
  (when (< a 0) (set! a height)))
