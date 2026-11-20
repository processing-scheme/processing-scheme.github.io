;; Vector
;; by Daniel Shiffman.
;;
;; Demonstration of some basic vector math: subtraction,
;; normalization, scaling. Normalizing a vector sets
;; its length to 1.

(import (processing))

(define (setup) (size 640 360))

(define (draw)
  (background 0)

  ; A vector that points to the mouse location
  (define mouse (PVector mouse-x mouse-y))

  ; A vector that points to the center of the window
  (define center (PVector (/ width 2) (/ height 2)))

  ; Subtract center from mouse which results in a
  ; vector that points from center to mouse
  (sub mouse center)

  ; Normalize the vector
  (normalize mouse)

  ; Multiply its length by 150 (Scaling its length)
  (mult mouse 150)

  (translate (/ width 2) (/ height 2))
  ; Draw the resulting vector
  (stroke 255)
  (stroke-weight 4)
  (line 0 0 (vec-x mouse) (vec-y mouse)))
