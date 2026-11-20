;; Linear Interpolation.
;;
;; Move the mouse across the screen and the symbol will follow.
;; Between drawing each frame of the animation, the ellipse moves
;; part of the distance (0.05) from its current position toward
;; the cursor using the lerp() function.

(import (processing))

(define x 0.0)
(define y 0.0)

(define (setup) (size 640 360) (no-stroke))

(define (draw)
  (background 51)

  ; lerp() calculates a number between two numbers at a specific increment.
  ; The amt parameter is the amount to interpolate between the two values
  ; where 0.0 equal to the first point, 0.1 is very near the first point, 0.5
  ; is half-way in between, etc.

  ; Here we are moving 5% of the way to the mouse location each frame
  (set! x (lerp x mouse-x 0.05))
  (set! y (lerp y mouse-y 0.05))

  (fill 255)
  (stroke 255)
  (ellipse x y 66 66))
