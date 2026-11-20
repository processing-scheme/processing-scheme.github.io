;; Primitives 3D.
;;
;; Placing mathematically 3D objects in synthetic space.
;; The lights() method reveals their imagined dimension.
;; The box() and sphere() functions each have one parameter
;; which is used to specify their size. These shapes are
;; positioned using the translate() function.

(import (processing))

(define (setup)
  (size 640 360 P3D)
  (background 0)
  (lights)

  (no-stroke)
  (push-matrix)
  (translate 130 (/ height 2) 0)
  (rotate-y 1.25)
  (rotate-x -0.4)
  (box 100)
  (pop-matrix)

  (no-fill)
  (stroke 255)
  (push-matrix)
  (translate 500 (* height 0.35) -200)
  (sphere 280)
  (pop-matrix))
