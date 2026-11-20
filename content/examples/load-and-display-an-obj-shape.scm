;; Load and Display an OBJ Shape.
;;
;; The loadShape() command is used to read simple SVG (Scalable Vector Graphics)
;; files and OBJ (Object) files into a Processing sketch. This example loads an
;; OBJ file of a rocket and displays it to the screen.

(import (processing))

(define rocket #f)

(define ry 0.0)

(define (setup)
  (size 640 360 P3D)

  (set! rocket (load-shape "rocket.obj")))

(define (draw)
  (background 0)
  (lights)

  (translate (/ width 2) (+ (/ height 2) 100) -200)
  (rotate-z PI)
  (rotate-y ry)
  (shape rocket)

  (set! ry (+ ry 0.02)))
