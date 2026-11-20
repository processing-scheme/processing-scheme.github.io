;; Reflection
;; by Simon Greenwold.
;;
;; Vary the specular reflection component of a material
;; with the horizontal position of the mouse.

(import (processing))

(define (setup)
  (size 640 360 P3D)
  (no-stroke)
  (color-mode RGB 1)
  (fill 0.4))

(define (draw)
  (background 0)
  (translate (/ width 2) (/ height 2))
  ; Set the specular color of lights that follow
  (light-specular 1 1 1)
  (directional-light 0.8 0.8 0.8 0 0 -1)
  (define s (/ mouse-x (float width)))
  (specular s s s)
  (sphere 120))
