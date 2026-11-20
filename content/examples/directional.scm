;; Directional.
;;
;; Move the mouse the change the direction of the light.
;; Directional light comes from one direction and is stronger
;; when hitting a surface squarely and weaker if it hits at a
;; a gentle angle. After hitting a surface, a directional lights
;; scatters in all directions.

(import (processing))

(define (setup) (size 640 360 P3D) (no-stroke) (fill 204))

(define (draw)
  (no-stroke)
  (background 0)
  (define dir-y (* (- (/ mouse-y (float height)) 0.5) 2))
  (define dir-x (* (- (/ mouse-x (float width)) 0.5) 2))
  (directional-light 204 204 204 (- dir-x) (- dir-y) -1)
  (translate (- (/ width 2) 100) (/ height 2) 0)
  (sphere 80)
  (translate 200 0 0)
  (sphere 80))
