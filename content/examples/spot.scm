;; Spot.
;;
;; Move the mouse the change the position and concentation
;; of a blue spot light.

(import (processing))

(define (setup)
  (size 640 360 P3D)
  (no-stroke)
  (fill 204)
  (sphere-detail 60))

(define (draw)
  (background 0)

  ; Light the bottom of the sphere
  (directional-light 51 102 126 0 -1 0)

  ; Orange light on the upper-right of the sphere
  (spot-light 204 153 0 360 160 600 0 0 -1 (/ PI 2) 600)

  ; Moving spotlight that follows the mouse
  (spot-light 102 153 204 360 mouse-y 600 0 0 -1 (/ PI 2) 600)

  (translate (/ width 2) (/ height 2) 0)
  (sphere 120))
