;; On/Off.
;;
;; Uses the default lights to show a simple box. The lights() function
;; is used to turn on the default lighting. Click the mouse to turn the
;; lights off.

(import (processing))

(define spin 0.0)

(define (setup) (size 640 360 P3D) (no-stroke))

(define (draw)
  (background 51)

  (when (not mouse-is-pressed) (lights))

  (set! spin (+ spin 0.01))

  (push-matrix)
  (translate (/ width 2) (/ height 2) 0)
  (rotate-x (/ PI 9))
  (rotate-y (+ (/ PI 5) spin))
  (box 150)
  (pop-matrix))
