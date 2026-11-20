;; Bezier.
;;
;; The first two parameters for the bezier() function specify the
;; first point in the curve and the last two parameters specify
;; the last point. The middle parameters set the control points
;; that define the shape of the curve.

(import (processing))

(define (setup) (size 640 360) (stroke 255) (no-fill))

(define (draw)
  (background 0)
  (do ((i 0 (+ i 20))) ((>= i 200))
    (bezier (- mouse-x (/ i 2.0))
            (+ 40 i)
            410
            20
            440
            300
            (- 240 (/ i 16.0))
            (+ 300 (/ i 8.0)))))
