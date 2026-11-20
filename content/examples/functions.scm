;; Functions.
;;
;; The drawTarget() function makes it easy to draw many distinct targets.
;; Each call to drawTarget() specifies the position, size, and number of
;; rings for each target.

(import (processing))

(define (setup)
  (size 640 360)
  (background 51)
  (no-stroke)
  (no-loop))

(define (draw)
  (draw-target (* width 0.25) (* height 0.4) 200 4)
  (draw-target (* width 0.5) (* height 0.5) 300 10)
  (draw-target (* width 0.75) (* height 0.3) 120 6))

(define (draw-target xloc yloc size-v num)
  (define grayvalues (/ 255 num))
  (define steps (/ size-v num))
  (do ((i 0 (+ i 1))) ((>= i num))
    (fill (* i grayvalues))
    (ellipse xloc yloc (- size-v (* i steps)) (- size-v (* i steps)))))
