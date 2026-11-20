;; Pie Chart
;;
;; Uses the arc() function to generate a pie chart from the data
;; stored in an array.

(import (processing))

(define angles (vector 30 10 45 35 60 38 75 67))

(define (setup)
  (size 640 360)
  (no-stroke)
  ; Run once and stop
  (no-loop))

(define (draw) (background 100) (pie-chart 300 angles))

(define (pie-chart diameter data)
  (define last-angle 0)
  (do ((i 0 (+ i 1))) ((>= i (vector-length data)))
    (define gray (map i 0 (vector-length data) 0 255))
    (fill gray)
    (arc (/ width 2)
         (/ height 2)
         diameter
         diameter
         last-angle
         (+ last-angle (radians (vector-ref data i))))
    (set! last-angle (+ last-angle (radians (vector-ref data i))))))
