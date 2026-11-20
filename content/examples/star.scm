;; Star
;;
;; The star() function created for this example is capable of drawing a
;; wide range of different forms. Try placing different numbers into the
;; star() function calls within draw() to explore.

(import (processing))

(define (setup) (size 640 360))

(define (draw)
  (background 102)

  (push-matrix)
  (translate (* width 0.2) (* height 0.5))
  (rotate (/ frame-count 200.0))
  (star 0 0 5 70 3)
  (pop-matrix)

  (push-matrix)
  (translate (* width 0.5) (* height 0.5))
  (rotate (/ frame-count 400.0))
  (star 0 0 80 100 40)
  (pop-matrix)

  (push-matrix)
  (translate (* width 0.8) (* height 0.5))
  (rotate (/ frame-count -100.0))
  (star 0 0 30 70 5)
  (pop-matrix))

(define (star x y radius1 radius2 npoints)
  (define angle (/ TWO-PI npoints))
  (define half-angle (/ angle 2.0))
  (begin-shape)
  (do ((a 0 (+ a angle))) ((>= a TWO-PI))
    (define sx (+ x (* (cos a) radius2)))
    (define sy (+ y (* (sin a) radius2)))
    (vertex sx sy)
    (set! sx (+ x (* (cos (+ a half-angle)) radius1)))
    (set! sy (+ y (* (sin (+ a half-angle)) radius1)))
    (vertex sx sy))
  (end-shape CLOSE))
