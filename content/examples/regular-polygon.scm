;; Regular Polygon
;;
;; What is your favorite? Pentagon? Hexagon? Heptagon?
;; No? What about the icosagon? The polygon() function
;; created for this example is capable of drawing any
;; regular polygon. Try placing different numbers into the
;; polygon() function calls within draw() to explore.

(import (processing))

(define (setup) (size 640 360))

(define (draw)
  (background 102)

  (push-matrix)
  (translate (* width 0.2) (* height 0.5))
  (rotate (/ frame-count 200.0))
  (polygon 0 0 82 3)  ; Triangle
  (pop-matrix)

  (push-matrix)
  (translate (* width 0.5) (* height 0.5))
  (rotate (/ frame-count 50.0))
  (polygon 0 0 80 20)  ; Icosagon
  (pop-matrix)

  (push-matrix)
  (translate (* width 0.8) (* height 0.5))
  (rotate (/ frame-count -100.0))
  (polygon 0 0 70 7)  ; Heptagon
  (pop-matrix))

(define (polygon x y radius npoints)
  (define angle (/ TWO-PI npoints))
  (begin-shape)
  (do ((a 0 (+ a angle))) ((>= a TWO-PI))
    (define sx (+ x (* (cos a) radius)))
    (define sy (+ y (* (sin a) radius)))
    (vertex sx sy))
  (end-shape CLOSE))
