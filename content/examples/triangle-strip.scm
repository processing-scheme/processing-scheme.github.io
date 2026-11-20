;; Triangle Strip
;; by Ira Greenberg.
;;
;; Generate a closed ring using the vertex() function and
;; beginShape(TRIANGLE_STRIP) mode. The outsideRadius and insideRadius
;; variables control ring's radii respectively.

(import (processing))

(define x 0)
(define y 0)
(define outside-radius 150)
(define inside-radius 100)

(define (setup)
  (size 640 360)
  (background 204)
  (set! x (quotient width 2))
  (set! y (quotient height 2)))

(define (draw)
  (background 204)

  (define num-points (int (map mouse-x 0 width 6 60)))
  (define angle 0)
  (define angle-step (/ 180.0 num-points))

  (begin-shape TRIANGLE-STRIP)
  (do ((i 0 (+ i 1))) ((> i num-points))
    (define px (+ x (* (cos (radians angle)) outside-radius)))
    (define py (+ y (* (sin (radians angle)) outside-radius)))
    (set! angle (+ angle angle-step))
    (vertex px py)
    (set! px (+ x (* (cos (radians angle)) inside-radius)))
    (set! py (+ y (* (sin (radians angle)) inside-radius)))
    (vertex px py)
    (set! angle (+ angle angle-step)))
  (end-shape))
