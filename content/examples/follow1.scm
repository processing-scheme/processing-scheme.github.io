;; Follow 1
;; based on code from Keith Peters.
;;
;; A line segment is pushed and pulled by the cursor.

(import (processing))

(define x 100)
(define y 100)
(define angle1 0.0)
(define seg-length 50)

(define (setup)
  (size 640 360)
  (stroke-weight 20.0)
  (stroke 255 100))

(define (draw)
  (background 0)

  (define dx (- mouse-x x))
  (define dy (- mouse-y y))
  (set! angle1 (atan2 dy dx))
  (set! x (- mouse-x (* (cos angle1) seg-length)))
  (set! y (- mouse-y (* (sin angle1) seg-length)))

  (segment x y angle1)
  (ellipse x y 20 20))

(define (segment x y a)
  (push-matrix)
  (translate x y)
  (rotate a)
  (line 0 0 seg-length 0)
  (pop-matrix))
