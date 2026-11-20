;; Reach 1
;; based on code from Keith Peters.
;;
;; The arm follows the position of the mouse by
;; calculating the angles with atan2().

(import (processing))

(define seg-length 80)
(define x 0.0)
(define y 0.0)
(define x2 0.0)
(define y2 0.0)

(define (setup)
  (size 640 360)
  (stroke-weight 20.0)
  (stroke 255 100)

  (set! x (/ width 2))
  (set! y (/ height 2))
  (set! x2 x)
  (set! y2 y))

(define (draw)
  (background 0)

  (define dx (- mouse-x x))
  (define dy (- mouse-y y))
  (define angle1 (atan2 dy dx))

  (define tx (- mouse-x (* (cos angle1) seg-length)))
  (define ty (- mouse-y (* (sin angle1) seg-length)))
  (set! dx (- tx x2))
  (set! dy (- ty y2))
  (define angle2 (atan2 dy dx))
  (set! x (+ x2 (* (cos angle2) seg-length)))
  (set! y (+ y2 (* (sin angle2) seg-length)))

  (segment x y angle1)
  (segment x2 y2 angle2))

(define (segment x y a)
  (push-matrix)
  (translate x y)
  (rotate a)
  (line 0 0 seg-length 0)
  (pop-matrix))
