;; Follow 2
;; based on code from Keith Peters.
;;
;; A two-segmented arm follows the cursor position. The relative
;; angle between the segments is calculated with atan2() and the
;; position calculated with sin() and cos().

(import (processing))

(define x (make-vector 2 0))
(define y (make-vector 2 0))
(define seg-length 50)

(define (setup)
  (size 640 360)
  (stroke-weight 20.0)
  (stroke 255 100))

(define (draw)
  (background 0)
  (drag-segment 0 mouse-x mouse-y)
  (drag-segment 1 (vector-ref x 0) (vector-ref y 0)))

(define (drag-segment i xin yin)
  (define dx (- xin (vector-ref x i)))
  (define dy (- yin (vector-ref y i)))
  (define angle (atan2 dy dx))
  (vector-set! x i (- xin (* (cos angle) seg-length)))
  (vector-set! y i (- yin (* (sin angle) seg-length)))
  (segment (vector-ref x i) (vector-ref y i) angle))

(define (segment x y a)
  (push-matrix)
  (translate x y)
  (rotate a)
  (line 0 0 seg-length 0)
  (pop-matrix))
