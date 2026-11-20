;; Follow 3
;; based on code from Keith Peters.
;;
;; A segmented line follows the mouse. The relative angle from
;; each segment to the next is calculated with atan2() and the
;; position of the next is calculated with sin() and cos().

(import (processing))

(define x (make-vector 20 0))
(define y (make-vector 20 0))
(define seg-length 18)

(define (setup)
  (size 640 360)
  (stroke-weight 9)
  (stroke 255 100))

(define (draw)
  (background 0)
  (drag-segment 0 mouse-x mouse-y)
  (do ((i 0 (+ i 1))) ((>= i (- (vector-length x) 1)))
    (drag-segment (+ i 1) (vector-ref x i) (vector-ref y i))))

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
