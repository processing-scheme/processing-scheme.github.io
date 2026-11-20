;; Reach 2
;; based on code from Keith Peters.
;;
;; The arm follows the position of the mouse by
;; calculating the angles with atan2().

(import (processing))

(define num-segments 10)
(define x (make-vector num-segments 0))
(define y (make-vector num-segments 0))
(define angle (make-vector num-segments 0))
(define seg-length 26)
(define target-x 0.0)
(define target-y 0.0)

(define (setup)
  (size 640 360)
  (stroke-weight 20.0)
  (stroke 255 100)
  (vector-set! x (- (vector-length x) 1) (/ width 2))  ; Set base x-coordinate
  ; Set base y-coordinate
  (vector-set! y (- (vector-length x) 1) height))

(define (draw)
  (background 0)

  (reach-segment 0 mouse-x mouse-y)
  (do ((i 1 (+ i 1))) ((>= i num-segments))
    (reach-segment i target-x target-y))
  (do ((i (- (vector-length x) 1) (- i 1))) ((< i 1))
    (position-segment i (- i 1)))
  (do ((i 0 (+ i 1))) ((>= i (vector-length x)))
    (segment (vector-ref x i) (vector-ref y i) (vector-ref angle i) (* (+ i 1) 2))))

(define (position-segment a b)
  (vector-set! x b (+ (vector-ref x a) (* (cos (vector-ref angle a)) seg-length)))
  (vector-set! y b (+ (vector-ref y a) (* (sin (vector-ref angle a)) seg-length))))

(define (reach-segment i xin yin)
  (define dx (- xin (vector-ref x i)))
  (define dy (- yin (vector-ref y i)))
  (vector-set! angle i (atan2 dy dx))
  (set! target-x (- xin (* (cos (vector-ref angle i)) seg-length)))
  (set! target-y (- yin (* (sin (vector-ref angle i)) seg-length))))

(define (segment x y a sw)
  (stroke-weight sw)
  (push-matrix)
  (translate x y)
  (rotate a)
  (line 0 0 seg-length 0)
  (pop-matrix))
