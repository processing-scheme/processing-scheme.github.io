;; Increment Decrement.
;;
;; Writing "a++" is equivalent to "a = a + 1".
;; Writing "a--" is equivalent to "a = a - 1".

(import (processing))

(define a 0)
(define b 0)
(define direction #f)

(define (setup)
  (size 640 360)
  (color-mode RGB width)
  (set! a 0)
  (set! b width)
  (set! direction #t)
  (frame-rate 30))

(define (draw)
  (set! a (+ a 1))
  (when (> a width) (set! a 0) (set! direction (not direction)))
  (if (eqv? direction #t) (stroke a) (stroke (- width a)))
  (line a 0 a (/ height 2))

  (set! b (- b 1))
  (when (< b 0) (set! b width))
  (if (eqv? direction #t) (stroke (- width b)) (stroke b))
  (line b (+ (/ height 2) 1) b height))
