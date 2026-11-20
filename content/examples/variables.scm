;; Variables.
;;
;; Variables are used for storing values. In this example, change
;; the values of variables to affect the composition.

(import (processing))

(define (setup)
  (size 640 360)
  (background 0)
  (stroke 153)
  (stroke-weight 4)
  (stroke-cap SQUARE)

  (define a 50)
  (define b 120)
  (define c 180)

  (line a b (+ a c) b)
  (line a (+ b 10) (+ a c) (+ b 10))
  (line a (+ b 20) (+ a c) (+ b 20))
  (line a (+ b 30) (+ a c) (+ b 30))

  (set! a (+ a c))
  (set! b (- height b))

  (line a b (+ a c) b)
  (line a (+ b 10) (+ a c) (+ b 10))
  (line a (+ b 20) (+ a c) (+ b 20))
  (line a (+ b 30) (+ a c) (+ b 30))

  (set! a (+ a c))
  (set! b (- height b))

  (line a b (+ a c) b)
  (line a (+ b 10) (+ a c) (+ b 10))
  (line a (+ b 20) (+ a c) (+ b 20))
  (line a (+ b 30) (+ a c) (+ b 30)))
