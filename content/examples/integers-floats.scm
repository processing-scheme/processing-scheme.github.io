;; Integers Floats.
;;
;; Integers and floats are two different kinds of numerical data.
;; An integer (more commonly called an int) is a number without
;; a decimal point. A float is a floating-point number, which means
;; it is a number that has a decimal place. Floats are used when
;; more precision is needed.

(import (processing))

(define a 0)  ; Create a variable "a" of the datatype "int"
(define b 0.0)  ; Create a variable "b" of the datatype "float"

(define (setup) (size 640 360) (stroke 255))

(define (draw)
  (background 0)

  (set! a (+ a 1))
  (set! b (+ b 0.2))
  (line a 0 a (/ height 2))
  (line b (/ height 2) b height)

  (when (> a width) (set! a 0))
  (when (> b width) (set! b 0)))
