;; True/False.
;;
;; A Boolean variable has only two possible values: true or false.
;; It is common to use Booleans with control statements to
;; determine the flow of a program. In this example, when the
;; boolean value "x" is true, vertical black lines are drawn and when
;; the boolean value "x" is false, horizontal gray lines are drawn.

(import (processing))

(define (setup)
  (define b #f)

  (size 640 360)
  (background 0)
  (stroke 255)

  (define d 20)
  (define middle (quotient width 2))

  (do ((i d (+ i d))) ((> i width))
    (if (< i middle) (set! b #t) (set! b #f))

    (when (eqv? b #t)
      ; Vertical line
      (line i d i (- height d)))

    (when (not b)
      ; Horizontal line
      (line middle (+ (- i middle) d) (- width d) (+ (- i middle) d)))))
