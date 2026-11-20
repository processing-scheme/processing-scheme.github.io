;; Logical Operators.
;;
;; The logical operators for AND (&&) and OR (||) are used to
;; combine simple relational statements into more complex expressions.
;; The NOT (!) operator is used to negate a boolean statement.

(import (processing))

(define (setup)
  (size 640 360)
  (background 126)

  (define test #f)

  (do ((i 5 (+ i 5))) ((> i height))
    ; Logical AND
    (stroke 0)
    (when (and (> i 35) (< i 100)) (line (/ width 4) i (/ width 2) i) (set! test #f))

    ; Logical OR
    (stroke 76)
    (when (or (<= i 35) (>= i 100)) (line (/ width 2) i width i) (set! test #t))

    ; Testing if a boolean value is "true"
    ; The expression "if(test)" is equivalent to "if(test == true)"
    (when test (stroke 0) (point (/ width 3) i))

    ; Testing if a boolean value is "false"
    ; The expression "if(!test)" is equivalent to "if(test == false)"
    (when (not test) (stroke 255) (point (/ width 4) i))))
