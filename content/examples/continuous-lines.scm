;; Continuous Lines.
;;
;; Click and drag the mouse to draw a line.

(import (processing))

(define (setup) (size 640 360) (background 102))

(define (draw)
  (stroke 255)
  (when (eqv? mouse-is-pressed #t) (line mouse-x mouse-y pmouse-x pmouse-y)))
