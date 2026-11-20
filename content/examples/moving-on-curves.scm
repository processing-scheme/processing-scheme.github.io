;; Moving On Curves.
;;
;; In this example, the circles moves along the curve y = x^4.
;; Click the mouse to have it move to a new position.

(import (processing))

(define begin-x 20.0)  ; Initial x-coordinate
(define begin-y 10.0)  ; Initial y-coordinate
(define end-x 570.0)  ; Final x-coordinate
(define end-y 320.0)  ; Final y-coordinate
(define dist-x 0.0)  ; X-axis distance to move
(define dist-y 0.0)  ; Y-axis distance to move
(define exponent 4)  ; Determines the curve
(define x 0.0)  ; Current x-coordinate
(define y 0.0)  ; Current y-coordinate
(define step 0.01)  ; Size of each step along the path
(define pct 0.0)  ; Percentage traveled (0.0 to 1.0)

(define (setup)
  (size 640 360)
  (no-stroke)
  (set! dist-x (- end-x begin-x))
  (set! dist-y (- end-y begin-y)))

(define (draw)
  (fill 0 2)
  (rect 0 0 width height)
  (set! pct (+ pct step))
  (when (< pct 1.0)
    (set! x (+ begin-x (* pct dist-x)))
    (set! y (+ begin-y (* (pow pct exponent) dist-y))))
  (fill 255)
  (ellipse x y 20 20))

(define (mouse-pressed)
  (set! pct 0.0)
  (set! begin-x x)
  (set! begin-y y)
  (set! end-x mouse-x)
  (set! end-y mouse-y)
  (set! dist-x (- end-x begin-x))
  (set! dist-y (- end-y begin-y)))
