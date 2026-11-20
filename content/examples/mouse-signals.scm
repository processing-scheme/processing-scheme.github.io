;; Mouse Signals
;;
;; Move and click the mouse to generate signals.
;; The top row is the signal from "mouseX",
;; the middle row is the signal from "mouseY",
;; and the bottom row is the signal from "mousePressed".

(import (processing))

(define xvals #f)
(define yvals #f)
(define bvals #f)

(define (setup)
  (size 640 360)
  (no-smooth)
  (set! xvals (make-vector width 0))
  (set! yvals (make-vector width 0))
  (set! bvals (make-vector width 0)))

(define (draw)
  (background 102)

  (do ((i 1 (+ i 1))) ((>= i width))
    (vector-set! xvals (- i 1) (vector-ref xvals i))
    (vector-set! yvals (- i 1) (vector-ref yvals i))
    (vector-set! bvals (- i 1) (vector-ref bvals i)))
  ; Add the new values to the end of the array
  (vector-set! xvals (- width 1) mouse-x)
  (vector-set! yvals (- width 1) mouse-y)

  (if (eqv? mouse-is-pressed #t)
    (vector-set! bvals (- width 1) 0)
    (vector-set! bvals (- width 1) (quotient height 3)))

  (fill 255)
  (no-stroke)
  (rect 0 (/ height 3) width (+ (/ height 3) 1))

  (do ((i 1 (+ i 1))) ((>= i width))
    ; Draw the x-values
    (stroke 255)
    (point i (map (vector-ref xvals i) 0 width 0 (- (/ height 3) 1)))

    ; Draw the y-values
    (stroke 0)
    (point i (+ (/ height 3) (/ (vector-ref yvals i) 3)))

    ; Draw the mouse presses
    (stroke 255)
    (line i
          (+ (/ (* 2 height) 3) (vector-ref bvals i))
          i
          (+ (/ (* 2 height) 3) (vector-ref bvals (- i 1))))))
