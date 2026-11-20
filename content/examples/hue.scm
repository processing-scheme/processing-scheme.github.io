;; Hue.
;;
;; Hue is the color reflected from or transmitted through an object
;; and is typically referred to as the name of the color such as
;; red, blue, or yellow. In this example, move the cursor vertically
;; over each bar to alter its hue.

(import (processing))

(define bar-width 20)
(define last-bar -1)

(define (setup)
  (size 640 360)
  (color-mode HSB height height height)
  (no-stroke)
  (background 0))

(define (draw)
  (define which-bar (quotient mouse-x bar-width))
  (when (not (= which-bar last-bar))
    (define bar-x (* which-bar bar-width))
    (fill mouse-y height height)
    (rect bar-x 0 bar-width height)
    (set! last-bar which-bar)))
