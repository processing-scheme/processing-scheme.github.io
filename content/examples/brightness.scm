;; Brightness
;; by Rusty Robison.
;;
;; Brightness is the relative lightness or darkness of a color.
;; Move the cursor vertically over each bar to alter its brightness.

(import (processing))

(define bar-width 20)
(define last-bar -1)

(define (setup)
  (size 640 360)
  (color-mode HSB width 100 height)
  (no-stroke)
  (background 0))

(define (draw)
  (define which-bar (quotient mouse-x bar-width))
  (when (not (= which-bar last-bar))
    (define bar-x (* which-bar bar-width))
    (fill bar-x 100 mouse-y)
    (rect bar-x 0 bar-width height)
    (set! last-bar which-bar)))
