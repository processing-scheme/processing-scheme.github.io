;; Mouse 2D.
;;
;; Moving the mouse changes the position and size of each box.

(import (processing))

(define (setup)
  (size 640 360)
  (no-stroke)
  (rect-mode CENTER))

(define (draw)
  (background 51)
  (fill 255 204)
  (rect mouse-x (/ height 2) (+ (/ mouse-y 2) 10) (+ (/ mouse-y 2) 10))
  (fill 255 204)
  (define inverse-x (- width mouse-x))
  (define inverse-y (- height mouse-y))
  (rect inverse-x (/ height 2) (+ (/ inverse-y 2) 10) (+ (/ inverse-y 2) 10)))
