;; Relativity.
;;
;; Each color is perceived in relation to other colors. The top and bottom
;; bars each contain the same component colors, but a different display order
;; causes individual colors to appear differently.

(import (processing))

(define a 0)
(define b 0)
(define c 0)
(define d 0)
(define e 0)

(define (setup)
  (size 640 360)
  (no-stroke)
  (set! a (color 165 167 20))
  (set! b (color 77 86 59))
  (set! c (color 42 106 105))
  (set! d (color 165 89 20))
  (set! e (color 146 150 127))
  ; Draw only one time
  (no-loop))

(define (draw)
  (draw-band a b c d e 0 (/ width 128))
  (draw-band c a d b e (/ height 2) (/ width 128)))

(define (draw-band v w x y z ypos bar-width)
  (define num 5)
  (define color-order (vector v w x y z))
  (do ((i 0 (+ i (* bar-width num)))) ((>= i width))
    (do ((j 0 (+ j 1))) ((>= j num))
      (fill (vector-ref color-order j))
      (rect (+ i (* j bar-width)) ypos bar-width (/ height 2)))))
