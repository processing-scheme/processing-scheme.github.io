;; Distance 1D.
;;
;; Move the mouse left and right to control the
;; speed and direction of the moving shapes.

(import (processing))

(define xpos1 0.0)
(define xpos2 0.0)
(define xpos3 0.0)
(define xpos4 0.0)
(define thin 8)
(define thick 36)

(define (setup)
  (size 640 360)
  (no-stroke)
  (set! xpos1 (/ width 2))
  (set! xpos2 (/ width 2))
  (set! xpos3 (/ width 2))
  (set! xpos4 (/ width 2)))

(define (draw)
  (background 0)

  (define mx (- (* mouse-x 0.4) (/ width 5.0)))

  (fill 102)
  (rect xpos2 0 thick (/ height 2))
  (fill 204)
  (rect xpos1 0 thin (/ height 2))
  (fill 102)
  (rect xpos4 (/ height 2) thick (/ height 2))
  (fill 204)
  (rect xpos3 (/ height 2) thin (/ height 2))

  (set! xpos1 (+ xpos1 (/ mx 16)))
  (set! xpos2 (+ xpos2 (/ mx 64)))
  (set! xpos3 (- xpos3 (/ mx 16)))
  (set! xpos4 (- xpos4 (/ mx 64)))

  (when (< xpos1 (- thin)) (set! xpos1 width))
  (when (> xpos1 width) (set! xpos1 (- thin)))
  (when (< xpos2 (- thick)) (set! xpos2 width))
  (when (> xpos2 width) (set! xpos2 (- thick)))
  (when (< xpos3 (- thin)) (set! xpos3 width))
  (when (> xpos3 width) (set! xpos3 (- thin)))
  (when (< xpos4 (- thick)) (set! xpos4 width))
  (when (> xpos4 width) (set! xpos4 (- thick))))
