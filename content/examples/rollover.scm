;; Rollover.
;;
;; Roll over the colored squares in the center of the image
;; to change the color of the outside rectangle.

(import (processing))

(define rect-x 0)
(define rect-y 0)  ; Position of square button
(define circle-x 0)
(define circle-y 0)  ; Position of circle button
(define rect-size 90)  ; Diameter of rect
(define circle-size 93)  ; Diameter of circle

(define rect-color 0)
(define circle-color 0)
(define base-color 0)

(define rect-over #f)
(define circle-over #f)

(define (setup)
  (size 640 360)
  (set! rect-color (color 0))
  (set! circle-color (color 255))
  (set! base-color (color 102))
  (set! circle-x (+ (quotient width 2) (quotient circle-size 2) 10))
  (set! circle-y (quotient height 2))
  (set! rect-x (- (quotient width 2) rect-size 10))
  (set! rect-y (- (quotient height 2) (quotient rect-size 2)))
  (ellipse-mode CENTER))

(define (draw)
  (update mouse-x mouse-y)

  (no-stroke)
  (cond
    (rect-over (background rect-color))
    (circle-over (background circle-color))
    (else (background base-color)))

  (stroke 255)
  (fill rect-color)
  (rect rect-x rect-y rect-size rect-size)
  (stroke 0)
  (fill circle-color)
  (ellipse circle-x circle-y circle-size circle-size))

(define (update x y)
  (cond
    ((over-circle circle-x circle-y circle-size)
     (set! circle-over #t)
     (set! rect-over #f))
    ((over-rect rect-x rect-y rect-size rect-size)
     (set! rect-over #t)
     (set! circle-over #f))
    (else (set! rect-over #f) (set! circle-over rect-over))))

(define (over-rect x y width-v height-v)
  (if (and (>= mouse-x x)
        (<= mouse-x (+ x width-v))
        (>= mouse-y y)
        (<= mouse-y (+ y height-v)))
    #t
    #f))

(define (over-circle x y diameter)
  (define dis-x (- x mouse-x))
  (define dis-y (- y mouse-y))
  (if (< (sqrt (+ (sq dis-x) (sq dis-y))) (/ diameter 2)) #t #f))
