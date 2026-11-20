;; Button.
;;
;; Click on one of the colored shapes in the
;; center of the image to change the color of
;; the background.

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
(define rect-highlight 0)
(define circle-highlight 0)
(define current-color 0)
(define rect-over #f)
(define circle-over #f)

(define (setup)
  (size 640 360)
  (set! rect-color (color 0))
  (set! rect-highlight (color 51))
  (set! circle-color (color 255))
  (set! circle-highlight (color 204))
  (set! base-color (color 102))
  (set! current-color base-color)
  (set! circle-x (+ (quotient width 2) (quotient circle-size 2) 10))
  (set! circle-y (quotient height 2))
  (set! rect-x (- (quotient width 2) rect-size 10))
  (set! rect-y (- (quotient height 2) (quotient rect-size 2)))
  (ellipse-mode CENTER))

(define (draw)
  (update mouse-x mouse-y)
  (background current-color)

  (if rect-over (fill rect-highlight) (fill rect-color))
  (stroke 255)
  (rect rect-x rect-y rect-size rect-size)

  (if circle-over (fill circle-highlight) (fill circle-color))
  (stroke 0)
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

(define (mouse-pressed)
  (when circle-over (set! current-color circle-color))
  (when rect-over (set! current-color rect-color)))

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
