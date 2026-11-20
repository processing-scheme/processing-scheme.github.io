;; Clock.
;;
;; The current time can be read with the second(), minute(),
;; and hour() functions. In this example, sin() and cos() values
;; are used to set the position of the hands.

(import (processing))

(define cx 0)
(define cy 0)
(define seconds-radius 0.0)
(define minutes-radius 0.0)
(define hours-radius 0.0)
(define clock-diameter 0.0)

(define (setup)
  (size 640 360)
  (stroke 255)

  (define radius (quotient (min width height) 2))
  (set! seconds-radius (* radius 0.72))
  (set! minutes-radius (* radius 0.60))
  (set! hours-radius (* radius 0.50))
  (set! clock-diameter (* radius 1.8))

  (set! cx (quotient width 2))
  (set! cy (quotient height 2)))

(define (draw)
  (background 0)

  ; Draw the clock background
  (fill 80)
  (no-stroke)
  (ellipse cx cy clock-diameter clock-diameter)

  ; Angles for sin() and cos() start at 3 o'clock;
  ; subtract HALF_PI to make them start at the top
  (define s (- (map (second) 0 60 0 TWO-PI) HALF-PI))
  (define m (- (map (+ (minute) (norm (second) 0 60)) 0 60 0 TWO-PI) HALF-PI))
  (define h (- (map (+ (hour) (norm (minute) 0 60)) 0 24 0 (* TWO-PI 2)) HALF-PI))

  ; Draw the hands of the clock
  (stroke 255)
  (stroke-weight 1)
  (line cx cy (+ cx (* (cos s) seconds-radius)) (+ cy (* (sin s) seconds-radius)))
  (stroke-weight 2)
  (line cx cy (+ cx (* (cos m) minutes-radius)) (+ cy (* (sin m) minutes-radius)))
  (stroke-weight 4)
  (line cx cy (+ cx (* (cos h) hours-radius)) (+ cy (* (sin h) hours-radius)))

  ; Draw the minute ticks
  (stroke-weight 2)
  (begin-shape POINTS)
  (do ((a 0 (+ a 6))) ((>= a 360))
    (define angle (radians a))
    (define x (+ cx (* (cos angle) seconds-radius)))
    (define y (+ cy (* (sin angle) seconds-radius)))
    (vertex x y))
  (end-shape))
