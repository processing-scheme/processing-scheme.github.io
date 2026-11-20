;; Sine Wave
;; by Daniel Shiffman.
;;
;; Render a simple sine wave.

(import (processing))

(define xspacing 16)  ; How far apart should each horizontal location be spaced
(define w 0)  ; Width of entire wave

(define theta 0.0)  ; Start angle at 0
(define amplitude 75.0)  ; Height of wave
(define period 500.0)  ; How many pixels before the wave repeats
(define dx 0.0)  ; Value for incrementing X, a function of period and xspacing
(define yvalues #f)  ; Using an array to store height values for the wave

(define (setup)
  (size 640 360)
  (set! w (+ width 16))
  (set! dx (* (/ TWO-PI period) xspacing))
  (set! yvalues (make-vector (quotient w xspacing) 0)))

(define (draw) (background 0) (calc-wave) (render-wave))

(define (calc-wave)
  ; Increment theta (try different values for 'angular velocity' here
  (set! theta (+ theta 0.02))

  ; For every x value, calculate a y value with sine function
  (define x theta)
  (do ((i 0 (+ i 1))) ((>= i (vector-length yvalues)))
    (vector-set! yvalues i (* (sin x) amplitude))
    (set! x (+ x dx))))

(define (render-wave)
  (no-stroke)
  (fill 255)
  ; A simple way to draw the wave with an ellipse at each location
  (do ((x 0 (+ x 1))) ((>= x (vector-length yvalues)))
    (ellipse (* x xspacing) (+ (/ height 2) (vector-ref yvalues x)) 16 16)))
