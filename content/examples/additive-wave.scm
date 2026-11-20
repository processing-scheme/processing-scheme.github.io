;; Additive Wave
;; by Daniel Shiffman.
;;
;; Create a more complex wave by adding two waves together.

(import (processing))

(define xspacing 8)  ; How far apart should each horizontal location be spaced
(define w 0)  ; Width of entire wave
(define maxwaves 4)  ; total # of waves to add together

(define theta 0.0)
(define amplitude (make-vector maxwaves 0))  ; Height of wave
(define dx (make-vector maxwaves 0))  ; Value for incrementing X, to be calculated as a function of period and xspacing
(define yvalues #f)  ; Using an array to store height values for the wave (not entirely necessary)

(define (setup)
  (size 640 360)
  (frame-rate 30)
  (color-mode RGB 255 255 255 100)
  (set! w (+ width 16))

  (do ((i 0 (+ i 1))) ((>= i maxwaves))
    (vector-set! amplitude i (random 10 30))
    (define period (random 100 300))  ; How many pixels before the wave repeats
    (vector-set! dx i (* (/ TWO-PI period) xspacing)))

  (set! yvalues (make-vector (quotient w xspacing) 0)))

(define (draw) (background 0) (calc-wave) (render-wave))

(define (calc-wave)
  ; Increment theta (try different values for 'angular velocity' here
  (set! theta (+ theta 0.02))

  ; Set all height values to zero
  (do ((i 0 (+ i 1))) ((>= i (vector-length yvalues)))
    (vector-set! yvalues i 0))

  ; Accumulate wave height values
  (do ((j 0 (+ j 1))) ((>= j maxwaves))
    (define x theta)
    (do ((i 0 (+ i 1))) ((>= i (vector-length yvalues)))
      ; Every other wave is cosine instead of sine
      (if (= (remainder j 2) 0)
        (vector-set! yvalues
                     i
                     (+ (vector-ref yvalues i) (* (sin x) (vector-ref amplitude j))))
        (vector-set! yvalues
                     i
                     (+ (vector-ref yvalues i) (* (cos x) (vector-ref amplitude j)))))
      (set! x (+ x (vector-ref dx j))))))

(define (render-wave)
  ; A simple way to draw the wave with an ellipse at each location
  (no-stroke)
  (fill 255 50)
  (ellipse-mode CENTER)
  (do ((x 0 (+ x 1))) ((>= x (vector-length yvalues)))
    (ellipse (* x xspacing) (+ (/ height 2) (vector-ref yvalues x)) 16 16)))
