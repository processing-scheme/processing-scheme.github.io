;; Random Gaussian.
;;
;; This sketch draws ellipses with x and y locations tied to a gaussian distribution of random numbers.

(import (processing))

(define (setup) (size 640 360) (background 0))

(define (draw)
  ; Get a gaussian random number w/ mean of 0 and standard deviation of 1.0
  (define val (random-gaussian))

  (define sd 60)  ; Define a standard deviation
  (define mean (/ width 2))  ; Define a mean value (middle of the screen along the x-axis)
  (define x (+ (* val sd) mean))  ; Scale the gaussian random number by standard deviation and mean

  (no-stroke)
  (fill 255 10)
  ; Draw an ellipse at our "normal" random location
  (ellipse x (/ height 2) 32 32))
