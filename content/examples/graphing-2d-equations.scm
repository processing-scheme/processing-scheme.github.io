;; Graphing 2D Equations
;; by Daniel Shiffman.
;;
;; Graphics the following equation:
;; sin(n*cos(r) + 5*theta)
;; where n is a function of horizontal mouse location.

(import (processing))

(define (setup) (size 640 360))

(define (draw)
  (load-pixels)
  (define n (/ (* mouse-x 10.0) width))
  (define w 16.0)  ; 2D space width
  (define h 16.0)  ; 2D space height
  (define dx (/ w width))  ; Increment x this amount per pixel
  (define dy (/ h height))  ; Increment y this amount per pixel
  (define x (/ (- w) 2))  ; Start x at -1 * width / 2
  (do ((i 0 (+ i 1))) ((>= i width))
    (define y (/ (- h) 2))  ; Start y at -1 * height / 2
    (do ((j 0 (+ j 1))) ((>= j height))
      (define r (sqrt (+ (* x x) (* y y))))  ; Convert cartesian to polar
      (define theta (atan2 y x))  ; Convert cartesian to polar
      ; Compute 2D polar coordinate function
      (define val (sin (+ (* n (cos r)) (* 5 theta))))  ; Results in a value between -1 and 1
      ; float val = cos(r);                            // Another simple function
      ; float val = sin(theta);                        // Another simple function
      ; Map resulting vale to grayscale value
      (vector-set! pixels (+ i (* j width)) (color (/ (* (+ val 1.0) 255.0) 2.0)))  ; Scale to between 0 and 255
      ; Increment y
      (set! y (+ y dy)))
    ; Increment x
    (set! x (+ x dx)))
  (update-pixels))
