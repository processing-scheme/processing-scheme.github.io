;; Brownian motion.
;;
;; Recording random movement as a continuous line.

(import (processing))

(define num 2000)
(define range 6)

(define ax (make-vector num 0))
(define ay (make-vector num 0))

(define (setup)
  (size 640 360)
  (do ((i 0 (+ i 1))) ((>= i num))
    (vector-set! ax i (/ width 2))
    (vector-set! ay i (/ height 2)))
  (frame-rate 30))

(define (draw)
  (background 51)

  ; Shift all elements 1 place to the left
  (do ((i 1 (+ i 1))) ((>= i num))
    (vector-set! ax (- i 1) (vector-ref ax i))
    (vector-set! ay (- i 1) (vector-ref ay i)))

  ; Put a new value at the end of the array
  (vector-set! ax (- num 1) (+ (vector-ref ax (- num 1)) (random (- range) range)))
  (vector-set! ay (- num 1) (+ (vector-ref ay (- num 1)) (random (- range) range)))

  ; Constrain all points to the screen
  (vector-set! ax (- num 1) (constrain (vector-ref ax (- num 1)) 0 width))
  (vector-set! ay (- num 1) (constrain (vector-ref ay (- num 1)) 0 height))

  ; Draw a line connecting the points
  (do ((i 1 (+ i 1))) ((>= i num))
    (define val (+ (* (/ (float i) num) 204.0) 51))
    (stroke val)
    (line (vector-ref ax (- i 1))
          (vector-ref ay (- i 1))
          (vector-ref ax i)
          (vector-ref ay i))))
