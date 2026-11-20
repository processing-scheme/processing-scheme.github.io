;; Storing Input.
;;
;; Move the mouse across the screen to change the position
;; of the circles. The positions of the mouse are recorded
;; into an array and played back every frame. Between each
;; frame, the newest value are added to the end of each array
;; and the oldest value is deleted.

(import (processing))

(define num 60)
(define mx (make-vector num 0))
(define my (make-vector num 0))

(define (setup) (size 640 360) (no-stroke) (fill 255 153))

(define (draw)
  (background 51)

  ; Cycle through the array, using a different entry on each frame.
  ; Using modulo (%) like this is faster than moving all the values over.
  (define which (remainder frame-count num))
  (vector-set! mx which mouse-x)
  (vector-set! my which mouse-y)

  (do ((i 0 (+ i 1))) ((>= i num))
    ; which+1 is the smallest (the oldest in the array)
    (define index (remainder (+ which 1 i) num))
    (ellipse (vector-ref mx index) (vector-ref my index) i i)))
