;; LoadFile 1
;;
;; Loads a text file that contains two numbers separated by a tab ('\t').
;; A new pair of numbers is loaded each frame and used to draw a point on the screen.

(import (processing))

(define lines #f)
(define index 0)

(define (setup)
  (size 640 360)
  (background 0)
  (stroke 255)
  (frame-rate 12)
  (set! lines (load-strings "positions.txt")))

(define (draw)
  (when (< index (vector-length lines))
    (define pieces (split (vector-ref lines index) #\tab))
    (when (= (vector-length pieces) 2)
      ; Scale the coordinates to match the size of the sketch window
      (define x (map (float (vector-ref pieces 0)) 0 100 0 width))
      (define y (map (float (vector-ref pieces 1)) 0 100 0 height))
      (point x y))
    ; Go to the next line for the next run through draw()
    (set! index (+ index 1))))
