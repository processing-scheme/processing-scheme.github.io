;; Array.
;;
;; An array is a list of data. Each piece of data in an array
;; is identified by an index number representing its position in
;; the array. Arrays are zero based, which means that the first
;; element in the array is [0], the second element is [1], and so on.
;; In this example, an array named "coswave" is created and
;; filled with the cosine values. This data is displayed three
;; separate ways on the screen.

(import (processing))

(define coswave #f)

(define (setup)
  (size 640 360)
  (set! coswave (make-vector width 0))
  (do ((i 0 (+ i 1))) ((>= i width))
    (define amount (map i 0 width 0 PI))
    (vector-set! coswave i (abs (cos amount))))
  (background 255)
  (no-loop))

(define (draw)
  (define y1 0)
  (define y2 (quotient height 3))
  (do ((i 0 (+ i 1))) ((>= i width))
    (stroke (* (vector-ref coswave i) 255))
    (line i y1 i y2))

  (set! y1 y2)
  (set! y2 (+ y1 y1))
  (do ((i 0 (+ i 1))) ((>= i width))
    (stroke (/ (* (vector-ref coswave i) 255) 4))
    (line i y1 i y2))

  (set! y1 y2)
  (set! y2 height)
  (do ((i 0 (+ i 1))) ((>= i width))
    (stroke (- 255 (* (vector-ref coswave i) 255)))
    (line i y1 i y2)))
