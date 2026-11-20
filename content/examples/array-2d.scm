;; Array 2D.
;;
;; Demonstrates the syntax for creating a two-dimensional (2D) array.
;; Values in a 2D array are accessed through two index values.
;; 2D arrays are useful for storing images. In this example, each dot
;; is colored in relation to its distance from the center of the image.

(import (processing))

; a vector of rows, each a vector of cols values (Java's new int[rows][cols])
(define (make-matrix rows cols v)
  (let ((m (make-vector rows #f)))
    (do ((i 0 (+ i 1))) ((= i rows) m)
      (vector-set! m i (make-vector cols v)))))

(define distances #f)
(define max-distance 0.0)
(define spacer 0)

(define (setup)
  (size 640 360)
  (set! max-distance (dist (/ width 2) (/ height 2) width height))
  (set! distances (make-matrix width height 0))
  (do ((y 0 (+ y 1))) ((>= y height))
    (do ((x 0 (+ x 1))) ((>= x width))
      (define distance (dist (/ width 2) (/ height 2) x y))
      (vector-set! (vector-ref distances x) y (* (/ distance max-distance) 255))))
  (set! spacer 10)
  (stroke-weight 6)
  ; Run once and stop
  (no-loop))

(define (draw)
  (background 0)
  ; This embedded loop skips over values in the arrays based on
  ; the spacer variable, so there are more values in the array
  ; than are drawn here. Change the value of the spacer variable
  ; to change the density of the points
  (do ((y 0 (+ y spacer))) ((>= y height))
    (do ((x 0 (+ x spacer))) ((>= x width))
      (stroke (vector-ref (vector-ref distances x) y))
      (point (+ x (/ spacer 2)) (+ y (/ spacer 2))))))
