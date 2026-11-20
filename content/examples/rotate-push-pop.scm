;; Rotate Push Pop.
;;
;; The push() and pop() functions allow for more control over transformations.
;; The push function saves the current coordinate system to the stack
;; and pop() restores the prior coordinate system.

(import (processing))

(define a 0.0)  ; Angle of rotation
(define offset (/ PI 24.0))  ; Angle offset between boxes
(define num 12)  ; Number of boxes

(define (setup) (size 640 360 P3D) (no-stroke))

(define (draw)
  (lights)

  (background 0 0 26)
  (translate (/ width 2) (/ height 2))

  (do ((i 0 (+ i 1))) ((>= i num))
    (define gray (map i 0 (- num 1) 0 255))
    (push-matrix)
    (fill gray)
    (rotate-y (+ a (* offset i)))
    (rotate-x (+ (/ a 2) (* offset i)))
    (box 200)
    (pop-matrix))

  (set! a (+ a 0.01)))
