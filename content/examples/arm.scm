;; Arm.
;;
;; The angle of each segment is controlled with the mouseX and
;; mouseY position. The transformations applied to the first segment
;; are also applied to the second segment because they are inside
;; the same pushMatrix() and popMatrix() group.

(import (processing))

(define x 0.0)
(define y 0.0)
(define angle1 0.0)
(define angle2 0.0)
(define seg-length 100)

(define (setup)
  (size 640 360)
  (stroke-weight 30)
  (stroke 255 160)

  (set! x (* width 0.3))
  (set! y (* height 0.5)))

(define (draw)
  (background 0)

  (set! angle1 (* (- (/ mouse-x (float width)) 0.5) (- PI)))
  (set! angle2 (* (- (/ mouse-y (float height)) 0.5) PI))

  (push-matrix)
  (segment x y angle1)
  (segment seg-length 0 angle2)
  (pop-matrix))

(define (segment x y a)
  (translate x y)
  (rotate a)
  (line 0 0 seg-length 0))
