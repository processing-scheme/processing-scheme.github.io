;; Simple Linear Gradient
;;
;; The lerpColor() function is useful for interpolating
;; between two colors.
; Constants

(import (processing))

(define Y-AXIS 1)
(define X-AXIS 2)
(define b1 0)
(define b2 0)
(define c1 0)
(define c2 0)

(define (setup)
  (size 640 360)

  ; Define colors
  (set! b1 (color 255))
  (set! b2 (color 0))
  (set! c1 (color 204 102 0))
  (set! c2 (color 0 102 153))

  (no-loop))

(define (draw)
  ; Background
  (set-gradient 0 0 (/ width 2) height b1 b2 X-AXIS)
  (set-gradient (/ width 2) 0 (/ width 2) height b2 b1 X-AXIS)
  ; Foreground
  (set-gradient 50 90 540 80 c1 c2 Y-AXIS)
  (set-gradient 50 190 540 80 c2 c1 X-AXIS))

(define (set-gradient x y w h c1 c2 axis)
  (no-fill)

  (cond
    ((= axis Y-AXIS)
     ; Top to bottom gradient
     (do ((i y (+ i 1))) ((> i (+ y h)))
       (define inter (map i y (+ y h) 0 1))
       (define c (lerp-color c1 c2 inter))
       (stroke c)
       (line x i (+ x w) i)))
    ((= axis X-AXIS)
     ; Left to right gradient
     (do ((i x (+ i 1))) ((> i (+ x w)))
       (define inter (map i x (+ x w) 0 1))
       (define c (lerp-color c1 c2 inter))
       (stroke c)
       (line i y i (+ y h))))))
