;; Bounce.
;;
;; When the shape hits the edge of the window, it reverses its direction.

(import (processing))

(define rad 60)  ; Width of the shape
(define xpos 0.0)
(define ypos 0.0)  ; Starting position of shape

(define xspeed 2.8)  ; Speed of the shape
(define yspeed 2.2)  ; Speed of the shape

(define xdirection 1)  ; Left or Right
(define ydirection 1)  ; Top to Bottom

(define (setup)
  (size 640 360)
  (no-stroke)
  (frame-rate 30)
  (ellipse-mode RADIUS)
  ; Set the starting position of the shape
  (set! xpos (/ width 2))
  (set! ypos (/ height 2)))

(define (draw)
  (background 102)

  ; Update the position of the shape
  (set! xpos (+ xpos (* xspeed xdirection)))
  (set! ypos (+ ypos (* yspeed ydirection)))

  ; Test to see if the shape exceeds the boundaries of the screen
  ; If it does, reverse its direction by multiplying by -1
  (when (or (> xpos (- width rad)) (< xpos rad)) (set! xdirection (* xdirection -1)))
  (when (or (> ypos (- height rad)) (< ypos rad)) (set! ydirection (* ydirection -1)))

  ; Draw the shape
  (ellipse xpos ypos rad rad))
