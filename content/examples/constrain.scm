;; Constrain.
;;
;; Move the mouse across the screen to move the circle.
;; The program constrains the circle to its box.

(import (processing))

(define mx 0.0)
(define my 0.0)
(define easing 0.05)
(define radius 24)
(define edge 100)
(define inner (+ edge radius))

(define (setup)
  (size 640 360)
  (no-stroke)
  (ellipse-mode RADIUS)
  (rect-mode CORNERS))

(define (draw)
  (background 51)

  (when (> (abs (- mouse-x mx)) 0.1) (set! mx (+ mx (* (- mouse-x mx) easing))))
  (when (> (abs (- mouse-y my)) 0.1) (set! my (+ my (* (- mouse-y my) easing))))

  (set! mx (constrain mx inner (- width inner)))
  (set! my (constrain my inner (- height inner)))
  (fill 76)
  (rect edge edge (- width edge) (- height edge))
  (fill 255)
  (ellipse mx my radius radius))
