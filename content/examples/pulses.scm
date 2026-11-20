;; Pulses.
;;
;; Software drawing instruments can follow a rhythm or abide by rules independent
;; of drawn gestures. This is a form of collaborative drawing in which the draftsperson
;; controls some aspects of the image and the software controls others.

(import (processing))

(define angle 0)

(define (setup)
  (size 640 360)
  (background 102)
  (no-stroke)
  (fill 0 102))

(define (draw)
  ; Draw only when mouse is pressed
  (when (eqv? mouse-is-pressed #t)
    (set! angle (+ angle 5))
    (define val (* (cos (radians angle)) 12.0))
    (do ((a 0 (+ a 75))) ((>= a 360))
      (define xoff (* (cos (radians a)) val))
      (define yoff (* (sin (radians a)) val))
      (fill 0)
      (ellipse (+ mouse-x xoff) (+ mouse-y yoff) val val))
    (fill 255)
    (ellipse mouse-x mouse-y 2 2)))
