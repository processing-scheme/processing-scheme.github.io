;; Mouse Functions.
;;
;; Click on the box and drag it across the screen.

(import (processing))

(define bx 0.0)
(define by 0.0)
(define box-size 75)
(define over-box #f)
(define locked #f)
(define x-offset 0.0)
(define y-offset 0.0)

(define (setup)
  (size 640 360)
  (set! bx (/ width 2.0))
  (set! by (/ height 2.0))
  (rect-mode RADIUS))

(define (draw)
  (background 0)

  ; Test if the cursor is over the box
  (cond
    ((and (> mouse-x (- bx box-size))
          (< mouse-x (+ bx box-size))
          (> mouse-y (- by box-size))
          (< mouse-y (+ by box-size)))
     (set! over-box #t)
     (when (not locked) (stroke 255) (fill 153)))
    (else (stroke 153) (fill 153) (set! over-box #f)))

  ; Draw the box
  (rect bx by box-size box-size))

(define (mouse-pressed)
  (cond (over-box (set! locked #t) (fill 255 255 255)) (else (set! locked #f)))
  (set! x-offset (- mouse-x bx))
  (set! y-offset (- mouse-y by)))

(define (mouse-dragged)
  (when locked (set! bx (- mouse-x x-offset)) (set! by (- mouse-y y-offset))))

(define (mouse-released) (set! locked #f))
