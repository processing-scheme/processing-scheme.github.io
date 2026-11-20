;; Sine Cosine.
;;
;; Linear movement with sin() and cos().
;; Numbers between 0 and PI*2 (TWO_PI which angles roughly 6.28)
;; are put into these functions and numbers between -1 and 1 are
;; returned. These values are then scaled to produce larger movements.

(import (processing))

(define x1 0.0)
(define x2 0.0)
(define y1 0.0)
(define y2 0.0)
(define angle1 0.0)
(define angle2 0.0)
(define scalar 70)

(define (setup)
  (size 640 360)
  (no-stroke)
  (rect-mode CENTER))

(define (draw)
  (background 0)

  (define ang1 (radians angle1))
  (define ang2 (radians angle2))

  (set! x1 (+ (/ width 2) (* scalar (cos ang1))))
  (set! x2 (+ (/ width 2) (* scalar (cos ang2))))

  (set! y1 (+ (/ height 2) (* scalar (sin ang1))))
  (set! y2 (+ (/ height 2) (* scalar (sin ang2))))

  (fill 255)
  (rect (* width 0.5) (* height 0.5) 140 140)

  (fill 0 102 153)
  (ellipse x1 (- (* height 0.5) 120) scalar scalar)
  (ellipse x2 (+ (* height 0.5) 120) scalar scalar)

  (fill 255 204 0)
  (ellipse (- (* width 0.5) 120) y1 scalar scalar)
  (ellipse (+ (* width 0.5) 120) y2 scalar scalar)

  (set! angle1 (+ angle1 2))
  (set! angle2 (+ angle2 3)))
