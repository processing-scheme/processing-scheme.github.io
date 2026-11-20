;; Noise Wave
;; by Daniel Shiffman.
;;
;; Using Perlin Noise to generate a wave-like pattern.

(import (processing))

(define yoff 0.0)  ; 2nd dimension of perlin noise

(define (setup) (size 640 360))

(define (draw)
  (background 51)

  (fill 255)
  ; We are going to draw a polygon out of the wave points
  (begin-shape)

  (define xoff 0)  ; Option #1: 2D Noise
  ; float xoff = yoff; // Option #2: 1D Noise

  ; Iterate over horizontal pixels
  (do ((x 0 (+ x 10))) ((> x width))
    ; Calculate a y value according to noise, map to
    (define y (map (noise xoff yoff) 0 1 200 300))  ; Option #1: 2D Noise
    ; float y = map(noise(xoff), 0, 1, 200,300);    // Option #2: 1D Noise

    ; Set the vertex
    (vertex x y)
    ; Increment x dimension for noise
    (set! xoff (+ xoff 0.05)))
  ; increment y dimension for noise
  (set! yoff (+ yoff 0.01))
  (vertex width height)
  (vertex 0 height)
  (end-shape CLOSE))
