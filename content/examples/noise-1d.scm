;; Noise1D.
;;
;; Using 1D Perlin Noise to assign location.

(import (processing))

(define xoff 0.0)
(define xincrement 0.01)

(define (setup) (size 640 360) (background 0) (no-stroke))

(define (draw)
  ; Create an alpha blended background
  (fill 0 10)
  (rect 0 0 width height)

  ; float n = random(0,width);  // Try this line instead of noise

  ; Get a noise value based on xoff and scale it according to the window's width
  (define n (* (noise xoff) width))

  ; With each cycle, increment xoff
  (set! xoff (+ xoff xincrement))

  ; Draw the ellipse at the value produced by perlin noise
  (fill 200)
  (ellipse n (/ height 2) 64 64))
