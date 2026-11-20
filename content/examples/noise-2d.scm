;; Noise2D
;; by Daniel Shiffman.
;;
;; Using 2D noise to create simple texture.

(import (processing))

(define increment 0.02)

(define (setup) (size 640 360))

(define (draw)
  (load-pixels)

  (define xoff 0.0)  ; Start xoff at 0
  (define detail (map mouse-x 0 width 0.1 0.6))
  (noise-detail 8 detail)

  ; For every x,y coordinate in a 2D space, calculate a noise value and produce a brightness value
  (do ((x 0 (+ x 1))) ((>= x width))
    (set! xoff (+ xoff increment))  ; Increment xoff
    (define yoff 0.0)  ; For every xoff, start yoff at 0
    (do ((y 0 (+ y 1))) ((>= y height))
      (set! yoff (+ yoff increment))  ; Increment yoff

      ; Calculate noise and scale by 255
      (define bright (* (noise xoff yoff) 255))

      ; Try using this line instead
      ; float bright = random(0,255);

      ; Set each pixel onscreen to a grayscale value
      (vector-set! pixels (+ x (* y width)) (color bright))))

  (update-pixels))
