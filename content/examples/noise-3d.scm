;; Noise3D.
;;
;; Using 3D noise to create simple animated texture.
;; Here, the third dimension ('z') is treated as time.

(import (processing))

(define increment 0.01)
; The noise function's 3rd argument, a global variable that increments once per cycle
(define zoff 0.0)
; We will increment zoff differently than xoff and yoff
(define zincrement 0.02)

(define (setup) (size 640 360) (frame-rate 30))

(define (draw)
  ; Optional: adjust noise detail here
  ; noiseDetail(8,0.65f);

  (load-pixels)

  (define xoff 0.0)  ; Start xoff at 0

  ; For every x,y coordinate in a 2D space, calculate a noise value and produce a brightness value
  (do ((x 0 (+ x 1))) ((>= x width))
    (set! xoff (+ xoff increment))  ; Increment xoff
    (define yoff 0.0)  ; For every xoff, start yoff at 0
    (do ((y 0 (+ y 1))) ((>= y height))
      (set! yoff (+ yoff increment))  ; Increment yoff

      ; Calculate noise and scale by 255
      (define bright (* (noise xoff yoff zoff) 255))

      ; Try using this line instead
      ; float bright = random(0,255);

      ; Set each pixel onscreen to a grayscale value
      (vector-set! pixels (+ x (* y width)) (color bright bright bright))))
  (update-pixels)

  ; Increment zoff
  (set! zoff (+ zoff zincrement)))
