;; Brightness Pixels
;; by Daniel Shiffman.
;;
;; This program adjusts the brightness of a part of the image by
;; calculating the distance of each pixel to the mouse.

(import (processing))

(define img #f)

(define (setup)
  (size 640 360)
  (frame-rate 30)
  (set! img (load-image "moon-wide.jpg"))
  (load-pixels img)
  ; Only need to load the pixels[] array once, because we're only
  ; manipulating pixels[] inside draw(), not drawing shapes.
  (load-pixels))

(define (draw)
  (do ((x 0 (+ x 1))) ((>= x (width img)))
    (do ((y 0 (+ y 1))) ((>= y (height img)))
      ; Calculate the 1D location from a 2D grid
      (define loc (+ x (* y (width img))))
      ; Get the R,G,B values from image
      (define r 0.0)
      (define g 0.0)
      (define b 0.0)
      (set! r (red (vector-ref (pixels img) loc)))
      ; g = green (img.pixels[loc]);
      ; b = blue (img.pixels[loc]);
      ; Calculate an amount to change brightness based on proximity to the mouse
      (define maxdist 50)  ; dist(0,0,width,height);
      (define d (dist x y mouse-x mouse-y))
      (define adjustbrightness (/ (* 255 (- maxdist d)) maxdist))
      (set! r (+ r adjustbrightness))
      ; g += adjustbrightness;
      ; b += adjustbrightness;
      ; Constrain RGB to make sure they are within 0-255 color range
      (set! r (constrain r 0 255))
      ; g = constrain(g, 0, 255);
      ; b = constrain(b, 0, 255);
      ; Make a new color and set pixel in the window
      ; color c = color(r, g, b);
      (define c (color r))
      (vector-set! pixels (+ (* y width) x) c)))
  (update-pixels))
