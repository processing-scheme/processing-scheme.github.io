;; Convolution
;; by Daniel Shiffman.
;;
;; Applies a convolution matrix to a portion of an image. Move mouse to
;; apply filter to different parts of the image. Click mouse to cycle
;; through different effects (kernels).

(import (processing))

(define img #f)
(define effect 0)
(define w 120)

; It's possible to convolve the image with many different
; matrices to produce different effects.  Here are some
; example kernels to try.
(define identity (vector (vector 0 0 0) (vector 0 1 0) (vector 0 0 0)))

(define darken (vector (vector 0 0 0) (vector 0 0.5 0) (vector 0 0 0)))

(define lighten (vector (vector 0 0 0) (vector 0 2 0) (vector 0 0 0)))

(define sharpen (vector (vector 0 -1 0) (vector -1 5 -1) (vector 0 -1 0)))

(define sharpen2 (vector (vector -1 -1 -1) (vector -1 9 -1) (vector -1 -1 -1)))

(define box-blur
  (vector (vector (/ 1.0 9.0) (/ 1.0 9.0) (/ 1.0 9.0))
          (vector (/ 1.0 9.0) (/ 1.0 9.0) (/ 1.0 9.0))
          (vector (/ 1.0 9.0) (/ 1.0 9.0) (/ 1.0 9.0))))

(define edge-det (vector (vector 0 1 0) (vector 1 -4 1) (vector 0 1 0)))

(define emboss (vector (vector -2 -1 0) (vector -1 1 1) (vector 0 1 2)))

; collect the kernels and names into arrays for our program
(define kernels
  (vector identity darken lighten sharpen sharpen2 box-blur edge-det emboss))

(define effect-names
  (vector "Identity (no change)"
          "Darken"
          "Lighten"
          "Sharpen"
          "Sharpen More"
          "Box Blur"
          "Edge Detect"
          "Emboss"))

;
(define (setup)
  (size 640 360)
  (set! img (load-image "moon-wide.jpg"))

  (no-loop))

; Clicking the mouse advances to the next effect
(define (mouse-pressed)
  (set! effect (+ effect 1))
  (when (>= effect (vector-length effect-names)) (set! effect 0))

  (redraw))

; Moving the mouse triggers a screen redraw
(define (mouse-moved) (redraw))

(define (mouse-dragged) (redraw))

(define (draw)
  ; We're only going to process a portion of the image
  ; so let's set the whole image as the background first
  (image img 0 0)

  ; Calculate the small rectangle we will process
  (define xstart (constrain (- mouse-x (/ w 2)) 0 (width img)))
  (define ystart (constrain (- mouse-y (/ w 2)) 0 (height img)))
  (define xend (constrain (+ mouse-x (/ w 2)) 0 (width img)))
  (define yend (constrain (+ mouse-y (/ w 2)) 0 (height img)))
  (define matrixsize 3)
  (load-pixels)
  ; Begin our loop for every pixel in the smaller image
  (do ((x xstart (+ x 1))) ((>= x xend))
    (do ((y ystart (+ y 1))) ((>= y yend))
      (define c (convolution x y (vector-ref kernels effect) matrixsize img))
      (define loc (+ x (* y (width img))))
      (vector-set! pixels loc c)))
  (update-pixels)

  (text-size 24)
  (text (vector-ref effect-names effect) 4 24))

(define (convolution x y matrix matrixsize img)
  (define rtotal 0.0)
  (define gtotal 0.0)
  (define btotal 0.0)
  (define offset (quotient matrixsize 2))
  (do ((i 0 (+ i 1))) ((>= i matrixsize))
    (do ((j 0 (+ j 1))) ((>= j matrixsize))
      ; What pixel are we testing
      (define xloc (- (+ x i) offset))
      (define yloc (- (+ y j) offset))
      (define loc (+ xloc (* (width img) yloc)))
      ; Make sure we haven't walked off our image, we could do better here
      (set! loc (constrain loc 0 (- (vector-length (pixels img)) 1)))
      ; Calculate the convolution
      (set! rtotal
            (+ rtotal
               (* (red (vector-ref (pixels img) loc))
                  (vector-ref (vector-ref matrix i) j))))
      (set! gtotal
            (+ gtotal
               (* (green (vector-ref (pixels img) loc))
                  (vector-ref (vector-ref matrix i) j))))
      (set! btotal
            (+ btotal
               (* (blue (vector-ref (pixels img) loc))
                  (vector-ref (vector-ref matrix i) j))))))
  ; Make sure RGB is within range
  (set! rtotal (constrain rtotal 0 255))
  (set! gtotal (constrain gtotal 0 255))
  (set! btotal (constrain btotal 0 255))
  ; Return the resulting color
  (color rtotal gtotal btotal))
