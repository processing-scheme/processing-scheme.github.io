;; Blur.
;;
;; This program analyzes every pixel in an image and blends it with the
;; neighboring pixels to blur the image.
;;
;; This is an example of an "image convolution" using a kernel (small matrix)
;; to analyze and transform a pixel based on the values of its neighbors.
;;
;; Image blur is also called a "low-pass filter".  Pixels of low frequency
;; change (similar brightness as neighbors) are left mostly unchanged, while
;; those with high frequency change (sharply different values) are smoothed
;; out.
;;
;; The kernel here is a Box Blur, in which all components are equally valued.
;; Another common blur is "Gaussian Blur", in which pixels nearer the center
;; of the kernel have more weight than those further away.
;;
;; An example 3x3 Gaussian kernel might be:          [ 1  2  1 ]
;; 1/16 * [ 2  4  2 ]
;; [ 1  2  1 ]
;;
;; An example 5x5 kernel, which creates a greater blur effect:
;; [ 1   4   6   4  1 ]
;; [ 4  16  24  16  4 ]
;; 1/256 * [ 6  24  36  24  6 ]
;; [ 4  16  24  16  4 ]
;; [ 1   4   6   4  1 ]

(import (processing))

(define v (/ 1.0 9.0))
(define kernel (vector (vector v v v) (vector v v v) (vector v v v)))

(define img #f)

(define (setup)
  (size 640 360)
  (set! img (load-image "moon.jpg"))  ; Load the original image
  (no-loop))

(define (draw)
  (image img 0 0)  ; Displays the image from point (0,0)
  (load-pixels img)

  ; Create an opaque image of the same size as the original
  (define blur-img (create-image (width img) (height img) RGB))

  ; Loop through every pixel in the image
  (do ((y 1 (+ y 1))) ((>= y (- (height img) 1)))
    ; Skip top and bottom edges
    (do ((x 1 (+ x 1))) ((>= x (- (width img) 1)))
      ; Skip left and right edges
      (define sum-red 0)  ; Kernel sums for this pixel
      (define sum-green 0)
      (define sum-blue 0)
      (do ((ky -1 (+ ky 1))) ((> ky 1))
        (do ((kx -1 (+ kx 1))) ((> kx 1))
          ; Calculate the adjacent pixel for this kernel point
          (define pos (+ (* (+ y ky) (width img)) x kx))

          ; Process each channel separately, Red first.
          (define val-red (red (vector-ref (pixels img) pos)))
          ; Multiply adjacent pixels based on the kernel values
          (set! sum-red
                (+ sum-red
                   (* (vector-ref (vector-ref kernel (+ ky 1)) (+ kx 1)) val-red)))

          ; Green
          (define val-green (green (vector-ref (pixels img) pos)))
          (set! sum-green
                (+ sum-green
                   (* (vector-ref (vector-ref kernel (+ ky 1)) (+ kx 1)) val-green)))

          ; Blue
          (define val-blue (blue (vector-ref (pixels img) pos)))
          (set! sum-blue
                (+ sum-blue
                   (* (vector-ref (vector-ref kernel (+ ky 1)) (+ kx 1)) val-blue)))))
      ; For this pixel in the new image, set the output value
      ; based on the sum from the kernel
      (vector-set! (pixels blur-img)
                   (+ (* y (width blur-img)) x)
                   (color sum-red sum-green sum-blue))))
  ; State that there are changes to blurImg.pixels[]
  (update-pixels blur-img)

  ; Draw the new image
  (image blur-img (/ width 2) 0))
