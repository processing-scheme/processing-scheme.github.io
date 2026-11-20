;; Edge Detection.
;;
;; This program analyzes every pixel in an image and compares it with thee
;; neighboring pixels to identify edges.
;;
;; This is an example of an "image convolution" using a kernel (small matrix)
;; to analyze and transform a pixel based on the values of its neighbors.
;;
;; This kernel describes a "Laplacian Edge Detector".  It is effective,
;; but sensitive to noise.  One common enhancement is to add a Gaussian
;; blur to the source image first, as in
;; grayImg.filter(BLUR);
;; to reduce impact of noise on the output.  The combination is often called
;; "Laplace of Gaussian", or "LoG" for short.
;;
;; For weaker detection effect, try this kernel:     [  0  -1   0 ]
;; [ -1   4  -1 ]
;; [  0  -1   0 ]

(import (processing))

(define kernel (vector (vector -1 -1 -1) (vector -1 8 -1) (vector -1 -1 -1)))

(define img #f)

(define (setup)
  (size 640 360)
  (set! img (load-image "moon.jpg"))  ; Load the original image
  (no-loop))

(define (draw)
  (image img 0 0)  ; Displays the image from point (0,0)
  (load-pixels img)

  ; Edge detection should be done on a grayscale image.
  ;  Create a copy of the source image, and convert to gray.
  (define gray-img (copy img))
  (filter gray-img GRAY)
  ; grayImg.filter(BLUR);

  ; Create an opaque image of the same size as the original
  (define edge-img (create-image (width gray-img) (height gray-img) RGB))

  ; Loop through every pixel in the image
  (do ((y 1 (+ y 1))) ((>= y (- (height gray-img) 1)))
    ; Skip top and bottom edges
    (do ((x 1 (+ x 1))) ((>= x (- (width gray-img) 1)))
      ; Skip left and right edges
      ; Output of this filter is shown as offset from 50% gray.
      ; This preserves transitions from low (dark) to high (light) value.
      ; Starting from zero will show only high edges on black instead.
      (define sum 128)
      (do ((ky -1 (+ ky 1))) ((> ky 1))
        (do ((kx -1 (+ kx 1))) ((> kx 1))
          ; Calculate the adjacent pixel for this kernel point
          (define pos (+ (* (+ y ky) (width gray-img)) x kx))

          ; Image is grayscale, red/green/blue are identical
          (define val (blue (vector-ref (pixels gray-img) pos)))
          ; Multiply adjacent pixels based on the kernel values
          (set! sum (+ sum (* (vector-ref (vector-ref kernel (+ ky 1)) (+ kx 1)) val)))))
      ; For this pixel in the new image, set the output value
      ; based on the sum from the kernel
      (vector-set! (pixels edge-img) (+ (* y (width edge-img)) x) (color sum))))
  ; State that there are changes to edgeImg.pixels[]
  (update-pixels edge-img)

  ; Draw the new image
  (image edge-img (/ width 2) 0))
