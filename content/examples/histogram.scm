;; Histogram.
;;
;; Calculates the histogram of an image.
;; A histogram is the frequency distribution
;; of the gray levels with the number of pure black values
;; displayed on the left and number of pure white values on the right.
;;
;; Note that this sketch will behave differently on Android,
;; since most images will no longer be full 24-bit color.

(import (processing))

(define (setup)
  (size 640 360)

  ; Load an image from the data directory
  ; Load a different image by modifying the comments
  (define img (load-image "frontier.jpg"))
  (image img 0 0)
  (define hist (make-vector 256 0))

  ; Calculate the histogram
  (do ((i 0 (+ i 1))) ((>= i (width img)))
    (do ((j 0 (+ j 1))) ((>= j (height img)))
      (define bright (int (brightness (get i j))))
      (vector-set! hist bright (+ (vector-ref hist bright) 1))))

  ; Find the largest value in the histogram
  (define hist-max (apply max (vector->list hist)))

  (stroke 255)
  ; Draw half of the histogram (skip every second value)
  (do ((i 0 (+ i 2))) ((>= i (width img)))
    ; Map i (from 0..img.width) to a location in the histogram (0..255)
    (define which (int (map i 0 (width img) 0 255)))
    ; Convert the histogram value to a location between
    ; the bottom and the top of the picture
    (define y (int (map (vector-ref hist which) 0 hist-max (height img) 0)))
    (line i (height img) i y)))
