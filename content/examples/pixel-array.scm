;; Pixel Array.
;;
;; Click and drag the mouse up and down to control the signal and
;; press and hold any key to see the current pixel being read.
;; This program sequentially reads the color of every pixel of an image
;; and displays this color to fill the window.

(import (processing))

(define img #f)
(define direction 1)
(define signal 0.0)

(define (setup)
  (size 640 360)
  (no-fill)
  (stroke 255)
  (frame-rate 30)
  (set! img (load-image "sea.jpg")))

(define (draw)
  (when (or (> signal (- (* (width img) (height img)) 1)) (< signal 0))
    (set! direction (* direction -1)))

  (cond
    (mouse-is-pressed (define mx (constrain mouse-x 0 (- (width img) 1)))
                      (define my (constrain mouse-y 0 (- (height img) 1)))
                      (set! signal (+ (* my (width img)) mx)))
    (else (set! signal (+ signal (* 0.33 direction)))))

  (define sx (remainder (int signal) (width img)))
  (define sy (quotient (int signal) (width img)))

  (cond
    (key-is-pressed
      (set 0 0 img)  ; fast way to draw an image
      (point sx sy)
      (rect (- sx 5) (- sy 5) 10 10))
    (else (define c (get img sx sy)) (background c))))
