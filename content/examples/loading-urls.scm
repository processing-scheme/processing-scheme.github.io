;; Loading URLs.
;;
;; Click on the button to open a URL in a browser.

(import (processing))

(define over-button #f)

(define (setup) (size 640 360))

(define (draw)
  (background 204)

  (if (eqv? over-button #t) (fill 255) (no-fill))
  (rect 105 60 75 75)
  (line 135 105 155 85)
  (line 140 85 155 85)
  (line 155 85 155 100))

(define (mouse-pressed) (when over-button (link "http://www.processing.org")))

(define (mouse-moved) (check-buttons))

(define (mouse-dragged) (check-buttons))

(define (check-buttons)
  (if (and (> mouse-x 105) (< mouse-x 180) (> mouse-y 60) (< mouse-y 135))
    (set! over-button #t)
    (set! over-button #f)))
