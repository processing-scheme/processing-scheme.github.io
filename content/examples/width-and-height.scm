;; Width and Height.
;;
;; The 'width' and 'height' variables contain the width and height
;; of the display window as defined in the size() function.

(import (processing))

(define (setup) (size 640 360))

(define (draw)
  (background 127)
  (no-stroke)
  (do ((i 0 (+ i 20))) ((>= i height))
    (fill 129 206 15)
    (rect 0 i width 10)
    (fill 255)
    (rect i 0 10 height)))
