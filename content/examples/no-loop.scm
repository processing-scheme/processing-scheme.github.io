;; No Loop.
;;
;; The noLoop() function causes draw() to only run once.
;; Without calling noLoop(), the code inside draw() is
;; run continually.

(import (processing))

(define y 180)

; The statements in the setup() block
; run once when the program begins
(define (setup)
  (size 640 360)  ; Size should be the first statement
  (stroke 255)  ; Set line drawing color to white
  (no-loop))

; In this example, the code in the draw() block
; runs only once because of the noLoop() in setup()
(define (draw)
  (background 0)  ; Set the background to black
  (line 0 y width y)
  (set! y (- y 1))
  (when (< y 0) (set! y height)))
