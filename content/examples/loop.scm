;; Loop.
;;
;; If noLoop() is run in setup(), the code in draw()
;; is only run once. In this example, click the mouse
;; to run the loop() function to cause the draw() the
;; run continuously.

(import (processing))

(define y 180)

; The statements in the setup() function
; run once when the program begins
(define (setup)
  (size 640 360)  ; Size should be the first statement
  (stroke 255)  ; Set stroke color to white
  (no-loop))

(define (draw)
  (background 0)  ; Set the background to black
  (line 0 y width y)
  (set! y (- y 1))
  (when (< y 0) (set! y height)))

(define (mouse-pressed) (loop))
