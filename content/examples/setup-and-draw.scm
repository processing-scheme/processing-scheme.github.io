;; Setup and Draw.
;;
;; The code inside the draw() function runs continuously
;; from top to bottom until the program is stopped. The
;; code in setup() is run once when the program starts.

(import (processing))

(define y 180)

; The statements in the setup() block run once
; when the program begins
(define (setup)
  (size 640 360)  ; Size must be the first statement
  ; Set line drawing color to white
  (stroke 255))

; The statements in draw() are run until the program
; is stopped. Each statement is run in sequence from top
; to bottom and after the last line is read, the
; first line is run again.
(define (draw)
  (background 0)  ; Clear the screen with a black background
  (line 0 y width y)
  (set! y (- y 1))
  (when (< y 0) (set! y height)))
