;; Redraw.
;;
;; The redraw() function makes draw() execute once.
;; In this example, draw() is executed once every time
;; the mouse is clicked.

(import (processing))

(define y 0.0)

; The statements in the setup() function
; execute once when the program begins
(define (setup)
  (size 640 360)  ; Size should be the first statement
  (stroke 255)  ; Set line drawing color to white
  (no-loop)
  (set! y (* height 0.5)))

; The code in draw() is run until the program
; is stopped. Each statement is executed in
; sequence and after the last line is read,
; the first line is run again.
(define (draw)
  (background 0)  ; Set the background to black
  (set! y (- y 4))
  (when (< y 0) (set! y height))
  (line 0 y width y))

(define (mouse-pressed) (redraw))
