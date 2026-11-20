;; Keyboard.
;;
;; Click on the image to give it focus and press the letter keys
;; to create forms in time and space. Each key has a unique identifying
;; number. These numbers can be used to position shapes in space.

(import (processing))

(define rect-width 0)

(define (setup)
  (size 640 360)
  (no-stroke)
  (background 0)
  (set! rect-width (quotient width 4)))

(define (draw)
  ; keep draw() here to continue looping while waiting for keys
  )

(define (key-pressed)
  (define key-index -1)
  (cond
    ((and (>= (char->integer key) (char->integer #\A))
          (<= (char->integer key) (char->integer #\Z)))
     (set! key-index (- (char->integer key) (char->integer #\A))))
    ((and (>= (char->integer key) (char->integer #\a))
          (<= (char->integer key) (char->integer #\z)))
     (set! key-index (- (char->integer key) (char->integer #\a)))))
  (cond
    ((= key-index -1)
     ; If it's not a letter key, clear the screen
     (background 0))
    (else
      ; It's a letter key, fill a rectangle
      (fill (remainder (millis) 255))
      (define x (map key-index 0 25 0 (- width rect-width)))
      (rect x 0 rect-width height))))
