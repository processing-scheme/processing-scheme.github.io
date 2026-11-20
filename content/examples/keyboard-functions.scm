;; Keyboard Functions
;; by Martin Gomez
;;
;; Click on the window to give it focus and press the letter keys to type colors.
;; The keyboard function keyPressed() is called whenever
;; a key is pressed. keyReleased() is another keyboard
;; function that is called when a key is released.
;;
;; Original 'Color Typewriter' concept by John Maeda.

(import (processing))

(define max-height 40)
(define min-height 20)
(define letter-height max-height)  ; Height of the letters
(define letter-width 20)  ; Width of the letter

(define x (- letter-width))  ; X position of the letters
(define y 0)  ; Y position of the letters

(define newletter #f)

(define num-chars 26)  ; There are 26 characters in the alphabet
(define colors (make-vector num-chars 0))

(define (setup)
  (size 640 360)
  (no-stroke)
  (color-mode HSB num-chars)
  (background (/ num-chars 2))
  ; Set a hue value for each key
  (do ((i 0 (+ i 1))) ((>= i num-chars))
    (vector-set! colors i (color i num-chars num-chars))))

(define (draw)
  (when (eqv? newletter #t)
    ; Draw the "letter"
    (define y-pos 0)
    (cond
      ((= letter-height max-height)
       (set! y-pos y)
       (rect x y-pos letter-width letter-height))
      (else
        (set! y-pos (+ y min-height))
        (rect x y-pos letter-width letter-height)
        (fill (/ num-chars 2))
        (rect x (- y-pos min-height) letter-width letter-height)))
    (set! newletter #f)))

(define (key-pressed)
  ; If the key is between 'A'(65) to 'Z' and 'a' to 'z'(122)
  (cond
    ((or (and (>= (char->integer key) (char->integer #\A))
              (<= (char->integer key) (char->integer #\Z)))
         (and (>= (char->integer key) (char->integer #\a))
              (<= (char->integer key) (char->integer #\z))))
     (define key-index 0)
     (cond
       ((<= (char->integer key) (char->integer #\Z))
        (set! key-index (- (char->integer key) (char->integer #\A)))
        (set! letter-height max-height)
        (fill (vector-ref colors key-index)))
       (else
         (set! key-index (- (char->integer key) (char->integer #\a)))
         (set! letter-height min-height)
         (fill (vector-ref colors key-index)))))
    (else (fill 0) (set! letter-height 10)))

  (set! newletter #t)

  ; Update the "letter" position
  (set! x (+ x letter-width))

  ; Wrap horizontally
  (when (> x (- width letter-width)) (set! x 0) (set! y (+ y max-height)))

  ; Wrap vertically
  (when (> y (- height letter-height))
    ; reset y to 0
    (set! y 0)))
