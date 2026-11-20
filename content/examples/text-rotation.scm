;; Text Rotation.
;;
;; Draws letters to the screen and rotates them at different angles.

(import (processing))

(define f #f)
(define angle-rotate 0.0)

(define (setup)
  (size 640 360)
  (background 0)

  ; Create the font from the .ttf file in the data folder
  (set! f (create-font "SourceCodePro-Regular.ttf" 18))
  (text-font f))

(define (draw)
  (background 0)

  (stroke-weight 1)
  (stroke 153)

  (push-matrix)
  (define angle1 (radians 45))
  (translate 100 180)
  (rotate angle1)
  (text "45 DEGREES" 0 0)
  (line 0 0 150 0)
  (pop-matrix)

  (push-matrix)
  (define angle2 (radians 270))
  (translate 200 180)
  (rotate angle2)
  (text "270 DEGREES" 0 0)
  (line 0 0 150 0)
  (pop-matrix)

  (push-matrix)
  (translate 440 180)
  (rotate (radians angle-rotate))
  (text (string-append (str (remainder (int angle-rotate) 360)) " DEGREES") 0 0)
  (line 0 0 150 0)
  (pop-matrix)

  (set! angle-rotate (+ angle-rotate 0.25))

  (stroke 255 0 0)
  (stroke-weight 4)
  (point 100 180)
  (point 200 180)
  (point 440 180))
