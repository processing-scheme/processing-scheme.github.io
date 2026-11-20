;; Create Graphics.
;;
;; The createGraphics() function creates an object from
;; the PGraphics class. PGraphics is the main graphics and
;; rendering context for Processing. The beginDraw() method
;; is necessary to prepare for drawing and endDraw() is
;; necessary to finish. Use this class if you need to draw
;; into an off-screen graphics buffer or to maintain two
;; drawing surfaces with different properties.

(import (processing))

(define pg #f)

(define (setup)
  (size 640 360)
  (set! pg (create-graphics 400 200)))

(define (draw)
  (fill 0 12)
  (rect 0 0 width height)
  (fill 255)
  (no-stroke)
  (ellipse mouse-x mouse-y 60 60)

  (begin-draw pg)
  (background pg 51)
  (no-fill pg)
  (stroke pg 255)
  (ellipse pg (- mouse-x 120) (- mouse-y 60) 60 60)
  (end-draw pg)

  ; Draw the offscreen buffer to the screen with image()
  (image pg 120 60))
