;; Disable Style
;; by George Brower.
;;
;; Shapes are loaded with style information that tells them how
;; to draw (e.g. color, stroke weight). The disableStyle()
;; method of PShape turns off this information so functions like
;; stroke() and fill() change the SVGs color. The enableStyle()
;; method turns the file's original styles back on.

(import (processing))

(define bot #f)

(define (setup)
  (size 640 360)
  ; The file "bot1.svg" must be in the data folder
  ; of the current sketch to load successfully
  (set! bot (load-shape "bot1.svg"))
  (no-loop))

(define (draw)
  (background 102)

  ; Draw left bot
  (disable-style bot)  ; Ignore the colors in the SVG
  (fill 0 102 153)  ; Set the SVG fill to blue
  (stroke 255)  ; Set the SVG fill to white
  (shape bot 20 25 300 300)

  ; Draw right bot
  (enable-style bot)
  (shape bot 320 25 300 300))
