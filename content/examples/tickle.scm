;; Tickle.
;;
;; The word "tickle" jitters when the cursor hovers over.
;; Sometimes, it can be tickled off the screen.

(import (processing))

(define message "tickle")
(define x 0.0)
(define y 0.0)  ; X and Y coordinates of text
(define hr 0.0)
(define vr 0.0)  ; horizontal and vertical radius of the text

(define (setup)
  (size 640 360)

  ; Create the font
  (text-font (create-font "SourceCodePro-Regular.ttf" 36))
  (text-align CENTER CENTER)

  (set! hr (/ (text-width message) 2))
  (set! vr (/ (+ (text-ascent) (text-descent)) 2))
  (no-stroke)
  (set! x (/ width 2))
  (set! y (/ height 2)))

(define (draw)
  ; Instead of clearing the background, fade it by drawing
  ; a semi-transparent rectangle on top
  (fill 204 120)
  (rect 0 0 width height)

  ; If the cursor is over the text, change the position
  (when (and (< (abs (- mouse-x x)) hr) (< (abs (- mouse-y y)) vr))
    (set! x (+ x (random -5 5)))
    (set! y (+ y (random -5 5))))
  (fill 0)
  (text "tickle" x y))
