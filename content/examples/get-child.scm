;; Get Child.
;;
;; SVG files can be made of many individual shapes.
;; Each of these shapes (called a "child") has its own name
;; that can be used to extract it from the "parent" file.
;; This example loads a map of the United States and creates
;; two new PShape objects by extracting the data from two states.

(import (processing))

(define usa #f)
(define michigan #f)
(define ohio #f)

(define (setup)
  (size 640 360)
  (set! usa (load-shape "usa-wikipedia.svg"))
  (set! michigan (get-child usa "MI"))
  (set! ohio (get-child usa "OH")))

(define (draw)
  (background 255)

  ; Draw the full map
  (shape usa -600 -180)

  ; Disable the colors found in the SVG file
  (disable-style michigan)
  ; Set our own coloring
  (fill 0 51 102)
  (no-stroke)
  ; Draw a single state
  (shape michigan -600 -180)  ; Wolverines!

  ; Disable the colors found in the SVG file
  (disable-style ohio)
  ; Set our own coloring
  (fill 153 0 0)
  (no-stroke)
  ; Draw a single state
  ; Buckeyes!
  (shape ohio -600 -180))
