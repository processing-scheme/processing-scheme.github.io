;; Morph.
;;
;; Changing one shape into another by interpolating
;; vertices from one to another
; Two ArrayLists to store the vertices for two shapes
; This example assumes that each shape will have the same
; number of vertices, i.e. the size of each ArrayList will be the same

(import (processing))

(define circle (list))
(define square (list))

; An ArrayList for a third set of vertices, the ones we will be drawing
; in the window
(define morph (list))

; This boolean variable will control if we are morphing to a circle or square
(define state #f)

(define (setup)
  (size 640 360)

  ; Create a circle using vectors pointing from center
  (do ((angle 0 (+ angle 9))) ((>= angle 360))
    ; Note we are not starting from 0 in order to match the
    ; path of a circle.
    (define v (from-angle (radians (- angle 135))))
    (mult v 100)
    (set! circle (append circle (list v)))
    ; Let's fill out morph ArrayList with blank PVectors while we are at it
    (set! morph (append morph (list (PVector 0 0)))))

  ; A square is a bunch of vertices along straight lines
  ; Top of square
  (do ((x -50 (+ x 10))) ((>= x 50))
    (set! square (append square (list (PVector x -50)))))
  ; Right side
  (do ((y -50 (+ y 10))) ((>= y 50))
    (set! square (append square (list (PVector 50 y)))))
  ; Bottom
  (do ((x 50 (- x 10))) ((<= x -50))
    (set! square (append square (list (PVector x 50)))))
  ; Left side
  (do ((y 50 (- y 10))) ((<= y -50))
    (set! square (append square (list (PVector -50 y))))))

(define (draw)
  (background 51)

  ; We will keep how far the vertices are from their target
  (define total-distance 0)

  ; Look at each vertex
  (do ((i 0 (+ i 1))) ((>= i (length circle)))
    (define v1 #f)
    ; Are we lerping to the circle or square?
    (if state (set! v1 (list-ref circle i)) (set! v1 (list-ref square i)))
    ; Get the vertex we will draw
    (define v2 (list-ref morph i))
    ; Lerp to the target
    (lerp v2 v1 0.1)
    ; Check how far we are from target
    (set! total-distance (+ total-distance (dist v1 v2))))

  ; If all the vertices are close, switch shape
  (when (< total-distance 0.1) (set! state (not state)))

  ; Draw relative to center
  (translate (/ width 2) (/ height 2))
  (stroke-weight 4)
  ; Draw a polygon that makes up all the vertices
  (begin-shape)
  (no-fill)
  (stroke 255)
  (for-each (lambda (v) (vertex (vec-x v) (vec-y v))) morph)
  (end-shape CLOSE))
