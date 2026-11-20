;; Shape Vertices.
;;
;; How to iterate over the vertices of a shape.
;; When loading an obj or SVG, getVertexCount()
;; will typically return 0 since all the vertices
;; are in the child shapes.
;;
;; You should iterate through the children and then
;; iterate through their vertices.
; The shape

(import (processing))

(define uk #f)

(define (setup)
  (size 640 360)
  ; Load the shape
  (set! uk (load-shape "uk.svg")))

(define (draw)
  (background 51)
  ; Center where we will draw all the vertices
  (translate (- (/ width 2) (/ (width uk) 2)) (- (/ height 2) (/ (height uk) 2)))

  ; Iterate over the children
  (define children (get-child-count uk))
  (do ((i 0 (+ i 1))) ((>= i children))
    (define child (get-child uk i))
    (define total (get-vertex-count child))

    ; Now we can actually get the vertices from each child
    (do ((j 0 (+ j 1))) ((>= j total))
      (define v (get-vertex child j))
      ; Cycling brightness for each vertex
      (stroke (remainder (+ frame-count (* (+ i 1) j)) 255))
      ; Just a dot for each one
      (point (vec-x v) (vec-y v)))))
