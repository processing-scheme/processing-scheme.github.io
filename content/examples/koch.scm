;; Koch Curve
;; by Daniel Shiffman.
;;
;; Renders a simple fractal, the Koch snowflake.
;; Each recursive level is drawn in sequence.

(import (processing))

; Koch Curve
; A class to manage the list of line segments in the snowflake pattern

(define-record-type koch-fractal (make-koch-fractal-record start end lines count)
  koch-fractal?
  (start koch-fractal-start set-koch-fractal-start!)
  (end koch-fractal-end set-koch-fractal-end!)
  (lines koch-fractal-lines set-koch-fractal-lines!)
  (count koch-fractal-count set-koch-fractal-count!))

; A PVector for the start
; A PVector for the end
; A list to keep track of all the lines
(define (make-koch-fractal)
  (let ((self (make-koch-fractal-record #f #f #f 0)))
    (set-koch-fractal-start! self (PVector 0 (- height 20)))
    (set-koch-fractal-end! self (PVector width (- height 20)))
    (set-koch-fractal-lines! self (list))
    (koch-fractal-restart self)
    self))

(define (koch-fractal-next-level self)
  ; For every line that is in the arraylist
  ; create 4 more lines in a new arraylist
  (set-koch-fractal-lines! self (koch-fractal-iterate self (koch-fractal-lines self)))
  (set-koch-fractal-count! self (+ (koch-fractal-count self) 1)))

(define (koch-fractal-restart self)
  (set-koch-fractal-count! self 0)  ; Reset count
  (set-koch-fractal-lines! self (list))  ; Empty the array list
  ; Add the initial line (from one end PVector to the other)
  (set-koch-fractal-lines! self
                           (append (koch-fractal-lines self)
                                   (list (make-koch-line (koch-fractal-start self)
                                                         (koch-fractal-end self))))))

(define (koch-fractal-get-count self) (koch-fractal-count self))

; This is easy, just draw all the lines
(define (koch-fractal-render self)
  (for-each (lambda (l) (koch-line-display l)) (koch-fractal-lines self)))

; This is where the **MAGIC** happens
; Step 1: Create an empty arraylist
; Step 2: For every line currently in the arraylist
;   - calculate 4 line segments based on Koch algorithm
;   - add all 4 line segments into the new arraylist
; Step 3: Return the new arraylist and it becomes the list of line segments for the structure
; As we do this over and over again, each line gets broken into 4 lines, which gets broken into 4 lines, and so on. . .
(define (koch-fractal-iterate self before)
  (define now (list))  ; Create emtpy list
  (for-each (lambda (l)
              ; Calculate 5 koch PVectors (done for us by the line object)
              (define a (koch-line-start l))
              (define b (koch-line-kochleft l))
              (define c (koch-line-kochmiddle l))
              (define d (koch-line-kochright l))
              (define e (koch-line-end l))
              ; Make line segments between all the PVectors and add them
              (set! now (append now (list (make-koch-line a b))))
              (set! now (append now (list (make-koch-line b c))))
              (set! now (append now (list (make-koch-line c d))))
              (set! now (append now (list (make-koch-line d e)))))
            before)
  now)

; The Nature of Code
; Daniel Shiffman
; http://natureofcode.com

; Koch Curve
; A class to describe one line segment in the fractal
; Includes methods to calculate midPVectors along the line according to the Koch algorithm

(define-record-type koch-line (make-koch-line-record a b)
  koch-line?
  (a koch-line-a set-koch-line-a!)
  (b koch-line-b set-koch-line-b!))

; Two PVectors,
; a is the "left" PVector and
; b is the "right PVector
(define (make-koch-line start end)
  (let ((self (make-koch-line-record #f #f)))
    (set-koch-line-a! self (copy start))
    (set-koch-line-b! self (copy end))
    self))

(define (koch-line-display self)
  (stroke 255)
  (line (vec-x (koch-line-a self))
        (vec-y (koch-line-a self))
        (vec-x (koch-line-b self))
        (vec-y (koch-line-b self))))

(define (koch-line-start self) (copy (koch-line-a self)))

(define (koch-line-end self) (copy (koch-line-b self)))

; This is easy, just 1/3 of the way
(define (koch-line-kochleft self)
  (define v (sub (copy (koch-line-b self)) (koch-line-a self)))
  (div v 3)
  (add v (koch-line-a self))
  v)

; More complicated, have to use a little trig to figure out where this PVector is!
(define (koch-line-kochmiddle self)
  (define v (sub (copy (koch-line-b self)) (koch-line-a self)))
  (div v 3)

  (define p (copy (koch-line-a self)))
  (add p v)

  (rotate v (- (radians 60)))
  (add p v)

  p)

; Easy, just 2/3 of the way
(define (koch-line-kochright self)
  (define v (sub (copy (koch-line-a self)) (koch-line-b self)))
  (div v 3)
  (add v (koch-line-b self))
  v)

(define k #f)

(define (setup)
  (size 640 360)
  (frame-rate 1)  ; Animate slowly
  (set! k (make-koch-fractal)))

(define (draw)
  (background 0)
  ; Draws the snowflake!
  (koch-fractal-render k)
  ; Iterate
  (koch-fractal-next-level k)
  ; Let's not do it more than 5 times. . .
  (when (> (koch-fractal-get-count k) 5) (koch-fractal-restart k)))
