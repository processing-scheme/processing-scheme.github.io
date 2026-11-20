;; Wolfram Cellular Automata
;; by Daniel Shiffman.
;;
;; Simple demonstration of a Wolfram's 1-dimensional
;; cellular automata. When the system reaches bottom
;; of the window, it restarts with a new ruleset.
;; Mouse click restarts as well.

(import (processing))

(define-record-type CA (make-CA-record cells generation scl rules)
  CA?
  (cells CA-cells set-CA-cells!)
  (generation CA-generation set-CA-generation!)
  (scl CA-scl set-CA-scl!)
  (rules CA-rules set-CA-rules!))

; An array of 0s and 1s
; How many generations?
; How many pixels wide/high is each cell?
; Array to store the rules, for example {0,1,1,0,1,1,0,1}
(define (make-CA r)
  (let ((self (make-CA-record #f 0 0 #f)))
    (set-CA-rules! self r)
    (set-CA-scl! self 1)
    (set-CA-cells! self (make-vector (quotient width (CA-scl self)) 0))
    (CA-restart self)
    self))

; Set the rules of the CA
(define (CA-set-rules self r) (set-CA-rules! self r))

; Make a random ruleset
(define (CA-randomize self)
  (do ((i 0 (+ i 1))) ((>= i 8))
    (vector-set! (CA-rules self) i (int (random 2)))))

; Reset to generation 0
(define (CA-restart self)
  (do ((i 0 (+ i 1))) ((>= i (vector-length (CA-cells self))))
    (vector-set! (CA-cells self) i 0))
  ; We arbitrarily start with just the middle
  ; cell having a state of "1"
  (vector-set! (CA-cells self) (quotient (vector-length (CA-cells self)) 2) 1)
  (set-CA-generation! self 0))

; The process of creating the new generation
(define (CA-generate self)
  ; First we create an empty array for the new values
  (define nextgen (make-vector (vector-length (CA-cells self)) 0))
  ; For every spot, determine new state by examing current
  ; state, and neighbor states
  ; Ignore edges that only have one neighor
  (do ((i 1 (+ i 1))) ((>= i (- (vector-length (CA-cells self)) 1)))
    (define left (vector-ref (CA-cells self) (- i 1)))  ; Left neighbor state
    (define me (vector-ref (CA-cells self) i))  ; Current state
    (define right (vector-ref (CA-cells self) (+ i 1)))  ; Right neighbor state
    ; Compute next generation state based on ruleset
    (vector-set! nextgen i (CA-execute-rules self left me right)))
  ; Copy the array into current value
  (do ((i 1 (+ i 1))) ((>= i (- (vector-length (CA-cells self)) 1)))
    (vector-set! (CA-cells self) i (vector-ref nextgen i)))
  ; cells = (int[]) nextgen.clone();
  (set-CA-generation! self (+ (CA-generation self) 1)))

; This is the easy part, just draw the cells,
; fill 255 for '1', fill 0 for '0'
(define (CA-render self)
  (do ((i 0 (+ i 1))) ((>= i (vector-length (CA-cells self))))
    (if (= (vector-ref (CA-cells self) i) 1) (fill 255) (fill 0))
    (no-stroke)
    (rect (* i (CA-scl self))
          (* (CA-generation self) (CA-scl self))
          (CA-scl self)
          (CA-scl self))))

; Implementing the Wolfram rules
; Could be improved and made more concise,
; but here we can explicitly see what is going on for each case
(define (CA-execute-rules self a b c)
  (cond
    ((and (= a 1) (= b 1) (= c 1)) (vector-ref (CA-rules self) 0))
    ((and (= a 1) (= b 1) (= c 0)) (vector-ref (CA-rules self) 1))
    ((and (= a 1) (= b 0) (= c 1)) (vector-ref (CA-rules self) 2))
    ((and (= a 1) (= b 0) (= c 0)) (vector-ref (CA-rules self) 3))
    ((and (= a 0) (= b 1) (= c 1)) (vector-ref (CA-rules self) 4))
    ((and (= a 0) (= b 1) (= c 0)) (vector-ref (CA-rules self) 5))
    ((and (= a 0) (= b 0) (= c 1)) (vector-ref (CA-rules self) 6))
    ((and (= a 0) (= b 0) (= c 0)) (vector-ref (CA-rules self) 7))
    (else 0)))

; The CA is done if it reaches the bottom of the screen
(define (CA-finished self) (if (> (CA-generation self) (/ height (CA-scl self))) #t #f))

(define ca #f)  ; An instance object to the cellular automata

(define (setup)
  (size 640 360)
  (define ruleset (vector 0 1 0 1 1 0 1 0))  ; An initial rule system
  (set! ca (make-CA ruleset))  ; Initialize CA
  (background 0))

(define (draw)
  (CA-render ca)  ; Draw the CA
  (CA-generate ca)  ; Generate the next level

  ; If we're done, clear the screen,
  ; pick a new ruleset and restart
  (when (CA-finished ca) (background 0) (CA-randomize ca) (CA-restart ca)))

(define (mouse-pressed)
  (background 0)
  (CA-randomize ca)
  (CA-restart ca))
