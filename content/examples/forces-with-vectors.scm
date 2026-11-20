;; Forces (Gravity and Fluid Resistence) with Vectors
;; by Daniel Shiffman.
;;
;; Demonstration of multiple forces acting on bodies.
;; Bodies experience gravity continuously and fluid
;; resistance when in "water".
; Five moving bodies

(import (processing))

;; Forces (Gravity and Fluid Resistence) with Vectors
;; by Daniel Shiffman.
;;
;; Demonstration of multiple force acting on bodies (Mover class)
;; Bodies experience gravity continuously
;; Bodies experience fluid resistance when in "water"

(define-record-type liquid (make-liquid-record x y w h c)
  liquid?
  (x liquid-x set-liquid-x!)
  (y liquid-y set-liquid-y!)
  (w liquid-w set-liquid-w!)
  (h liquid-h set-liquid-h!)
  (c liquid-c set-liquid-c!))

; Liquid is a rectangle
; Coefficient of drag
(define (make-liquid x y w h c)
  (let ((self (make-liquid-record 0.0 0.0 0.0 0.0 0.0)))
    (set-liquid-x! self x)
    (set-liquid-y! self y)
    (set-liquid-w! self w)
    (set-liquid-h! self h)
    (set-liquid-c! self c)
    self))

; Is the Mover in the Liquid?
(define (liquid-contains self m)
  (define l (mover-position m))
  (if (and (> (vec-x l) (liquid-x self))
        (< (vec-x l) (+ (liquid-x self) (liquid-w self)))
        (> (vec-y l) (liquid-y self))
        (< (vec-y l) (+ (liquid-y self) (liquid-h self))))
    #t
    #f))

; Calculate drag force
(define (liquid-drag self m)
  ; Magnitude is coefficient * speed squared
  (define speed (mag (mover-velocity m)))
  (define drag-magnitude (* (liquid-c self) speed speed))

  ; Direction is inverse of velocity
  (define drag (copy (mover-velocity m)))
  (mult drag -1)

  ; Scale according to magnitude
  (set-mag drag drag-magnitude)
  drag)

(define (liquid-display self)
  (no-stroke)
  (fill 127)
  (rect (liquid-x self) (liquid-y self) (liquid-w self) (liquid-h self)))

;; Forces (Gravity and Fluid Resistence) with Vectors
;; by Daniel Shiffman.
;;
;; Demonstration of multiple force acting on bodies (Mover class)
;; Bodies experience gravity continuously
;; Bodies experience fluid resistance when in "water"

(define-record-type mover (make-mover-record position velocity acceleration mass)
  mover?
  (position mover-position set-mover-position!)
  (velocity mover-velocity set-mover-velocity!)
  (acceleration mover-acceleration set-mover-acceleration!)
  (mass mover-mass set-mover-mass!))

; position, velocity, and acceleration
; Mass is tied to size
(define (make-mover m x y)
  (let ((self (make-mover-record #f #f #f 0.0)))
    (set-mover-mass! self m)
    (set-mover-position! self (PVector x y))
    (set-mover-velocity! self (PVector 0 0))
    (set-mover-acceleration! self (PVector 0 0))
    self))

; Newton's 2nd law: F = M * A
; or A = F / M
(define (mover-apply-force self force)
  ; Divide by mass
  (define f (div (copy force) (mover-mass self)))
  ; Accumulate all forces in acceleration
  (add (mover-acceleration self) f))

(define (mover-update self)
  ; Velocity changes according to acceleration
  (add (mover-velocity self) (mover-acceleration self))
  ; position changes by velocity
  (add (mover-position self) (mover-velocity self))
  ; We must clear acceleration each frame
  (mult (mover-acceleration self) 0))

; Draw Mover
(define (mover-display self)
  (stroke 255)
  (stroke-weight 2)
  (fill 255 200)
  (ellipse (vec-x (mover-position self))
           (vec-y (mover-position self))
           (* (mover-mass self) 16)
           (* (mover-mass self) 16)))

; Bounce off bottom of window
(define (mover-check-edges self)
  (when (> (vec-y (mover-position self)) height)
    (set-vec-y! (mover-velocity self) (* (vec-y (mover-velocity self)) -0.9))  ; A little dampening when hitting the bottom
    (set-vec-y! (mover-position self) height)))

(define movers (make-vector 10 #f))

; Liquid
(define liquid #f)

(define (setup)
  (size 640 360)
  (reset)
  ; Create liquid object
  (set! liquid (make-liquid 0 (/ height 2) width (/ height 2) 0.1)))

(define (draw)
  (background 0)

  ; Draw water
  (liquid-display liquid)

  (vector-for-each (lambda (mover)
                     ; Is the Mover in the liquid?
                     (when (liquid-contains liquid mover)
                       ; Calculate drag force
                       (define drag (liquid-drag liquid mover))
                       ; Apply drag force to Mover
                       (mover-apply-force mover drag))

                     ; Gravity is scaled by mass here!
                     (define gravity (PVector 0 (* 0.1 (mover-mass mover))))
                     ; Apply gravity
                     (mover-apply-force mover gravity)

                     ; Update and display
                     (mover-update mover)
                     (mover-display mover)
                     (mover-check-edges mover))
                   movers)

  (fill 255)
  (text "click mouse to reset" 10 30))

(define (mouse-pressed) (reset))

; Restart all the Mover objects randomly
(define (reset)
  (do ((i 0 (+ i 1))) ((>= i (vector-length movers)))
    (vector-set! movers i (make-mover (random 0.5 3) (+ 40 (* i 70)) 0))))
