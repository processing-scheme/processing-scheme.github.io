;; Multiple Particle Systems
;; by Daniel Shiffman.
;;
;; Click the mouse to generate a burst of particles
;; at mouse position.
;;
;; Each burst is one instance of a particle system
;; with Particles and CrazyParticles (a subclass of Particle).
;; Note use of Inheritance and Polymorphism.

(import (processing))

; the list without its i-th item
(define (remove-at xs i)
  (concat (subset xs 0 i) (subset xs (+ i 1))))

; A subclass of Particle

(define-record-type crazy-particle (make-crazy-particle-record position velocity acceleration lifespan theta)
  crazy-particle?
  (position crazy-particle-position set-crazy-particle-position!)
  (velocity crazy-particle-velocity set-crazy-particle-velocity!)
  (acceleration crazy-particle-acceleration set-crazy-particle-acceleration!)
  (lifespan crazy-particle-lifespan set-crazy-particle-lifespan!)
  (theta crazy-particle-theta set-crazy-particle-theta!))

; Just adding one new variable to a CrazyParticle
; It inherits all other fields from "Particle", and we don't have to retype them!
; The CrazyParticle constructor can call the parent class (super class) constructor
(define (make-crazy-particle l)
  (let ((self (make-crazy-particle-record #f #f #f 0.0 0.0)))
    (define base (make-particle l))
    (set-crazy-particle-position! self (particle-position base))
    (set-crazy-particle-velocity! self (particle-velocity base))
    (set-crazy-particle-acceleration! self (particle-acceleration base))
    (set-crazy-particle-lifespan! self (particle-lifespan base))
    ; "super" means do everything from the constructor in Particle

    ; One more line of code to deal with the new variable, theta
    (set-crazy-particle-theta! self 0.0)
    self))

; Notice we don't have the method run() here; it is inherited from Particle
; This update() method overrides the parent class update() method
(define (crazy-particle-update self)
  (particle-update self)
  ; Increment rotation based on horizontal velocity
  (define theta-vel
    (/ (* (vec-x (particle-velocity self)) (mag (particle-velocity self))) 10.0))
  (set-crazy-particle-theta! self (+ (crazy-particle-theta self) theta-vel)))

; This display() method overrides the parent class display() method
(define (crazy-particle-display self)
  ; Render the ellipse just like in a regular particle
  (particle-display self)
  ; Then add a rotating line
  (push-matrix)
  (translate (vec-x (particle-position self)) (vec-y (particle-position self)))
  (rotate (crazy-particle-theta self))
  (stroke 255 (particle-lifespan self))
  (line 0 0 25 0)
  (pop-matrix))

; A simple Particle class

(define-record-type particle (make-particle-record position velocity acceleration lifespan)
  particle?
  (position particle-position set-particle-position!)
  (velocity particle-velocity set-particle-velocity!)
  (acceleration particle-acceleration set-particle-acceleration!)
  (lifespan particle-lifespan set-particle-lifespan!))

(define (make-particle l)
  (let ((self (make-particle-record #f #f #f 0.0)))
    (set-particle-acceleration! self (PVector 0 0.05))
    (set-particle-velocity! self (PVector (random -1 1) (random -2 0)))
    (set-particle-position! self (copy l))
    (set-particle-lifespan! self 255.0)
    self))

(define (particle-run self)
  (particle-update self)
  (particle-display self))

; Method to update position
(define (particle-update self)
  (add (particle-velocity self) (particle-acceleration self))
  (add (particle-position self) (particle-velocity self))
  (set-particle-lifespan! self (- (particle-lifespan self) 2.0)))

; Method to display
(define (particle-display self)
  (stroke 255 (particle-lifespan self))
  (fill 255 (particle-lifespan self))
  (ellipse (vec-x (particle-position self)) (vec-y (particle-position self)) 8 8))

; Is the particle still useful?
(define (particle-is-dead self) (< (particle-lifespan self) 0.0))

; An ArrayList is used to manage the list of Particles

(define-record-type particle-system (make-particle-system-record particles origin)
  particle-system?
  (particles particle-system-particles set-particle-system-particles!)
  (origin particle-system-origin set-particle-system-origin!))

; An arraylist for all the particles
; An origin point for where particles are birthed
(define (make-particle-system num v)
  (let ((self (make-particle-system-record #f #f)))
    (set-particle-system-particles! self (list))  ; Initialize the arraylist
    (set-particle-system-origin! self (copy v))  ; Store the origin point
    (do ((i 0 (+ i 1))) ((>= i num))
      ; Add "num" amount of particles to the arraylist
      (set-particle-system-particles! self
                                      (append (particle-system-particles self)
                                              (list (make-particle
                                                      (particle-system-origin self))))))
    self))

(define (particle-system-run self)
  ; Cycle through the ArrayList backwards, because we are deleting while iterating
  (do ((i (- (length (particle-system-particles self)) 1) (- i 1))) ((< i 0))
    (define p (list-ref (particle-system-particles self) i))
    (particle-run p)
    (when (particle-is-dead p)
      (set-particle-system-particles! self
                                      (remove-at (particle-system-particles self) i)))))

(define (particle-system-add-particle0 self)
  (define p #f)
  ; Add either a Particle or CrazyParticle to the system
  (if (= (int (random 0 2)) 0)
    (set! p (make-particle (particle-system-origin self)))
    (set! p (make-crazy-particle (particle-system-origin self))))
  (set-particle-system-particles! self
                                  (append (particle-system-particles self) (list p))))

(define (particle-system-add-particle self p)
  (set-particle-system-particles! self
                                  (append (particle-system-particles self) (list p))))

; A method to test if the particle system still has particles
(define (particle-system-dead self) (null? (particle-system-particles self)))

(define systems #f)

(define (setup) (size 640 360) (set! systems (list)))

(define (draw)
  (background 0)
  (for-each (lambda (ps) (particle-system-run ps) (particle-system-add-particle0 ps))
            systems)
  (when (null? systems)
    (fill 255)
    (text-align CENTER)
    (text "click mouse to add particle systems" (/ width 2) (/ height 2))))

(define (mouse-pressed)
  (set! systems
        (append systems (list (make-particle-system 1 (PVector mouse-x mouse-y))))))
