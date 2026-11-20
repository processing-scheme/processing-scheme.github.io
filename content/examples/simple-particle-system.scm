;; Simple Particle System
;; by Daniel Shiffman.
;;
;; Particles are generated each cycle through draw(),
;; fall with gravity, and fade out over time.
;; A ParticleSystem object manages a variable size (ArrayList)
;; list of particles.

(import (processing))

; the list without its i-th item
(define (remove-at xs i)
  (concat (subset xs 0 i) (subset xs (+ i 1))))

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
  (set-particle-lifespan! self (- (particle-lifespan self) 1.0)))

; Method to display
(define (particle-display self)
  (stroke 255 (particle-lifespan self))
  (fill 255 (particle-lifespan self))
  (ellipse (vec-x (particle-position self)) (vec-y (particle-position self)) 8 8))

; Is the particle still useful?
(define (particle-is-dead self) (if (< (particle-lifespan self) 0.0) #t #f))

; A class to describe a group of Particles
; An ArrayList is used to manage the list of Particles

(define-record-type particle-system (make-particle-system-record particles origin)
  particle-system?
  (particles particle-system-particles set-particle-system-particles!)
  (origin particle-system-origin set-particle-system-origin!))

(define (make-particle-system position)
  (let ((self (make-particle-system-record #f #f)))
    (set-particle-system-origin! self (copy position))
    (set-particle-system-particles! self (list))
    self))

(define (particle-system-add-particle self)
  (set-particle-system-particles! self
                                  (append (particle-system-particles self)
                                          (list (make-particle
                                                  (particle-system-origin self))))))

(define (particle-system-run self)
  (do ((i (- (length (particle-system-particles self)) 1) (- i 1))) ((< i 0))
    (define p (list-ref (particle-system-particles self) i))
    (particle-run p)
    (when (particle-is-dead p)
      (set-particle-system-particles! self
                                      (remove-at (particle-system-particles self) i)))))

(define ps #f)

(define (setup)
  (size 640 360)
  (set! ps (make-particle-system (PVector (/ width 2) 50))))

(define (draw)
  (background 0)
  (particle-system-add-particle ps)
  (particle-system-run ps))
