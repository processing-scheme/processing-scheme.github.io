;; Flocking
;; by Daniel Shiffman.
;;
;; An implementation of Craig Reynold's Boids program to simulate
;; the flocking behavior of birds. Each boid steers itself based on
;; rules of avoidance, alignment, and coherence.
;;
;; Click the mouse to add a new boid.

(import (processing))

; The Boid class

(define-record-type boid (make-boid-record position velocity acceleration r maxforce maxspeed)
  boid?
  (position boid-position set-boid-position!)
  (velocity boid-velocity set-boid-velocity!)
  (acceleration boid-acceleration set-boid-acceleration!)
  (r boid-r set-boid-r!)
  (maxforce boid-maxforce set-boid-maxforce!)
  (maxspeed boid-maxspeed set-boid-maxspeed!))

; Maximum steering force
; Maximum speed
(define (make-boid x y)
  (let ((self (make-boid-record #f #f #f 0.0 0.0 0.0)))
    (set-boid-acceleration! self (PVector 0 0))

    ; This is a new PVector method not yet implemented in JS
    ; velocity = PVector.random2D();

    ; Leaving the code temporarily this way so that this example runs in JS
    (define angle (random TWO-PI))
    (set-boid-velocity! self (PVector (cos angle) (sin angle)))

    (set-boid-position! self (PVector x y))
    (set-boid-r! self 2.0)
    (set-boid-maxspeed! self 2)
    (set-boid-maxforce! self 0.03)
    self))

(define (boid-run self boids)
  (boid-flock self boids)
  (boid-update self)
  (boid-borders self)
  (boid-render self))

(define (boid-apply-force self force)
  ; We could add mass here if we want A = F / M
  (add (boid-acceleration self) force))

; We accumulate a new acceleration each time based on three rules
(define (boid-flock self boids)
  (define sep (boid-separate self boids))  ; Separation
  (define ali (boid-align self boids))  ; Alignment
  (define coh (boid-cohesion self boids))  ; Cohesion
  ; Arbitrarily weight these forces
  (mult sep 1.5)
  (mult ali 1.0)
  (mult coh 1.0)
  ; Add the force vectors to acceleration
  (boid-apply-force self sep)
  (boid-apply-force self ali)
  (boid-apply-force self coh))

; Method to update position
(define (boid-update self)
  ; Update velocity
  (add (boid-velocity self) (boid-acceleration self))
  ; Limit speed
  (limit (boid-velocity self) (boid-maxspeed self))
  (add (boid-position self) (boid-velocity self))
  ; Reset accelertion to 0 each cycle
  (mult (boid-acceleration self) 0))

; A method that calculates and applies a steering force towards a target
; STEER = DESIRED MINUS VELOCITY
(define (boid-seek self target)
  (define desired (sub (copy target) (boid-position self)))  ; A vector pointing from the position to the target
  ; Scale to maximum speed
  (normalize desired)
  (mult desired (boid-maxspeed self))

  ; Above two lines of code below could be condensed with new PVector setMag() method
  ; Not using this method until Processing.js catches up
  ; desired.setMag(maxspeed);

  ; Steering = Desired minus Velocity
  (define steer (sub (copy desired) (boid-velocity self)))
  (limit steer (boid-maxforce self))  ; Limit to maximum steering force
  steer)

(define (boid-render self)
  ; Draw a triangle rotated in the direction of velocity
  (define theta (+ (heading (boid-velocity self)) (radians 90)))
  ; heading2D() above is now heading() but leaving old syntax until Processing.js catches up

  (fill 200 100)
  (stroke 255)
  (push-matrix)
  (translate (vec-x (boid-position self)) (vec-y (boid-position self)))
  (rotate theta)
  (begin-shape TRIANGLES)
  (vertex 0 (* (- (boid-r self)) 2))
  (vertex (- (boid-r self)) (* (boid-r self) 2))
  (vertex (boid-r self) (* (boid-r self) 2))
  (end-shape)
  (pop-matrix))

; Wraparound
(define (boid-borders self)
  (when (< (vec-x (boid-position self)) (- (boid-r self)))
    (set-vec-x! (boid-position self) (+ width (boid-r self))))
  (when (< (vec-y (boid-position self)) (- (boid-r self)))
    (set-vec-y! (boid-position self) (+ height (boid-r self))))
  (when (> (vec-x (boid-position self)) (+ width (boid-r self)))
    (set-vec-x! (boid-position self) (- (boid-r self))))
  (when (> (vec-y (boid-position self)) (+ height (boid-r self)))
    (set-vec-y! (boid-position self) (- (boid-r self)))))

; Separation
; Method checks for nearby boids and steers away
(define (boid-separate self boids)
  (define desiredseparation 25.0)
  (define steer (PVector 0 0 0))
  (define count 0)
  ; For every boid in the system, check if it's too close
  (for-each (lambda (other)
              (define d (dist (boid-position self) (boid-position other)))
              ; If the distance is greater than 0 and less than an arbitrary amount (0 when you are yourself)
              (when (and (> d 0) (< d desiredseparation))
                ; Calculate vector pointing away from neighbor
                (define diff (sub (copy (boid-position self)) (boid-position other)))
                (normalize diff)
                (div diff d)  ; Weight by distance
                (add steer diff)
                ; Keep track of how many
                (set! count (+ count 1))))
            boids)
  ; Average -- divide by how many
  (when (> count 0) (div steer (float count)))

  ; As long as the vector is greater than 0
  (when (> (mag steer) 0)
    ; First two lines of code below could be condensed with new PVector setMag() method
    ; Not using this method until Processing.js catches up
    ; steer.setMag(maxspeed);

    ; Implement Reynolds: Steering = Desired - Velocity
    (normalize steer)
    (mult steer (boid-maxspeed self))
    (sub steer (boid-velocity self))
    (limit steer (boid-maxforce self)))
  steer)

; Alignment
; For every nearby boid in the system, calculate the average velocity
(define (boid-align self boids)
  (define neighbordist 50)
  (define sum (PVector 0 0))
  (define count 0)
  (for-each (lambda (other)
              (define d (dist (boid-position self) (boid-position other)))
              (when (and (> d 0) (< d neighbordist))
                (add sum (boid-velocity other))
                (set! count (+ count 1))))
            boids)
  (cond
    ((> count 0)
     (div sum (float count))
     ; First two lines of code below could be condensed with new PVector setMag() method
     ; Not using this method until Processing.js catches up
     ; sum.setMag(maxspeed);

     ; Implement Reynolds: Steering = Desired - Velocity
     (normalize sum)
     (mult sum (boid-maxspeed self))
     (define steer (sub (copy sum) (boid-velocity self)))
     (limit steer (boid-maxforce self))
     steer)
    (else (PVector 0 0))))

; Cohesion
; For the average position (i.e. center) of all nearby boids, calculate steering vector towards that position
(define (boid-cohesion self boids)
  (define neighbordist 50)
  (define sum (PVector 0 0))  ; Start with empty vector to accumulate all positions
  (define count 0)
  (for-each (lambda (other)
              (define d (dist (boid-position self) (boid-position other)))
              (when (and (> d 0) (< d neighbordist))
                (add sum (boid-position other))  ; Add position
                (set! count (+ count 1))))
            boids)
  (cond
    ((> count 0)
     (div sum count)
     ; Steer towards the position
     (boid-seek self sum))
    (else (PVector 0 0))))

; The Flock (a list of Boid objects)

(define-record-type flock (make-flock-record boids)
  flock?
  (boids flock-boids set-flock-boids!))

; An ArrayList for all the boids
(define (make-flock)
  (let ((self (make-flock-record #f)))
    (set-flock-boids! self (list))  ; Initialize the ArrayList
    self))

(define (flock-run self)
  (for-each (lambda (b)
              ; Passing the entire list of boids to each boid individually
              (boid-run b (flock-boids self)))
            (flock-boids self)))

(define (flock-add-boid self b)
  (set-flock-boids! self (append (flock-boids self) (list b))))

(define flock #f)

(define (setup)
  (size 640 360)
  (set! flock (make-flock))
  ; Add an initial set of boids into the system
  (do ((i 0 (+ i 1))) ((>= i 150))
    (flock-add-boid flock (make-boid (/ width 2) (/ height 2)))))

(define (draw) (background 50) (flock-run flock))

; Add a new boid into the System
(define (mouse-pressed) (flock-add-boid flock (make-boid mouse-x mouse-y)))
