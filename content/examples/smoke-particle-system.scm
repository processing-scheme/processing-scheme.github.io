;; Smoke Particle System
;; by Daniel Shiffman.
;;
;; A basic smoke effect using a particle system. Each particle
;; is rendered as an alpha masked image.

(import (processing))

; the list without its i-th item
(define (remove-at xs i)
  (concat (subset xs 0 i) (subset xs (+ i 1))))

; A simple Particle class, renders the particle as an image

(define-record-type particle (make-particle-record loc vel acc lifespan img)
  particle?
  (loc particle-loc set-particle-loc!)
  (vel particle-vel set-particle-vel!)
  (acc particle-acc set-particle-acc!)
  (lifespan particle-lifespan set-particle-lifespan!)
  (img particle-img set-particle-img!))

(define (make-particle l img)
  (let ((self (make-particle-record #f #f #f 0.0 #f)))
    (set-particle-acc! self (PVector 0 0))
    (define vx (* (random-gaussian) 0.3))
    (define vy (- (* (random-gaussian) 0.3) 1.0))
    (set-particle-vel! self (PVector vx vy))
    (set-particle-loc! self (copy l))
    (set-particle-lifespan! self 100.0)
    (set-particle-img! self img)
    self))

(define (particle-run self)
  (particle-update self)
  (particle-render self))

; Method to apply a force vector to the Particle object
; Note we are ignoring "mass" here
(define (particle-apply-force self f) (add (particle-acc self) f))

; Method to update position
(define (particle-update self)
  (add (particle-vel self) (particle-acc self))
  (add (particle-loc self) (particle-vel self))
  (set-particle-lifespan! self (- (particle-lifespan self) 2.5))
  ; clear Acceleration
  (mult (particle-acc self) 0))

; Method to display
(define (particle-render self)
  (image-mode CENTER)
  (tint 255 (particle-lifespan self))
  (image (particle-img self) (vec-x (particle-loc self)) (vec-y (particle-loc self)))
  ; Drawing a circle instead
  ; fill(255,lifespan);
  ; noStroke();
  ; ellipse(loc.x,loc.y,img.width,img.height);
  )

; Is the particle still useful?
(define (particle-is-dead self) (if (<= (particle-lifespan self) 0.0) #t #f))

; A class to describe a group of Particles
; An ArrayList is used to manage the list of Particles

(define-record-type particle-system (make-particle-system-record particles origin img)
  particle-system?
  (particles particle-system-particles set-particle-system-particles!)
  (origin particle-system-origin set-particle-system-origin!)
  (img particle-system-img set-particle-system-img!))

; An arraylist for all the particles
; An origin point for where particles are birthed
(define (make-particle-system num v img)
  (let ((self (make-particle-system-record #f #f #f)))
    (set-particle-system-particles! self (list))  ; Initialize the arraylist
    (set-particle-system-origin! self (copy v))  ; Store the origin point
    (set-particle-system-img! self img)
    (do ((i 0 (+ i 1))) ((>= i num))
      ; Add "num" amount of particles to the arraylist
      (set-particle-system-particles! self
                                      (append (particle-system-particles self)
                                              (list (make-particle
                                                      (particle-system-origin self)
                                                      (particle-system-img self))))))
    self))

(define (particle-system-run self)
  (do ((i (- (length (particle-system-particles self)) 1) (- i 1))) ((< i 0))
    (define p (list-ref (particle-system-particles self) i))
    (particle-run p)
    (when (particle-is-dead p)
      (set-particle-system-particles! self
                                      (remove-at (particle-system-particles self) i)))))

; Method to add a force vector to all particles currently in the system
(define (particle-system-apply-force self dir)
  ; Enhanced loop!!!
  (for-each (lambda (p) (particle-apply-force p dir)) (particle-system-particles self)))

(define (particle-system-add-particle self)
  (set-particle-system-particles! self
                                  (append (particle-system-particles self)
                                          (list (make-particle
                                                  (particle-system-origin self)
                                                  (particle-system-img self))))))

(define ps #f)

(define (setup)
  (size 640 360)
  (define img (load-image "texture.png"))
  (set! ps (make-particle-system 0 (PVector (/ width 2) (- height 60)) img)))

(define (draw)
  (background 0)

  ; Calculate a "wind" force based on mouse horizontal position
  (define dx (map mouse-x 0 width -0.2 0.2))
  (define wind (PVector dx 0))
  (particle-system-apply-force ps wind)
  (particle-system-run ps)
  (do ((i 0 (+ i 1))) ((>= i 2))
    (particle-system-add-particle ps))

  ; Draw an arrow representing the wind force
  (draw-vector wind (PVector (/ width 2) 50 0) 500))

; Renders a vector object 'v' as an arrow and a position 'loc'
(define (draw-vector v loc scayl)
  (push-matrix)
  (define arrowsize 4)
  ; Translate to position to render vector
  (translate (vec-x loc) (vec-y loc))
  (stroke 255)
  ; Call vector heading function to get direction (note that pointing up is a heading of 0) and rotate
  (rotate (heading v))
  ; Calculate length of vector & scale it to be bigger or smaller if necessary
  (define len (* (mag v) scayl))
  ; Draw three lines to make an arrow (draw pointing up since we've rotate to the proper direction)
  (line 0 0 len 0)
  (line len 0 (- len arrowsize) (/ arrowsize 2))
  (line len 0 (- len arrowsize) (/ (- arrowsize) 2))
  (pop-matrix))
