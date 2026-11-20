;; Non-orthogonal Reflection
;; by Ira Greenberg.
;;
;; Based on the equation (R = 2N(N*L)-L) where R is the
;; reflection vector, N is the normal, and L is the incident
;; vector.
; Position of left hand side of floor

(import (processing))

(define base1 #f)
; Position of right hand side of floor
(define base2 #f)
; Length of floor
(define base-length 0.0)

; An array of subpoints along the floor path
(define coords #f)

; Variables related to moving ball
(define position #f)
(define velocity #f)
(define r 6)
(define speed 3.5)

(define (setup)
  (size 640 360)

  (fill 128)
  (set! base1 (PVector 0 (- height 150)))
  (set! base2 (PVector width height))
  (create-ground)

  ; start ellipse at middle top of screen
  (set! position (PVector (/ width 2) 0))

  ; calculate initial random velocity
  (set! velocity (random2-d))
  (mult velocity speed))

(define (draw)
  ; draw background
  (fill 0 12)
  (no-stroke)
  (rect 0 0 width height)

  ; draw base
  (fill 200)
  (quad (vec-x base1)
        (vec-y base1)
        (vec-x base2)
        (vec-y base2)
        (vec-x base2)
        height
        0
        height)

  ; calculate base top normal
  (define base-delta (sub (copy base2) base1))
  (normalize base-delta)
  (define normal (PVector (- (vec-y base-delta)) (vec-x base-delta)))

  ; draw ellipse
  (no-stroke)
  (fill 255)
  (ellipse (vec-x position) (vec-y position) (* r 2) (* r 2))

  ; move elipse
  (add position velocity)

  ; normalized incidence vector
  (define incidence (mult (copy velocity) -1))
  (normalize incidence)

  ; detect and handle collision
  (do ((i 0 (+ i 1))) ((>= i (vector-length coords)))
    ; check distance between ellipse and base top coordinates
    (when (< (dist position (vector-ref coords i)) r)
      ; calculate dot product of incident vector and base top normal
      (define dot-v (dot incidence normal))

      ; calculate reflection vector
      ; assign reflection vector to direction vector
      (set velocity
           (- (* 2 (vec-x normal) dot-v) (vec-x incidence))
           (- (* 2 (vec-y normal) dot-v) (vec-y incidence))
           0)
      (mult velocity speed)

      ; draw base top normal at collision point
      (stroke 255 128 0)
      (line (vec-x position)
            (vec-y position)
            (- (vec-x position) (* (vec-x normal) 100))
            (- (vec-y position) (* (vec-y normal) 100)))))

  ; detect boundary collision
  ; right
  (when (> (vec-x position) (- width r))
    (set-vec-x! position (- width r))
    (set-vec-x! velocity (* (vec-x velocity) -1)))
  ; left
  (when (< (vec-x position) r)
    (set-vec-x! position r)
    (set-vec-x! velocity (* (vec-x velocity) -1)))
  ; top
  (when (< (vec-y position) r)
    (set-vec-y! position r)
    (set-vec-y! velocity (* (vec-y velocity) -1))
    ; randomize base top
    (set-vec-y! base1 (random (- height 100) height))
    (set-vec-y! base2 (random (- height 100) height))
    (create-ground)))

; Calculate variables for the ground
(define (create-ground)
  ; calculate length of base top
  (set! base-length (dist base1 base2))

  ; fill base top coordinate array
  (set! coords (make-vector (ceil base-length) #f))
  (do ((i 0 (+ i 1))) ((>= i (vector-length coords)))
    (vector-set! coords i (PVector 0 0))
    (set-vec-x! (vector-ref coords i)
                (+ (vec-x base1) (* (/ (- (vec-x base2) (vec-x base1)) base-length) i)))
    (set-vec-y! (vector-ref coords i)
                (+ (vec-y base1) (* (/ (- (vec-y base2) (vec-y base1)) base-length) i)))))
