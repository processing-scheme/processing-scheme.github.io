;; Circle Collision with Swapping Velocities
;; by Ira Greenberg.
;;
;; Based on Keith Peter's Solution in
;; Foundation Actionscript Animation: Making Things Move!

(import (processing))

(define-record-type ball (make-ball-record position velocity radius m)
  ball?
  (position ball-position set-ball-position!)
  (velocity ball-velocity set-ball-velocity!)
  (radius ball-radius set-ball-radius!)
  (m ball-m set-ball-m!))

(define (make-ball x y r)
  (let ((self (make-ball-record #f #f 0.0 0.0)))
    (set-ball-position! self (PVector x y))
    (set-ball-velocity! self (random2-d))
    (mult (ball-velocity self) 3)
    (set-ball-radius! self r)
    (set-ball-m! self (* (ball-radius self) 0.1))
    self))

(define (ball-update self) (add (ball-position self) (ball-velocity self)))

(define (ball-check-boundary-collision self)
  (cond
    ((> (vec-x (ball-position self)) (- width (ball-radius self)))
     (set-vec-x! (ball-position self) (- width (ball-radius self)))
     (set-vec-x! (ball-velocity self) (* (vec-x (ball-velocity self)) -1)))
    ((< (vec-x (ball-position self)) (ball-radius self))
     (set-vec-x! (ball-position self) (ball-radius self))
     (set-vec-x! (ball-velocity self) (* (vec-x (ball-velocity self)) -1)))
    ((> (vec-y (ball-position self)) (- height (ball-radius self)))
     (set-vec-y! (ball-position self) (- height (ball-radius self)))
     (set-vec-y! (ball-velocity self) (* (vec-y (ball-velocity self)) -1)))
    ((< (vec-y (ball-position self)) (ball-radius self))
     (set-vec-y! (ball-position self) (ball-radius self))
     (set-vec-y! (ball-velocity self) (* (vec-y (ball-velocity self)) -1)))))

(define (ball-check-collision self other)
  ; Get distances between the balls components
  (define distance-vect (sub (copy (ball-position other)) (ball-position self)))

  ; Calculate magnitude of the vector separating the balls
  (define distance-vect-mag (mag distance-vect))

  ; Minimum distance before they are touching
  (define min-distance (+ (ball-radius self) (ball-radius other)))

  (when (< distance-vect-mag min-distance)
    (define distance-correction (/ (- min-distance distance-vect-mag) 2.0))
    (define d (copy distance-vect))
    (define correction-vector (mult (normalize d) distance-correction))
    (add (ball-position other) correction-vector)
    (sub (ball-position self) correction-vector)

    ; get angle of distanceVect
    (define theta (heading distance-vect))
    ; precalculate trig values
    (define sine (sin theta))
    (define cosine (cos theta))

    ;; bTemp will hold rotated ball positions. You
    ;; just need to worry about bTemp[1] position
    (define b-temp (vector (PVector 0 0) (PVector 0 0)))

    ;; this ball's position is relative to the other
    ;; so you can use the vector between them (bVect) as the
    ;; reference point in the rotation expressions.
    ;; bTemp[0].position.x and bTemp[0].position.y will initialize
    ;; automatically to 0.0, which is what you want
    ;; since b[1] will rotate around b[0]
    (set-vec-x! (vector-ref b-temp 1)
                (+ (* cosine (vec-x distance-vect)) (* sine (vec-y distance-vect))))
    (set-vec-y! (vector-ref b-temp 1)
                (- (* cosine (vec-y distance-vect)) (* sine (vec-x distance-vect))))

    ; rotate Temporary velocities
    (define v-temp (vector (PVector 0 0) (PVector 0 0)))

    (set-vec-x! (vector-ref v-temp 0)
                (+ (* cosine (vec-x (ball-velocity self)))
                   (* sine (vec-y (ball-velocity self)))))
    (set-vec-y! (vector-ref v-temp 0)
                (- (* cosine (vec-y (ball-velocity self)))
                   (* sine (vec-x (ball-velocity self)))))
    (set-vec-x! (vector-ref v-temp 1)
                (+ (* cosine (vec-x (ball-velocity other)))
                   (* sine (vec-y (ball-velocity other)))))
    (set-vec-y! (vector-ref v-temp 1)
                (- (* cosine (vec-y (ball-velocity other)))
                   (* sine (vec-x (ball-velocity other)))))

    ;; Now that velocities are rotated, you can use 1D
    ;; conservation of momentum equations to calculate
    ;; the final velocity along the x-axis.
    (define v-final (vector (PVector 0 0) (PVector 0 0)))

    ; final rotated velocity for b[0]
    (set-vec-x! (vector-ref v-final 0)
                (/ (+ (* (- (ball-m self) (ball-m other)) (vec-x (vector-ref v-temp 0)))
                      (* 2 (ball-m other) (vec-x (vector-ref v-temp 1))))
                   (+ (ball-m self) (ball-m other))))
    (set-vec-y! (vector-ref v-final 0) (vec-y (vector-ref v-temp 0)))

    ; final rotated velocity for b[0]
    (set-vec-x! (vector-ref v-final 1)
                (/ (+ (* (- (ball-m other) (ball-m self)) (vec-x (vector-ref v-temp 1)))
                      (* 2 (ball-m self) (vec-x (vector-ref v-temp 0))))
                   (+ (ball-m self) (ball-m other))))
    (set-vec-y! (vector-ref v-final 1) (vec-y (vector-ref v-temp 1)))

    ; hack to avoid clumping
    (set-vec-x! (vector-ref b-temp 0)
                (+ (vec-x (vector-ref b-temp 0)) (vec-x (vector-ref v-final 0))))
    (set-vec-x! (vector-ref b-temp 1)
                (+ (vec-x (vector-ref b-temp 1)) (vec-x (vector-ref v-final 1))))

    ;; Rotate ball positions and velocities back
    ;; Reverse signs in trig expressions to rotate
    ;; in the opposite direction
    ; rotate balls
    (define b-final (vector (PVector 0 0) (PVector 0 0)))

    (set-vec-x! (vector-ref b-final 0)
                (- (* cosine (vec-x (vector-ref b-temp 0)))
                   (* sine (vec-y (vector-ref b-temp 0)))))
    (set-vec-y! (vector-ref b-final 0)
                (+ (* cosine (vec-y (vector-ref b-temp 0)))
                   (* sine (vec-x (vector-ref b-temp 0)))))
    (set-vec-x! (vector-ref b-final 1)
                (- (* cosine (vec-x (vector-ref b-temp 1)))
                   (* sine (vec-y (vector-ref b-temp 1)))))
    (set-vec-y! (vector-ref b-final 1)
                (+ (* cosine (vec-y (vector-ref b-temp 1)))
                   (* sine (vec-x (vector-ref b-temp 1)))))

    ; update balls to screen position
    (set-vec-x! (ball-position other)
                (+ (vec-x (ball-position self)) (vec-x (vector-ref b-final 1))))
    (set-vec-y! (ball-position other)
                (+ (vec-y (ball-position self)) (vec-y (vector-ref b-final 1))))

    (add (ball-position self) (vector-ref b-final 0))

    ; update velocities
    (set-vec-x! (ball-velocity self)
                (- (* cosine (vec-x (vector-ref v-final 0)))
                   (* sine (vec-y (vector-ref v-final 0)))))
    (set-vec-y! (ball-velocity self)
                (+ (* cosine (vec-y (vector-ref v-final 0)))
                   (* sine (vec-x (vector-ref v-final 0)))))
    (set-vec-x! (ball-velocity other)
                (- (* cosine (vec-x (vector-ref v-final 1)))
                   (* sine (vec-y (vector-ref v-final 1)))))
    (set-vec-y! (ball-velocity other)
                (+ (* cosine (vec-y (vector-ref v-final 1)))
                   (* sine (vec-x (vector-ref v-final 1)))))))

(define (ball-display self)
  (no-stroke)
  (fill 204)
  (ellipse (vec-x (ball-position self))
           (vec-y (ball-position self))
           (* (ball-radius self) 2)
           (* (ball-radius self) 2)))

(define balls (vector (make-ball 100 400 20) (make-ball 700 400 80)))

(define (setup) (size 640 360))

(define (draw)
  (background 51)

  (vector-for-each (lambda (b)
                     (ball-update b)
                     (ball-display b)
                     (ball-check-boundary-collision b))
                   balls)

  (ball-check-collision (vector-ref balls 0) (vector-ref balls 1)))
