;; Non-orthogonal Collision with Multiple Ground Segments
;; by Ira Greenberg.
;;
;; Based on Keith Peter's Solution in
;; Foundation Actionscript Animation: Making Things Move!

(import (processing))

(define-record-type ground (make-ground-record x1 y1 x2 y2 x y len rot)
  ground?
  (x1 ground-x1 set-ground-x1!)
  (y1 ground-y1 set-ground-y1!)
  (x2 ground-x2 set-ground-x2!)
  (y2 ground-y2 set-ground-y2!)
  (x ground-x set-ground-x!)
  (y ground-y set-ground-y!)
  (len ground-len set-ground-len!)
  (rot ground-rot set-ground-rot!))

; Constructor
(define (make-ground x1 y1 x2 y2)
  (let ((self (make-ground-record 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)))
    (set-ground-x1! self x1)
    (set-ground-y1! self y1)
    (set-ground-x2! self x2)
    (set-ground-y2! self y2)
    (set-ground-x! self (/ (+ x1 x2) 2))
    (set-ground-y! self (/ (+ y1 y2) 2))
    (set-ground-len! self (dist x1 y1 x2 y2))
    (set-ground-rot! self (atan2 (- y2 y1) (- x2 x1)))
    self))

(define-record-type orb (make-orb-record position velocity r damping)
  orb?
  (position orb-position set-orb-position!)
  (velocity orb-velocity set-orb-velocity!)
  (r orb-r set-orb-r!)
  (damping orb-damping set-orb-damping!))

; Orb has positio and velocity
; A damping of 80% slows it down when it hits the ground
(define (make-orb x y r)
  (let ((self (make-orb-record #f #f 0.0 0.0)))
    (set-orb-damping! self 0.8)
    (set-orb-position! self (PVector x y))
    (set-orb-velocity! self (PVector 0.5 0))
    (set-orb-r! self r)
    self))

(define (orb-move self)
  ; Move orb
  (add (orb-velocity self) gravity)
  (add (orb-position self) (orb-velocity self)))

(define (orb-display self)
  ; Draw orb
  (no-stroke)
  (fill 200)
  (ellipse (vec-x (orb-position self))
           (vec-y (orb-position self))
           (* (orb-r self) 2)
           (* (orb-r self) 2)))

; Check boundaries of window
(define (orb-check-wall-collision self)
  (cond
    ((> (vec-x (orb-position self)) (- width (orb-r self)))
     (set-vec-x! (orb-position self) (- width (orb-r self)))
     (set-vec-x! (orb-velocity self)
                 (* (vec-x (orb-velocity self)) (- (orb-damping self)))))
    ((< (vec-x (orb-position self)) (orb-r self))
     (set-vec-x! (orb-position self) (orb-r self))
     (set-vec-x! (orb-velocity self)
                 (* (vec-x (orb-velocity self)) (- (orb-damping self)))))))

(define (orb-check-ground-collision self ground-segment)
  ; Get difference between orb and ground
  (define delta-x (- (vec-x (orb-position self)) (ground-x ground-segment)))
  (define delta-y (- (vec-y (orb-position self)) (ground-y ground-segment)))

  ; Precalculate trig values
  (define cosine (cos (ground-rot ground-segment)))
  (define sine (sin (ground-rot ground-segment)))

  ;; Rotate ground and velocity to allow
  ;; orthogonal collision calculations
  (define ground-x-temp (+ (* cosine delta-x) (* sine delta-y)))
  (define ground-y-temp (- (* cosine delta-y) (* sine delta-x)))
  (define velocity-x-temp
    (+ (* cosine (vec-x (orb-velocity self))) (* sine (vec-y (orb-velocity self)))))
  (define velocity-y-temp
    (- (* cosine (vec-y (orb-velocity self))) (* sine (vec-x (orb-velocity self)))))

  ;; Ground collision - check for surface
  ;; collision and also that orb is within
  ;; left/rights bounds of ground segment
  (when (and (> ground-y-temp (- (orb-r self)))
        (> (vec-x (orb-position self)) (ground-x1 ground-segment))
        (< (vec-x (orb-position self)) (ground-x2 ground-segment)))
    ; keep orb from going into ground
    (set! ground-y-temp (- (orb-r self)))
    ; bounce and slow down orb
    (set! velocity-y-temp (* velocity-y-temp -1.0))
    (set! velocity-y-temp (* velocity-y-temp (orb-damping self))))

  ; Reset ground, velocity and orb
  (set! delta-x (- (* cosine ground-x-temp) (* sine ground-y-temp)))
  (set! delta-y (+ (* cosine ground-y-temp) (* sine ground-x-temp)))
  (set-vec-x! (orb-velocity self)
              (- (* cosine velocity-x-temp) (* sine velocity-y-temp)))
  (set-vec-y! (orb-velocity self)
              (+ (* cosine velocity-y-temp) (* sine velocity-x-temp)))
  (set-vec-x! (orb-position self) (+ (ground-x ground-segment) delta-x))
  (set-vec-y! (orb-position self) (+ (ground-y ground-segment) delta-y)))

(define orb #f)

(define gravity (PVector 0 0.05))
; The ground is an array of "Ground" objects
(define segments 40)
(define ground (make-vector segments #f))

(define (setup)
  (size 640 360)
  ; An orb object that will fall and bounce around
  (set! orb (make-orb 50 50 3))

  ; Calculate ground peak heights
  (define peak-heights (make-vector (+ segments 1) 0))
  (do ((i 0 (+ i 1))) ((>= i (vector-length peak-heights)))
    (vector-set! peak-heights i (random (- height 40) (- height 30))))

  ;; Float value required for segment width (segs)
  ;; calculations so the ground spans the entire
  ;; display window, regardless of segment number.
  (define segs segments)
  (do ((i 0 (+ i 1))) ((>= i segments))
    (vector-set! ground
                 i
                 (make-ground (* (/ width segs) i)
                              (vector-ref peak-heights i)
                              (* (/ width segs) (+ i 1))
                              (vector-ref peak-heights (+ i 1))))))

(define (draw)
  ; Background
  (no-stroke)
  (fill 0 15)
  (rect 0 0 width height)

  ; Move and display the orb
  (orb-move orb)
  (orb-display orb)
  ; Check walls
  (orb-check-wall-collision orb)

  ; Check against all the ground segments
  (do ((i 0 (+ i 1))) ((>= i segments))
    (orb-check-ground-collision orb (vector-ref ground i)))

  ; Draw ground
  (fill 127)
  (begin-shape)
  (do ((i 0 (+ i 1))) ((>= i segments))
    (vertex (ground-x1 (vector-ref ground i)) (ground-y1 (vector-ref ground i)))
    (vertex (ground-x2 (vector-ref ground i)) (ground-y2 (vector-ref ground i))))
  (vertex (ground-x2 (vector-ref ground (- segments 1))) height)
  (vertex (ground-x1 (vector-ref ground 0)) height)
  (end-shape CLOSE))
