;; Bouncy Bubbles
;; based on code from Keith Peters.
;;
;; Multiple-object collision.

(import (processing))

(define-record-type ball (make-ball-record x y diameter vx vy id others)
  ball?
  (x ball-x set-ball-x!)
  (y ball-y set-ball-y!)
  (diameter ball-diameter set-ball-diameter!)
  (vx ball-vx set-ball-vx!)
  (vy ball-vy set-ball-vy!)
  (id ball-id set-ball-id!)
  (others ball-others set-ball-others!))

(define (make-ball xin yin din idin oin)
  (let ((self (make-ball-record 0.0 0.0 0.0 0.0 0.0 0 #f)))
    (set-ball-vx! self 0)
    (set-ball-vy! self 0)
    (set-ball-x! self xin)
    (set-ball-y! self yin)
    (set-ball-diameter! self din)
    (set-ball-id! self idin)
    (set-ball-others! self oin)
    self))

(define (ball-collide self)
  (do ((i (+ (ball-id self) 1) (+ i 1))) ((>= i num-balls))
    (define dx (- (ball-x (vector-ref (ball-others self) i)) (ball-x self)))
    (define dy (- (ball-y (vector-ref (ball-others self) i)) (ball-y self)))
    (define distance (sqrt (+ (* dx dx) (* dy dy))))
    (define min-dist
      (+ (/ (ball-diameter (vector-ref (ball-others self) i)) 2)
         (/ (ball-diameter self) 2)))
    (when (< distance min-dist)
      (define angle (atan2 dy dx))
      (define target-x (+ (ball-x self) (* (cos angle) min-dist)))
      (define target-y (+ (ball-y self) (* (sin angle) min-dist)))
      (define ax (* (- target-x (ball-x (vector-ref (ball-others self) i))) spring))
      (define ay (* (- target-y (ball-y (vector-ref (ball-others self) i))) spring))
      (set-ball-vx! self (- (ball-vx self) ax))
      (set-ball-vy! self (- (ball-vy self) ay))
      (set-ball-vx! (vector-ref (ball-others self) i)
                    (+ (ball-vx (vector-ref (ball-others self) i)) ax))
      (set-ball-vy! (vector-ref (ball-others self) i)
                    (+ (ball-vy (vector-ref (ball-others self) i)) ay)))))

(define (ball-move self)
  (set-ball-vy! self (+ (ball-vy self) gravity))
  (set-ball-x! self (+ (ball-x self) (ball-vx self)))
  (set-ball-y! self (+ (ball-y self) (ball-vy self)))
  (cond
    ((> (+ (ball-x self) (/ (ball-diameter self) 2)) width)
     (set-ball-x! self (- width (/ (ball-diameter self) 2)))
     (set-ball-vx! self (* (ball-vx self) friction)))
    ((< (- (ball-x self) (/ (ball-diameter self) 2)) 0)
     (set-ball-x! self (/ (ball-diameter self) 2))
     (set-ball-vx! self (* (ball-vx self) friction))))
  (cond
    ((> (+ (ball-y self) (/ (ball-diameter self) 2)) height)
     (set-ball-y! self (- height (/ (ball-diameter self) 2)))
     (set-ball-vy! self (* (ball-vy self) friction)))
    ((< (- (ball-y self) (/ (ball-diameter self) 2)) 0)
     (set-ball-y! self (/ (ball-diameter self) 2))
     (set-ball-vy! self (* (ball-vy self) friction)))))

(define (ball-display self)
  (ellipse (ball-x self) (ball-y self) (ball-diameter self) (ball-diameter self)))

(define num-balls 12)
(define spring 0.05)
(define gravity 0.03)
(define friction -0.9)
(define balls (make-vector num-balls #f))

(define (setup)
  (size 640 360)
  (do ((i 0 (+ i 1))) ((>= i num-balls))
    (vector-set! balls
                 i
                 (make-ball (random width) (random height) (random 30 70) i balls)))
  (no-stroke)
  (fill 255 204))

(define (draw)
  (background 0)
  (vector-for-each (lambda (ball)
                     (ball-collide ball)
                     (ball-move ball)
                     (ball-display ball))
                   balls))
