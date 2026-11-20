;; Pentigree L-System
;; by Geraldine Sarmiento.
;;
;; This example was based on Patrick Dwyer's L-System class.

(import (processing))

(define-record-type l-system (make-l-system-record steps axiom rule production start-length draw-length theta generations)
  l-system?
  (steps l-system-steps set-l-system-steps!)
  (axiom l-system-axiom set-l-system-axiom!)
  (rule l-system-rule set-l-system-rule!)
  (production l-system-production set-l-system-production!)
  (start-length l-system-start-length set-l-system-start-length!)
  (draw-length l-system-draw-length set-l-system-draw-length!)
  (theta l-system-theta set-l-system-theta!)
  (generations l-system-generations set-l-system-generations!))

(define (make-l-system)
  (let ((self (make-l-system-record 0 "" "" "" 0.0 0.0 0.0 0)))
    (set-l-system-steps! self 0)
    (set-l-system-axiom! self "F")
    (set-l-system-rule! self "F+F-F")
    (set-l-system-start-length! self 90.0)
    (set-l-system-theta! self (radians 120.0))
    (l-system-reset self)
    self))

(define (l-system-reset self)
  (set-l-system-production! self (l-system-axiom self))
  (set-l-system-draw-length! self (l-system-start-length self))
  (set-l-system-generations! self 0))

(define (l-system-get-age self) (l-system-generations self))

(define (l-system-render self)
  (translate (/ width 2) (/ height 2))
  (set-l-system-steps! self (+ (l-system-steps self) 5))
  (when (> (l-system-steps self) (string-length (l-system-production self)))
    (set-l-system-steps! self (string-length (l-system-production self))))
  (do ((i 0 (+ i 1))) ((>= i (l-system-steps self)))
    (define step (string-ref (l-system-production self) i))
    (cond
      ((eqv? step #\F)
       (rect 0 0 (- (l-system-draw-length self)) (- (l-system-draw-length self)))
       (no-fill)
       (translate 0 (- (l-system-draw-length self))))
      ((eqv? step #\+) (rotate (l-system-theta self)))
      ((eqv? step #\-) (rotate (- (l-system-theta self))))
      ((eqv? step #\[) (push-matrix))
      ((eqv? step #\]) (pop-matrix)))))

(define (l-system-simulate self gen)
  (do () ((>= (l-system-get-age self) gen))
    (set-l-system-production! self
                              (l-system-iterate self
                                                (l-system-production self)
                                                (l-system-rule self)))))

(define (l-system-iterate self prod rule)
  (set-l-system-draw-length! self (* (l-system-draw-length self) 0.6))
  (set-l-system-generations! self (+ (l-system-generations self) 1))
  (define new-production prod)
  (set! new-production (join (split new-production "F") rule))
  new-production)

(define-record-type pentigree-l-system (make-pentigree-l-system-record steps axiom rule production start-length draw-length theta generations somestep xoff)
  pentigree-l-system?
  (steps pentigree-l-system-steps set-pentigree-l-system-steps!)
  (axiom pentigree-l-system-axiom set-pentigree-l-system-axiom!)
  (rule pentigree-l-system-rule set-pentigree-l-system-rule!)
  (production pentigree-l-system-production set-pentigree-l-system-production!)
  (start-length pentigree-l-system-start-length set-pentigree-l-system-start-length!)
  (draw-length pentigree-l-system-draw-length set-pentigree-l-system-draw-length!)
  (theta pentigree-l-system-theta set-pentigree-l-system-theta!)
  (generations pentigree-l-system-generations set-pentigree-l-system-generations!)
  (somestep pentigree-l-system-somestep set-pentigree-l-system-somestep!)
  (xoff pentigree-l-system-xoff set-pentigree-l-system-xoff!))

(define (make-pentigree-l-system)
  (let ((self (make-pentigree-l-system-record 0 "" "" "" 0.0 0.0 0.0 0 0.0 0.0)))
    (set-l-system-steps! self 0)
    (set-pentigree-l-system-somestep! self 0.1)
    (set-pentigree-l-system-xoff! self 0.01)
    (set-l-system-axiom! self "F-F-F-F-F")
    (set-l-system-rule! self "F-F++F+F-F-F")
    (set-l-system-start-length! self 60.0)
    (set-l-system-theta! self (radians 72))
    (pentigree-l-system-reset self)
    self))

(define (pentigree-l-system-use-rule self r) (set-l-system-rule! self r))

(define (pentigree-l-system-use-axiom self a) (set-l-system-axiom! self a))

(define (pentigree-l-system-use-length self l) (set-l-system-start-length! self l))

(define (pentigree-l-system-use-theta self t) (set-l-system-theta! self (radians t)))

(define (pentigree-l-system-reset self)
  (set-l-system-production! self (l-system-axiom self))
  (set-l-system-draw-length! self (l-system-start-length self))
  (set-l-system-generations! self 0))

(define (pentigree-l-system-get-age self) (l-system-generations self))

(define (pentigree-l-system-render self)
  (translate (/ width 4) (/ height 2))
  (set-pentigree-l-system-steps! self (+ (pentigree-l-system-steps self) 3))
  (when (> (pentigree-l-system-steps self) (string-length (l-system-production self)))
    (set-pentigree-l-system-steps! self (string-length (l-system-production self))))

  (do ((i 0 (+ i 1))) ((>= i (pentigree-l-system-steps self)))
    (define step (string-ref (l-system-production self) i))
    (cond
      ((eqv? step #\F)
       (no-fill)
       (stroke 255)
       (line 0 0 0 (- (l-system-draw-length self)))
       (translate 0 (- (l-system-draw-length self))))
      ((eqv? step #\+) (rotate (l-system-theta self)))
      ((eqv? step #\-) (rotate (- (l-system-theta self))))
      ((eqv? step #\[) (push-matrix))
      ((eqv? step #\]) (pop-matrix)))))

(define ps #f)

(define (setup)
  (size 640 360)
  (set! ps (make-pentigree-l-system))
  (l-system-simulate ps 3))

(define (draw)
  (background 0)
  (pentigree-l-system-render ps))
