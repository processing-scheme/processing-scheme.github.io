;; Penrose Snowflake L-System
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

(define-record-type penrose-snowflake-l-system (make-penrose-snowflake-l-system-record steps axiom rule production start-length draw-length theta generations rule-f)
  penrose-snowflake-l-system?
  (steps penrose-snowflake-l-system-steps set-penrose-snowflake-l-system-steps!)
  (axiom penrose-snowflake-l-system-axiom set-penrose-snowflake-l-system-axiom!)
  (rule penrose-snowflake-l-system-rule set-penrose-snowflake-l-system-rule!)
  (production penrose-snowflake-l-system-production
              set-penrose-snowflake-l-system-production!)
  (start-length penrose-snowflake-l-system-start-length
                set-penrose-snowflake-l-system-start-length!)
  (draw-length penrose-snowflake-l-system-draw-length
               set-penrose-snowflake-l-system-draw-length!)
  (theta penrose-snowflake-l-system-theta set-penrose-snowflake-l-system-theta!)
  (generations penrose-snowflake-l-system-generations
               set-penrose-snowflake-l-system-generations!)
  (rule-f penrose-snowflake-l-system-rule-f set-penrose-snowflake-l-system-rule-f!))

(define (make-penrose-snowflake-l-system)
  (let ((self (make-penrose-snowflake-l-system-record 0 "" "" "" 0.0 0.0 0.0 0 "")))
    (set-l-system-steps! self 0)
    (set-l-system-axiom! self "F3-F3-F3-F3-F")
    (set-penrose-snowflake-l-system-rule-f! self "F3-F3-F45-F++F3-F")
    (set-l-system-start-length! self 450.0)
    (set-l-system-theta! self (radians 18))
    (penrose-snowflake-l-system-reset self)
    self))

(define (penrose-snowflake-l-system-use-rule self r) (set-l-system-rule! self r))

(define (penrose-snowflake-l-system-use-axiom self a) (set-l-system-axiom! self a))

(define (penrose-snowflake-l-system-use-length self l)
  (set-l-system-start-length! self l))

(define (penrose-snowflake-l-system-use-theta self t)
  (set-l-system-theta! self (radians t)))

(define (penrose-snowflake-l-system-reset self)
  (set-l-system-production! self (l-system-axiom self))
  (set-l-system-draw-length! self (l-system-start-length self))
  (set-l-system-generations! self 0))

(define (penrose-snowflake-l-system-get-age self) (l-system-generations self))

(define (penrose-snowflake-l-system-render self)
  (translate width height)
  (define repeats 1)

  (set-l-system-steps! self (+ (l-system-steps self) 3))
  (when (> (l-system-steps self) (string-length (l-system-production self)))
    (set-l-system-steps! self (string-length (l-system-production self))))

  (do ((i 0 (+ i 1))) ((>= i (l-system-steps self)))
    (define step (string-ref (l-system-production self) i))
    (cond
      ((eqv? step #\F)
       (do ((j 0 (+ j 1))) ((>= j repeats))
         (line 0 0 0 (- (l-system-draw-length self)))
         (translate 0 (- (l-system-draw-length self))))
       (set! repeats 1))
      ((eqv? step #\+)
       (do ((j 0 (+ j 1))) ((>= j repeats))
         (rotate (l-system-theta self)))
       (set! repeats 1))
      ((eqv? step #\-)
       (do ((j 0 (+ j 1))) ((>= j repeats))
         (rotate (- (l-system-theta self))))
       (set! repeats 1))
      ((eqv? step #\[) (push-matrix))
      ((eqv? step #\]) (pop-matrix))
      ((and (>= (char->integer step) 48) (<= (char->integer step) 57))
       (set! repeats (+ repeats (- (char->integer step) 48)))))))

(define (penrose-snowflake-l-system-iterate self prod rule)
  (define new-production "")
  (do ((i 0 (+ i 1))) ((>= i (string-length prod)))
    (define step (string-ref (l-system-production self) i))
    (cond
      ((eqv? step #\F)
       (set! new-production
             (string-append new-production (penrose-snowflake-l-system-rule-f self))))
      ((not (eqv? step #\F))
       (set! new-production (string-append new-production (str step))))))
  (set-l-system-draw-length! self (* (l-system-draw-length self) 0.4))
  (set-l-system-generations! self (+ (l-system-generations self) 1))
  new-production)

(define ps #f)

(define (setup)
  (size 640 360)
  (stroke 255)
  (no-fill)
  (set! ps (make-penrose-snowflake-l-system))
  (l-system-simulate ps 4))

(define (draw)
  (background 0)
  (penrose-snowflake-l-system-render ps))
