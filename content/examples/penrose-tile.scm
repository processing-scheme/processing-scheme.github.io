;; Penrose Tile L-System
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
    (set-l-system-start-length! self 190.0)
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

(define-record-type penrose-l-system (make-penrose-l-system-record steps axiom rule production start-length draw-length theta generations somestep rule-w rule-x rule-y rule-z)
  penrose-l-system?
  (steps penrose-l-system-steps set-penrose-l-system-steps!)
  (axiom penrose-l-system-axiom set-penrose-l-system-axiom!)
  (rule penrose-l-system-rule set-penrose-l-system-rule!)
  (production penrose-l-system-production set-penrose-l-system-production!)
  (start-length penrose-l-system-start-length set-penrose-l-system-start-length!)
  (draw-length penrose-l-system-draw-length set-penrose-l-system-draw-length!)
  (theta penrose-l-system-theta set-penrose-l-system-theta!)
  (generations penrose-l-system-generations set-penrose-l-system-generations!)
  (somestep penrose-l-system-somestep set-penrose-l-system-somestep!)
  (rule-w penrose-l-system-rule-w set-penrose-l-system-rule-w!)
  (rule-x penrose-l-system-rule-x set-penrose-l-system-rule-x!)
  (rule-y penrose-l-system-rule-y set-penrose-l-system-rule-y!)
  (rule-z penrose-l-system-rule-z set-penrose-l-system-rule-z!))

(define (make-penrose-l-system)
  (let ((self (make-penrose-l-system-record 0 "" "" "" 0.0 0.0 0.0 0 0.0 "" "" "" "")))
    (set-l-system-steps! self 0)
    (set-penrose-l-system-somestep! self 0.1)
    (set-l-system-axiom! self "[X]++[X]++[X]++[X]++[X]")
    (set-penrose-l-system-rule-w! self "YF++ZF4-XF[-YF4-WF]++")
    (set-penrose-l-system-rule-x! self "+YF--ZF[3-WF--XF]+")
    (set-penrose-l-system-rule-y! self "-WF++XF[+++YF++ZF]-")
    (set-penrose-l-system-rule-z! self "--YF++++WF[+ZF++++XF]--XF")
    (set-l-system-start-length! self 460.0)
    (set-l-system-theta! self (radians 36))
    (penrose-l-system-reset self)
    self))

(define (penrose-l-system-use-rule self r) (set-l-system-rule! self r))

(define (penrose-l-system-use-axiom self a) (set-l-system-axiom! self a))

(define (penrose-l-system-use-length self l) (set-l-system-start-length! self l))

(define (penrose-l-system-use-theta self t) (set-l-system-theta! self (radians t)))

(define (penrose-l-system-reset self)
  (set-l-system-production! self (l-system-axiom self))
  (set-l-system-draw-length! self (l-system-start-length self))
  (set-l-system-generations! self 0))

(define (penrose-l-system-get-age self) (l-system-generations self))

(define (penrose-l-system-render self)
  (translate (/ width 2) (/ height 2))
  (define pushes 0)
  (define repeats 1)
  (set-penrose-l-system-steps! self (+ (penrose-l-system-steps self) 12))
  (when (> (penrose-l-system-steps self) (string-length (l-system-production self)))
    (set-penrose-l-system-steps! self (string-length (l-system-production self))))

  (do ((i 0 (+ i 1))) ((>= i (penrose-l-system-steps self)))
    (define step (string-ref (l-system-production self) i))
    (cond
      ((eqv? step #\F)
       (stroke 255 60)
       (do ((j 0 (+ j 1))) ((>= j repeats))
         (line 0 0 0 (- (l-system-draw-length self)))
         (no-fill)
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
      ((eqv? step #\[) (set! pushes (+ pushes 1)) (push-matrix))
      ((eqv? step #\]) (pop-matrix) (set! pushes (- pushes 1)))
      ((and (>= (char->integer step) 48) (<= (char->integer step) 57))
       (set! repeats (- (int step) 48)))))

  ; Unpush if we need too
  (do () ((<= pushes 0))
    (pop-matrix)
    (set! pushes (- pushes 1))))

(define (penrose-l-system-iterate self prod rule)
  (define new-production "")
  (do ((i 0 (+ i 1))) ((>= i (string-length prod)))
    (define step (string-ref (l-system-production self) i))
    (cond
      ((eqv? step #\W)
       (set! new-production
             (string-append new-production (penrose-l-system-rule-w self))))
      ((eqv? step #\X)
       (set! new-production
             (string-append new-production (penrose-l-system-rule-x self))))
      ((eqv? step #\Y)
       (set! new-production
             (string-append new-production (penrose-l-system-rule-y self))))
      ((eqv? step #\Z)
       (set! new-production
             (string-append new-production (penrose-l-system-rule-z self))))
      ((not (eqv? step #\F))
       (set! new-production (string-append new-production (str step))))))

  (set-l-system-draw-length! self (* (l-system-draw-length self) 0.5))
  (set-l-system-generations! self (+ (l-system-generations self) 1))
  new-production)

(define ds #f)

(define (setup)
  (size 640 360)
  (set! ds (make-penrose-l-system))
  (l-system-simulate ds 4))

(define (draw) (background 0) (penrose-l-system-render ds))
