;; Inheritance
;;
;; A class can be defined using another class as a foundation. In object-oriented
;; programming terminology, one class can inherit fi elds and methods from another.
;; An object that inherits from another is called a subclass, and the object it
;; inherits from is called a superclass. A subclass extends the superclass.

(import (processing))

(define-record-type spin (make-spin-record x y speed angle)
  spin?
  (x spin-x set-spin-x!)
  (y spin-y set-spin-y!)
  (speed spin-speed set-spin-speed!)
  (angle spin-angle set-spin-angle!))

(define (make-spin xpos ypos s)
  (let ((self (make-spin-record 0.0 0.0 0.0 0.0)))
    (set-spin-angle! self 0.0)
    (set-spin-x! self xpos)
    (set-spin-y! self ypos)
    (set-spin-speed! self s)
    self))

(define (spin-update self)
  (set-spin-angle! self (+ (spin-angle self) (spin-speed self))))

(define-record-type spin-arm (make-spin-arm-record x y speed angle)
  spin-arm?
  (x spin-arm-x set-spin-arm-x!)
  (y spin-arm-y set-spin-arm-y!)
  (speed spin-arm-speed set-spin-arm-speed!)
  (angle spin-arm-angle set-spin-arm-angle!))

(define (make-spin-arm x y s)
  (let ((self (make-spin-arm-record 0.0 0.0 0.0 0.0)))
    (define base (make-spin x y s))
    (set-spin-arm-x! self (spin-x base))
    (set-spin-arm-y! self (spin-y base))
    (set-spin-arm-speed! self (spin-speed base))
    (set-spin-arm-angle! self (spin-angle base))
    self))

(define (spin-arm-display self)
  (stroke-weight 1)
  (stroke 0)
  (push-matrix)
  (translate (spin-x self) (spin-y self))
  (set-spin-angle! self (+ (spin-angle self) (spin-speed self)))
  (rotate (spin-angle self))
  (line 0 0 165 0)
  (pop-matrix))

(define-record-type spin-spots (make-spin-spots-record x y speed angle dim)
  spin-spots?
  (x spin-spots-x set-spin-spots-x!)
  (y spin-spots-y set-spin-spots-y!)
  (speed spin-spots-speed set-spin-spots-speed!)
  (angle spin-spots-angle set-spin-spots-angle!)
  (dim spin-spots-dim set-spin-spots-dim!))

(define (make-spin-spots x y s d)
  (let ((self (make-spin-spots-record 0.0 0.0 0.0 0.0 0.0)))
    (define base (make-spin x y s))
    (set-spin-spots-x! self (spin-x base))
    (set-spin-spots-y! self (spin-y base))
    (set-spin-spots-speed! self (spin-speed base))
    (set-spin-spots-angle! self (spin-angle base))
    (set-spin-spots-dim! self d)
    self))

(define (spin-spots-display self)
  (no-stroke)
  (push-matrix)
  (translate (spin-x self) (spin-y self))
  (set-spin-angle! self (+ (spin-angle self) (spin-speed self)))
  (rotate (spin-angle self))
  (ellipse (/ (- (spin-spots-dim self)) 2)
           0
           (spin-spots-dim self)
           (spin-spots-dim self))
  (ellipse (/ (spin-spots-dim self) 2) 0 (spin-spots-dim self) (spin-spots-dim self))
  (pop-matrix))

(define spots #f)
(define arm #f)

(define (setup)
  (size 640 360)
  (set! arm (make-spin-arm (/ width 2) (/ height 2) 0.01))
  (set! spots (make-spin-spots (/ width 2) (/ height 2) -0.02 90.0)))

(define (draw)
  (background 204)
  (spin-update arm)
  (spin-arm-display arm)
  (spin-update spots)
  (spin-spots-display spots))
