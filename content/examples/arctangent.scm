;; Arctangent.
;;
;; Move the mouse to change the direction of the eyes.
;; The atan2() function computes the angle from each eye
;; to the cursor.

(import (processing))

(define-record-type eye (make-eye-record x y size angle)
  eye?
  (x eye-x set-eye-x!)
  (y eye-y set-eye-y!)
  (size eye-size set-eye-size!)
  (angle eye-angle set-eye-angle!))

(define (make-eye tx ty ts)
  (let ((self (make-eye-record 0 0 0 0.0)))
    (set-eye-angle! self 0.0)
    (set-eye-x! self tx)
    (set-eye-y! self ty)
    (set-eye-size! self ts)
    self))

(define (eye-update self mx my)
  (set-eye-angle! self (atan2 (- my (eye-y self)) (- mx (eye-x self)))))

(define (eye-display self)
  (push-matrix)
  (translate (eye-x self) (eye-y self))
  (fill 255)
  (ellipse 0 0 (eye-size self) (eye-size self))
  (rotate (eye-angle self))
  (fill 153 204 0)
  (ellipse (/ (eye-size self) 4) 0 (/ (eye-size self) 2) (/ (eye-size self) 2))
  (pop-matrix))

(define e1 #f)
(define e2 #f)
(define e3 #f)

(define (setup)
  (size 640 360)
  (no-stroke)
  (set! e1 (make-eye 250 16 120))
  (set! e2 (make-eye 164 185 80))
  (set! e3 (make-eye 420 230 220)))

(define (draw)
  (background 102)

  (eye-update e1 mouse-x mouse-y)
  (eye-update e2 mouse-x mouse-y)
  (eye-update e3 mouse-x mouse-y)

  (eye-display e1)
  (eye-display e2)
  (eye-display e3))
