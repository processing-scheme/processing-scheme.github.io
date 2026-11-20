;; Objects
;; by hbarragan.
;;
;; Move the cursor across the image to change the speed and positions
;; of the geometry. The class MRect defines a group of lines.

(import (processing))

(define-record-type m-rect (make-m-rect-record w xpos h ypos d t)
  m-rect?
  (w m-rect-w set-m-rect-w!)
  (xpos m-rect-xpos set-m-rect-xpos!)
  (h m-rect-h set-m-rect-h!)
  (ypos m-rect-ypos set-m-rect-ypos!)
  (d m-rect-d set-m-rect-d!)
  (t m-rect-t set-m-rect-t!))

; single bar width
; rect xposition
; rect height
; rect yposition
; single bar distance
; number of bars
(define (make-m-rect iw ixp ih iyp id it)
  (let ((self (make-m-rect-record 0 0.0 0.0 0.0 0.0 0.0)))
    (set-m-rect-w! self iw)
    (set-m-rect-xpos! self ixp)
    (set-m-rect-h! self ih)
    (set-m-rect-ypos! self iyp)
    (set-m-rect-d! self id)
    (set-m-rect-t! self it)
    self))

(define (m-rect-move self pos-x pos-y damping)
  (define dif (- (m-rect-ypos self) pos-y))
  (when (> (abs dif) 1) (set-m-rect-ypos! self (- (m-rect-ypos self) (/ dif damping))))
  (set! dif (- (m-rect-xpos self) pos-x))
  (when (> (abs dif) 1) (set-m-rect-xpos! self (- (m-rect-xpos self) (/ dif damping)))))

(define (m-rect-display self)
  (do ((i 0 (+ i 1))) ((>= i (m-rect-t self)))
    (rect (+ (m-rect-xpos self) (* i (+ (m-rect-d self) (m-rect-w self))))
          (m-rect-ypos self)
          (m-rect-w self)
          (* height (m-rect-h self)))))

(define r1 #f)
(define r2 #f)
(define r3 #f)
(define r4 #f)

(define (setup)
  (size 640 360)
  (fill 255 204)
  (no-stroke)
  (set! r1 (make-m-rect 1 134.0 0.532 (* 0.1 height) 10.0 60.0))
  (set! r2 (make-m-rect 2 44.0 0.166 (* 0.3 height) 5.0 50.0))
  (set! r3 (make-m-rect 2 58.0 0.332 (* 0.4 height) 10.0 35.0))
  (set! r4 (make-m-rect 1 120.0 0.0498 (* 0.9 height) 15.0 60.0)))

(define (draw)
  (background 0)

  (m-rect-display r1)
  (m-rect-display r2)
  (m-rect-display r3)
  (m-rect-display r4)

  (m-rect-move r1 (- mouse-x (/ width 2)) (+ mouse-y (* height 0.1)) 30)
  (m-rect-move r2
               (modulo (+ mouse-x (* width 0.05)) width)
               (+ mouse-y (* height 0.025))
               20)
  (m-rect-move r3 (/ mouse-x 4) (- mouse-y (* height 0.025)) 40)
  (m-rect-move r4 (- mouse-x (/ width 2)) (- height mouse-y) 50))
