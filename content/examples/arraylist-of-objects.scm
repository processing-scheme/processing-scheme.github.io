;; ArrayList of objects
;; by Daniel Shiffman.
;;
;; This example demonstrates how to use a Java ArrayList to store
;; a variable number of objects.  Items can be added and removed
;; from the ArrayList.
;;
;; Click the mouse to add bouncing balls.

(import (processing))

; the list without its i-th item
(define (remove-at xs i)
  (concat (subset xs 0 i) (subset xs (+ i 1))))

; Simple bouncing ball class

(define-record-type ball (make-ball-record x y speed gravity w life)
  ball?
  (x ball-x set-ball-x!)
  (y ball-y set-ball-y!)
  (speed ball-speed set-ball-speed!)
  (gravity ball-gravity set-ball-gravity!)
  (w ball-w set-ball-w!)
  (life ball-life set-ball-life!))

(define (make-ball temp-x temp-y temp-w)
  (let ((self (make-ball-record 0.0 0.0 0.0 0.0 0.0 0.0)))
    (set-ball-life! self 255)
    (set-ball-x! self temp-x)
    (set-ball-y! self temp-y)
    (set-ball-w! self temp-w)
    (set-ball-speed! self 0)
    (set-ball-gravity! self 0.1)
    self))

(define (ball-move self)
  ; Add gravity to speed
  (set-ball-speed! self (+ (ball-speed self) (ball-gravity self)))
  ; Add speed to y location
  (set-ball-y! self (+ (ball-y self) (ball-speed self)))
  ; If square reaches the bottom
  ; Reverse speed
  (when (> (ball-y self) height)
    ; Dampening
    (set-ball-speed! self (* (ball-speed self) -0.8))
    (set-ball-y! self height)))

(define (ball-finished self)
  ; Balls fade out
  (set-ball-life! self (- (ball-life self) 1))
  (if (< (ball-life self) 0) #t #f))

(define (ball-display self)
  ; Display the circle
  (fill 0 (ball-life self))
  ; stroke(0,life);
  (ellipse (ball-x self) (ball-y self) (ball-w self) (ball-w self)))

(define balls #f)
(define ball-width 48)

(define (setup)
  (size 640 360)
  (no-stroke)

  ; Create an empty ArrayList (will store Ball objects)
  (set! balls (list))

  ; Start by adding one element
  (set! balls (append balls (list (make-ball (/ width 2) 0 ball-width)))))

(define (draw)
  (background 255)

  ; With an array, we say balls.length, with an ArrayList, we say balls.size()
  ; The length of an ArrayList is dynamic
  ; Notice how we are looping through the ArrayList backwards
  ; This is because we are deleting elements from the list
  (do ((i (- (length balls) 1) (- i 1))) ((< i 0))
    ; An ArrayList doesn't know what it is storing so we have to cast the object coming out
    (define ball (list-ref balls i))
    (ball-move ball)
    (ball-display ball)
    (when (ball-finished ball)
      ; Items can be deleted with remove()
      (set! balls (remove-at balls i)))))

(define (mouse-pressed)
  ; A new ball object is added to the ArrayList (by default to the end)
  (set! balls (append balls (list (make-ball mouse-x mouse-y ball-width)))))
