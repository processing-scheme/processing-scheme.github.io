;; Animated Sprite (Shifty + Teddy)
;; by James Paterson.
;;
;; Press the mouse button to change animations.
;; Demonstrates loading, displaying, and animating GIF images.
;; It would be easy to write a program to display
;; animated GIFs, but would not allow as much control over
;; the display sequence and rate of display.

(import (processing))

; Class for animating a sequence of GIFs

(define-record-type animation (make-animation-record images image-count frame)
  animation?
  (images animation-images set-animation-images!)
  (image-count animation-image-count set-animation-image-count!)
  (frame animation-frame set-animation-frame!))

(define (make-animation image-prefix count)
  (let ((self (make-animation-record #f 0 0)))
    (set-animation-image-count! self count)
    (set-animation-images! self (make-vector (animation-image-count self) #f))

    (do ((i 0 (+ i 1))) ((>= i (animation-image-count self)))
      ; Use nf() to number format 'i' into four digits
      (define filename (string-append image-prefix (nf i 4) ".gif"))
      (vector-set! (animation-images self) i (load-image filename)))
    self))

(define (animation-display self xpos ypos)
  (set-animation-frame! self
                        (remainder (+ (animation-frame self) 1)
                                   (animation-image-count self)))
  (image (vector-ref (animation-images self) (animation-frame self)) xpos ypos))

(define (animation-get-width self) (width (vector-ref (animation-images self) 0)))

(define animation1 #f)
(define animation2 #f)

(define xpos 0.0)
(define ypos 0.0)
(define drag 30.0)

(define (setup)
  (size 640 360)
  (background 255 204 0)
  (frame-rate 24)
  (set! animation1 (make-animation "PT_Shifty_" 38))
  (set! animation2 (make-animation "PT_Teddy_" 60))
  (set! ypos (* height 0.25)))

(define (draw)
  (define dx (- mouse-x xpos))
  (set! xpos (+ xpos (/ dx drag)))

  ; Display the sprite at the position xpos, ypos
  (cond
    (mouse-is-pressed (background 153 153 0)
                      (animation-display animation1
                                         (- xpos (/ (animation-get-width animation1) 2))
                                         ypos))
    (else
      (background 255 204 0)
      (animation-display animation2
                         (- xpos (/ (animation-get-width animation1) 2))
                         ypos))))
