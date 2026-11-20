;; Scrollbar.
;;
;; Move the scrollbars left and right to change the positions of the images.
; True if a mouse button was pressed while no other button was.

(import (processing))

(define-record-type h-scrollbar (make-h-scrollbar-record swidth sheight xpos ypos spos newspos spos-min spos-max loose over locked ratio)
  h-scrollbar?
  (swidth h-scrollbar-swidth set-h-scrollbar-swidth!)
  (sheight h-scrollbar-sheight set-h-scrollbar-sheight!)
  (xpos h-scrollbar-xpos set-h-scrollbar-xpos!)
  (ypos h-scrollbar-ypos set-h-scrollbar-ypos!)
  (spos h-scrollbar-spos set-h-scrollbar-spos!)
  (newspos h-scrollbar-newspos set-h-scrollbar-newspos!)
  (spos-min h-scrollbar-spos-min set-h-scrollbar-spos-min!)
  (spos-max h-scrollbar-spos-max set-h-scrollbar-spos-max!)
  (loose h-scrollbar-loose set-h-scrollbar-loose!)
  (over h-scrollbar-over set-h-scrollbar-over!)
  (locked h-scrollbar-locked set-h-scrollbar-locked!)
  (ratio h-scrollbar-ratio set-h-scrollbar-ratio!))

; width and height of bar
; x and y position of bar
; x position of slider
; max and min values of slider
; how loose/heavy
; is the mouse over the slider?
(define (make-h-scrollbar xp yp sw sh l)
  (let ((self (make-h-scrollbar-record 0 0 0.0 0.0 0.0 0.0 0.0 0.0 0 #f #f 0.0)))
    (set-h-scrollbar-swidth! self sw)
    (set-h-scrollbar-sheight! self sh)
    (define widthtoheight (- sw sh))
    (set-h-scrollbar-ratio! self (/ (float sw) (float widthtoheight)))
    (set-h-scrollbar-xpos! self xp)
    (set-h-scrollbar-ypos! self (- yp (/ (h-scrollbar-sheight self) 2)))
    (set-h-scrollbar-spos! self
                           (- (+ (h-scrollbar-xpos self)
                                 (/ (h-scrollbar-swidth self) 2))
                              (/ (h-scrollbar-sheight self) 2)))
    (set-h-scrollbar-newspos! self (h-scrollbar-spos self))
    (set-h-scrollbar-spos-min! self (h-scrollbar-xpos self))
    (set-h-scrollbar-spos-max! self
                               (- (+ (h-scrollbar-xpos self) (h-scrollbar-swidth self))
                                  (h-scrollbar-sheight self)))
    (set-h-scrollbar-loose! self l)
    self))

(define (h-scrollbar-update self)
  (if (h-scrollbar-over-event self)
    (set-h-scrollbar-over! self #t)
    (set-h-scrollbar-over! self #f))
  (when (and first-mouse-press (h-scrollbar-over self))
    (set-h-scrollbar-locked! self #t))
  (when (not mouse-is-pressed) (set-h-scrollbar-locked! self #f))
  (when (h-scrollbar-locked self)
    (set-h-scrollbar-newspos! self
                              (h-scrollbar-constrain self
                                                     (- mouse-x
                                                        (/ (h-scrollbar-sheight self) 2))
                                                     (h-scrollbar-spos-min self)
                                                     (h-scrollbar-spos-max self))))
  (when (> (abs (- (h-scrollbar-newspos self) (h-scrollbar-spos self))) 1)
    (set-h-scrollbar-spos! self
                           (+ (h-scrollbar-spos self)
                              (/ (- (h-scrollbar-newspos self) (h-scrollbar-spos self))
                                 (h-scrollbar-loose self))))))

(define (h-scrollbar-constrain self val minv maxv) (min (max val minv) maxv))

(define (h-scrollbar-over-event self)
  (if (and (> mouse-x (h-scrollbar-xpos self))
        (< mouse-x (+ (h-scrollbar-xpos self) (h-scrollbar-swidth self)))
        (> mouse-y (h-scrollbar-ypos self))
        (< mouse-y (+ (h-scrollbar-ypos self) (h-scrollbar-sheight self))))
    #t
    #f))

(define (h-scrollbar-display self)
  (no-stroke)
  (fill 204)
  (rect (h-scrollbar-xpos self)
        (h-scrollbar-ypos self)
        (h-scrollbar-swidth self)
        (h-scrollbar-sheight self))
  (if (or (h-scrollbar-over self) (h-scrollbar-locked self))
    (fill 0 0 0)
    (fill 102 102 102))
  (rect (h-scrollbar-spos self)
        (h-scrollbar-ypos self)
        (h-scrollbar-sheight self)
        (h-scrollbar-sheight self)))

(define (h-scrollbar-get-pos self)
  ; Convert spos to be values between
  ; 0 and the total width of the scrollbar
  (* (h-scrollbar-spos self) (h-scrollbar-ratio self)))

(define first-mouse-press #f)
(define hs1 #f)
(define hs2 #f)  ; Two scrollbars
(define img1 #f)
(define img2 #f)  ; Two images to load

(define (setup)
  (size 640 360)
  (no-stroke)

  (set! hs1 (make-h-scrollbar 0 (- (/ height 2) 8) width 16 16))
  (set! hs2 (make-h-scrollbar 0 (+ (/ height 2) 8) width 16 16))

  ; Load images
  (set! img1 (load-image "seedTop.jpg"))
  (set! img2 (load-image "seedBottom.jpg")))

(define (draw)
  (background 255)

  ; Get the position of the img1 scrollbar
  ; and convert to a value to display the img1 image
  (define img1-pos (- (h-scrollbar-get-pos hs1) (/ width 2)))
  (fill 255)
  (image img1 (+ (- (/ width 2) (/ (width img1) 2)) (* img1-pos 1.5)) 0)

  ; Get the position of the img2 scrollbar
  ; and convert to a value to display the img2 image
  (define img2-pos (- (h-scrollbar-get-pos hs2) (/ width 2)))
  (fill 255)
  (image img2 (+ (- (/ width 2) (/ (width img2) 2)) (* img2-pos 1.5)) (/ height 2))

  (h-scrollbar-update hs1)
  (h-scrollbar-update hs2)
  (h-scrollbar-display hs1)
  (h-scrollbar-display hs2)

  (stroke 0)
  (line 0 (/ height 2) width (/ height 2))

  ; After it has been used in the sketch, set it back to false
  (when first-mouse-press (set! first-mouse-press #f)))

(define (mouse-pressed) (when (not first-mouse-press) (set! first-mouse-press #t)))
