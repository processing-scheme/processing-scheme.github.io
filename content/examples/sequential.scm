;; Sequential
;; by James Paterson.
;;
;; Displaying a sequence of images creates the illusion of motion.
;; Twelve images are loaded and each is displayed individually in a loop.

(import (processing))

(define num-frames 12)  ; The number of frames in the animation
(define current-frame 0)
(define images (make-vector num-frames #f))

(define (setup)
  (size 640 360)
  (frame-rate 24)

  (vector-set! images 0 (load-image "PT_anim0000.gif"))
  (vector-set! images 1 (load-image "PT_anim0001.gif"))
  (vector-set! images 2 (load-image "PT_anim0002.gif"))
  (vector-set! images 3 (load-image "PT_anim0003.gif"))
  (vector-set! images 4 (load-image "PT_anim0004.gif"))
  (vector-set! images 5 (load-image "PT_anim0005.gif"))
  (vector-set! images 6 (load-image "PT_anim0006.gif"))
  (vector-set! images 7 (load-image "PT_anim0007.gif"))
  (vector-set! images 8 (load-image "PT_anim0008.gif"))
  (vector-set! images 9 (load-image "PT_anim0009.gif"))
  (vector-set! images 10 (load-image "PT_anim0010.gif"))
  (vector-set! images 11 (load-image "PT_anim0011.gif"))

  ; If you don't want to load each image separately
  ; and you know how many frames you have, you
  ; can create the filenames as the program runs.
  ; The nf() command does number formatting, which will
  ; ensure that the number is (in this case) 4 digits.
  ; for (int i = 0; i < numFrames; i++) {
  ;  String imageName = "PT_anim" + nf(i, 4) + ".gif";
  ;  images[i] = loadImage(imageName);
  ; }
  )

(define (draw)
  (background 0)
  (set! current-frame (remainder (+ current-frame 1) num-frames))  ; Use % to cycle through frames
  (define offset 0)
  (do ((x -100 (+ x (width (vector-ref images 0))))) ((>= x width))
    (image (vector-ref images (remainder (+ current-frame offset) num-frames)) x -20)
    (set! offset (+ offset 2))
    (image (vector-ref images (remainder (+ current-frame offset) num-frames))
           x
           (/ height 2))
    (set! offset (+ offset 2))))
