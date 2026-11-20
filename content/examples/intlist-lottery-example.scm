;; IntList Lottery example
;; by Daniel Shiffman.
;;
;; This example demonstrates an IntList can be used to store a list of numbers.
;; While an array of integers serves a similar purpose it is of fixed size.  The
;; An IntList can easily have values added or deleted and it can also be
;; shuffled and sorted.  For lists of floats or Strings, you can use FloatList
;; and StringList.  For lists of objects, use ArrayList.
;;
;; In this example, three lists of integers are created.  One is a pool of numbers
;; that is shuffled and picked randomly from.  One is the list of "picked" numbers.
;; And one is a lottery "ticket" which includes 5 numbers that are trying to be matched.
; Three lists of integers

(import (processing))

(define lottery #f)
(define results #f)
(define ticket #f)

(define (setup)
  (size 640 360)
  (frame-rate 30)
  ; Create empy lists
  (set! lottery (list))
  (set! results (list))
  (set! ticket (list))

  ; Add 20 integers in order to the lottery list
  (do ((i 0 (+ i 1))) ((>= i 20))
    (set! lottery (append lottery (list i))))

  ; Pick five numbers from the lottery list to go into the Ticket list
  (do ((i 0 (+ i 1))) ((>= i 5))
    (define index (int (random (length lottery))))
    (set! ticket (append ticket (list (list-ref lottery index))))))

(define (draw)
  (background 51)

  ; The shuffle() method randomly shuffles the order of the values in the list
  (shuffle lottery)

  ; Call a method that will display the integers in the list at an x,y location
  (show-list lottery 16 48)
  (show-list results 16 100)
  (show-list ticket 16 140)

  ; This loop checks if the picked numbers (results)
  ; match the ticket numbers
  (do ((i 0 (+ i 1))) ((>= i (length results)))
    ; Are the integers equal?
    (cond
      ((= (list-ref results i) (list-ref ticket i))
       ; if so green
       (fill 0 255 0 100))
      (else
        ; if not red
        (fill 255 0 0 100)))
    (ellipse (+ 16 (* i 32)) 140 24 24))

  ; One every 30 frames we pick a new lottery number to go in results
  (when (= (remainder frame-count 30) 0)
    (cond
      ((< (length results) 5)
       ; Get the first value in the lottery list and remove it
       (define val (remove lottery 0))
       ; Put it in the results
       (set! results (append results (list val))))
      (else
        ; Ok we picked five numbers, let's reset
        (do ((i 0 (+ i 1))) ((>= i (length results)))
          ; Put the picked results back into the lottery
          (set! lottery (append lottery (list (list-ref results i)))))
        ; Clear the results and start over
        (set! results (list))))))

; Draw a list of numbers starting at an x,y location
(define (show-list list-v x y)
  (do ((i 0 (+ i 1))) ((>= i (length list-v)))
    ; Use get() to pull a value from the list at the specified index
    (define val (list-ref list-v i))
    (stroke 255)
    (no-fill)
    (ellipse (+ x (* i 32)) y 24 24)
    (text-align CENTER)
    (fill 255)
    (text val (+ x (* i 32)) (+ y 6))))
