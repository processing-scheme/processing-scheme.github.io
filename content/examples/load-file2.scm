;; LoadFile 2
;;
;; This example loads a data file about cars. Each element is separated
;; with a tab and corresponds to a different aspect of each car. The file stores
;; the miles per gallon, cylinders, displacement, etc., for more than 400 different
;; makes and models. Press a mouse button to advance to the next group of entries.

(import (processing))

(define-record-type record (make-record-record name mpg cylinders displacement horsepower weight acceleration year origin)
  record?
  (name record-name set-record-name!)
  (mpg record-mpg set-record-mpg!)
  (cylinders record-cylinders set-record-cylinders!)
  (displacement record-displacement set-record-displacement!)
  (horsepower record-horsepower set-record-horsepower!)
  (weight record-weight set-record-weight!)
  (acceleration record-acceleration set-record-acceleration!)
  (year record-year set-record-year!)
  (origin record-origin set-record-origin!))

(define (make-record pieces)
  (let ((self (make-record-record "" 0.0 0 0.0 0.0 0.0 0.0 0 0.0)))
    (set-record-name! self (vector-ref pieces 0))
    (set-record-mpg! self (float (vector-ref pieces 1)))
    (set-record-cylinders! self (int (vector-ref pieces 2)))
    (set-record-displacement! self (float (vector-ref pieces 3)))
    (set-record-horsepower! self (float (vector-ref pieces 4)))
    (set-record-weight! self (float (vector-ref pieces 5)))
    (set-record-acceleration! self (float (vector-ref pieces 6)))
    (set-record-year! self (int (vector-ref pieces 7)))
    (set-record-origin! self (float (vector-ref pieces 8)))
    self))

(define records #f)
(define lines #f)
(define record-count 0)
(define body #f)
(define num 9)  ; Display this many entries on each screen.
(define starting-entry 0)  ; Display from this entry number

(define (setup)
  (size 640 360)
  (fill 255)
  (no-loop)

  (set! body (load-font "TheSans-Plain-12.vlw"))
  (text-font body)
  (text-size 20)

  (set! lines (load-strings "cars2.tsv"))
  (set! records (make-vector (vector-length lines) #f))
  (do ((i 0 (+ i 1))) ((>= i (vector-length lines)))
    (define pieces (split (vector-ref lines i) "\t"))  ; Load data into array
    (when (= (vector-length pieces) 9)
      (vector-set! records record-count (make-record pieces))
      (set! record-count (+ record-count 1))))
  (when (not (= record-count (vector-length records)))
    (set! records (subset records 0 record-count))))

(define (draw)
  (background 0)
  (do ((i 0 (+ i 1))) ((>= i num))
    (define this-entry (+ starting-entry i))
    (when (< this-entry record-count)
      (text (string-append (str this-entry)
                           " > "
                           (record-name (vector-ref records this-entry)))
            20
            (+ 20 (* i 20))))))

(define (mouse-pressed)
  (set! starting-entry (+ starting-entry num))
  (when (> starting-entry (vector-length records))
    ; go back to the beginning
    (set! starting-entry 0))
  (redraw))
