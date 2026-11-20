;; Loading Tabular Data
;; by Daniel Shiffman.
;;
;; This example demonstrates how to use loadTable()
;; to retrieve data from a CSV file and make objects
;; from that data.
;;
;; Here is what the CSV looks like:
;;
;; x,y,diameter,name
;; 160,103,43.19838,Happy
;; 372,137,52.42526,Sad
;; 273,235,61.14072,Joyous
;; 121,179,44.758068,Melancholy
; An Array of Bubble objects

(import (processing))

; A Bubble class

(define-record-type bubble (make-bubble-record x y diameter name over)
  bubble?
  (x bubble-x set-bubble-x!)
  (y bubble-y set-bubble-y!)
  (diameter bubble-diameter set-bubble-diameter!)
  (name bubble-name set-bubble-name!)
  (over bubble-over set-bubble-over!))

; Create  the Bubble
(define (make-bubble x y diameter s)
  (let ((self (make-bubble-record 0.0 0.0 0.0 "" #f)))
    (set-bubble-over! self #f)
    (set-bubble-x! self x)
    (set-bubble-y! self y)
    (set-bubble-diameter! self diameter)
    (set-bubble-name! self s)
    self))

; CHecking if mouse is over the Bubble
(define (bubble-rollover self px py)
  (define d (dist px py (bubble-x self) (bubble-y self)))
  (if (< d (/ (bubble-diameter self) 2))
    (set-bubble-over! self #t)
    (set-bubble-over! self #f)))

; Display the Bubble
(define (bubble-display self)
  (stroke 0)
  (stroke-weight 2)
  (no-fill)
  (ellipse (bubble-x self)
           (bubble-y self)
           (bubble-diameter self)
           (bubble-diameter self))
  (when (bubble-over self)
    (fill 0)
    (text-align CENTER)
    (text (bubble-name self)
          (bubble-x self)
          (+ (bubble-y self) (/ (bubble-diameter self) 2) 20))))

(define bubbles #f)
; A Table object
(define table #f)

(define (setup) (size 640 360) (load-data))

(define (draw)
  (background 255)
  ; Display all bubbles
  (vector-for-each (lambda (b) (bubble-display b) (bubble-rollover b mouse-x mouse-y))
                   bubbles)

  (text-align LEFT)
  (fill 0)
  (text "Click to add bubbles." 10 (- height 10)))

(define (load-data)
  ; Load CSV file into a Table object
  ; "header" option indicates the file has a header row
  (set! table (load-table "data.csv" "header"))

  ; The size of the array of Bubble objects is determined by the total number of rows in the CSV
  (set! bubbles (make-vector (get-row-count table) #f))

  ; You can access iterate over all the rows in a table
  (define row-count 0)
  (for-each (lambda (row)
              ; You can access the fields via their column name (or index)
              (define x (get-float row "x"))
              (define y (get-float row "y"))
              (define d (get-float row "diameter"))
              (define n (get-string row "name"))
              ; Make a Bubble object out of the data read
              (vector-set! bubbles row-count (make-bubble x y d n))
              (set! row-count (+ row-count 1)))
            (rows table)))

(define (mouse-pressed)
  ; Create a new row
  (define row (add-row table))
  ; Set the values of that row
  (set-float row "x" mouse-x)
  (set-float row "y" mouse-y)
  (set-float row "diameter" (random 40 80))
  (set-string row "name" "Blah")

  ; If the table has more than 10 rows
  (when (> (get-row-count table) 10)
    ; Delete the oldest row
    (remove-row table 0))

  ; Writing the CSV back to the same file
  (save-table table "data/data.csv")
  ; And reloading it
  (load-data))
