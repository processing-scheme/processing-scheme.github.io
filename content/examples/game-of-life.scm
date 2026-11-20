;; Game of Life
;; by Joan Soler-Adillon.
;;
;; Press SPACE BAR to pause and change the cell's values
;; with the mouse. On pause, click to activate/deactivate
;; cells. Press 'R' to randomly reset the cells' grid.
;; Press 'C' to clear the cells' grid. The original Game
;; of Life was created by John Conway in 1970.
; Size of cells

(import (processing))

; a vector of rows, each a vector of cols values (Java's new int[rows][cols])
(define (make-matrix rows cols v)
  (let ((m (make-vector rows #f)))
    (do ((i 0 (+ i 1))) ((= i rows) m)
      (vector-set! m i (make-vector cols v)))))

(define cell-size 5)

; How likely for a cell to be alive at start (in percentage)
(define probability-of-alive-at-start 15)

; Variables for timer
(define interval 100)
(define last-recorded-time 0)

; Colors for active/inactive cells
(define alive (color 0 200 0))
(define dead (color 0))

; Array of cells
(define cells #f)
; Buffer to record the state of the cells and use this
; while changing the others in the interations
(define cells-buffer #f)

; Pause
(define pause #f)

(define (setup)
  (size 640 360)

  ; Instantiate arrays
  (set! cells (make-matrix (quotient width cell-size) (quotient height cell-size) 0))
  (set! cells-buffer
        (make-matrix (quotient width cell-size) (quotient height cell-size) 0))

  ; This stroke will draw the background grid
  (stroke 48)

  (no-smooth)

  ; Initialization of cells
  (do ((x 0 (+ x 1))) ((>= x (/ width cell-size)))
    (do ((y 0 (+ y 1))) ((>= y (/ height cell-size)))
      (define state (random 100))
      (if (> state probability-of-alive-at-start) (set! state 0) (set! state 1))
      ; Save state of each cell
      (vector-set! (vector-ref cells x) y (int state))))
  ; Fill in black in case cells don't cover all the windows
  (background 0))

(define (draw)
  ; Draw grid
  (do ((x 0 (+ x 1))) ((>= x (/ width cell-size)))
    (do ((y 0 (+ y 1))) ((>= y (/ height cell-size)))
      (cond
        ((= (vector-ref (vector-ref cells x) y) 1)
         ; If alive
         (fill alive))
        (else
          ; If dead
          (fill dead)))
      (rect (* x cell-size) (* y cell-size) cell-size cell-size)))
  ; Iterate if timer ticks
  (when (> (- (millis) last-recorded-time) interval)
    (when (not pause) (iteration) (set! last-recorded-time (millis))))

  ; Create  new cells manually on pause
  (cond
    ((and pause mouse-is-pressed)
     ; Map and avoid out of bound errors
     (define x-cell-over (int (map mouse-x 0 width 0 (/ width cell-size))))
     (set! x-cell-over (constrain x-cell-over 0 (- (/ width cell-size) 1)))
     (define y-cell-over (int (map mouse-y 0 height 0 (/ height cell-size))))
     (set! y-cell-over (constrain y-cell-over 0 (- (/ height cell-size) 1)))

     ; Check against cells in buffer
     (cond
       ((= (vector-ref (vector-ref cells-buffer x-cell-over) y-cell-over) 1)
        ; Cell is alive
        (vector-set! (vector-ref cells x-cell-over) y-cell-over 0)  ; Kill
        ; Fill with kill color
        (fill dead))
       (else
         ; Cell is dead
         (vector-set! (vector-ref cells x-cell-over) y-cell-over 1)  ; Make alive
         ; Fill alive color
         (fill alive))))
    ((and pause (not mouse-is-pressed))
     ; And then save to buffer once mouse goes up
     ; Save cells to buffer (so we opeate with one array keeping the other intact)
     (do ((x 0 (+ x 1))) ((>= x (/ width cell-size)))
       (do ((y 0 (+ y 1))) ((>= y (/ height cell-size)))
         (vector-set! (vector-ref cells-buffer x) y (vector-ref (vector-ref cells x) y)))))))

(define (iteration)
  ; When the clock ticks
  ; Save cells to buffer (so we opeate with one array keeping the other intact)
  (do ((x 0 (+ x 1))) ((>= x (/ width cell-size)))
    (do ((y 0 (+ y 1))) ((>= y (/ height cell-size)))
      (vector-set! (vector-ref cells-buffer x) y (vector-ref (vector-ref cells x) y))))

  ; Visit each cell:
  ; End of x loop
  (do ((x 0 (+ x 1))) ((>= x (/ width cell-size)))
    ; End of y loop
    (do ((y 0 (+ y 1))) ((>= y (/ height cell-size)))
      ; And visit all the neighbours of each cell
      (define neighbours 0)  ; We'll count the neighbours
      (do ((xx (- x 1) (+ xx 1))) ((> xx (+ x 1)))
        ; End of yy loop
        (do ((yy (- y 1) (+ yy 1))) ((> yy (+ y 1)))
          ; End of if
          (when (and (>= xx 0)
                (< xx (/ width cell-size))
                (>= yy 0)
                (< yy (/ height cell-size)))
            ; Make sure you are not out of bounds
            ; End of if
            (when (not (and (= xx x) (= yy y)))
              ; Make sure to to check against self
              (when (= (vector-ref (vector-ref cells-buffer xx) yy) 1)
                ; Check alive neighbours and count them
                (set! neighbours (+ neighbours 1)))))))  ; End of xx loop
      ; We've checked the neigbours: apply rules!
      ; End of if
      (cond
        ((= (vector-ref (vector-ref cells-buffer x) y) 1)
         ; The cell is alive: kill it if necessary
         (when (or (< neighbours 2) (> neighbours 3))
           ; Die unless it has 2 or 3 neighbours
           (vector-set! (vector-ref cells x) y 0)))
        (else
          ; The cell is dead: make it live if necessary
          (when (= neighbours 3)
            ; Only if it has 3 neighbours
            (vector-set! (vector-ref cells x) y 1)))))))  ; End of function

(define (key-pressed)
  (when (or (eqv? key #\r) (eqv? key #\R))
    ; Restart: reinitialization of cells
    (do ((x 0 (+ x 1))) ((>= x (/ width cell-size)))
      (do ((y 0 (+ y 1))) ((>= y (/ height cell-size)))
        (define state (random 100))
        (if (> state probability-of-alive-at-start) (set! state 0) (set! state 1))
        ; Save state of each cell
        (vector-set! (vector-ref cells x) y (int state)))))
  (when (eqv? key #\space)
    ; On/off of pause
    (set! pause (not pause)))
  (when (or (eqv? key #\c) (eqv? key #\C))
    ; Clear all
    (do ((x 0 (+ x 1))) ((>= x (/ width cell-size)))
      (do ((y 0 (+ y 1))) ((>= y (/ height cell-size)))
        ; Save all to zero
        (vector-set! (vector-ref cells x) y 0)))))
