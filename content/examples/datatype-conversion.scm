;; Datatype Conversion.
;;
;; It is sometimes beneficial to convert a value from one type of
;; data to another. Each of the conversion functions converts its parameter
;; to an equivalent representation within its datatype.
;; The conversion functions include int(), float(), char(), byte(), and others.

(import (processing))

(define (setup)
  (size 640 360)
  (background 0)
  (no-stroke)

  (text-font (create-font "SourceCodePro-Regular.ttf" 24))

  (define c 0)  ; Chars are used for storing alphanumeric symbols
  (define f 0.0)  ; Floats are decimal numbers
  (define i 0)  ; Integers are values between 2,147,483,647 and -2147483648
  (define b 0)  ; Bytes are values between -128 and 127

  (set! c #\A)
  (set! f (float c))  ; Sets f = 65.0
  (set! i (int (* f 1.4)))  ; Sets i to 91
  (set! b (byte (/ (char->integer c) 2)))  ; Sets b to 32

  ; println(f);
  ; println(i);
  ; println(b);

  (text (string-append "The value of variable c is " (str c)) 50 100)
  (text (string-append "The value of variable f is " (str f)) 50 150)
  (text (string-append "The value of variable i is " (str i)) 50 200)
  (text (string-append "The value of variable b is " (str b)) 50 250))
