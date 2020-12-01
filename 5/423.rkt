;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |423|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; String Natural -> [List-of String]
; produces a list of string chunks of size n
; assumes n > 0 ??
(check-expect (partition "" 3) empty)
(check-expect (partition "abcdefg" 3)
              (list "abc" "def" "g"))
(check-expect (partition "ab" 3) (list "ab"))

;(define (partition s n) empty) ;stub

(define (partition s n)
  (cond [(string=? "" s) empty]
        [else
         (local ((define str-len (string-length s))
                 (define max-i (if (<= n str-len) n str-len)))
         (cons (substring s 0 max-i)
              (partition (substring s max-i) n)))]))