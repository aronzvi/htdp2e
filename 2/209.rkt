;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |209|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Word is one of:
; – '() or
; – (cons 1String Word)
; interpretation a Word is a list of 1Strings (letters)
(define W1 empty)
(define W2 (list "r" "a" "t")) ;rat
(define W3 (list "f" "u" "n" "n" "y")) ;funny

(define (fn-for-word w)
  (cond
    [(empty? w) (...)]
    [else
     (... (first w)
          (fn-for-word (rest w)))])) ;Word

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons 1String Word)
;; - self-reference: (rest w) is Word

;; Functions:

; String -> Word
; converts s to the chosen word representation
(check-expect (string->word "") empty)
(check-expect (string->word "h") (list "h"))
(check-expect (string->word "hello") (list "h" "e" "l" "l" "o"))

;(define (string->word s) empty) ;stub

;(define (string->word s) (...)) ;template

;; Template rules used:
;; atomic non-distinct: String

(define (string->word s)
  (explode s))

; Word -> String
; converts w to a string
(check-expect (word->string empty) "")
(check-expect (word->string (list "h")) "h")
(check-expect (word->string (list "h" "e" "l" "l" "o")) "hello")

;(define (word->string w) "") ;stub

(define (word->string w)
  (implode w))