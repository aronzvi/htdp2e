;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |212|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Data definitions

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

; A List-of-words is one of:
; - empty
; - (cons Word List-of-words)
; interp. a list of Words
(define LOW1 empty)
(define LOW2 (list W2 W3))

(define (fn-for-low low)
  (cond
    [(empty? low) (...)]
    [else
     (... (fn-for-word (first low))   ;Word
          (fn-for-low (rest low)))])) ;List-of-words

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Word List-of-words)
;; - reference: (first low) is Word
;; - self-reference: (rest low) is List-of-words

;; Functions:

; Word -> List-of-words
; finds all rearrangements of word
;(check-expect (arrangements empty) ???)
(check-expect (arrangements (list "d" "e")) (list (list "d" "e") (list "d" "d")))

(define (arrangements word)
  (list word))