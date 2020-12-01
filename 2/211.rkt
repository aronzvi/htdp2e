;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |211|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

;; Constants:

(define LOCATION "/usr/share/dict/words")
(define AS-LIST (read-lines LOCATION))

;; Data definitions:

;; List-of-strings is one of:
;; - empty
;; - (cons String List-of-strings)
;; interp. a list of strings
(define LOS1 empty)
(define LOS2 (list "apple" "hello" "orange"))

(define (fn-for-los los)
  (cond
    [(empty? los) (...)]
    [else
     (... (first los)
          (fn-for-los (rest los)))])) ;List-of-strings

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons String List-of-strings)
;; - self-reference: (rest los) is List-of-strings

; A Dictionary is a List-of-strings.

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

; List-of-strings -> Boolean
(define (all-words-from-rat? w)
  (and (member? "rat" w)
       (member? "art" w)
       (member? "tar" w)))

; String -> List-of-strings
; finds all words that use the same letters as s
(check-member-of (alternative-words "cat")
                 (list "act" "cat")
                 (list "cat" "act"))
(check-satisfied (alternative-words "rat")
                 all-words-from-rat?)

;(define (alternative-words s) empty) ;stub

(define (alternative-words s)
  (in-dictionary (words->strings (arrangements (string->word s)))))

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

; Word -> List-of-words
; finds all rearrangements of word
(define (arrangements word)
  (list word))

;; List-of-words -> List-of-strings
;; produces list of strings from low
(check-expect (words->strings empty) empty)
(check-expect (words->strings (list (list "h"))) (list "h"))
(check-expect (words->strings (list (list "h" "e" "l" "l"))) (list "hell"))
(check-expect (words->strings (list (list "h" "e" "l" "l") (list "w" "o" "r" "l" "d"))) (list "hell" "world"))

;(define (words->strings low) empty) ;stub

(define (words->strings low)
  (cond
    [(empty? low) empty]
    [else
     (cons (word->string (first low))   
           (words->strings (rest low)))])) 

; Word -> String
; converts w to a string
(check-expect (word->string empty) "")
(check-expect (word->string (list "h")) "h")
(check-expect (word->string (list "h" "e" "l" "l" "o")) "hello")

;(define (word->string w) "") ;stub

(define (word->string w)
  (implode w))

; List-of-strings -> List-of-strings
; picks out all those Strings in los that occur in the AS-LIST dictionary
(check-expect (in-dictionary empty) empty)
(check-expect (in-dictionary (list "apple")) (list "apple"))
(check-expect (in-dictionary (list "apple" "ggg")) (list "apple"))
(check-expect (in-dictionary (list "apple" "ggg" "orange" "hhh")) (list "apple" "orange"))

;(define (in-dictionary los) empty) ;stub

(define (in-dictionary los)
  (cond
    [(empty? los) empty]
    [else
     (if (member? (first los) AS-LIST)
         (cons (first los) (in-dictionary (rest los)))
               (in-dictionary (rest los)))])) 

