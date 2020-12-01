;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |196|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

;; Constants

(define LOCATION "/usr/share/dict/words")
(define AS-LIST (read-lines LOCATION))

; A Letter is one of the following 1Strings: 
; – "a"
; – ... 
; – "z"
; or, equivalently, a member? of this list: 
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))
(define L1 "a")
(define L2 "z")

;; ListOfLetter is one of:
;; - empty
;; - (cons Letter ListOfLetter)
;; interp. a list of letters
(define LOT1 empty)
(define LOT2 (list "a" "b" "c" "z"))

(define (fn-for-lot lot)
  (cond
    [(empty? lot) (...)]
    [else
     (... (fn-for-letter (first lot)) ;Letter
          (fn-for-lot (rest lot)))])) ;ListOfLetter

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Letter ListOfLetter)
;; - reference: (first lot) is Letter
;; - self-reference: (rest lot) is ListOfLetter


;; ListOfString is one of:
;; - empty
;; - (cons String ListOfString)
;; interp. a lit of strings
(define LOS1 empty)
(define LOS2 (list "deeee" "doooo"))

(define (fn-for-los los)
  (cond
    [(empty? los) (...)]
    [else
     (... (first los)  
          (fn-for-los (rest los)))])) ;ListOfString

;; Template rules used:
;; one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons String ListOfString)
;; - self-reference: (rest los) is List-of-strings

; A Dictionary is a ListOfString.

(define-struct letter-counts [letter count])
;; A Letter-Counts is (make-letter-counts Letter Number)
;; interp. the number of occurences for a letter
(define LC1 (make-letter-counts "a" 1))
(define LC2 (make-letter-counts "z" 5))

(define (fn-for-letter-count lc)
  (... (fn-for-letter (letter-counts-letter lc)) ;Letter
       (letter-counts-count lc)))

;; Template rules used:
;; - compound: 2 fields
;; - reference: (letter-counts-letter lc)) is Letter

;; Functions

;; Dictionary -> ListOfLetterCounts
;; counts how often each letter is used as the first one of a word in the given dictionary
(check-expect (count-by-letter empty)
              (list (make-letter-counts "a" 0)
                    (make-letter-counts "b" 0)
                    (make-letter-counts "c" 0)
                    (make-letter-counts "d" 0)
                    (make-letter-counts "e" 0)
                    (make-letter-counts "f" 0)
                    (make-letter-counts "g" 0)
                    (make-letter-counts "h" 0)
                    (make-letter-counts "i" 0)
                    (make-letter-counts "j" 0)
                    (make-letter-counts "k" 0)
                    (make-letter-counts "l" 0)
                    (make-letter-counts "m" 0)
                    (make-letter-counts "n" 0)
                    (make-letter-counts "o" 0)
                    (make-letter-counts "p" 0)
                    (make-letter-counts "q" 0)
                    (make-letter-counts "r" 0)
                    (make-letter-counts "s" 0)
                    (make-letter-counts "t" 0)
                    (make-letter-counts "u" 0)
                    (make-letter-counts "v" 0)
                    (make-letter-counts "w" 0)
                    (make-letter-counts "x" 0)
                    (make-letter-counts "y" 0)
                    (make-letter-counts "z" 0)))
(check-expect (count-by-letter (list "addd" "axx" "hhh" "qww" "tfff" "tff" "zx"))
              (list (make-letter-counts "a" 2)
                    (make-letter-counts "b" 0)
                    (make-letter-counts "c" 0)
                    (make-letter-counts "d" 0)
                    (make-letter-counts "e" 0)
                    (make-letter-counts "f" 0)
                    (make-letter-counts "g" 0)
                    (make-letter-counts "h" 1)
                    (make-letter-counts "i" 0)
                    (make-letter-counts "j" 0)
                    (make-letter-counts "k" 0)
                    (make-letter-counts "l" 0)
                    (make-letter-counts "m" 0)
                    (make-letter-counts "n" 0)
                    (make-letter-counts "o" 0)
                    (make-letter-counts "p" 0)
                    (make-letter-counts "q" 1)
                    (make-letter-counts "r" 0)
                    (make-letter-counts "s" 0)
                    (make-letter-counts "t" 2)
                    (make-letter-counts "u" 0)
                    (make-letter-counts "v" 0)
                    (make-letter-counts "w" 0)
                    (make-letter-counts "x" 0)
                    (make-letter-counts "y" 0)
                    (make-letter-counts "z" 1)))

;(define (count-by-letter d) empty) ;stub

(define (count-by-letter d)
  (count-by-letters LETTERS d))

;; ListOfLetter Dictionary -> ListOfLetterCounts
;; counts how often each letter in lot is used as the first one of a word in d
(check-expect (count-by-letters empty (list "addd" "axx" "hhh" "qww" "tfff" "tff" "zx")) empty)
(check-expect (count-by-letters (list "a") (list "addd" "axx" "hhh" "qww" "tfff" "tff" "zx"))
              (list (make-letter-counts "a" 2)))
(check-expect (count-by-letters (list "a" "t" "x") (list "addd" "axx" "hhh" "qww" "tfff" "tff" "zx"))
              (list (make-letter-counts "a" 2)
                    (make-letter-counts "t" 2)
                    (make-letter-counts "x" 0)))

;(define (count-by-letters lot d) empty) ;stub

(define (count-by-letters lot d)
  (cond
    [(empty? lot) empty]
    [else
     (cons (make-letter-counts (first lot) (starts-with# (first lot) d))
          (count-by-letters (rest lot) d))]))

;; Letter Dictionary -> Number
;; counts how many words in d start with the l
(check-expect (starts-with# "a" empty) 0)
(check-expect (starts-with# "a" (list "afff")) 1)
(check-expect (starts-with# "a" (list "dooo" "ass")) 1)
(check-expect (starts-with# "a" (list "dooo" "bass")) 0)
(check-expect (starts-with# "a" (list "apple" "dooo" "are")) 2)

;(define (starts-with# l d) 0) ;stub

(define (starts-with# l los)
  (cond
    [(empty? los) 0]
    [else
     (if (starts-with? (first los) l)
         (+ 1 (starts-with# l (rest los)))
          (starts-with# l (rest los)))]))

;; String 1String -> Boolean
;; produces true if string s starts with letter l
(check-expect (starts-with? "afff" "a") #true)
(check-expect (starts-with? "zfff" "z") #true)
(check-expect (starts-with? "dooo" "b") #false)

;(define (starts-with? s l) #false) ; stub

;(define (starts-with? s l) (... s l)) ;Template

;; Template rules used:
;; - atomic non-distinct

(define (starts-with? s l)
  (string=? (string-ith s 0) l))

(count-by-letter AS-LIST)
