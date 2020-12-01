;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |275|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

;; Constants

(define LOCATION "/usr/share/dict/words")
(define AS-LIST (read-lines LOCATION))

;; Data Definitions:

; A Letter is one of the following 1Strings: 
; – "a"
; – ... 
; – "z"
; or, equivalently, a member? of this list: 
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))
(define L1 "a")
(define L2 "z")

(define-struct letter-count [letter count])
;; A LetterCount is (make-letter-count Letter Number)
;; interp. the number of occurences for a letter
(define LC1 (make-letter-count "a" 1))
(define LC2 (make-letter-count "z" 5))

(define (fn-for-letter-count lc)
  (... (fn-for-letter (letter-count-letter lc)) ;Letter
       (letter-count-count lc)))

;; Template rules used:
;; - compound: 2 fields
;; - reference: (letter-count-letter lc)) is Letter

; A Dictionary is a [List-of String]

;; Dictionary -> LetterCount
;; produces the LetterCount for the letter that occurs most often as the first one in the given Dictionary
(check-expect (most-frequent empty) (make-letter-count "a" 0))
(check-expect (most-frequent (list "add")) (make-letter-count "a" 1))
(check-expect (most-frequent (list "addd" "axx" "hhh" "qww" "rfff" "tf;f" "zx"))
              (make-letter-count "a" 2))
(check-expect (most-frequent (list "addd" "axx" "hhh" "qww" "tfff" "tff" "zx"))
              (make-letter-count "a" 2))
(check-expect (most-frequent (list "addd" "axx" "hhh" "qww" "tfff" "tff" "ttt" "zx"))
              (make-letter-count "t" 3))

;(define (most-frequent d) (make-letter-count "a" 0)) ;stub is (make-letter-count "a" 0) a good value here?


#;
(define (most-frequent d)
  ;(String LetterCount -> LetterCount)   LetterCount
  (foldr             ...                             ...       d))

; I don't see how we can use fold to go directly from Dictionary -> LetterCount

(define (most-frequent d)
  (local (; LetterCount LetterCount -> Boolean
          ; produces true if lc1 count is larger than lc2 count
          (define (higher-count? lc1 lc2) (> (letter-count-count lc1) (letter-count-count lc2)))
          )
    (first (sort (count-by-letter d) higher-count?))))

;; Dictionary -> ListOfLetterCount
;; counts how often each letter is used as the first one of a word in the given dictionary
(check-expect (count-by-letter empty)
              (list (make-letter-count "a" 0)
                    (make-letter-count "b" 0)
                    (make-letter-count "c" 0)
                    (make-letter-count "d" 0)
                    (make-letter-count "e" 0)
                    (make-letter-count "f" 0)
                    (make-letter-count "g" 0)
                    (make-letter-count "h" 0)
                    (make-letter-count "i" 0)
                    (make-letter-count "j" 0)
                    (make-letter-count "k" 0)
                    (make-letter-count "l" 0)
                    (make-letter-count "m" 0)
                    (make-letter-count "n" 0)
                    (make-letter-count "o" 0)
                    (make-letter-count "p" 0)
                    (make-letter-count "q" 0)
                    (make-letter-count "r" 0)
                    (make-letter-count "s" 0)
                    (make-letter-count "t" 0)
                    (make-letter-count "u" 0)
                    (make-letter-count "v" 0)
                    (make-letter-count "w" 0)
                    (make-letter-count "x" 0)
                    (make-letter-count "y" 0)
                    (make-letter-count "z" 0)))
(check-expect (count-by-letter (list "addd" "axx" "hhh" "qww" "tfff" "tff" "zx"))
              (list (make-letter-count "a" 2)
                    (make-letter-count "b" 0)
                    (make-letter-count "c" 0)
                    (make-letter-count "d" 0)
                    (make-letter-count "e" 0)
                    (make-letter-count "f" 0)
                    (make-letter-count "g" 0)
                    (make-letter-count "h" 1)
                    (make-letter-count "i" 0)
                    (make-letter-count "j" 0)
                    (make-letter-count "k" 0)
                    (make-letter-count "l" 0)
                    (make-letter-count "m" 0)
                    (make-letter-count "n" 0)
                    (make-letter-count "o" 0)
                    (make-letter-count "p" 0)
                    (make-letter-count "q" 1)
                    (make-letter-count "r" 0)
                    (make-letter-count "s" 0)
                    (make-letter-count "t" 2)
                    (make-letter-count "u" 0)
                    (make-letter-count "v" 0)
                    (make-letter-count "w" 0)
                    (make-letter-count "x" 0)
                    (make-letter-count "y" 0)
                    (make-letter-count "z" 1)))

;(define (count-by-letter d) empty) ;stub

#;
(define (count-by-letter d)
       (Letter -> LetterCount)
  (map      ...                  LETTERS))

#;
(define (count-by-letter d)
  (local (;; Letter -> LetterCount
          ;; produces LetterCount of how many words in d start with l
          (define (starts-with-lc l) (make-letter-count l (starts-with# l d))) ;stub

          ;; Letter Dictionary -> Number
          ;; counts how many words in d start with the l
          ;;                                (String Number -> Number)
          (define (starts-with# l d) (foldr            ...              0 d))) ;stub
    (map starts-with-lc LETTERS)))

(define (count-by-letter d)
  (local (;; Letter -> LetterCount
          ;; produces LetterCount of how many words in d start with l
          (define (starts-with-lc l) (make-letter-count l (starts-with# l d))) ;stub

          ;; Letter Dictionary -> Number
          ;; counts how many words in d start with the l
          (define (starts-with# l d)
            (local (; (String Number -> Number)
                    ; adds 1 to n if s starts with l
                    (define (starts-with-inc s n) (if (starts-with? s l)
                                                      (+ n 1)
                                                      n))
                    (define (starts-with? s l)
                      (string=? (string-ith s 0) l)))
              (foldr starts-with-inc 0 d)))) 
    (map starts-with-lc LETTERS)))

;; Dictionary -> ListOfDictionary
;; produces a list of list of words, one per first letter. empty list if words not found for a letter
(check-expect (words-by-first-letter empty)
              (list empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty))
(check-expect (words-by-first-letter (list "aaa" "aaa" "d" "dfdf" "fdd" "ggg" "yfdf" "yyy" "yyff" "z"))
              (list (list "aaa" "aaa")
                    empty
                    empty
                    (list "d" "dfdf")
                    empty
                    (list "fdd")
                    (list "ggg")
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    (list  "yfdf" "yyy" "yyff")
                    (list "z")))

;(define (words-by-first-letter d) empty) ;stub

#;
(define (words-by-first-letter d)
;      (Letter -> [List-of String])
  (map        ...                 LETTERS))

(define (words-by-first-letter d)
  (local (;; (Letter -> [List-of String])
          ;; produces a list of words from d starting with l
          (define (words-by-first-letter-with-letter l)
            (local (;; produces true if s starts with l
                    (define (starts-with-l? s) (string=? (string-ith s 0) l)))
              (filter starts-with-l? d))))
    (map  words-by-first-letter-with-letter LETTERS)))
