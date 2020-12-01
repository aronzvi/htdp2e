;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |195|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

;; Functions:

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

(starts-with# "e" AS-LIST)
(starts-with# "z" AS-LIST)
