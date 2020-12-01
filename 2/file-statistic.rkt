;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname file-statistic) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

;; Data definitions:

;; ListOfString is one of:
;; - empty
;; - (cons String ListOfString)
;; interp. a list of strings
(define LINE0 (cons "hello" (cons "world" '())))
(define LINE1 '())

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons String ListOfString)
;; - self-reference: (rest los) is ListOfString

(define (fn-for-los los)
  (cond
    [(empty? los) (...)]
    [else
     (... (first los)                 ;String
          (fn-for-los (rest los)))])) ;ListOfString

;; LLS is one of:
;; - empty
;; (cons ListOfString LLS)
;; interp. a list of ListOfString
(define LLS0 '())
(define LLS1 (cons LINE0 (cons LINE1 '())))

(define (fn-for-lls lls)
  (cond
    [(empty? lls) (...)]
    [else
     (... (fn-for-los (first lls))    ;ListOfString
          (fn-for-lls (rest lls)))])) ;LLS

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons ListOfString ListOfListOfString)
;; - reference:  (first lolos) is ListOfString
;; - self-reference: (rest lolos) is LLS

;; Functions:

; LLS -> List-of-numbers
; determines the number of words on each line
(check-expect (words-on-line LLS0) empty)
(check-expect (words-on-line LLS1)
              (cons 2 (cons 0 empty)))

;(define (words-on-line lls) '()) ;stub

(define (words-on-line lls)
  (cond
    [(empty? lls) empty]
    [else
     (cons (length (first lls))      
           (words-on-line (rest lls)))])) 

; String -> List-of-numbers
; counts the words on each line in the given file
(define (file-statistic file-name)
  (words-on-line
    (read-words/line file-name)))

(file-statistic "ttt.txt")