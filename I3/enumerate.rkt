;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname enumerate) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; [List-of X] -> [List-of [List-of Natural X]]
; produces a list of the same items in lox paired with their relative index
(check-expect (enumerate-existing (list "a" "b" "c")) (list (list 1 "a") (list 2 "b") (list 3 "c")))

;(define (enumerate-existing lox) empty) ;stub

(define (enumerate-existing lox)
  (map list
       (build-list (length lox)
                   (lambda (i) (add1 i)))
        lox))

; [List-of X] -> [List-of [List N X]]
; pairs each item in lx with its index 
(check-expect (enumerate '(a b c)) '((1 a) (2 b) (3 c)))

;(define (enumerate lox) empty) ;stub

(define (enumerate lox)
  (for/list ([x lox]
            [idx (length lox)])
  (list (+ idx 1) x))) 
