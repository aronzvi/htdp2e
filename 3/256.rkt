;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |256|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [X] [X -> Number] [NEList-of X] -> X 
; finds the (first) item in lx that maximizes f
; if (argmax f (list x-1 ... x-n)) == x-i, 
; then (>= (f x-i) (f x-1)), (>= (f x-i) (f x-2)), ...
;(define (argmax f lx) ...)

; produces the (first )x in nelx for which the result of (f x) is the largest

(check-expect (argmax add1 (list 1 3 5 4)) 5)
(check-expect (argmax string-length (list "d" "ssdsd" "ddd" "wwwwww")) "wwwwww")

; [X] [X -> Number] [NEList-of X] -> X 
; finds the (first) item in lx that minimizes f
; if (argmin f (list x-1 ... x-n)) == x-i, 
; then (<= (f x-i) (f x-1)), (<= (f x-i) (f x-2)), ...
;(define (argmin f lx) ...)

; produces the (first) x in nelx for which the result of (f x) is the smallest

(check-expect (argmin add1 (list 1 3 5 4)) 1)
(check-expect (argmin string-length (list "d" "ssdsd" "ddd" "wwwwww")) "d")