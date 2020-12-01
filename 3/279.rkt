;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |279|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;1.
; legal 
(lambda (x y) (x y y))

;2.
; ilegal - need at leas one variable
(lambda () 10)

;3.
; legal - in form of (lambda (variable-1 ... variable-N) expression)
(lambda (x) x)

;4.
; legal - (lambda (variable-1 ... variable-N) expression)
(lambda (x y) x)

;5.
; ilegal - missing parenthaces around x
(lambda x 10)


