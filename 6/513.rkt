;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |513|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct expr [para body])
; an Expr is (make-expr Symbol Lam)
; interp: a lambda expression with its parameter and body

;############ Ben doesn't like Expr name. Using fun instead ############

(define-struct fun [para body])
; an Fun is (make-fun Symbol Lam)
; interp: a lambda expression with its parameter and body

(define-struct app [fun arg])
; an Application is (make-app Lam Lam)
; interp: a lambda application with it function and argument

; A Lam is one of: 
; – Symbol
; – Fun ;Expr
; – Application

;(define ex1 (make-expr 'x 'x))               
;(define ex2  (make-expr 'x 'y))               
;(define ex3 (make-expr 'y (make-expr 'x 'y))) 
;(define ex4 (make-app (make-expr 'x (make-app 'x 'x)) (make-expr 'x (make-app 'x 'x))))

(define fun1 (make-fun 'x 'x))               
(define fun2  (make-fun 'x 'y))               
(define ex3 (make-fun 'y (make-fun 'x 'y))) 
(define ex4 (make-app (make-fun 'x (make-app 'x 'x)) (make-fun 'x (make-app 'x 'x))))