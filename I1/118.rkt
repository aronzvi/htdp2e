;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |118|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
1. (define (f x) x)
 f,x in (f x) are variables hence (variable variable)
 hence:  (define (variable variable) x)
 x is a variable which is also an expression
 hence: (define (variable variable) expr) a valid definition
 

2. (define (f x) y)
 f, x in (f x) are variables hence (variable variable)
 hence:  (define (variable variable) y)
 y is a variable which is also an expression
 hence: (define (variable variable) expr) a valid definition
 

3. (define (f x y) 3)
 f, x, y in (f x y) are variables hence (variable variable variable)
 hence: (define (variable variable variable) 3)
 3 is a value which is also an expression
 hence (define (variable variable variable) expr) a valid definition