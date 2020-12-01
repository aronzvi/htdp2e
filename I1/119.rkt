;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |119|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
1. (define (f "x") x)
"x" is a string which is a value which is an expression hence:
(f value) or (f expr)
f is a variable hence (variable value/expr) hence
(define (variable value/expr) x) which is illegal since the paretheres part of define only allows variables after the define

2. (define (f x y z) (x))
f,x,y,z are variables hence (variable variable variable variable)
(define (variable variabl variable variable) (x))
for the defintion to be legal (x) must be an expression and it is not since:
x is a variable (variable) is not a valid function application witout at least two following expressions
(variable) is not a valid expression as a variable with enclosed parentheses
