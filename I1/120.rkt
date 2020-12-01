;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |120|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
1. (x)
illegal.
x is a variable.
(variable) is not a valid function application expression witout at least two expressions
(variable) is not a valid expression as a variable expression with enclosed parentheses


2. (+ 1 (not x))
legal.
(not x):
 not is a primitive
 x is a variable whish is an expression
 hence (primitive expr)
 (primitive expr) is an expression hence expr
 

hence (+ 1 expr)
+ is a primitive
1 is a number which is a value which is an expression
hence (primitive expr expr) which is expr, a valid expression

is an expression

3. (+ 1 2 3)
legal.
+ is a primitive
1, 2, ,3 are numbers which are values which are expressions.
hence (primitive expr expr expr) which is expr

is an expression