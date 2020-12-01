;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |116|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
1. x
is variable expression since x is a variable and every variable is an expression

2. (= y z)
is a primitive application expression since:
  y and z are variables and every variable is an expression
  = is primitive
  hence (primitive expr expr) which is a primitive application expression
   
3. (= (= y z) 0)
is primitive application expression since:
  (= y z) is expr a primitive application expression. see above
  hence (= expr 0)
  0 is a number which is a value which is also an expr
  = is a primitive
  hence (primitive expr expr) which is a primitive application expression
  