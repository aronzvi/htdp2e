;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |465|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define ERROR-RES-FIRST-COEFF-LESS-THAN-0 "error: result first coefficent < 0")

; An Equation is a [List-of Number].
; constraint an Equation contains at least two numbers. 
; interpretation if (list a1 ... an b) is an Equation, 
; a1, ..., an are the left-hand-side variable coefficients 
; and b is the right-hand side

(define (fn-for-equation eq)
  (cond [(empty? (rest (rest eq)))
         (... (first eg)
              (second eq))]
        [else
         (... (first eq)
              (fn-for-equation (rest eq)))])) ;Equation

; Equation Equation -> Equation
; “subtracts” a multiple of the second equation from the first, item by item, so that the resulting Equation has a 0 in the first position
; returns the rest of the list (drops the 0)
; assumtion: equations are of equal length
(check-expect (subtract '(0 3 9 21) '(2 2 3 10))  ;subtract 0 * eq2 times
              '(3 9 21))
(check-expect (subtract '(2 5 12 31) '(2 2 3 10))  ;subtract 1 * eq2
              '(3 9 21))
(check-expect (subtract '(4 1 -2  1) '(2 2 3 10)) ;subtract 2 * eq2
              '(-3 -8 -19))
(check-expect (subtract '(-3 -8  -19) '(3  9   21)) ;subtract -1 * eq2
              '(1 2))
;(check-error (subtract (...) ERROR-RES-FIRST-COEFF-LESS-THAN-0) ;Ben says to check-error as well but I don't know which input can cause this

;(define (subtract eq1 eq2) eq1) ;stub

#;
(define (subtract eq1 eq2)
  (local ((define multiple (/ (first eq1) (first eq2)))
          (define eq2-multiplied (map (lambda (n) (* n multiple)) eq2))
          (define subtracted (map - eq1 eq2-multiplied)))
    (if (zero? (first subtracted))
        (rest subtracted)
        subtracted)))

(define (subtract eq1 eq2)
  (local ((define multiple (/ (first eq1) (first eq2)))
          (define eq2-multiplied (map (lambda (n) (* n multiple)) eq2))
          (define subtracted (map - eq1 eq2-multiplied)))
    (if (zero? (first subtracted))
        (rest subtracted)
        (error ERROR-RES-FIRST-COEFF-LESS-THAN-0)))) ; Ben said this is needed for bad input but I don't know which input can cause this
