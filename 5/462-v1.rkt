;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 462-v1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; An SOE is a non-empty Matrix.
; constraint for (list r1 ... rn), (length ri) is (+ n 1)
; interpretation represents a system of linear equations

(define M ; an SOE 
  (list (list 2 2  3 10) ; an Equation 
        (list 2 5 12 31)
        (list 4 1 -2  1)))

(define (fn-for-soe soe)
  (cond [(empty? (rest soe)) (... (fn-for-equation (first soe)))] ;Equation
        [else
         (...
          (fn-for-equation (first soe)) ;Equation
          (fn-for-soe (rest soe)))]))   ;SOE
 
; A Solution is a [List-of Number]
; interp, list of values for variables in order ... x, y, z 
(define S '(1 1 2)) ; a Solution. x=1,y=1,z=1
 
; SOE Solution -> Boolean
; produces #true if plugging in the numbers from solution for the variables in the Equations of soe produces equal left-hand-side values and right-hand-side values
(check-expect (check-solution M S) #true)
(check-expect (check-solution M '(1 1 3)) #false)
(check-satisfied S (lambda (s) (check-solution M s))) ;not sure about this and where to put it

;(define (check-solution solution soe) #false) ;stub

;template from SOE 
#;
(define (check-solution soe solution)
  (local ((define (equation-lhs-rhs=? eq) (= (plug-in (lhs eq) solution) (rhs eq))))
    (cond [(empty? (rest soe)) (equation-lhs-rhs=? (first soe))]
          [else
           (and
            (equation-lhs-rhs=? (first soe))
            (check-solution (rest soe) solution))])))

(define (check-solution soe solution)
  (andmap (lambda (eq) (= (plug-in (lhs eq) solution) (rhs eq))) soe))

; [List-of Number] Solution -> Number
; calculates out the value of the left-hand side when the numbers from the solution are plugged in for the variables
; assumption: the left-hand side alwasys contains all the system's variables: (length solution) = (length lhs)
(check-expect (plug-in '(2) '(5))
              (+ (* 2 5) 0))
(check-expect (plug-in '(-2) '(5))
              (+ (* -2 5) 0))
(check-expect (plug-in '(2 4) '(5 4))        
              (+ (* 2 5) (* 4 4) 0))
(check-expect (plug-in '(2 2 3) '(1 1 2))    
              (+ (* 2 1) (* 2 1) (* 3 2)))
(check-expect (plug-in '(4 1 -2) '(1 1 2)) 
              (+ (* 4 1) (* 1 1) (* -2 2)))

;(define (plug-in left-hand-side solution) 0) ;stub

; left-hand-side and solution are guaranteed to be of same length and are processed in parallel
; Processing Two Lists Simultaneously: Case 2
#;
(define (plug-in left-hand-side solution)
  (cond [(empty? left-hand-side) 0]
        [else
         (+ (* (first left-hand-side) (first solution))
            (plug-in (rest left-hand-side) (rest solution)))]))

(define (plug-in left-hand-side solution)
  (foldr (lambda (l-n s-n res) (+ (* l-n s-n) res)) 0 left-hand-side solution))

; Equation -> [List-of Number]
; extracts the left-hand side from a row in a matrix
(check-expect (lhs (first M)) '(2 2 3))
(define (lhs e)
  (reverse (rest (reverse e))))
 
; Equation -> Number
; extracts the right-hand side from a row in a matrix
(check-expect (rhs (first M)) 10)
(define (rhs e)
  (first (reverse e)))