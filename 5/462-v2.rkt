;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 462-v2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
; the equations in the system do not have to contain all system variables.
; In this case, the variables in the equation will start from after the missing ones. See M2 below

(define M ; an SOE 
  (list (list 2 2  3 10) ; an Equation 
        (list 2 5 12 31)
        (list 4 1 -2  1)))

(define M2
  (list (list 2 2  3 10)    ; 2x + 2y + 3z = 10 
        (list   3  9 21)    ; 3y + 9z = 21
        (list      1  2)))  ; z = 2 

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
;(check-expect (check-solution M S) #true)
;(check-expect (check-solution M2 S) #true)
;(check-expect (check-solution M '(1 1 3)) #false)

(define (solves-m? s) (check-solution M s))
(define (solves-m2? s) (check-solution M2 s))
(define (does-not-solve-m? s) (not (check-solution M s)))

(check-satisfied S solves-m?)
(check-satisfied S solves-m2?)
(check-satisfied '(1 1 3) does-not-solve-m?)
;(check-satisfied S (lambda (s) (check-solution M s))) ;not sure about this and where to put it

;(define (check-solution solution soe) #false) ;stub

;used SOE template 
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
; assumption: (len solution) >= (len lhs)
; when (len solution) > (len lhs) this means that the left-hand side does not contain all the system's variables
; in this case, the given variables start from after the missing ones:
; e.g if the system has a total 3 variables (x,y,z) and the lhs has only one. We are missing 2 so the variable is z
(check-expect (plug-in '(2) '(5))
              (+ (* 2 5) 0))
(check-expect (plug-in '(2) '(5 4 3))        ;given left-hand side: 2z 
              (+ (* 2 3) 0))
(check-expect (plug-in '(2 3) '(5 4 3))      ;given left-hand side: 2y + 3z 
              (+ (* 2 4) (* 3 3)  0))
(check-expect (plug-in '(2 2 3) '(1 1 2))    ;given left-hand side: 2x + 2y + 3z
              (+ (* 2 1) (* 2 1) (* 3 2)))
(check-expect (plug-in '(4 1 -2) '(1 1 2))   ;given left-hand side: 4x + y -2z
              (+ (* 4 1) (* 1 1) (* -2 2)))

;(define (plug-in left-hand-side solution) 0) ;stub

(define (plug-in left-hand-side solution)
  (local ((define solution-only-needed-vals (drop solution (- (length solution) (length left-hand-side)))))
  (foldr (lambda (l-n s-n res) (+ (* l-n s-n) res)) 0 left-hand-side solution-only-needed-vals)))

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

; from Figure 147
; [List-of X] N -> [List-of X]
; removes the first n items from l if possible or everything
(define (drop l n)
  (cond
    [(zero? n) l]
    [(empty? l) l]
    [else (drop (rest l) (sub1 n))]))