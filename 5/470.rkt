;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |470|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define ERROR-RES-FIRST-COEFF-LESS-THAN-0 "error: result first coefficent < 0")
(define ERROR-ALL-LEADING-COEFFIFIENTS-0 "error: all leading coefficiants are 0")

; An Equation is a [List-of Number].
; constraint an Equation contains at least two numbers. 
; interpretation if (list a1 ... an b) is an Equation, 
; a1, ..., an are the left-hand-side variable coefficients 
; and b is the right-hand side

(define EQ1 '(5 3))      ;5x = 3
(define EQ2 '(5 3 -2 4)) ;5x + 3y - 2z = 4

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

(define M1 (list (list 2  3  3 8)
                 (list 2  3 -2 3)
                 (list 4 -2  2 4)))

(define (fn-for-soe soe)
  (cond [(empty? (rest soe)) (... (fn-for-equation (first soe)))] ;Equation
        [else
         (...
          (fn-for-equation (first soe)) ;Equation
          (fn-for-soe (rest soe)))]))   ;SOE

; A TM is an [NEList-of Equation]
; such that the Equations are of decreasing length: 
;   n + 1, n, n - 1, ..., 2. 
; interpretation represents a triangular matrix

(define TM1 (list (list 2 2  3 10)
                  (list   3  9 21)
                  (list      1  2)))
(define TM2 (list (list 2  3  3   8)
                  (list   -8 -4 -12)
                  (list      -5  -5)))

(define (fn-for-tm tm)
  (cond [(empty? (rest tm)) (... (fn-for-equation (first tm)))] ;Equation
        [else
         (...
          (fn-for-equation (first tm)) ;Equation
          (fn-for-tm (rest tm)))]))    ;TM

; A Solution is a [List-of Number]
  
(define S '(1 1 2))
(define S1 '(1 1 1))

; SOE -> Solution
; solves the soe
(check-expect (gauss M) S)
(check-expect (gauss M1) S1)
(check-error (gauss (list '(2 2 2 6)
                          '(2 2 4 8)
                          '(2 2 1 2)))
             ERROR-ALL-LEADING-COEFFIFIENTS-0)

;(define (gauss soe) empty) ;stub

(define (gauss soe)
  (solve (triangulate soe)))

; SOE -> TM
; triangulates the given system of equations
(check-expect (triangulate (list (list 2 2  3 10)
                                 (list 2 5 12 31)
                                 (list 4 1 -2 1)))
              (list (list 2 2  3 10)
                    (list   3  9 21)
                    (list      1  2)))
(check-expect (triangulate (list (list 2  3  3 8)
                                 (list 2  3 -2 3)
                                 (list 4 -2  2 4)))
              (list (list 2  3  3   8)
                    (list   -8 -4 -12)
                    (list      -5  -5)))
(check-error (triangulate (list (list 0 0 0)))
             ERROR-ALL-LEADING-COEFFIFIENTS-0)
(check-error (triangulate (list (list 2 2 2)
                                (list 2 2 4)
                                (list 2 2 1)))
             ERROR-ALL-LEADING-COEFFIFIENTS-0)

;(define (triangulate M) '((0 0))) ;stub

(define (triangulate soe)
  (local ((define (all-leading-coefficients-0? soe)
            (andmap (lambda (eq) (zero? (first eq))) soe)))
    (cond [(all-leading-coefficients-0? soe) (error ERROR-ALL-LEADING-COEFFIFIENTS-0)]
          [(empty? (rest soe)) soe]
        
          [else
           (local ((define rotated (rotate-equations soe)))
             (cons
              (first rotated)
              (triangulate (subtract-from-loe (first rotated) (rest rotated)))))])))

; SOE -> SOE
; rotates the equations to find one with a leading coefficient that is not 0
; generative: rotates the list by repeatedly performing: (append (rest L) (list (first L)))
; termination: we rotate the equations until we eventually have a first one whose first coefficient is not 0.
; We will not terminate for an SOE where all the equations at a given call to rotate-equations have a first coefficient of 0
(check-expect (rotate-equations (list (list  -8 -4 -12)
                                      (list   0 -5  -5)))
              (list (list  -8 -4 -12)
                    (list   0 -5  -5)))
(check-expect (rotate-equations (list (list   0 -5  -5)
                                      (list  -8 -4 -12)))
              (list (list  -8 -4 -12)
                    (list   0 -5  -5)))
(check-expect (rotate-equations (list (list   0 -5  -5)
                                      (list   0  2   3)
                                      (list  -8 -4 -12)))
              (list (list  -8 -4 -12)
                    (list   0 -5  -5)
                    (list   0  2   3)))

;(define (rotate-equations soe) soe) ;stub

(define (rotate-equations soe)
  (cond [(not (zero? (first (first soe)))) soe]
        [else
         (local ((define rotated (append (rest soe) (list (first soe)))))
           (rotate-equations rotated))]))

; Equation [List-of Equation] -> [List-of Equation]
; subtracts eq from all equations in loe
(check-expect (subtract-from-loe '(3 9 21) empty) empty)
(check-expect (subtract-from-loe '(3 9 21)
                                 '((-3 -8 -19)))
              '((1  2)))
(check-expect (subtract-from-loe '(2 2 3 10)
                                 '((2 5 12 31)
                                   (4 1 -2 1)))
              '((3 9 21)
                (-3 -8  -19)))

;(define (subtract-from-loe eq loe) loe) ;stub

(define (subtract-from-loe eq loe)
  (map (lambda (eq-i) (subtract eq-i eq)) loe))

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
;missing check-error for input that causes first coeff of result < 0

;(define (subtract eq1 eq2) eq1) ;stub

(define (subtract eq1 eq2)
  (local ((define multiple (/ (first eq1) (first eq2)))
          (define eq2-multiplied (map (lambda (n) (* n multiple)) eq2))
          (define subtracted (map - eq1 eq2-multiplied)))
    (if (zero? (first subtracted))
        (rest subtracted)
        (error ERROR-RES-FIRST-COEFF-LESS-THAN-0))))

; TM -> Solution
; produces a solution for the tm
(check-expect (solve (list '(1 2))) '(2))
(check-expect (solve TM1) S)
(check-expect (solve TM2) '(1 1 1))

;(define (solve tm) empty) ;stub

; template from TM

(define (solve tm)
  (cond [(empty? (rest tm)) (list (solve-single (first tm) empty))] 
        [else
         (local ((define solution-of-rest (solve (rest tm))))
           (cons
            (solve-single (first tm)
                          solution-of-rest)
            solution-of-rest))]))

; Equation Solution -> Solution
; solves a single linear equation with n+1 variables, given a solution for the last n variables
(check-expect (solve-single '(3 9 21) '(2))               ; 3y + 9z = 21; z = 2 
              (/ (- 21 (* 9 2)) 3))                       ; y = (21 - (9 * 2)) / 3; y = 1
(check-expect (solve-single  '(2 2  3 10) '(1 2))         ; 2x + 3y + 3z = 10; y = 1, z = 2  
              (/ (- (rhs '(2 2  3 10))                    ; x = (10 - (2 * 1 + 3 * 2)) / 2
                    (plug-in (rest (lhs '(2 2  3 10))) '(1 2)))
                 (first '(2 2  3 10))))
(check-expect (solve-single '(4 2) '())       ; 4z = 2
              (/ 2 4))                        ; z = 2 / 4

;(define (solve-single eq s-1) s-1) ;stub

; template from Equation

(define (solve-single eq s-1)
  (cond [(empty? (rest (rest eq)))
         (/ (second eq) (first eq))]
        [else
         (/ (- (rhs eq)
               (plug-in (rest (lhs eq)) s-1))
            (first eq))]))

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

; from Figure 147
; [List-of X] N -> [List-of X]
; removes the first n items from l if possible or everything
(define (drop l n)
  (cond
    [(zero? n) l]
    [(empty? l) l]
    [else (drop (rest l) (sub1 n))]))
