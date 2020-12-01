;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |354|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define ERROR "error: non numeric expression")

(define-struct add [left right])
(define-struct mul [left right])

; A BSL-var-expr is one of: 
; – Number
; – Symbol 
; – (make-add BSL-var-expr BSL-var-expr)
; – (make-mul BSL-var-expr BSL-var-expr)

(define (fn-for-bsl-vexp bvexp)
  (cond [(number? bvexp) (... bvexp)]
        [(symbol? bvexp) (... bvexp)]
        [(add? bvexp) 
         (... (fn-for-bsl-vexp (add-left bvexp))      ;BSL-var-expr
              (fn-for-bsl-vexp (add-right bvexp)))]   ;BSL-var-expr         
        [(mul? bvexp) 
         (... (fn-for-bsl-vexp (mul-left bvexp))      ;BSL-var-expr
              (fn-for-bsl-vexp (mul-right bvexp)))])) ;BSL-var-expr

(define BSLVEXP0 'x)
(define BSLVEXP1 (make-add 'x 3))
(define BSLVEXP2 (make-mul 'x 3)) 
(define BSLVEXP3 (make-add (make-mul 'x 'x)
                           (make-mul 'y 'y)))

; An AL (short for association list) is [List-of Association].
; An Association is a list of two items:
;   (cons Symbol (cons Number '())).

(define AL0 empty)
(define AL1 (list (list 'x 1)))
(define AL2 '((x 1) (y 2) (z 3)))

; BSL-var-expr AL -> Number or ERROR
; applies subst to all associations in da.
; If numeric? holds for the result, it determines its value; otherwise it signals the same error as eval-variable.
(check-expect (eval-variable* 2 empty) 2)
(check-expect (eval-variable* 2 '((x 1) (y 2) (z 3))) 2)
(check-expect (eval-variable* (make-add 'x 3) '((x 1) (y 2) (z 3)))
              (eval-variable (make-add 1 3)))
(check-expect (eval-variable* (make-mul 'x 'y) '((x 1) (y 2) (z 3)))
              (eval-variable (make-mul 1 2)))
(check-expect (eval-variable* (make-add (make-mul 'x (make-mul 'y (make-add 'x (make-add 'z (make-add 'y 4))))) 'z)
                              '((x 1) (y 2) (z 3)))
              (eval-variable (make-add (make-mul 1 (make-mul 2 (make-add 1 (make-add 3 (make-add 2 4))))) 3)))
(check-error (eval-variable* (make-mul 'v 'n) '((x 1) (y 2) (z 3)))
             ERROR)
(check-error (eval-variable* (make-mul 'z (make-mul 'x (make-add 'y 'v))) '((x 1) (y 2) (z 3)))
             ERROR)

;(define (eval-variable* ex da) 0) ;stub

(define (eval-variable* ex da)
  (eval-variable (foldr (lambda (a exp) (subst exp (first a) (second a))) ex da)))

; BSL-var-expr -> Number or ERROR
; determines the value of ex if numeric? yields true for ex. Otherwise signals an error.
(check-expect (eval-variable 2) 2)
(check-error (eval-variable 'x) ERROR)
(check-expect (eval-variable (make-add 4 4)) (eval-expression (make-add 4 4)))
(check-error (eval-variable (make-add 'x 4)) ERROR)
(check-error (eval-variable (make-mul 8 'z)) ERROR)
(check-expect (eval-variable (make-add (make-mul 1 1) 10)) (eval-expression (make-add (make-mul 1 1) 10)))
(check-error (eval-variable (make-add (make-mul 4 (make-mul 4 'd)) 9)) ERROR)

;(define (eval-variable ex) 0) ;stub

(define (eval-variable ex)
  (if (numeric? ex)
      (eval-expression ex)
      (error ERROR)))

; BSL-var-expr -> Boolean
; determines whether a BSL-var-expr is also a BSL-expr - produces true if ex has no symbols
(check-expect (numeric? 2) true)
(check-expect (numeric? 'x) false)
(check-expect (numeric? (make-add 4 4)) true)
(check-expect (numeric? (make-add 'x 4)) false)

;(define (numeric? ex) false) ;stub

(define (numeric? bvexp)
  (cond [(number? bvexp) true]
        [(symbol? bvexp) false]
        [(add? bvexp) 
         (and (numeric? (add-left bvexp))      
              (numeric? (add-right bvexp)))]         
        [(mul? bvexp) 
         (and (numeric? (mul-left bvexp))      
              (numeric? (mul-right bvexp)))]))

; BSL-expr -> Number
; computes value of bexp
(check-expect (eval-expression 3) 3)
(check-expect (eval-expression (make-add 1 1)) 2)
(check-expect (eval-expression (make-mul 3 10)) 30)
(check-expect (eval-expression (make-add (make-mul 1 1) 10)) 11)

;(define (eval-expression bexp) 0) ;stub

(define (eval-expression bexp)
  (local ((define (fn-for-add-exp aexp)
            (+ (fn-for-bsl-exp (add-left aexp))    
               (fn-for-bsl-exp (add-right aexp)))) 

          (define (fn-for-mul-exp mexp)
            (* (fn-for-bsl-exp (mul-left mexp))    
               (fn-for-bsl-exp (mul-right mexp)))) 

          (define (fn-for-bsl-exp bexp)
            (cond [(number? bexp) bexp]
                  [(add? bexp) (fn-for-add-exp bexp)]             
                  [(mul? bexp) (fn-for-mul-exp bexp)])))  
    (fn-for-bsl-exp bexp)))

; BSL-var-expr Symbol Number -> BSL-var-expr
; produces a BSL-var-expr like ex with all occurrences of x replaced by v
(check-expect (subst 'x 'x 2) 2)
(check-expect (subst (make-add 'x 4) 'x 5) (make-add 5 4))
(check-expect (subst (make-mul 'x 'z) 'z 7) (make-mul 'x 7))
(check-expect (subst (make-mul (make-add (make-mul 'y 5) 'y) 'z) 'y 8)
              (make-mul (make-add (make-mul 8 5) 8) 'z))
(check-expect (subst 'z 'x 2) 'z)

;(define (subst ex x v) ex) ;stub

(define (subst bvexp x v)
  (cond [(number? bvexp) bvexp]
        [(symbol? bvexp) (if (symbol=? bvexp x) v bvexp)]
        [(add? bvexp) 
         (make-add (subst (add-left bvexp) x v)      
                   (subst (add-right bvexp) x v))]          
        [(mul? bvexp) 
         (make-mul (subst (mul-left bvexp) x v)      
                   (subst (mul-right bvexp) x v))])) 
