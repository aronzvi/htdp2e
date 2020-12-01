;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |347|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct add [left right])
; an Add-Expr is (make-add BSL-expr BSL-expr)
; interp. represents BSL expresion (+ BSL-expr BSL-expr) 

(define-struct mul [left right])
; an Mul-Expr is (make-mul BSL-expr BSL-expr)
; interp. represents BSL expresion (* BSL-expr BSL-expr)

; An BSL-expr is one of: 
; – Number
; – Add-Expr
; - Mul-Expr

(define BSLEXP0 3)
(define BSLEXP1 (make-add 10 -10))
(define BSLEXP2 (make-add (make-mul 20 3) 33))
(define BSLEXP3 (make-add (make-mul 3.14 (make-mul 2 3)) (make-mul 3.14 (make-mul -1 -9))))

(define (fn-for-bsl-exp bexp)
  (local ((define (fn-for-add-exp aexp)
            (... (fn-for-bsl-exp (add-left aexp))    ;BSL-expr
                 (fn-for-bsl-exp (add-right aexp)))) ;BSL-expr

          (define (fn-for-mul-exp mexp)
            (... (fn-for-bsl-exp (mul-left mexp))    ;BSL-expr
                 (fn-for-bsl-exp (mul-right mexp)))) ;BSL-expr

          (define (fn-for-bsl-exp bexp)
            (cond [(number? bexp) (... bexp)]
                  [(add? bexp) (... (fn-for-add-exp bexp))]     ;Add-Expr           
                  [(mul? bexp) (... (fn-for-mul-exp bexp))])))  ;Mul-Expr
    (fn-for-bsl-exp bexp)))

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
