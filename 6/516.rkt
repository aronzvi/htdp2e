;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |516|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct expr [para body])
; an Expr is (make-expr Symbol Lam)
; interp: a lambda expression with its parameter and body

(define-struct app [fun arg])
; an Application is (make-app Lam Lam)
; interp: a lambda application with it function and argument


; A Lam is one of: 
; – Symbol
; – Expr
; – Application

(define ex1 (make-expr 'x 'x))               
(define ex2  (make-expr 'x 'y))               
(define ex3 (make-expr 'y (make-expr 'x 'y))) 
(define ex4 (make-app (make-expr 'x (make-app 'x 'x)) (make-expr 'x (make-app 'x 'x))))

(define (fn-for-expr expr)
  (... (expr-para expr)  
       (fn-for-lam (expr-body expr)))) ;Lam

(define (fn-for-app app)
  (... (fn-for-lam (app-fun app))   ;Lam
       (fn-for-lam (app-arg app)))) ;Lam

(define (fn-for-lam lam)
  (cond [(symbol? lam) (... lam)]
        [(expr? lam)
         (... (expr-para lam)   
              (fn-for-lam (expr-body lam)))] ;Lam
        [else
         (... (fn-for-lam (app-fun lam))     ;Lam
              (fn-for-lam (app-arg lam)))])) ;Lam

; Lam -> Lam 
; replaces all symbols s in le with '*undeclared
; if they do not occur within the body of a λ 
; expression whose parameter is s
(check-expect (undeclareds ex1) ex1)
(check-expect (undeclareds ex2) (make-expr 'x '*undeclared))
(check-expect (undeclareds ex3) ex3)
(check-expect (undeclareds ex4) ex4)
 
;(define (undeclareds le0) le0) ;stub

; accumulator template + template from Lam

#;
(define (undeclareds lam0)
  (local (; Lam ??? -> Lam
          ; accumulator ...
          (define (undeclareds/a lam declareds)
            (cond [(symbol? lam) (... lam)]
                  [(expr? lam)
                   (... (expr-para lam)   
                        (undeclareds/a (expr-body lam) ... declareds))] 
                  [else
                   (... (undeclareds/a (app-fun lam) ... declareds)     
                        (undeclareds/a (app-arg lam)) ... declareds)])))
    (undeclareds/a lam0 ...)))

; accumulator is exactly the same here as for the list version

(define (undeclareds lam0)
  (local (; Lam [List-of Symbol] -> Lam
          ; accumulator declareds is a list of all λ 
          ; parameters on the path from le0 to le
          (define (undeclareds/a lam declareds)
            (cond [(symbol? lam)
                   (if (member? lam declareds) lam '*undeclared)]
                  [(expr? lam)
                   (make-expr (expr-para lam)   
                              (undeclareds/a (expr-body lam)
                                             (cons (expr-para lam) declareds)))] 
                  [else
                   (make-app (undeclareds/a (app-fun lam) declareds)     
                             (undeclareds/a (app-arg lam) declareds))])))
    (undeclareds/a lam0 '()))) 