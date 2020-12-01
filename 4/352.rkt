;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |352|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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


  



  



