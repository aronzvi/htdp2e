;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |355|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
(check-expect (eval-var-lookup 2 empty) 2)
(check-expect (eval-var-lookup 2 '((x 1) (y 2) (z 3))) 2)
(check-expect (eval-var-lookup (make-add 'x 3) '((x 1) (y 2) (z 3)))
              (+ 1 3))
(check-expect (eval-var-lookup (make-mul 'x 'y) '((x 1) (y 2) (z 3)))
              (* 1 2))
(check-expect (eval-var-lookup (make-add (make-mul 'x (make-mul 'y (make-add 'x (make-add 'z (make-add 'y 4))))) 'z)
                              '((x 1) (y 2) (z 3)))
              (+ (* 1 (* 2 (+ 1 (+ 3 (+ 2 4))))) 3))
(check-error (eval-var-lookup (make-mul 'v 'n) '((x 1) (y 2) (z 3)))
             ERROR)
(check-error (eval-var-lookup (make-mul 'z (make-mul 'x (make-add 'y 'v))) '((x 1) (y 2) (z 3)))
             ERROR)

;(define (eval-var-lookup ex da) 0) ;stub

(define (eval-var-lookup e da)
  (cond [(number? e) e]
        [(symbol? e) (if (false? (assq e da))
                         (error ERROR)
                         (second (assq e da)))]
        [(add? e) 
         (+ (eval-var-lookup (add-left e) da)      
              (eval-var-lookup (add-right e) da))]            
        [(mul? e) 
         (* (eval-var-lookup (mul-left e) da)      
              (eval-var-lookup (mul-right e) da))])) 
