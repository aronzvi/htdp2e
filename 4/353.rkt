;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |353|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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