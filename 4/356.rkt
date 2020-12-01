;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |356|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct add [left right])
(define-struct mul [left right])
(define-struct fn [name arg])

; A BSL-fun-expr is one of: 
; – Number
; – Symbol 
; – (make-add BSL-fun-expr BSL-fun-expr)
; – (make-mul BSL-fun-expr BSL-fun-expr)
; - (make-fn Symbol BSL-fun-expr)

(define BSLFNEXP0 (make-fn 'k (make-add 1 1)))
(define BSLFNEXP1 (make-mul 5 (make-fn 'k (make-add 1 1))))
(define BSLFNEXP2 (make-mul (make-fn 'i 5) (make-fn 'k (make-add 1 1 ))))

#;
(define (fn-for-fun-bsl-expr e)
  (cond [(number? e) (... e)]
        [(symbol? e) (... e)]
        [(add? e) 
         (... (fn-for-fun-bsl-expr (add-left e))      ;BSL-fun-expr
              (fn-for-fun-bsl-expr (add-right e)))]   ;BSL-fun-expr         
        [(mul? e) 
         (... (fn-for-fun-bsl-expr (mul-left e))      ;BSL-fun-expr
              (fn-for-fun-bsl-expr (mul-right e)))]   ;BSL-fun-expr
        [(fn? e)
         (... (fn-name e)                   ;Symbol
              (fn-arg e))]))                ;BSL-fun-expr