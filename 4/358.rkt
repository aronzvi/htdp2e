;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |358|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define FN-LOOKUP-ERROR "error: function not found")

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


(define-struct bsl-fun-def (name param body))
; A BSL-fun-def is (make-bsl-fun-def Symbol Symbol BSL-fun-expr)
; interp. a function definition where:
;   - name is the function's name
;   - param is the function’s parameter, which is also a name
;   - body is the function’s body, which is a function expression

(define f (make-bsl-fun-def 'f 'x (make-add 3 'x)))
(define g (make-bsl-fun-def 'g 'y (make-fn 'f (make-mul 2 'y))))
(define h (make-bsl-fun-def 'h 'v (make-add (make-fn 'f 'v) (make-fn 'g 'v))))

; BSL-fun-def* is [List-of BSL-fun-def]

(define da-fgh (list f g h))

; BSL-fun-def* Symbol -> BSL-fun-def
; retrieves the definition of f in da
; signals an error if there is none
(check-expect (lookup-def da-fgh 'g) g)
(check-error (lookup-def da-fgh 'z) FN-LOOKUP-ERROR)

;(define (lookup-def da f) (make-bsl-fun-def 't 'x 'x)) ;stub

(define (lookup-def da f)
  (local ((define func-def (filter (lambda (fd) (symbol=? (bsl-fun-def-name fd) f))  da)))
    (if (empty? func-def)
        (error FN-LOOKUP-ERROR)
        (first func-def))))

