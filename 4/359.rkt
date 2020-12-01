;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |359|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define FN-NOT-DEFINED "error: function not defined")
(define VAR-NOT-DEFINED "error: variable not defined")

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

; BSL-fun-expr BSL-fun-def* -> Number
; evaluates ex assuming the function definitions in ex are available in da
(check-expect (eval-function* (make-fn 'f 1) da-fgh)
              (+ 3 1))
(check-expect (eval-function* (make-fn 'g 5) da-fgh)
              (+ 3 (* 2 5)))
(check-expect (eval-function* (make-fn 'h 7) da-fgh)
              (+ (+ 3 7) (+ 3 (* 2 7))))
(check-error (eval-function* (make-fn 'h (make-fn 'z 5)) da-fgh)
             FN-NOT-DEFINED)
(check-error (eval-function* (make-fn 'h (make-fn 'z 5)) da-fgh)
             FN-NOT-DEFINED)

;(define (eval-function* ex da) 0) ;stub

(define (eval-function* e da)
  (cond [(number? e) e]
        [(symbol? e) (error VAR-NOT-DEFINED)]
        [(add? e) 
         (+ (eval-function* (add-left e) da)      
            (eval-function* (add-right e) da))]           
        [(mul? e) 
         (* (eval-function* (mul-left e) da)      
            (eval-function* (mul-right e) da))]   
        [(fn? e)
         (local ((define value (eval-function* (fn-arg e) da))
                 (define def-f (lookup-def da (fn-name e)))
                 (define plug-d (subst (bsl-fun-def-body def-f) (bsl-fun-def-param def-f) value)))
           (eval-function* plug-d da))]))               

; BSL-fun-def* Symbol -> BSL-fun-def
; retrieves the definition of f in da
; signals an error if there is none
(check-expect (lookup-def da-fgh 'g) g)
(check-error (lookup-def da-fgh 'z) FN-NOT-DEFINED)

;(define (lookup-def da f) (make-bsl-fun-def 't 'x 'x)) ;stub

(define (lookup-def da f)
  (local ((define func-def (filter (lambda (fd) (symbol=? (bsl-fun-def-name fd) f))  da)))
    (if (empty? func-def)
        (error FN-NOT-DEFINED)
        (first func-def))))

; BSL-fun-expr Symbol Number -> BSL-fun-expr
; produces a BSL-var-expr like ex with all occurrences of x replaced by v
(check-expect (subst 'x 'x 2) 2)
(check-expect (subst (make-add 'x 4) 'x 5) (make-add 5 4))
(check-expect (subst (make-mul 'x 'z) 'z 7) (make-mul 'x 7))
(check-expect (subst (make-mul (make-add (make-mul 'y 5) 'y) 'z) 'y 8)
              (make-mul (make-add (make-mul 8 5) 8) 'z))
(check-expect (subst 'z 'x 2) 'z)
(check-expect (subst (make-add (make-fn 'f 'v) (make-fn 'g 'v)) 'v 5)
              (make-add (make-fn 'f 5) (make-fn 'g 5)))

;(define (subst ex x v) ex) ;stub

(define (subst ex x v)
  (cond [(number? ex) ex]
        [(symbol? ex) (if (symbol=? ex x) v ex)]
        [(add? ex) 
         (make-add (subst (add-left ex) x v)      
                   (subst (add-right ex) x v))]          
        [(mul? ex) 
         (make-mul (subst (mul-left ex) x v)      
                   (subst (mul-right ex) x v))]
        [(fn? ex)
         (make-fn (fn-name ex)
                   (subst (fn-arg ex) x v))])) 