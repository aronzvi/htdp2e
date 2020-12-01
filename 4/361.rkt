;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |361|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
              (fn-for-fun-bsl-expr (fn-arg e)))]))                ;BSL-fun-expr


; An AL (short for association list) is [List-of Association].
; An Association is a list of two items:
;   (cons Symbol (cons Number '())).

(define AL0 empty)
(define AL1 (list (list 'x 1)))
(define AL2 '((x 1) (y 2) (z 3)))

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

(define-struct bsl-da-all (constants functions))
; A BSL-da-all is is (make-bsl-da-all AL BSL-fun-def*)
; interp. all constants and function definitions

(define (fn-for-bsl-da-all da)
  (... (fn-for-al (bsl-da-all-constants da))             ;AL 
       (fn-for-bsn-fun-def* (bsl-da-all-functions da)))) ;BSL-fun-def*

(define DA-ALL0 (make-bsl-da-all '((close-to-pi 3.14))
                                 (list (make-bsl-fun-def 'area-of-circle
                                                         'r
                                                         (make-mul 'close-to-pi (make-mul 'r 'r)))
                                       (make-bsl-fun-def 'volume-of-10-cylinder
                                                         'r
                                                         (make-mul 10 (make-fn 'area-of-circle 'r))))))

; BSL-fun-expr BSL-da-all -> Number
; produces the value of the expression ex assuming that all appropriate definitions are available in da
(check-within (eval-all (make-fn 'area-of-circle 1) DA-ALL0)
              #i3.14
              0.001)
(check-within (eval-all (make-fn 'volume-of-10-cylinder 1) DA-ALL0)
              #i31.400000000000002
              0.001)
(check-within (eval-all (make-mul 3 'close-to-pi) DA-ALL0)
              #i9.42
              0.001)

;(define (eval-all ex da) 0) ;stub

(define (eval-all e da)
  (cond [(number? e) e]
        [(symbol? e) (second (lookup-con-def da e))]
        [(add? e) 
         (+ (eval-all (add-left e) da)     
            (eval-all (add-right e) da))]      
        [(mul? e) 
         (* (eval-all (mul-left e) da)      
            (eval-all (mul-right e) da))]   
        [(fn? e)
         (local ((define value (eval-all (fn-arg e) da))
                 (define def-f (lookup-fun-def da (fn-name e)))
                 (define plug-d (subst (bsl-fun-def-body def-f) (bsl-fun-def-param def-f) value)))              
           (eval-all plug-d da))]))               

; BSL-da-all Symbol -> Association
; produces the constant definition representation for x if exists in da; otherwise signals an error
(check-expect (lookup-con-def DA-ALL0 'close-to-pi) '(close-to-pi 3.14))
(check-error (lookup-con-def DA-ALL0 'f) "'f: no such constant can be found")

;(define (lookup-con-def da x) '(z 0)) ;stub

(define (lookup-con-def da x)
  (local ((define con-def (assq x (bsl-da-all-constants da))))
    (if (false? con-def)
        (error (string-append "'" (symbol->string x) ": no such constant can be found"))
        con-def)))

; BSL-da-all Symbol -> BSL-fun-def
; produces the function definition representation for f if exists in da; otherwise signals an error
(check-expect (lookup-fun-def DA-ALL0 'area-of-circle)
              (make-bsl-fun-def 'area-of-circle
                                'r
                                (make-mul 'close-to-pi (make-mul 'r 'r))))
(check-error (lookup-fun-def DA-ALL0 'test) "'test: no such function can be found")

;(define (lookup-fun-def da f) (make-bsl-fun-def 'z 'x 'x)) ;stub

(define (lookup-fun-def da f)
  (local ((define func-def (filter (lambda (fd) (symbol=? (bsl-fun-def-name fd) f)) (bsl-da-all-functions da))))
    (if (empty? func-def)
        (error "'" (symbol->string f) ": no such function can be found")
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
