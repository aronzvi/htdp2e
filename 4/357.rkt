;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |357|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define ERROR-VAR "error: non numeric expression")
(define ERROR-FN-NOT-FOUND "error: function not found")

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

; BSL-fun-expr Symbol Symbol BSL-fun-expr -> Number
; determines the value of ex given:
; - f the function name, 
; - x the functions’s parameter
; - b the function’s body
(check-expect (eval-definition1 2 'f 'x (make-mul 2 'x)) 2)
(check-expect (eval-definition1 (make-add 1 1) 'f 'x (make-mul 2 'x)) 2)
(check-expect (eval-definition1 (make-mul 1 1) 'f 'x (make-mul 2 'x)) 1)
(check-expect (eval-definition1 (make-fn 'f 2) 'f 'n (make-mul 2 'n))
              (* 2 2))
(check-expect (eval-definition1 (make-fn 'f (make-mul (make-add 2 5) (make-mul 4 7)))
                                'f
                                'x
                                (make-mul 2 'x))
              (* 2 (* (+ 2 5) (* 4 7))))
(check-expect (eval-definition1 (make-fn 'f (make-fn 'f 2))
                                'f
                                'x
                                (make-add 5 'x))
              (+ 5 (+ 5 2)))
(check-expect (eval-definition1 (make-fn 'f (make-mul (make-fn 'f 2) (make-fn 'f 5)))
                                'f
                                'x
                                (make-add 5 'x))
              (+ 5 (* (+ 5 2) (+ 5 5))))
(check-error (eval-definition1 (make-mul 3 'x) 'f 'n (make-mul 2 'n))
              ERROR-VAR)
(check-error (eval-definition1 (make-fn 'f 'x) 'f 'n (make-mul 2 'n))
              ERROR-VAR)
(check-error (eval-definition1 (make-fn 'f 'x) 'f 'n (make-mul 2 'n))
              ERROR-VAR)
(check-error (eval-definition1  (make-fn 'f 5) 'ff 'n (make-mul 2 'n))
              ERROR-FN-NOT-FOUND)

;(define (eval-definition1 ex f x b) 0) ;stub

(define (eval-definition1 e f x b)
  (cond [(number? e) e]
        [(symbol? e) (error ERROR-VAR)]
        [(add? e) 
         (+ (eval-definition1 (add-left e) f x b)     
            (eval-definition1 (add-right e) f x b))]         
        [(mul? e) 
         (* (eval-definition1 (mul-left e) f x b)      
            (eval-definition1 (mul-right e) f x b))]   
        [(fn? e)
         (if (symbol=? f (fn-name e))
             (local ((define value (eval-definition1 (fn-arg e) f x b))
                     (define plugd (subst b x value)))
               (eval-definition1 plugd f x b))
             (error ERROR-FN-NOT-FOUND))]))               

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

; BSL-fun-expr Symbol Symbol BSL-fun-expr -> Number
; determines the value of ex given:
; - f the function name, 
; - x the functions’s parameter
; - b the function’s body
(check-expect (eval-definition1.v2 2 'f 'x (make-mul 2 'x)) 2)
(check-expect (eval-definition1.v2 (make-add 1 1) 'f 'x (make-mul 2 'x)) 2)
(check-expect (eval-definition1.v2 (make-mul 1 1) 'f 'x (make-mul 2 'x)) 1)
(check-expect (eval-definition1.v2 (make-fn 'f 2) 'f 'n (make-mul 2 'n))
              (* 2 2))
(check-expect (eval-definition1.v2 (make-fn 'f (make-mul (make-add 2 5) (make-mul 4 7)))
                                'f
                                'x
                                (make-mul 2 'x))
              (* 2 (* (+ 2 5) (* 4 7))))
(check-expect (eval-definition1.v2 (make-fn 'f (make-fn 'f 2))
                                'f
                                'x
                                (make-add 5 'x))
              (+ 5 (+ 5 2)))
(check-expect (eval-definition1.v2 (make-fn 'f (make-mul (make-fn 'f 2) (make-fn 'f 5)))
                                'f
                                'x
                                (make-add 5 'x))
              (+ 5 (* (+ 5 2) (+ 5 5))))
(check-error (eval-definition1.v2 (make-mul 3 'x) 'f 'n (make-mul 2 'n))
              ERROR-VAR)
(check-error (eval-definition1.v2 (make-fn 'f 'x) 'f 'n (make-mul 2 'n))
              ERROR-VAR)
(check-error (eval-definition1.v2 (make-fn 'f 'x) 'f 'n (make-mul 2 'n))
              ERROR-VAR)
(check-error (eval-definition1.v2  (make-fn 'f 5) 'ff 'n (make-mul 2 'n))
              ERROR-FN-NOT-FOUND)

;(define (eval-definition1 ex f x b) 0) ;stub

(define (eval-definition1.v2 e f x b)
  (cond [(number? e) e]
        [(symbol? e) (error ERROR-VAR)]
        [(add? e) 
         (+ (eval-definition1.v2 (add-left e) f x b)     
            (eval-definition1.v2 (add-right e) f x b))]         
        [(mul? e) 
         (* (eval-definition1.v2 (mul-left e) f x b)      
            (eval-definition1.v2 (mul-right e) f x b))]   
        [(fn? e)
         (if (symbol=? f (fn-name e))
             (local ((define value (eval-definition1.v2 (fn-arg e) f x b))
                     (define plugd (subst.v2 b x value)))
               (eval-definition1.v2 plugd f x b))
             (error ERROR-FN-NOT-FOUND))]))    

; BSL-fun-expr Symbol Number -> BSL-fun-expr
; produces a BSL-fun-expr like ex with all occurrences of x replaced by v
(check-expect (subst.v2 'x 'x 2) 2)
(check-expect (subst.v2 (make-add 'x 4) 'x 5) (make-add 5 4))
(check-expect (subst.v2 (make-mul 'x 'z) 'z 7) (make-mul 'x 7))
(check-expect (subst.v2 (make-mul (make-add (make-mul 'y 5) 'y) 'z) 'y 8)
              (make-mul (make-add (make-mul 8 5) 8) 'z))
(check-expect (subst.v2 'z 'x 2) 'z)

(check-expect (subst.v2 (make-fn 'f 'x) 'x 5) (make-fn 'f 5))
(check-expect (subst.v2 (make-fn 'f (make-mul 'x 5)) 'x 5) (make-fn 'f (make-mul 5 5)))

(define (subst.v2 bvexp x v)
  (cond [(number? bvexp) bvexp]
        [(symbol? bvexp) (if (symbol=? bvexp x) v bvexp)]
        [(add? bvexp) 
         (make-add (subst.v2 (add-left bvexp) x v)      
                   (subst.v2 (add-right bvexp) x v))]          
        [(mul? bvexp) 
         (make-mul (subst.v2 (mul-left bvexp) x v)      
                   (subst.v2 (mul-right bvexp) x v))]
        [(fn? bvexp)
         (make-fn (fn-name bvexp) (subst.v2 (fn-arg bvexp) x v))])) 