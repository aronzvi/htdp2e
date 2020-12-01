;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |517|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Lam is one of: 
; – a Symbol
; – (list 'λ (list Symbol) Lam)
; – (list Lam Lam)

(define ex1 '(λ (x) x))
(define ex2 '(λ (x) y))
(define ex3 '(λ (y) (λ (x) y)))
(define ex4 '((λ (x) x) (λ (x) x))) 
(define ex5 '((λ (x) (x x)) (λ (x) (x x))))
(define ex6 '(((λ (y) (λ (x) y)) (λ (z) z)) (λ (w) w)))
(define ex7 'x)
(define ex8 '(x x))

(define (fn-for-lam lam)
  (cond [(is-var? lam) ...]
        [(is-λ? lam)
         (... (first lam)                 ;Symbol
              (λ-para lam)                ;Symbol
              (fn-for-lam (λ-body lam)))] ;Lam
        [else
         (... (fn-for-lam (app-fun lam))     ;Lam
              (fn-for-lam (app-arg lam)))])) ;Lam

(define-struct var [name steps])
; a Var is (make-var 'Symbol Natural)
; interp: A defined variable and how many steps (how many 'λ) since it was declared

(define v1 (make-var 'x 0))
(define v2 (make-var 'x 2))

(define (fn-for-var v)
  (... (var-name v)
       (var-steps v)))

;A LamWithDistance is a Lam with all occurrences of variables replaced with a natural number that represents how far away the declaring λ is

; Lam -> LamWithDistance
; replaces all occurrences of variables with a natural number that represents how far away the declaring λ is
; Assumption: No free variables in given Lam
(check-expect (static-distance ex1)
              '(λ (x) 0))
(check-expect (static-distance '((λ (x) ((λ (x) x) x)) (λ (x) x)))
              '((λ (x) ((λ (x) 0) 0)) (λ (x) 0)))
(check-expect (static-distance '((λ (x) ((λ (y) (y x)) x)) (λ (z) z)))
              '((λ (x) ((λ (y) (0 1)) 0)) (λ (z) 0)))

;(define (static-distance lam) 0) ;stub

#;
(define (static-distance lam0)
  (local (; Lam ??? -> LamWithDistance
          ; accumulator vars ...
          (define (static-distance/a lam vars)
            (cond [(is-var? lam) (fn-for-lov vars)]
                  [(is-λ? lam)
                   (... (fn-for-lov vars)
                        (first lam)                 
                        (λ-para lam)                
                        (static-distance/a (λ-body lam)
                                           (fn-for-lov vars)))] 
                  [else
                   (... (fn-for-lov vars)
                        (static-distance/a (app-fun lam)
                                           (fn-for-lov vars))     
                        (static-distance/a (app-arg lam)
                                           (fn-for-lov vars)))]))) 
    (static-distance/a lam0 ...)))

#;
(define (static-distance lam0)
  (local (; Lam [List-of Var] -> NaturalOrFalse
          ; produces the steps for the first matching var found in lov
          (define (get-var-steps lam lov) 0)

          ; [List-of Var] -> [List-of Var]
          ; increments the steps of all vars in lov by one
          (define (inc-var-steps lov) lov)

          ; Lam [List-of Var] -> LamWithDistance
          ; accumulator vars is list of λ parameters encountered on the path from the top of le0 to the top of le
          ; along with the number of steps taken since the decleration of each parameter
          (define (static-distance/a lam vars)
            (cond [(is-var? lam) (get-var-steps lam vars)]
                  [(is-λ? lam)
                   (local ((define para (λ-para lam))
                           (define body (λ-body lam))
                           (define new-vars (cons (make-var para 0) (inc-var-steps vars))))
                     (list 'λ (list para)
                           (static-distance/a body new-vars)))] 
                  [else
                   (list (static-distance/a (app-fun lam) vars)     
                         (static-distance/a (app-arg lam) vars))]))) 
    (static-distance/a lam0 '())))

(define (static-distance lam0)
  (local (; Lam [List-of Var] -> NaturalOrFalse
          ; produces the steps for the first matching var found in lov
          (define (get-var-steps lam lov)
            (cond [(empty? lov) #false]
                  [else
                   (if (symbol=? lam (var-name (first lov)))
                       (var-steps (first lov))
                       (get-var-steps lam (rest lov)))]))

          ; [List-of Var] -> [List-of Var]
          ; increments the steps of all vars in lov by one
          (define (inc-var-steps lov)
            (map (lambda (v) (make-var (var-name v) (add1 (var-steps v)))) lov))

          ; Lam [List-of Var] -> LamWithDistance
          ; accumulator vars is list of λ parameters encountered on the path from the top of le0 to the top of le
          ; along with the number of steps taken since the decleration of each parameter
          (define (static-distance/a lam vars)
            (cond [(is-var? lam) (get-var-steps lam vars)]
                  [(is-λ? lam)
                   (local ((define para (λ-para lam))
                           (define body (λ-body lam))
                           (define new-vars (cons (make-var para 0) (inc-var-steps vars))))
                     (list 'λ (list para)
                           (static-distance/a body new-vars)))] 
                  [else
                   (list (static-distance/a (app-fun lam) vars)     
                         (static-distance/a (app-arg lam) vars))]))) 
    (static-distance/a lam0 '())))


; Lam -> LamWithDistance
; replaces all occurrences of variables with a natural number that represents how far away the declaring λ is
; Assumption: No free variables in given Lam
(check-expect (static-distance.v2 ex1)
              '(λ (x) 0))
(check-expect (static-distance.v2 '((λ (x) ((λ (x) x) x)) (λ (x) x)))
              '((λ (x) ((λ (x) 0) 0)) (λ (x) 0)))
(check-expect (static-distance.v2 '((λ (x) ((λ (y) (y x)) x)) (λ (z) z)))
              '((λ (x) ((λ (y) (0 1)) 0)) (λ (z) 0)))

(define (static-distance.v2 lam0)
  (local (; Lam [List-of Var] -> LamWithDistance
          ; accumulator vars is list of λ parameters encountered on the path from the top of le0 to the top of le
          (define (static-distance/a lam vars)
            (cond [(is-var? lam) (var-index lam vars)]
                  [(is-λ? lam)
                   (local ((define para (λ-para lam))
                           (define body (λ-body lam))
                           (define new-vars (cons para vars)))
                     (list 'λ (list para)
                           (static-distance/a body new-vars)))] 
                  [else
                   (list (static-distance/a (app-fun lam) vars)     
                         (static-distance/a (app-arg lam) vars))]))) 
    (static-distance/a lam0 '())))

; Symbol [List-of Symbol] -> NaturalOrFalse
; produces the zero-based index of the first occurance of s in los. Produce false if no occurance found
(check-expect (var-index.v1 'x empty) #false)
(check-expect (var-index.v1 'y '(y x z w)) 0)
(check-expect (var-index.v1 'z '(y x z w)) 2)
(check-expect (var-index.v1 'b '(y x z w)) #false)
(check-expect (var-index.v1 'x '(y x x z w)) 1)

;(define (var-index.1 s los) #false) ;stub

; template from [List-f Symbol]

#;
(define (var-index.v1 s los)
  (cond [(empty? los) #false]
        [else
         (... (first los)
              (var-index.v1 s (rest los)))]))

(define (var-index.v1 s los)
  (cond [(empty? los) #false]
        [else
         (if (symbol=? s (first los))
             0
             (local ((define res (var-index.v1 s (rest los))))
               (if (boolean? res)
                   res
                   (+ 1 res))))]))

; Symbol [List-of Symbol] -> NaturalOrFalse
; produces the zero-based index of the first occurance of s in los. Produce false if no occurance found
(check-expect (var-index 'x empty) #false)
(check-expect (var-index 'y '(y x z w)) 0)
(check-expect (var-index 'z '(y x z w)) 2)
(check-expect (var-index 'b '(y x z w)) #false)
(check-expect (var-index 'x '(y x x z w)) 1)

;(define (var-index s los) #false) ;stub

#;
(define (var-index s los0)
  (local (; [List-of Symbol] ??? -> NaturalOrFalse
          ; accumulator a ...
          (define (var-index/a los a)
            (cond [(empty? los) (... a)]
                  [else
                   (... a
                        (first los)
                        (var-index/a (rest los)) ... a)])))
    (var-index/a los0 ...)))

(define (var-index s los0)
  (local (; [List-of Symbol] Natural -> NaturalOrFalse
          ; accumulator a is the number of items in los0 missing from los -- the current zero-based index
          (define (var-index/a los a)
            (cond [(empty? los) #false]
                  [else
                   (if (symbol=? s (first los))
                       a
                       (var-index/a (rest los) (add1 a)))])))
    (var-index/a los0 0)))

; Lam -> Boolean
; produces true if lam is a var
(check-expect (is-var? 'x) #true)
(check-expect (is-var? ex1) #false)
(check-expect (is-var? ex4) #false)
(check-expect (is-var? '(x x)) #false)
 
;(define (is-var? lam) #false) ;stub

(define (is-var? lam)
  (symbol? lam))

; Lam -> Boolean
; produces true if lam is a definition

(check-expect (is-λ? ex1) #true)
(check-expect (is-λ? 'x) #false)
(check-expect (is-λ? ex4) #false)
(check-expect (is-λ? '(x x)) #false)

;(define (is-λ? lam) #false) ;stub

(define (is-λ? lam) (and (cons? lam)
                         (symbol? (first lam))
                         (symbol=? (first lam) 'λ)))

; Lam -> Boolean
; produces true if lam is an application
(check-expect (is-app? ex4) #true)
(check-expect (is-app? 'x) #false)
(check-expect (is-app? ex3) #false)
(check-expect (is-app? '(x x)) #true)

(define (is-app? lam)
  (not (or (is-var? lam)
           (is-λ? lam))))

; Lam -> Symbol
; extracts the parameter from a λ expression. error if not given a λ expression
(check-expect (λ-para ex2) 'x)
(check-expect (λ-para ex3) 'y)
(check-error (λ-para 'x) "not λ expression")
(check-error (λ-para ex4) "not λ expression")

;(define (λ-para lam) 'x) ;stub

(define (λ-para lam)
  (cond
    [(is-λ? lam) (first (second lam))] 
    [else  (error "not λ expression")]))

; Lam -> Lam
; extracts the body from a λ expression. error if not given a λ expression
(check-expect (λ-body ex2) 'y)
(check-expect (λ-body ex3) '(λ (x) y))
(check-error (λ-body 'x) "not λ expression")
(check-error (λ-body ex4) "not λ expression")

;(define (λ-body lam) 'x) ;stub

(define (λ-body lam)
  (cond
    [(is-λ? lam) (third lam)]
    [else  (error "not λ expression")])) 

; Lam -> Lam
; extracts the function from an application. error if not given an application
(check-expect (app-fun ex5) '(λ (x) (x x)))
(check-expect (app-fun ex6) '((λ (y) (λ (x) y)) (λ (z) z)))
(check-error (app-fun 'x) "not an application")
(check-error (app-fun ex3) "not an application")

;(define (app-fun lam) 'x) ;stub

(define (app-fun lam)
  (cond
    [(is-app? lam) (first lam)]   
    [else  (error "not an application")]))

; Lam -> Lam
; extracts the argument from an application
(check-expect (app-arg ex5) '(λ (x) (x x)))
(check-error (app-fun 'x) "not an application")
(check-error (app-fun ex3) "not an application")

;(define (app-arg lam) 'x) ;stub

(define (app-arg lam)
  (cond
    [(is-app? lam) (second lam)]   
    [else  (error "not an aplication")]))