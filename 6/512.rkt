;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |512|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

#;
(define (fn-for-lam.v1 lam)
  (cond [(symbol? lam) ...]
        [(symbol=? (first lam) 'λ)
         (... (first lam)                ;Symbol
              (first (second lam))       ;Symbol
              (fn-for-lam (third lam)))] ;Lam
        [else
         (... (fn-for-lam (first lam))      ;Lam
              (fn-for-lam (second lam)))])) ;Lam

(define (fn-for-lam.v2 lam)
  (cond [(is-var? lam) ...]
        [(is-λ? lam)
         (... (first lam)                 ;Symbol
              (λ-para lam)                ;Symbol
              (fn-for-lam (λ-body lam)))] ;Lam
        [else
         (... (fn-for-lam (app-fun lam))     ;Lam
              (fn-for-lam (app-arg lam)))])) ;Lam


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

#;
(define (λ-para lam)
  (cond [(symbol? lam) (error "not λ expression")]
        [(symbol=? (first lam) 'λ)
         (... (first lam)                ;Symbol
              (first (second lam))       ;Symbol
              (λ-para (third lam)))]     ;Lam
        [else  (error "not λ expression")])) 

#;
(define (λ-para lam)
  (cond
    [(is-λ? lam)
     (... (first lam)                ;Symbol
          (first (second lam))       ;Symbol
          (λ-para (third lam)))]     ;Lam
    [else  (error "not λ expression")]))

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

#;
(define (λ-body lam)
  (cond
    [(is-λ? lam)
     (... (first lam)                ;Symbol
          (first (second lam))       ;Symbol
          (λ-body (third lam)))]     ;Lam
    [else  (error "not λ expression")])) 

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

#;
(define (app-fun lam)
  (cond
    [(is-app? lam)
     (... (fn-for-lam (first lam))      ;Lam
          (fn-for-lam (second lam)))]   ;Lam
    [else  (error "not an aplication")]))

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

; Lam -> [List-of Symbol]
; produces the list of all symbols used as λ parameters in a λ term
(check-expect (declareds ex1) '(x))
(check-expect (declareds ex2) '(x))
(check-expect (declareds ex3) '(y x))
(check-expect (declareds ex4) '(x x))
(check-expect (declareds ex5) '(x x))
(check-expect (declareds ex6) '(y x z w))

;(define (declareds lam) '()) ;stub

(define (declareds lam)
  (cond [(is-var? lam) '()]
        [(is-λ? lam)
         (cons 
          (λ-para lam)                
          (declareds (λ-body lam)))] 
        [else
         (append (declareds (app-fun lam))     
                 (declareds (app-arg lam)))]))