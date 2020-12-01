;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |317|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An Atom is one of: 
; – Number
; – String
; – Symbol

(define A0 3)
(define A1 "eff")
(define A2 's)

#;
(define (fn-for-atom a)
  (cond [(number? a) (... a)]
        [(string? a) (... a)]
        [(symbol? a) (... a)]))

; An SL is one of: 
; – '()
; – (cons S-expr SL)

; An S-expr is one of: 
; – Atom
; – SL

(define SL0 '())
(define SL1 (cons 'hello (cons 20.12 (cons "world" '()))))
(define SL2 (cons (cons 'hello (cons 20.12 (cons "world" '())))
                  '()))
(define SEXP0 A1)
(define SEXP1 SL2)

#;
(define (fn-for-sexp sexp)
  (cond [(atom? sexp) (... (fn-for-atom sexp))] ;Atom
        [else
         (... (fn-for-sl sexp))])) ;Sl

#;
(define (fn-for-sl sl)
  (cond [(empty? sl) (...)]
        [else
         (... (fn-for-sexp (first sl))   ;S-expr
              (fn-for-sl (rest sl)))]))  ;SL

; S-expr Symbol -> N
; SL Symbol -> N
; counts all occurrences of sy in sexp
(check-expect (count 'world 'hello) 0)
(check-expect (count '(world hello) 'hello) 1)
(check-expect (count '(((world) hello) hello) 'hello) 2)
(check-expect (count-sl empty 'world) 0)

;(define (count sexp sy) 0) ;stub

(define (count sexp sy)
  (cond [(atom? sexp) (count-atom sexp sy)]
        [else
         (count-sl sexp sy)])) 

(define (count-sl sl sy)
  (cond [(empty? sl) 0]
        [else
         (+ (count (first sl) sy)   
            (count-sl (rest sl) sy))]))

; Atom Symbol -> Boolean
; produces true 1 if at is equal to sy. else 0
(check-expect (count-atom 'a 'a) 1)
(check-expect (count-atom 'a 's) 0)
(check-expect (count-atom 2 's) 0)
(check-expect (count-atom "f" 's) 0)

;(define (count-atom at sy) false) ;stub

(define (count-atom at sy)
  (cond [(number? at) 0]
        [(string? at) 0]
        [(symbol? at) (if (symbol=? at sy) 1 0)]))

; ??? -> Boolean
; produces true if x is an Atom
(check-expect (atom? "s") true)
(check-expect (atom? 1) true)
(check-expect (atom? 's) true)
(check-expect (atom? true) false)

;(define (atom? x) false) ;stub

(define (atom? x)
  (or
   (number? x)
   (string? x)
   (symbol? x)))