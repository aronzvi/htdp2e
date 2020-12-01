;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |319|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Data definitions

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

;Functions
; S-expr Symbol Symbol ->  S-expr
; replaces all occurrences of old in s with new
(check-expect (substitute 'dd 'dd 'zz) 'zz)
(check-expect (substitute 'dd 'xx 'zz) 'dd)
(check-expect (substitute '(2 "d" dd (dd ("fff" dd) 4)) 'dd 'zz)
              '(2 "d" zz (zz ("fff" zz) 4)))

;(define (substitute s old new) s)      ;stub
;(define (substitute-sl sl old new) sl) ;stub

(define (substitute sexp old new)
  (local (;SL ->  SL
          ; replaces all occurrences of old in s with new
          (define (substitute-sl sl)
            (cond [(empty? sl) empty]
                  [else
                   (cons (substitute (first sl) old new)   
                         (substitute-sl (rest sl)))]))
          ; Atom Symbol Symbol -> Atom
          ; replaces all occurrences of old in at with new
          (define (substitute-atom a)
            (cond [(number? a) a]
                  [(string? a) a]
                  [(symbol? a) (if (symbol=? a old) new a)])))
    (cond [(atom? sexp) (substitute-atom sexp)] 
          [else
           (substitute-sl sexp)]))) 

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