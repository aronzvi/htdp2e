;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |318|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; S-expr -> Natural
; determines the depth of sexp.
; An Atom has a depth of 1. The depth of a list of S-expressions is the maximum depth of its items plus 1
(check-expect (depth 'aa) 1)
;(check-expect (depth-sl empty) 0)
(check-expect (depth '(ff ff 1)) 2)
(check-expect (depth '(ff (ff) 1)) 3)
(check-expect (depth '(ff (ff (4)) 1)) 4)

;(define (depth sexp) 0) ;stub
;(define (depth-sl sl) 0) ;stub

(define (depth sexp)
  (local (; SL -> Natural
          (define (depth-sl sl)
            (cond [(empty? sl) 0]
                  [else
                   (local ((define depth-first (depth (first sl)))
                           (define depth-rest (depth-sl (rest sl))))
                     (if (> depth-first depth-rest)
                         depth-first
                         depth-rest))])))
    (cond [(atom? sexp) 1]
          [else
           (+ 1 (depth-sl sexp))]))) 

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
