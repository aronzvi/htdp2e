;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |348|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct and-exp (left right))
(define-struct or-exp (left right))
(define-struct not-exp (exp))

; A Bool-Expr is one of:
; - Boolean
; - (make-and Bool-Expr Bool-Expr)
; - (make-or Bool-Expr Bool-Expr)
; - (make-not Bool-Expr)
;interp. A representation of BSL boolean expression

(define BEXP0 #true)
(define BEXP1 #false)
(define BEXP2 (make-and-exp #true #false))
(define BEXP3 (make-or-exp #true #false))
(define BEXP4 (make-not-exp #true))
(define BEXP5 (make-and-exp (make-or-exp #true #false) #true))
(define BEXP6 (make-and-exp (make-or-exp #true #false) (make-not-exp #false)))
(define BEXP7 (make-and-exp (make-or-exp #true #false)
                        (make-not-exp (make-and-exp (make-or-exp #true #false)
                                            (make-and-exp #false #true)))))
#;
(define (fn-for-bool-exp bexp)
  (cond [(boolean? bexp) (... bexp)]
        [(and-exp? bexp)
         (... (fn-for-bool-exp (and-exp-left bexp))    ;Bool-Expr
              ( fn-for-bool-exp (and-exp-right bexp)))] ;Bool-Expr
        [(or-exp? bexp)
         (... (fn-for-bool-exp (or-exp-left bexp))     ;Bool-Expr
              ( fn-for-bool-exp (or-exp-right bexp)))]  ;Bool-Expr
        [(not-exp? bexp)
         (... (fn-for-bool-exp (not-exp-exp bexp)))]))       ;Bool-Expr

; Bool-Expr -> Boolean
; computes value of bexp
(check-expect (eval-bool-expression true) true)
(check-expect (eval-bool-expression false) false)
(check-expect (eval-bool-expression (make-and-exp true true)) true)
(check-expect (eval-bool-expression (make-and-exp false true)) false)
(check-expect (eval-bool-expression (make-and-exp false false)) false)
(check-expect (eval-bool-expression (make-and-exp true false)) false)
(check-expect (eval-bool-expression (make-or-exp true true)) true)
(check-expect (eval-bool-expression (make-or-exp false true)) true)
(check-expect (eval-bool-expression (make-or-exp false false)) false)
(check-expect (eval-bool-expression (make-or-exp true false)) true)
(check-expect (eval-bool-expression (make-not-exp true)) false)
(check-expect (eval-bool-expression (make-not-exp false)) true)
(check-expect (eval-bool-expression (make-and-exp (make-or-exp true false)
                                                  (make-and-exp (make-and-exp true true)
                                                                (make-not-exp (make-or-exp true false)))))
              false)
         

;(define (eval-bool-expression bexp) false) ;stub

(define (eval-bool-expression bexp)
  (cond [(boolean? bexp) bexp]
        [(and-exp? bexp)
         (and (eval-bool-expression (and-exp-left bexp))    ;Bool-Expr
              ( eval-bool-expression (and-exp-right bexp)))] ;Bool-Expr
        [(or-exp? bexp)
         (or (eval-bool-expression (or-exp-left bexp))     ;Bool-Expr
              ( eval-bool-expression (or-exp-right bexp)))]  ;Bool-Expr
        [(not-exp? bexp)
         (not (eval-bool-expression (not-exp-exp bexp)))]))       ;Bool-Expr



