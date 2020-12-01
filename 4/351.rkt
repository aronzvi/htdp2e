;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |351|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Constants:

(define WRONG "Invalid S-expr for BSL-expr")


;; Data definitions:

; An S-expr is one of: 
; – Atom
; – SL
 
; An SL is one of: 
; – '()
; – (cons S-expr SL)
          

; An Atom is one of: 
; – Number
; – String
; – Symbol


(define-struct add [left right])
; an Add-Expr is (make-add BSL-expr BSL-expr)
; interp. represents BSL expresion (+ BSL-expr BSL-expr) 

(define-struct mul [left right])
; an Mul-Expr is (make-mul BSL-expr BSL-expr)
; interp. represents BSL expresion (* BSL-expr BSL-expr)

; An BSL-expr is one of: 
; – Number
; – (make-add BSL-expr BSL-expr)
; - (make-mul BSL-expr BSL-expr)

(define BSLEXP0 3)
(define BSLEXP1 (make-add 10 -10))
(define BSLEXP2 (make-add (make-mul 20 3) 33))
(define BSLEXP3 (make-add (make-mul 3.14 (make-mul 2 3)) (make-mul 3.14 (make-mul -1 -9))))

#;
(define (fn-for-bsl-exp bexp)
  (cond [(number? bexp) (... bexp)]
        [(add? bexp) 
         (... (fn-for-bsl-exp (add-left bexp))      ;BSL-expr
              (fn-for-bsl-exp (add-right bexp)))]   ;BSL-expr         
        [(mul? bexp) 
         (... (fn-for-bsl-exp (mul-left bexp))      ;BSL-expr
              (fn-for-bsl-exp (mul-right bexp)))])) ;BSL-expr

; S-expr -> Number
; produces value of sexp if parse recognizes them as BSL-expr. Otherwise, it signals the same error as parse
(check-expect (interpreter-expr 2) 2)
(check-expect (interpreter-expr '(* 2 4))
              (eval-expression (make-mul 2 4)))
(check-expect (interpreter-expr '(* 2 (+ (* 1 5) 4)))
              (eval-expression (make-mul 2 (make-add (make-mul 1 5) 4))))
(check-expect (interpreter-expr '(+ 2 (* 4 (+ 5 6))))
              (eval-expression (make-add 2 (make-mul 4 (make-add 5 6)))))
(check-error (interpreter-expr '("f" 2 4)) WRONG)
(check-error (interpreter-expr '(/ 2 4)) WRONG)

;(define (interpreter-expr sexp) 0) ;stub

(define (interpreter-expr sexp)
  (eval-expression (parse sexp)))

; S-expr -> BSL-expr
(check-expect (parse 2) 2)
(check-error (parse "sss") WRONG)
(check-error (parse '+) WRONG)
(check-expect (parse '(+ 2 4)) (make-add 2 4))
(check-expect (parse '(* 2 4)) (make-mul 2 4))
(check-error (parse '(* 2)) WRONG)
(check-error (parse '("f" 2 4)) WRONG)
(check-error (parse '(/ 2 4)) WRONG)
(check-expect (parse '(+ 2 (* 4 (+ 5 6)))) (make-add 2 (make-mul 4 (make-add 5 6))))

(define (parse s)
  (cond
    [(atom? s) (parse-atom s)]
    [else (parse-sl s)]))
 
; SL -> BSL-expr 
(define (parse-sl s)
  (local ((define L (length s)))
    (cond
      [(< L 3) (error WRONG)]
      [(and (= L 3) (symbol? (first s)))
       (cond
         [(symbol=? (first s) '+)
          (make-add (parse (second s)) (parse (third s)))]
         [(symbol=? (first s) '*)
          (make-mul (parse (second s)) (parse (third s)))]
         [else (error WRONG)])]
      [else (error WRONG)])))
 
; Atom -> BSL-expr 
(define (parse-atom s)
  (cond
    [(number? s) s]
    [(string? s) (error WRONG)]
    [(symbol? s) (error WRONG)]))

(define (atom? x)
  (or (number? x)
      (string? x)
      (symbol? x)))

; BSL-expr -> Number
; computes value of bexp
(check-expect (eval-expression 3) 3)
(check-expect (eval-expression (make-add 1 1)) 2)
(check-expect (eval-expression (make-mul 3 10)) 30)
(check-expect (eval-expression (make-add (make-mul 1 1) 10)) 11)

;(define (eval-expression bexp) 0) ;stub

(define (eval-expression bexp)
  (local ((define (fn-for-add-exp aexp)
            (+ (fn-for-bsl-exp (add-left aexp))    
               (fn-for-bsl-exp (add-right aexp)))) 

          (define (fn-for-mul-exp mexp)
            (* (fn-for-bsl-exp (mul-left mexp))    
               (fn-for-bsl-exp (mul-right mexp)))) 

          (define (fn-for-bsl-exp bexp)
            (cond [(number? bexp) bexp]
                  [(add? bexp) (fn-for-add-exp bexp)]             
                  [(mul? bexp) (fn-for-mul-exp bexp)])))  
    (fn-for-bsl-exp bexp)))