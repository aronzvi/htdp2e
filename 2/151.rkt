;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |151|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An N is one of: 
; – 0
; – (add1 N)
; interpretation represents the counting numbers

(define N1 0)
(define N2 (add1 N1))
(define N3 (add1 N2))

#;
(define (fn-for-n n)
  (cond
    [(zero? n) (...)]
    [(positive? n)
     (... (fn-for-n (sub1 n)))])) ;N

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: 0
;; - compound: (add1 N)
;; - self-reference: (sub1 n) is N

;; N Number -> Number
;; multiplies n by x
(check-expect (multiply 0 1) 0)
(check-expect (multiply 3 1) (* 3 1))
(check-within (multiply 3 pi) (* 3 pi) 0.001)

;(define (multiply n x) n) ;stub

(define (multiply n x)
  (cond
    [(zero? n) 0]
    [(positive? n)
     (+ x (multiply (sub1 n) x))]))

(multiply 3 2)

; 3 X 2 = 2 + 2 + 2. Which is how multiply produces its result 