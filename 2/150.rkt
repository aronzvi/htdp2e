;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |150|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; N -> Number
; computes (+ n pi) without using +
(check-within (add-to-pi 0) pi 0.001)
(check-within (add-to-pi 3) (+ 3 pi) 0.001)
 
;(define (add-to-pi n) pi) ;stub

(define (add-to-pi n)
  (cond
    [(zero? n) pi]
    [(positive? n)
     (add1 (add-to-pi (sub1 n)))]))

;; N Number -> Number
;; adds a natural number n to some arbitrary number x
(check-within (add 0 0.1) 0.1 0.001)
(check-expect (add 2 10) (+ 2 10))

;(define (add n x) x) ;stub

(define (add n x)
  (cond
    [(zero? n) x]
    [(positive? n)
     (add1 (add (sub1 n) x))]))

; we use check-within since we cannot compare inexact numbers (pi for example) and thos need to be tested as well