;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |306|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; ### 1 ###
; Natural -> [List-of Natural]
; creates the list (list 0 ... (- n 1)) for any natural number n
(check-expect (0-to-n-minus-1 0) empty)
(check-expect (0-to-n-minus-1 3) (list 0 1 2))

;(define (0-to-n-minus-1 n) empty) ;stub

(define (0-to-n-minus-1 n)
  (for/list ([i n])
    i))

; ### 2 ###
; Natural -> [List-of Natural]
; creates the list (list 1 ... n) for any natural number n
(check-expect (1-to-n 0) empty)
(check-expect (1-to-n 1) (list 1))
(check-expect (1-to-n 3) (list 1 2 3))

;(define (1-to-n n) empty) ;stub

(define (1-to-n n)
  (for/list ([i n])
    (+ i 1)))

; ### 3 ###

; Natural -> [List-of Number]
; creates the list (list 1 1/2 ... 1/n) for any natural number n
(check-expect (1-to-divided-by-n 0) empty)
(check-expect (1-to-divided-by-n 1) (list (/ 1 1)))
(check-expect (1-to-divided-by-n 2) (list (/ 1 1) (/ 1 2)))

;(define (1-to-divided-by-n n) empty) ;stub

(define (1-to-divided-by-n n)
  (for/list ([i n])
    (/ 1 (+ i 1))))

; ### 4 ###

; Natural -> [List-of Natural]
; creates the list of the first n even numbers
(check-expect (n-even-numbers 0) empty)
(check-expect (n-even-numbers 1) (list 2))
(check-expect (n-even-numbers 2) (list 2 4))

;(define (n-even-numbers n) empty) ;stub

(define (n-even-numbers n)
  (for/list ([i (in-range 2 (* 2 (+ n 1)) 2)])
    i))

; ### 5 ###
; Natural -> [List-of [List-of Natural]]
; Creates diagonal squares of 0s and 1s of size n

(check-expect (identityM 0)
              empty)
(check-expect (identityM 1)
              (list (list 1)))
(check-expect (identityM 2)
              (list (list 1 0) (list 0 1)))
(check-expect (identityM 3)
              (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))
(check-expect (identityM 4)
              (list (list 1 0 0 0) (list 0 1 0 0) (list 0 0 1 0) (list 0 0 0 1)))

;(define (identityM n) empty) ;stub

(define (identityM n)
  (for/list ([i n])
    (for/list ([j n])
      (if (= j i) 1 0))))

; Natural [Natural -> X] -> [List-of X]
; tabulates f between n and 0 (incl.) in a list
(check-expect (tabulate 0 sqr) (list (sqr 0)))
(check-expect (tabulate 2 sqr) (list (sqr 2) (sqr 1) (sqr 0)))
(check-within (tabulate 2 tan) (list (tan 2) (tan 1) (tan 0)) 0.1)

#;
(define (tabulate n f)
  (cond
    [(= n 0) (list (f 0))]
    [else
     (cons
      (f n)
      (tabulate (sub1 n) f))]))

(define (tabulate n f)
  (append
   (for/list ([i n])
     (f (- n i)))
     (list (f 0))))