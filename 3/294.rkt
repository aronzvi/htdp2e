;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |294|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; X [List-of X] -> [Maybe N]
; determine the index of the first occurrence
; of x in l, #false otherwise
(check-expect (index 2 empty) false)
(check-expect (index 2 (list 1 2 3)) 1)
(check-expect (index 2 (list 1 5 3)) false)

(define (index x l)
  (cond
    [(empty? l) #false]
    [else (if (equal? (first l) x)
              0
              (local ((define i (index x (rest l))))
                (if (boolean? i) i (+ i 1))))]))

; X [List-of X] -> ([Maybe N] -> Boolean)
; is n the index of of the first occurance of x in lox
; - n is valid index of lox; n < length of lox
; - val at index n is equal to x
(check-expect ((is-index? 2 empty) false) true)
(check-expect ((is-index? 2 (list 1 2 3)) 1) true)
(check-expect ((is-index? 2 (list 1 2 3)) 5) false)
(check-expect ((is-index? 5 (list 1 2 3)) false) true)
(check-expect ((is-index? 2 (list 1 2 3 2)) 1) true)
(check-expect ((is-index? 2 (list 1 2 3 2)) 3) false)

;(define (is-index? x lox) (lambda (n) false)) ;stub

(define (is-index? x lox)
  (lambda (n)
    if (and (boolean? n)
            ())))



