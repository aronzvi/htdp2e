;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |251|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of Number] -> Number
; computes the sum of 
; the numbers on l
(check-expect (sum empty) 0)
(check-expect (sum (list 1 5 9)) 15)

(define (sum l)
  (cond
    [(empty? l) 0]
    [else
     (+ (first l)
        (sum (rest l)))]))
  
; [List-of Number] -> Number
; computes the product of 
; the numbers on l
(check-expect (product empty) 1)
(check-expect (product (list 1 6 4)) 24)

(define (product l)
  (cond
    [(empty? l) 1]
    [else
     (* (first l)
        (product (rest l)))]))

; [List-of Number] -> Number
; computes the sum of 
; the numbers on l
(check-expect (sum-from-fold1 empty) 0)
(check-expect (sum-from-fold1 (list 1 5 9)) 15)

(define (sum-from-fold1 l)
  (fold1 l + 0))

; [List-of Number] -> Number
; computes the product of 
; the numbers on l
(check-expect (product-from-fold1 empty) 1)
(check-expect (product-from-fold1 (list 1 6 4)) 24)

(define (product-from-fold1 l)
  (fold1 l * 1))

; ? ? -> ?
; ?
(define (fold1 l f b)
  (cond
    [(empty? l) b]
    [else
     (f (first l)
        (fold1 (rest l) f b))]))
