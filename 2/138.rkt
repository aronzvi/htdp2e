;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |138|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;         -------------------------                       
;         v                        |
; A List-of-amounts is one of:     |
; – '()                            |
; – (cons PositiveNumber List-of-amounts)

(define LOA1 empty)
(define LOA2 (cons 2 LOA1))
(define LOA3 (cons 4 LOA2))
(define LOA4 (cons 10 LOA3))

(define (fn-for-loa loa)
  (cond
    [(empty? loa) (...)]
    [else
     (... (first loa)                ;PositiveNumber
          (fn-for-loa (rest loa)))])) ;List-of-amounts

;; Template rules used:
;; - one-of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons PositiveNumber List-of-amounts)
;; - self-reference: (rest loa) is List-of-amounts

;; List-of-amounts -> PositiveNumber
;; computes the sum of the amounts in given loa
(check-expect (sum empty) 0)
(check-expect (sum (cons 34 empty)) (+ 34 0))
(check-expect (sum (cons 15 (cons 34 empty))) (+ 15 34))

;(define (sum loa) 0) ;stub

(define (sum loa)
  (cond
    [(empty? loa) 0]
    [else
     (+ (first loa)                
        (sum (rest loa)))]))

(sum (cons 15 (cons 34 empty)))

