;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |152|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

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

;; N Image -> Image
;; produces a column—a vertical arrangement—of n copies of img
(check-expect (col 0 (square 15 "outline" "black")) empty-image)
(check-expect (col 3 (square 15 "outline" "black"))
              (above (square 15 "outline" "black")
                     (above (square 15 "outline" "black")
                            (above (square 15 "outline" "black") empty-image))))

;(define (col n img) img) ;stub

(define (col n img)
  (cond
    [(zero? n) empty-image]
    [(positive? n)
     (above img (col (sub1 n) img))]))

;; N Image -> Image
;; produces a row—a horizontal arrangement—of n copies of img
(check-expect (row 0 (square 15 "outline" "black")) empty-image)
(check-expect (row 3 (square 15 "outline" "black"))
              (beside (square 15 "outline" "black")
                     (beside (square 15 "outline" "black")
                            (beside (square 15 "outline" "black") empty-image))))

;(define (row n img) img) ;stub

(define (row n img)
  (cond
    [(zero? n) empty-image]
    [(positive? n)
     (beside img (row (sub1 n) img))]))


