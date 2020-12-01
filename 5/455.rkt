;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |455|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define ε 0.1)

; [Number -> Number] Number -> Number
; produces the slope of f at r1 using ε
(check-expect (slope horiz 2) 0)
(check-expect (slope linear 2) 1)
(check-expect (slope poly 2) -2)

;(define (slope f r1) 0) ;stub

(define (slope f r1)
  (/ (- (f (+ r1 ε)) (f (- r1 ε)))
     (* 2 ε)))

(define (horiz x) 5)
(define (linear x) (+ x 2))
(define (poly x) (* (- x 2) (- x 4)))
