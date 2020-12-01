;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |244|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (f x) (x 10))

; x is a function and we apply (x 10)

(define (f x) (x f))

; x is a function and we apply (x f)

(define (f x y) (x 'a y 'b))

; x is a function and we apply  (x 'a y 'b)