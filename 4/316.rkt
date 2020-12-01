;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |316|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An Atom is one of: 
; – Number
; – String
; – Symbol

(define A0 3)
(define A1 "eff")
(define A2 's)

; X -> Boolean
; predicate for atom
(check-expect (atom? 5) true)
(check-expect (atom? 3.5) true)
(check-expect (atom? "ddd") true)
(check-expect (atom? 'w) true)
(check-expect (atom? true) false)
(check-expect (atom? (list 1)) false)

;(define (atom? x) false) ;stub


(define (atom? x)
  (cond [(number? x) true]
        [(string? x) true]
        [(symbol? x) true]
        [else false]))

