;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname depth) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

(define-struct layer [color doll])
; An RD.v2 (short for Russian doll) is one of: 
; – "doll"
; – (make-layer String RD.v2)

; RD.v2 -> N
; how many dolls are a part of an-rd
(check-expect (depth "doll") 0)
(check-expect (depth (make-layer "red" "doll")) 1)
(check-expect (depth (make-layer "blue" (make-layer "red" "doll"))) 2)

;(define (depth a-doll) 0) ;stub

(define (depth a-doll)
  (match a-doll
    ["doll" 0]
    [(layer c inside) (+ 1 (depth inside))]))