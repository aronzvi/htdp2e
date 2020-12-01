;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |240|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct layer [stuff])


; An LStr is one of: 
; – String
; – (make-layer LStr)
(define LSTR1 "hello")
(define LSTR2 (make-layer "hello"))
(define LSTR3 (make-layer (make-layer "boo")))
    
; An LNum is one of: 
; – Number
; – (make-layer LNum)
(define LNUM1 1)
(define LNUM2 (make-layer 1))
(define LNUM3 (make-layer (make-layer 2)))

; An [L X] is one of: 
; – X
; – (make-layer X)

; An LNum is [L String]

; An LString is [L Number]
