;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |239|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;   A [List X Y] is a structure: 
;   (cons X (cons Y '()))

; A NumberPair is [List Number Number]
; interp. a pair of numbers
(define PN1 (cons 1 (cons 2 '())))

; A Number1StringPair is  [List Number 1String]
; interp. a pair of Number and 1String
(define PN1S1 (cons  3 (cons "f" '())))

; A StringBooleanPair is [List String Boolean]
; interp, a pair of String and Boolean
(define PSB1 (cons "dfdf" (cons #true '())))
