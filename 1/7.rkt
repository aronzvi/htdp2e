;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |7|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define sunny #true)
(define friday #false)

(or (not sunny) friday)

; We can associate 4 boolean combinations with sunny and friday:

; sunny | friday
; ______________
; true  |  true
; false |  false
; true  |  false
; false |  true