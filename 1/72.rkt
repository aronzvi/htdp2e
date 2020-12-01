;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |72|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct phone [area number])
; A Phone is a structure:
;  (make-phone Number String)
; interpretation: a phone number
; area the area code
; number the phone number

(define-struct phone# [area switch num])
; A Phone# is a structure:
;  (make-phone# Number Number Number)
; interpretation: a phone number 
; area is the area code [200, 999]
; switch is the phone switch (exchange) [200, 999]
; num is the number in the neighborhood [0000, 9999]