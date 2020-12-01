;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |78|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Word is one of:
; - 3 instances of 1String [a, z]
; - #false

; ############3

; A Letter is one of:
; - 1String
; - #false

(define-struct word [let1 let2 let3])
; A Word is a structure
;  (make-word Letter Letter Letter)
; interpetation a 3 letter word1string?