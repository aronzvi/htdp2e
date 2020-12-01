;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |125|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; 1.

(define-struct oops [])

;legal! - variable followed by sequence of zero or more variables inside parenthesis

(define-struct child [parents dob date])

; legal - variable followed by sequence of zero or more variables inside parenthesis

(define-struct (child person) [dob date])

; illegal - (child person) is not a variable