;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |226|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; FSM-State is a Color.

; FSM-State FSM-State -> Boolean
; produces true if state1 is equal to state2
(check-expect (state=? "red" "red") #true)
(check-expect (state=? "red" "green") #false)

;(define (state=? state1 state2) #false)

(define (state=? state1 state2)
  (string=? state1 state2))
 