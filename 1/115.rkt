;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |115|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define MESSAGE-PRE-ARG-NUMBER "light=?:expects a trafficlight as")

(define MESSAGE-POST-ARG-NUMBER "argument")

; Any -> Boolean
; is the given value an element of TrafficLight
(define (light? x)
  (cond
    [(string? x) (or (string=? "red" x)
                     (string=? "green" x)
                     (string=? "yellow" x))]
    [else #false]))
 
; Any Any -> Boolean
; are the two values elements of TrafficLight and, 
; if so, are they equal
 
(check-expect (light=? "red" "red") #true)
(check-expect (light=? "red" "green") #false)
(check-expect (light=? "green" "green") #true)
(check-expect (light=? "yellow" "yellow") #true)
 
(define (light=? a-value another-value)
 (cond
   [(not (light? a-value)) (error (error-msg "1st"))]
   [(not (light? another-value)) (error (error-msg "2nd"))]
   [else (string=? a-value another-value)]))

;; String -> String
;; produces arg error message with arg number
(check-expect (error-msg "1st") "light=?:expects a light as 1st argument")
(check-expect (error-msg "2nd") "light=?:expects a light as 2nd argument")

;(define (error-msg arg) arg) ; stub

;(define (error-msg arg) (... arg)) ; Template

(define (error-msg arg)
  (string-append MESSAGE-PRE-ARG-NUMBER " " arg " " MESSAGE-POST-ARG-NUMBER))

  