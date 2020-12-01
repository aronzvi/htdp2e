;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |60|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An N-TrafficLight is one of:
; – 0 interpretation the traffic light shows red
; – 1 interpretation the traffic light shows green
; – 2 interpretation the traffic light shows yellow

(define N-TL-RED 0)
(define N-TL-GREEN 1)
(define N-TL-YELLOW 2)

; N-TrafficLight -> N-TrafficLight
; yields the next state, given current state cs
(check-expect (tl-next-numeric N-TL-RED) N-TL-GREEN)
(check-expect (tl-next-numeric N-TL-GREEN) N-TL-YELLOW)
(check-expect (tl-next-numeric N-TL-YELLOW) N-TL-RED)
(define (tl-next-numeric cs) (modulo (+ cs 1) 3))

; The numeric version's intention is less clear since it uses numbers.
; We cannot see the simple transition from color to color