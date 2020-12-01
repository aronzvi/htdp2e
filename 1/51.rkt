;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |51|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; A TrafficLight is one of the following Strings:
; – "red"
; – "green"
; – "yellow"
; interpretation the three strings represent the three 
; possible states that a traffic light may assume

(define TRAFFIC-LIGHT-SIZE 10)

; TrafficLight -> Image
; draw circle of appropriate color
(check-expect (render "red") (circle TRAFFIC-LIGHT-SIZE "solid" "red"))
(check-expect (render "green") (circle TRAFFIC-LIGHT-SIZE "solid" "green"))
(check-expect (render "yellow") (circle TRAFFIC-LIGHT-SIZE "solid" "yellow"))
(define (render tl) (circle TRAFFIC-LIGHT-SIZE "solid" tl))
 
; TrafficLight -> TrafficLight
; yields the next state given current state s
(check-expect (traffic-light-next "red") "green")
(check-expect (traffic-light-next "green") "yellow")
(check-expect (traffic-light-next "yellow") "red")
(define (traffic-light-next tl)
  (cond
    [(string=? "red" tl) "green"]
    [(string=? "green" tl) "yellow"]
    [(string=? "yellow" tl) "red"]))

; TrafficLight -> TrafficLight
; launches the program from some initial state 
(define (main tl)
   (big-bang tl
     [on-tick traffic-light-next]
     [to-draw render]))