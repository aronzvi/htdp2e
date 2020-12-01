;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |59|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 90)
(define HEIGHT 30)
(define MTSCN (empty-scene WIDTH HEIGHT))
(define BULB-R 10)
(define SPACE (rectangle BULB-R (/ BULB-R 2) "solid" "white"))

; A TrafficLight is one of the following Strings:
; – "red"
; – "green"
; – "yellow"
; interpretation the three strings represent the three 
; possible states that a traffic light may assume 

; TrafficLight -> TrafficLight
; yields the next state, given current state cs
(check-expect (tl-next "red") "green") ; red -> green
(check-expect (tl-next "green") "yellow") ; green -> yellow
(check-expect (tl-next "yellow") "red") ; yellow -> red
(define (tl-next cs)
  (cond
    [(string=? cs "red") "green"]
    [(string=? cs "green") "yellow"]
    [(string=? cs "yellow") "red"]))
 
; TrafficLight -> Image
; renders the current state cs as an image
(check-expect (tl-render "red") (place-image (beside (render-bulb "red" true)
                                                     SPACE
                                                     (render-bulb "yellow" false)
                                                     SPACE
                                                     (render-bulb "green" false))
                                             (/ WIDTH 2)
                                             (/ HEIGHT 2)
                                             MTSCN)) ; red
(check-expect (tl-render "yellow") (place-image (beside (render-bulb "red" false)
                                                        SPACE
                                                        (render-bulb "yellow" true)
                                                        SPACE
                                                        (render-bulb "green" false))
                                                (/ WIDTH 2)
                                                (/ HEIGHT 2)
                                                MTSCN)) ; yellow
(check-expect (tl-render "green") (place-image (beside (render-bulb "red" false)
                                                       SPACE
                                                       (render-bulb "yellow" false)
                                                       SPACE
                                                       (render-bulb "green" true))
                                               (/ WIDTH 2)
                                               (/ HEIGHT 2)
                                               MTSCN)) ; green
(define (tl-render current-state)
  (place-image
   (cond
     [(string=? current-state "red") (beside (render-bulb "red" true)
                                 SPACE
                                 (render-bulb "yellow" false)
                                 SPACE
                                 (render-bulb "green" false))]
     [(string=? current-state "yellow") (beside (render-bulb "red" false)
                                    SPACE
                                    (render-bulb "yellow" true)
                                    SPACE
                                    (render-bulb "green" false))]
     [(string=? current-state "green") (beside (render-bulb "red" false)
                                   SPACE
                                   (render-bulb "yellow" false)
                                   SPACE
                                   (render-bulb "green" true))])
   (/ WIDTH 2)
   (/ HEIGHT 2)
   MTSCN))

; TrafficLight Boolean -> Image
; renders a given traffic light buld. Either on or off
(check-expect (render-bulb "red" true) (circle BULB-R "solid" "red")) ; red on
(check-expect (render-bulb "red" false) (circle BULB-R "outline" "red")) ; red off
(check-expect (render-bulb "yellow" true) (circle BULB-R "solid" "yellow")) ; yellow on
(check-expect (render-bulb "yellow" false) (circle BULB-R "outline" "yellow")) ; yellow off
(check-expect (render-bulb "green" true) (circle BULB-R "solid" "green")) ; green on
(check-expect (render-bulb "green" false) (circle BULB-R "outline" "green")) ; green off
(define (render-bulb tl on?)
  (circle BULB-R (if on? "solid" "outline") tl))

; TrafficLight -> TrafficLight
; simulates a clock-based American traffic light
(define (traffic-light-simulation initial-state)
  (big-bang initial-state
    [to-draw tl-render]
    [on-tick tl-next 1]))