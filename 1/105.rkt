;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |105|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; =================
;; Constants

(define HEIGHT 300)
(define WIDTH 500)
(define CANVAS (empty-scene WIDTH HEIGHT))
(define POINT (circle 5 "solid" "black"))
(define DEFAULT-X 50)
(define DEFAULT-Y 100)

;; =================
;; Data definitions

; A Coordinate is one of: 
; – a NegativeNumber 
; interpretation on the y axis, distance from top
; – a PositiveNumber 
; interpretation on the x axis, distance from left
; – a Posn
; interpretation an ordinary Cartesian point

(define C1 -1)
(define C2 (- HEIGHT))
(define C3 1)
(define C4 WIDTH)
(define C5 (make-posn 10 40))
(define C6 (make-posn 70 60))

;; Sketches
(place-image POINT DEFAULT-X (- C1) CANVAS)
(place-image POINT DEFAULT-X (- C2) CANVAS)
(place-image POINT C3 DEFAULT-Y CANVAS)
(place-image POINT C4 DEFAULT-Y CANVAS)
(place-image POINT 10 40 CANVAS)
(place-image POINT 70 60 CANVAS)