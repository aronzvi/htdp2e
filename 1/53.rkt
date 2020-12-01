;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |53|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define HEIGHT 300) ; distances in pixels 
(define WIDTH  100)
 
(define MTSCN  (empty-scene WIDTH HEIGHT))
(define ROCKET (rectangle 5 30 "solid" "red"))
(define ROCKET-X (/ WIDTH 2))

; An LR (short for launching rocket) is one of:
; – "resting"
; – NonnegativeNumber
; interpretation "resting" represents a grounded rocket
; a number denotes the number of pixels between the
; top of the canvas and the center of the rocket (its height)

; example data
(define LR1 "resting")
(define LR2 0)      ; Almost off screen
(define LR5 HEIGHT) ; Almost below the ground

; Sample drawings
(place-image ROCKET ROCKET-X LR5 MTSCN) ; Almost below the ground
(place-image ROCKET ROCKET-X 0 MTSCN)   ; Almost off screen

