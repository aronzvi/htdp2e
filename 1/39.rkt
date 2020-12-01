;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |39|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
 
(define WHEEL-RADIUS 20)
(define WHEEL-DISTANCE (* WHEEL-RADIUS 8))


(define WHEEL
  (circle WHEEL-RADIUS "solid" "black"))
(define CHASIS
  (rectangle WHEEL-DISTANCE ( * WHEEL-RADIUS 1.5) "solid" "red"))
(define TOP (rectangle (/ WHEEL-DISTANCE 2) WHEEL-RADIUS "solid" "red"))

(define CHASIS-WITH-TOP
  (above TOP CHASIS))

(define SPACE (rectangle ( - WHEEL-DISTANCE (* WHEEL-RADIUS 5)) (/ WHEEL-RADIUS 3) "solid" "white"))
(define WHEELS-WITH-SPACE (beside WHEEL SPACE WHEEL))
(define CAR (overlay/align/offset "middle" "bottom"  WHEELS-WITH-SPACE 0 (- WHEEL-RADIUS) CHASIS-WITH-TOP))

