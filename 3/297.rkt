;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |297|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Number Number Posn -> Number
; computes the distance between the points (x, y) and p
(check-expect (distance-between 3 4 (make-posn 0 0)) 5)
(check-within (distance-between 3 4 (make-posn 0 9)) (sqrt (+ (sqr (- 3 0)) (sqr (- 4 9)))) 0.01)
(check-within (distance-between 3 4 (make-posn -1 3)) (sqrt (+ (sqr (- 3 -1)) (sqr (- 4 3)))) 0.01)

;(define (distance-between x y p) 0) ;stub

; template from Posn

(define (distance-between x y p)
  (sqrt (+ (sqr (- x (posn-x p)))
           (sqr (- y (posn-y p))))))