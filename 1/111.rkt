;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |111|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct vec [x y])
; A Vec is
;   (make-vec PositiveNumber PositiveNumber)
; interpretation represents a velocity vector

; Any Any -> Vec
; produces a vec with given x and y. ensures that x and y are positive numbers
; tests ???

;(define checked-make-vec x y) ; stub

(define (checked-make-vec x y)
  (cond
    [(and (number? x) (number? y) (>= x 0) (>= y 0)) (make-vec x y)]
    [else (error "make-vec: positive numbers expected")]))

