;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname r3-distance-to-origin) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct r3 [x y z])
; An R3 is a structure:
;   (make-r3 Number Number Number)
 
(define ex1 (make-r3 1 2 13))
(define ex2 (make-r3 -1 0 3))

; R3 -> Number
; computes the distance of given r3 to the origin
(check-within (r3-distance-to-origin ex1) (sqrt (+ (sqr 1) (sqr 2) (sqr 13))) 0.01)
(check-within (r3-distance-to-origin ex2) (sqrt (+ (sqr -1) (sqr 0) (sqr 3))) 0.01)

;(define (r3-distance-to-origin r3) ; template
;  (... (r3-x r3) ; Number
;   ... (r3-y r3) ; Number
;   ...(r3-z r3) ; Number
;   ...))

(define (r3-distance-to-origin r3) 
  (sqrt (+ (sqr (r3-x r3)) 
           (sqr (r3-y r3))
           (sqr (r3-z r3)))))