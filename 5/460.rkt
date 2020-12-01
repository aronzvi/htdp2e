;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |460|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define ε 0.01)
(define INTERVAL_SMALL_ENOUGH 0.1)

; [Number -> Number] Number Number
; integrates f between the boundaries a and b using a divide-and-conquer strategy with Keplar's rule
; we split the interval in half and sum up the itegration of both halfs.
; trivial case: when the interval is <= to INTERVAL_SMALL_ENOUGH for which we get the itegration using keplar's method
; termination: we split the interval every iteration until it is eventually <= INTERVAL_SMALL_ENOUGH
(check-within (integrate-dc (lambda (x) 20) 12 22) 200 ε)
(check-within (integrate-dc (lambda (x) (* 2 x)) 0 10) 100 ε)
(check-within (integrate-dc (lambda (x) (* 3 (sqr x))) 0 10) 
              1000
              ε)

;(define (integrate-dc f a b) #i0.0) ;stub

(define (integrate-dc f a b)
  (cond [(<= (- b a) INTERVAL_SMALL_ENOUGH) (integrate-kepler f a b)]
        [else
         (local ((define mid (/ (+ a b) 2)))
           (+   (integrate-dc f a mid)
                (integrate-dc f mid b)))]))

; [Number -> Number] Number Number -> Number
; computes an estimate of the area under f between a and b using Keplar's rule 
(check-within (integrate-kepler (lambda (x) 20) 12 22) 200 ε)
(check-within (integrate-kepler (lambda (x) (* 2 x)) 0 10) 100 ε)
(check-within (integrate-kepler (lambda (x) (* 3 (sqr x))) 0 10) ; fails
              1000
              ε)

(define (integrate-kepler f a b)
  (local ((define mid (/ (+ a b) 2))
          (define (trapezoid-area l f-l r f-r)
            (/ (* (- (posn-x r) (posn-x l)) (+ (posn-y f-l) (posn-y f-r)))
               2)))
    (+ (trapezoid-area (make-posn a 0)
                       (make-posn a (f a))
                       (make-posn mid 0)
                       (make-posn mid (f mid)))
       (trapezoid-area (make-posn mid 0)
                       (make-posn mid (f mid))
                       (make-posn b 0)
                       (make-posn b (f b))))))