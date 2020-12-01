;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |456|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define ε 0.1)

; [Number -> Number] Number -> Number
; produces the root of the tangent through (r1,(f r1)) (using point-slope form) 
#;
(check-expect (root-of-tangent horiz 2) (- 2 (/ (horiz 2)
                                                (slope horiz 2)))) ; horiz has slope 0. division by zero. Can't use horiz
#;
(check-expect (root-of-tangent poly 3) (- 3 (/ (poly 3)
                                               (slope poly 3)))) ; (slope poly 3) is 0. division by zero. 

(check-expect (root-of-tangent linear 2) (- 2 (/ (linear 2)
                                                   (slope linear 2))))

;(define (root-of-tangent f r1) 0) ;stub

(define (root-of-tangent f r1)
  (- r1 (/ (f r1)
           (slope f r1))))

; [Number -> Number] Number -> Number
; produces the slope of f at r1 using ε
(check-expect (slope horiz 2) 0)
(check-expect (slope linear 2) 1)
(check-expect (slope poly 2) -2)

;(define (slope f r1) 0) ;stub

(define (slope f r1)
  (/ (- (f (+ r1 ε)) (f (- r1 ε)))
     (* 2 ε)))

(define (horiz x) 5)
(define (linear x) (+ x 2))
(define (poly x) (* (- x 2) (- x 4)))
