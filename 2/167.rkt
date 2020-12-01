;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |167|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ListOfPosn is one of:
;; - empty
;; (cons Posn ListOfPosn)
;; interp. a list of posns
(define LOP1 empty)
(define LOP2 (cons (make-posn 3 4) LOP1))
(define LOP3 (cons (make-posn 80 50) LOP2))

(define (fn-for-lop lop)
  (cond
    [(empty? lop) (...)]
    [else
     (... (fn-for-posn (first lop))   ;Posn
          (fn-for-lop (rest lop)))])) ;ListOfPosn

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - reference: (first lop) is Posn
;; - self-reference: (rest lop) is ListOfPosn

;; ListOfPosn -> Number
;; produces the sum of all x-coordinates in lop
(check-expect (sum empty) 0)
(check-expect (sum (cons (make-posn 5 7) empty)) (+ 5 0))
(check-expect (sum (cons (make-posn 30 9)(cons (make-posn 5 7) empty))) (+ 30 5 0))
              
;(define (sum lop) 0) ;stub

(define (sum lop)
  (cond
    [(empty? lop) 0]
    [else
     (+ (posn-x (first lop))   
          (sum (rest lop)))]))

