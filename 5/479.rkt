;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |479|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define QUEENS 8)
; A QP is a structure:
;   (make-posn CI CI)
; A CI is an N in [0,QUEENS).
; interpretation (make-posn r c) denotes the square at 
; the r-th row and c-th column

; QP QP -> Boolean
; determines whether queens placed on the two qps would threaten each other
(check-expect (threatening? (make-posn 0 7) (make-posn 0 2)) true) ;vertical. Same x
(check-expect (threatening? (make-posn 5 7) (make-posn 2 7)) true) ;horizontal. Same y
(check-expect (threatening? (make-posn 5 3) (make-posn 2 0)) true) ;diagonal lb-rt. Difference is the same
(check-expect (threatening? (make-posn 6 2) (make-posn 4 4)) true) ;diagonal rb-lt. Sum is the same
(check-expect (threatening? (make-posn 0 3) (make-posn 7 6)) false)
(check-expect (threatening? (make-posn 8 6) (make-posn 0 8)) false)

;(define (threatening? qp1 qp2) false) ;stub

(define (threatening? qp1 qp2)
  (local ((define (threatening-v? qp1 qp2) (= (posn-x qp1) (posn-x qp2)))
          (define (threatening-h? qp1 qp2) (= (posn-y qp1) (posn-y qp2)))
          (define (threatening-d-lb-rt? qp1 qp2) (= (- (posn-x qp1) (posn-y qp1))
                                                    (- (posn-x qp2) (posn-y qp2))))
          (define (threatening-d-rb-lt? qp1 qp2) (= (+ (posn-x qp1) (posn-y qp1))
                                                    (+ (posn-x qp2) (posn-y qp2)))))
    (or (threatening-v? qp1 qp2)
        (threatening-h? qp1 qp2)
        (threatening-d-lb-rt? qp1 qp2)
        (threatening-d-rb-lt? qp1 qp2))))

