;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname move-right) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; [List-of Posn] -> [List-of Posn] 
; moves each object right by delta-x pixels
 
(define input  `(,(make-posn 1 1) ,(make-posn 10 14)))
(define expect `(,(make-posn 4 1) ,(make-posn 13 14)))

(check-expect (move-right empty 3) empty)
(check-expect (move-right input 3) expect)
 
;(define (move-right lop delta-x) lop) ;stub

#;
(define (move-right lop delta-x)
  (match lop
    ['() empty]
    [(cons (posn x y) rst) (cons (make-posn (+ x delta-x) y) (move-right rst delta-x))]))

(define (move-right lop delta-x)
  (for/list ((p lop))
    (match p
      [(posn x y) (make-posn (+ x delta-x) y)])))

#;
(define (move-right lop delta-x)
  (for/list ((p lop))
    (make-posn (+ (posn-x p) delta-x) (posn-y p))))

#;
(define (move-right lop delta-x)
  (cond
    [(empty? lop) empty]
    [else (cons (make-posn (+ (posn-x (first lop)) delta-x) (posn-y (first lop)))
                (move-right (rest lop) delta-x))]))