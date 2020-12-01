;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |404|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;[X Y -> Boolean] [List-of X] [List-of Y] -> Boolean
; applies f to pairs of corresponding values l1 and l2. produces true if f produced true for all pairs. 
; andmap but for two lists.
; constraint. l1 and l2 are of of equal length
(check-expect (andmap2 (lambda (x y) (y x)) empty empty)
              true)
(check-expect (andmap2 (lambda (x y) (y x)) (list 1 "s" 'd) (list number? string? symbol?))
              true)
(check-expect (andmap2 (lambda (x y) (y x)) (list 1 "s" 'd) (list number? string? string?))
              false)
(check-expect (andmap2 > (list 1 2 3) (list 0 1 2))
              true)
(check-expect (andmap2 > (list 1 2 3) (list 0 5 2))
              false)

;(define (andmap2 f l1 l2) false) ;stub

#;
(define (andmap2 f l1 l2)
  (cond [(empty? l1) true]
        [else
         (... f 
              (first l1)  ;X
              (first l2)  ;Y
              (andmap2 f (rest l1) (rest l2)))])) ;[X Y -> Boolean] [List-of X] [List-of Y]

(define (andmap2 f l1 l2)
  (cond [(empty? l1) true]
        [else
         (and (f (first l1) (first l2))
              (andmap2 f (rest l1) (rest l2)))]))