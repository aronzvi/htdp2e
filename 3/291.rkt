;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |291|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; (X -> Y) [List-of X] -> [List-of Y]
;; constructs a list by applying f to each item on lx
(check-expect (map-via-fold add1 empty) empty)
(check-expect (map-via-fold add1 (list 1 2 3)) (list (add1 1) (add1 2) (add1 3)))
(check-expect (map-via-fold string-length (list "q" "ddd" "rrrrr")) (list 1 3 5))

;(define (map-via-fold f lox) empty) ;stub

#;
(define (map-via-fold f lox)
        ;(X [List-of Y] -> [List-of Y]) 
  (foldr ...                            empty lox))

(define (map-via-fold f lox)
  (foldr (lambda (x loy)
           (cons (f x) loy))
         empty
         lox)) 