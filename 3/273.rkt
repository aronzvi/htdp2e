;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |273|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of X] [X -> Y] -> [List-of Y]
; produces a list (list (f x-1) ... (f x-n)) for all elements in lox
(check-expect (map-from-fold (list 1 2 3) add1) (list 2 3 4))
(check-expect (map-from-fold (list 1 2 3) sqr) (list 1 4 9))

;(define (map-from-fold lox f) lox) ;stub

#;
(define (map-from-fold lox f) ;template
         [X [List-of Y] -> [List-of Y]]   [List-of Y]
  (foldr     ...                           ...        lox)) 


(define (map-from-fold lox f)
      (local (; X [List-of Y] -> [List-of Y]
              ; applies (f x) and prepends it to loy
              (define (apply-and-prepend x loy) (cons (f x) loy)))    
  (foldr apply-and-prepend empty lox)))