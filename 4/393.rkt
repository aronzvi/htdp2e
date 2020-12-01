;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |393|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Son is one of: 
; – empty 
; – (cons Number Son)
; 
; Son Son -> Son
; produces a set that contains the elements of s1 and s1
(check-expect (union empty (list 1)) (list 1))
(check-expect (union (list 1) empty) (list 1))
(check-expect (union (list 1 2) (list 3 4)) (append (list 1 2) (list 3 4)))

;(define (union s1 s2) s1) ;stub

; treating s2 as atomic? They probbaly want you to do something on your own here and not use append
(define (union s1 s2)
  (append s1 s2))

; Son Son -> Son
; produces the set of exactly those elements that occur in both s1 and s2
(check-expect (intersect empty empty) empty)
(check-expect (intersect empty (list 1)) empty)
(check-expect (intersect (list 1) empty) empty)
(check-expect (intersect (list 1) (list 1)) (list 1))
(check-expect (intersect (list 1) (list 3 4)) empty)
(check-expect (intersect (list 1 2) (list 1)) (list 1))
(check-expect (intersect (list 1 2 3) (list 1 5 4 3))  (list 1 3))

;(define (intersect si s2) s1) ;stub

; Template from [List-of] on s1

; treating s2 as atomic
(define (intersect s1 s2)
  (cond [(empty? s1) empty]
        [else
         (if (member? (first s1) s2) 
          (cons (first s1) (intersect (rest s1) s2))
          (intersect (rest s1) s2))]))