;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |236|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Lon -> Lon
; adds 1 to each item on l
(check-expect (add1* empty) empty)
(check-expect (add1* (list 1 2 3)) (list 2 3 4))

#;
(define (add1* l)
  (cond
    [(empty? l) '()]
    [else
     (cons
       (add1 (first l))
       (add1* (rest l)))]))

(define (add1* l)
  (add-n 1 l))
     

; Lon -> Lon
; adds 5 to each item on l
(check-expect (plus5 empty) empty)
(check-expect (plus5 (list 1 2 3)) (list 6 7 8))

#;
(define (plus5 l)
  (cond
    [(empty? l) '()]
    [else
     (cons
       (+ (first l) 5)
       (plus5 (rest l)))]))

(define (plus5 l)
  (add-n 5 l))

; Number Lon -> Lon
; adds n to each item on l
(check-expect (add-n 5 empty) empty)
(check-expect (add-n 1 (list 1 2 3)) (list 2 3 4))
(check-expect (add-n 10 (list 1 2 3)) (list 11 12 13))

(define (add-n n l)
  (cond
    [(empty? l) '()]
    [else
     (cons
       (+ (first l) n)
       (add-n n (rest l)))]))

; Lon -> Lon
; subtracts 2 from each number on l
(check-expect (sub2 empty) empty)
(check-expect (sub2 (list 1 2 3)) (list -1 0 1))

;(define (sub2 l) l) ;stub

(define (sub2 l)
  (add-n -2 l)) 