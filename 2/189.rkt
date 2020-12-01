;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |189|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; ListOfNumber is one of: 
; – '()
; – (cons Number ListOfNumber)
; interp. a list of numbers
(define LON1 empty)
(define LON2 (cons 1 (cons 2 empty)))

(define (fn-for-lon lon)
  (cond
    [(empty? lon) (...)]
    [else
     (... (first lon)
          (fn-for-lon (rest lon)))]))

;; Number ListOfNumber -> Boolean
;; produces true if number occurs in a sorted (ascending) list of numbers
(check-expect (sorted-search 1 empty) #false)
(check-expect (sorted-search 1 (list 1 2 3)) #true)
(check-expect (sorted-search 3 (list 1 2 3)) #true)
(check-expect (sorted-search 1 (list 2 3 5)) #false)

;(define (sorted-search n lon) #false) ;stub

(define (sorted-search n lon)
  (cond
    [(empty? lon) #false]
    [else
     (if (<= n (first lon))
         (= n (first lon))
          (sorted-search n (rest lon)))]))

