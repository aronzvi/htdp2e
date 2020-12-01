;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname insert_sort_numbers) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List-of-numbers is one of: 
; – '()
; – (cons Number List-of-numbers)
; interp. a list of numbers
(define LON1 empty)
(define LON2 (cons 1 (cons 2 empty)))

(define (fn-for-lon lon)
  (cond
    [(empty? lon) (...)]
    [else
     (... (first lon)
          (fn-for-lon (rest lon)))]))

; List-of-numbers -> List-of-numbers 
; produces a sorted version of alon in descending order
(check-expect (sort> empty) empty)
(check-expect (sort> (list 3 2 1)) (list 3 2 1))
(check-expect (sort> (list 1 2 3)) (list 3 2 1))
(check-expect (sort> (list 12 20 -5))
              (list 20 12 -5))

;(define (sort> alon) alon) ;stub

(define (sort> lon)
  (cond
    [(empty? lon) empty]
    [else
     (insert (first lon)
             (sort> (rest lon)))]))

; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted list of numbers alon
(check-expect (insert 5 '()) (list 5))
(check-expect (insert 5 (list 6)) (list 6 5))
(check-expect (insert 5 (list 4)) (list 5 4))
(check-expect (insert 12 (list 20 -5))
              (list 20 12 -5))
(check-expect (insert 3 (list 2 1))
              (list 3 2 1))
(check-expect (insert 1 (list 3 2))
              (list 3 2 1))
(check-expect (insert 2 (list 2 1))
              (list 2 2 1))

;(define (insert n alon) alon) ;stub

(define (insert n lon)
  (cond
    [(empty? lon) (list n)]
    [else
     (if (>= n (first lon))
         (cons n lon)
         (cons (first lon)
               (insert n (rest lon))))]))


