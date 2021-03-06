;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |427|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define THRESHOLD 3)

; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
; assume that numbers are distinct
(check-expect (quick-sort< empty) empty)
(check-expect (quick-sort< '(1 4 2 7 6 100 32 33 90 5))
              '(1 2 4 5 6 7 32 33 90 100))

;(define (quick-sort< alon) alon) ;stub

(define (quick-sort< alon)
  (cond
    [(empty? alon) '()]
    [(= (length alon) 1) alon]
    [(< (length alon) THRESHOLD) (sort< alon)]
    [else (local ((define pivot (first alon)))
            (append (quick-sort< (smallers alon pivot))
                    (list pivot)
                    (quick-sort< (largers alon pivot))))]))

; [List-of Number] Number -> [List-of Number]
; produces list of numbers in l that are smaller than n
(check-expect (smallers '() 10) '())
(check-expect (smallers '(10) 10) '())
(check-expect (smallers '(1 3 5 10 15 22 11 13) 9)
              '(1 3 5))

;(define (smallers l n) l) ;stub

(define (smallers l n)
  (filter (lambda (ni) (< ni n)) l))

; [List-of Number] Number -> [List-of Number]
; produces list of numbers in l that are larger than n
(check-expect (largers '() 10) '())
(check-expect (largers '(10) 10) '())
(check-expect (largers '(1 3 5 10 15 22 11 13) 9)
              '(10 15 22 11 13))

;(define (largers l n) l) ;stub

(define (largers l n)
  (filter (lambda (ni) (> ni n)) l))

; List-of-numbers -> List-of-numbers
; produces a sorted version of l in ascending order
(check-expect (sort< '()) '())
(check-expect (sort< (list 3 2 1)) (list 1 2 3))
(check-expect (sort< (list 1 2 3)) (list 1 2 3))
(check-expect (sort< (list 12 20 -5))
              (list -5 12 20))
(define (sort< l)
  (cond
    [(empty? l) '()]
    [(cons? l) (insert (first l) (sort< (rest l)))]))
 
; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted in ascending order list of numbers l 
(check-expect (insert 5 '()) (list 5))
(check-expect (insert 5 (list 6)) (list 5 6))
(check-expect (insert 5 (list 4)) (list 4 5))

(define (insert n l)
  (cond
    [(empty? l) (cons n '())]
    [else (if (<= n (first l))
              (cons n l)
              (cons (first l) (insert n (rest l))))]))
