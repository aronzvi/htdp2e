;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |442|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define MAX 1000000)

; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
; assume the numbers are all distinct
(check-expect (quick-sort< '()) '())
(check-expect (quick-sort< (list 3 2 1)) (list 1 2 3))
(check-expect (quick-sort< (list 1 2 3)) (list 1 2 3))
(check-expect (quick-sort< (list 12 20 -5))
              (list -5 12 20))
(define (quick-sort< alon)
  (cond
    [(empty? alon) '()]
    [else (local ((define pivot (first alon)))
            (append (quick-sort< (smallers alon pivot))
                    (list pivot)
                    (quick-sort< (largers alon pivot))))]))
 
; [List-of Number] Number -> [List-of Number]
(define (largers alon n)
  (cond
    [(empty? alon) '()]
    [else (if (> (first alon) n)
              (cons (first alon) (largers (rest alon) n))
              (largers (rest alon) n))]))
 
; [List-of Number] Number -> [List-of Number]
(define (smallers alon n)
  (cond
    [(empty? alon) '()]
    [else (if (< (first alon) n)
              (cons (first alon) (smallers (rest alon) n))
              (smallers (rest alon) n))]))

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

; Natural -> [List-of Natural]
; generate a list of random naturals of length n
(check-expect (length (create-tests 1000)) 1000)
(check-expect (length (create-tests 10541)) 10541)

;(define (create-tests n) empty) ;stub

(define (create-tests n)
  (build-list n (lambda (i) (random MAX))))

(define T-15 (create-tests 15))
(define T-20 (create-tests 20))
(define T-30 (create-tests 30))
(define T-50 (create-tests 50))
(define T-75 (create-tests 75))
(define T-100 (create-tests 100))
(define T-500 (create-tests 250))
(define T-250 (create-tests 500))
(define T-1000 (create-tests 1000))


; 402 and 28
;(time (sort< T-1000))
;(time (quick-sort< T-1000))

; 91, 13
;(time (sort< T-500))
;(time (quick-sort< T-500))

; 88,13
;(time (sort< T-250))
;(time (quick-sort< T-250))

; 4, 2
;(time (sort< T-100))
;(time (quick-sort< T-100))

;(time (sort< T-75))
;(time (quick-sort< T-75))

; 1,1
(time (sort< T-50))
(time (quick-sort< T-50))

;(time (sort< T-30))
;(time (quick-sort< T-30))

;(time (sort< T-20))
;(time (quick-sort< T-20))

;(time (sort< T-15))
;(time (quick-sort< T-15))
                                           