;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |429|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
; assume that numbers are distinct
(check-expect (quick-sort< empty) empty)
(check-expect (quick-sort< '(1 4 2 7 6 100 32 33 90 5))
              '(1 2 4 5 6 7 32 33 90 100))
(check-expect (quick-sort< '(1 1 1))
              '(1 1 1))
(check-expect (quick-sort< '(5 5 2 7 10 10 10 1))
              '(1 2 5 5 7 10 10 10))
;(define (quick-sort< alon) alon) ;stub

(define (quick-sort< alon)
  (cond
    [(empty? alon) '()]
    [(= (length alon) 1) alon]
    [else (local ((define pivot (first alon)))
            (append (quick-sort< (smallers alon pivot))
                    (filter (lambda (n) (= n pivot)) alon)
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