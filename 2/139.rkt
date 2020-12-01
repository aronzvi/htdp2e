;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |139|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List-of-numbers is one of: 
; – '()
; – (cons Number List-of-numbers)

(define LON1 empty)
(define LON2 (cons 2 LON1))
(define LON3 (cons -3 LON2))

(define (fn-for-lon lon)
  (cond
    [(empty? lon) (...)]
    [else
     (... (first lon)                 ;Number
          (fn-for-lon (rest lon)))])) ;List-of-numbers

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Number List-of-numbers)
;; - self-reference: (rest lon) is List-of-numbers

;; List-of-numbers -> Boolean
;; produces true if all numbers in lon are positive numbers
(check-expect (pos? empty) #true)
(check-expect (pos? (cons 35 empty)) #true)
(check-expect (pos? (cons -35 empty)) #false)
(check-expect (pos? (cons 25 (cons 35 empty))) #true)
(check-expect (pos? (cons 34 (cons -35 empty))) #false)

;(define (pos? lon) #false) ;sub

(define (pos? lon)
  (cond
    [(empty? lon) #true]
    [else
     (and (>= (first lon) 0)                
          (pos? (rest lon)))]))

(pos? (cons 5 '()))
(pos? (cons -1 '()))

;; List-of-amounts -> PositiveNumber
;; computes the sum of the amounts in given loa
(check-expect (sum empty) 0)
(check-expect (sum (cons 34 empty)) (+ 34 0))
(check-expect (sum (cons 15 (cons 34 empty))) (+ 15 34))

;(define (sum loa) 0) ;stub

(define (sum loa)
  (cond
    [(empty? loa) 0]
    [else
     (+ (first loa)                
        (sum (rest loa)))]))

;; List-of-numbers -> PositiveNumber
;; produces sum of lon if all numbers in lon are positive. else, produces error
(check-expect (checked-sum empty) 0)
(check-expect (checked-sum (cons 35 empty)) (+ 35 0))
(check-expect (checked-sum (cons 24 (cons 35 empty))) (+ 24 35 0))
(check-error (checked-sum (cons -35 empty)))
(check-error (checked-sum (cons 34 (cons -35 empty))))

;(define (checked-sum lon) 0) ;stub

#;
(define (checked-sum lon)
  (cond
    [(empty? lon) 0]
    [else
     (if (< (first lon) 0)
         (error "checked-sum: expects list of positive numbers")
         (+ (first lon)
            (checked-sum (rest lon))))]))

; since we are using pos? and sum we don't need full tests
(check-expect (checked-sum (cons 24 (cons 35 empty))) (+ 24 35 0))
(check-error (checked-sum (cons 34 (cons -35 empty))))

(define (checked-sum lon)
  (if (pos? lon)
      (sum lon)
      (error "checked-sum: expects list of positive numbers"))) 

; sum computes for an element of List-of-numbers?: the sum of the numbers subtracting when negative??
(check-expect (sum (cons 34 (cons -35 empty))) (+ 34 -35 0))
