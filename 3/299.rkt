;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |299|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Set is a function:
; [Number -> Boolean]
; produces true only if element belongs to set

; Set Number -> Boolean
; produces true if number belongs to set
; !!!
(define (belongs? s ed) (s ed))

; Number -> Boolean
; produces true if number is odd. (infinite?)
(check-expect (belongs? odd-numbers 1) true)
(check-expect (belongs? odd-numbers 2) false)

;(define (odd-numbers ed) false) ;stub

(define (odd-numbers ed) (odd? ed))

; Number -> Boolean
; produces true if number is even. (infinite?)
(check-expect (belongs? even-numbers 6) true)
(check-expect (belongs? even-numbers 7) false)

;(define (even-numbers ed) false) ;stub

(define (even-numbers ed) (even? ed))

; Number -> Boolean
; produces true if number is divisible by 10. (infinite?)
(check-expect (belongs? div-by-10-numbers 20) true)
(check-expect (belongs? div-by-10-numbers 81) false)

;(define (div-by-10-numbers ed) false) ;stub

(define (div-by-10-numbers ed)
  (= (modulo ed 10) 0))

; [List-of Number] -> Set
; creates a  (finite) set with given lon.
(check-expect (belongs? (mk-set (list 1 2 3 4)) 3) true)
(check-expect (belongs? (mk-set (list 1 2 3 4)) 7) false)

;(define (mk-set lon) (lambda (ed) false)) ;stub

(define (mk-set lon) (lambda (ed) (member? ed lon))) 

; Number Set -> Set
; adds an element to a set
(check-expect (belongs? (add-element 5 (mk-set (list 1 2 3 4))) 5) true)
(check-expect (belongs? (add-element 7 even-numbers) 7) true)

;(define (add-element ed s) (lambda (ed1) false)) ;stub

(define (add-element ed s)
  (lambda (ed1)
    (or (s ed1)
        (= ed ed1))))

; Set Set -> Set
; combines the elements of two sets
(check-expect (belongs? (union (mk-set (list 1 2 3 4)) (mk-set (list 5 8 3 4))) 8) true)
(check-expect (belongs? (union (mk-set (list 1 2 3 4)) (mk-set (list 5 8 3 4))) 10) false)

;(define (union s1 s2) (lambda (ed) false)) ;stub

(define (union s1 s2) (lambda (ed)
                        (or (s1 ed)
                            (s2 ed))))

; Set Set -> Set
; collects all elements common to two sets
(check-expect (belongs? (intersect (mk-set (list 1 2 3 4)) (mk-set (list 5 8 3 4))) 3) true)
(check-expect (belongs? (intersect (mk-set (list 1 2 3 4)) (mk-set (list 5 8 3 4))) 8) false)

;(define (intersect s1 s2) (lambda (ed) false)) ;stub

(define (intersect s1 s2) (lambda (ed)
                            (and (s1 ed)
                                 (s2 ed))))


