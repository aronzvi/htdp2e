;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |186|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Data definitions:

; An NEList-of-number is one of: 
; – (cons Number '())
; – (cons Number NEList-of-number)
; interpretation list of non-empty numbers
(define NELON1 (cons 100 '()))
(define NELON2 (cons -1 NELON1))
(define NELON3 (cons -100 NELON2))

(define (fn-for-nelon nelon)
  (cond
    [(empty? (rest nelon)) (... (first nelon))]
    [else
     (... (first nelon)          ; Number
          (sum (rest nelon)))])) ; NEList-of-number

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

;; Functions:

; List-of-numbers -> List-of-numbers 
; produces a sorted version of alon in descending order
(check-expect (sort> empty) empty)
(check-satisfied (sort> (list 3 2 1)) sorted>?)
(check-satisfied (sort> (list 1 2 3)) sorted>?)
(check-satisfied (sort> (list 12 20 -5))
              sorted>?)

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

; List-of-numbers -> List-of-numbers
; produces a sorted version of l
(check-expect (sort>/bad empty) empty) ;This should fail and show that sort>/bad is not a sorting function
(check-expect (sort>/bad (list 3 2 1)) (list 3 2 1)) ;This should fail and show that sort>/bad is not a sorting function
(check-satisfied (sort>/bad (list 3 2 1)) sorted>?) ;This will not fail and will not show that sort>/bad is not a sortng function

(define (sort>/bad l)
  (list 9 8 7 6 5 4 3 2 1 0))

;; NEList-of-number -> Boolean
;; produces #true if the numbers are sorted in descending order
(check-expect (sorted>? (cons 2 empty)) #true)
(check-expect (sorted>? (cons 1 (cons 2 empty))) #false)
(check-expect (sorted>? (cons 3 (cons 2 empty))) #true)
(check-expect (sorted>? (cons 0 (cons 3 (cons 2 empty)))) #false)

;(define (sorted>? nelon) #false) ;stub

(define (sorted>? nelon)
  (cond
    [(empty? (rest nelon)) #true]
    [else
     (and (> (first nelon) (first (rest nelon)))          
          (sorted>? (rest nelon)))]))