;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |147|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; NEList-of-Booleans is on of:
;; - (cons Boolean empty)
;; - (cons Boolean NEList-of-Booleans)
;; interp. a non-empty list of Booleans

(define NELOB1 (cons #true empty))
(define NELOB2 (cons #true NELOB1))
(define NELOB3 (cons #false NELOB2))

(define (fn-for-nelob nelob)
  (cond
    [(empty? (rest nelob)) (... (first nelob))]
    [else
     (... (first nelob)                    ;Boolean
          (fn-for-nelob (rest nelob)))]))  ;NEList-of-Booleans 

;; Template rules used:
;; - one of: 2 cases
;; - compound: (cons Boolean empty)
;; should be selectors for first case. What rules are we using to add the empty - the base case?
;; - compound: (cons Boolean NEList-of-Booleans)
;; - self-reference: (rest nelob) is NEList-of-Booleans

;; NEList-of-Booleans -> Boolean
;; consumes a list of Boolean values and determines whether all of them are #true
(check-expect (all-true (cons #true empty)) #true)    ; base case
(check-expect (all-true (cons #false empty)) #false)  ; base case
(check-expect (all-true (cons #true (cons #true (cons #true empty)))) #true)
(check-expect (all-true (cons #true (cons #false (cons #true empty)))) #false)

;(define (all-true nelob) #false) ;stub

(define (all-true nelob)
  (cond
    [(empty? (rest nelob)) (first nelob)]
    [else
     (and (first nelob)                   
          (all-true (rest nelob)))]))

;; NEList-of-Booleans -> Boolean
;; determines whether at least one item on the list is #true.
(check-expect (one-true (cons #true empty)) #true)    ; base case
(check-expect (one-true (cons #false empty)) #false)  ; base case
(check-expect (one-true (cons #false (cons #false (cons #true empty)))) #true)
(check-expect (one-true (cons #false (cons #false (cons #false empty)))) #false)

;(define (one-true nelob) #false) ;stub

(define (one-true nelob)
  (cond
    [(empty? (rest nelob)) (first nelob)]
    [else
     (or (first nelob)                   
         (one-true (rest nelob)))])) 


