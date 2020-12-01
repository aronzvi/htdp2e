;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |162|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Constants:
(define WAGE 14)
(define MAX-HOURS 100)

;; Data definitions:

;; List-of-numbers is one of:
;; - empty
;; (cons Number List-of-numbers)
;; interp. a list of numbers

(define LON1 empty)
(define LON2 (cons 2 LON1))
(define LON3 (cons 5 LON2))

#;
(define (fn-for-lon lon)
  (cond
    [(empty? lon) ...]
    [else
     (... (first lon)                 ;Number
          (fn-for-lon (rest lon)))])) ;List-of-numbers

;; Functions:

; List-of-numbers -> List-of-numbers
; computes the weekly wages for all given weekly hours
(check-expect (wage* empty) empty)
(check-expect (wage* (cons 28 empty)) (cons (* 28 WAGE) empty))
(check-expect (wage* (cons 4 (cons 2 empty))) (cons (* 4 WAGE) (cons (* 2 WAGE) empty)))
(check-error (wage* (cons 4 (cons (+ MAX-HOURS 1) (cons 2 empty)))))
    
(define (wage* whrs)
  (cond
    [(empty? whrs) empty]
    [else
     (if (> (first whrs) MAX-HOURS)
         (error "Max hours exceeded")
         (cons (wage (first whrs)) (wage* (rest whrs))))]))
 
; Number -> Number
; computes the wage for h hours of work
(define (wage h)
  (* WAGE h))

