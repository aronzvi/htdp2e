;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |166|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Data definitions:

(define-struct employee [name number])
;; Employee is (make-struct String Number)
;; interp. an employee's information. name and number

(define EI1 (make-employee "Robby" 123))
(define EI2 (make-employee "Matthew" 124))

(define (fn-for-employee e)
  (... (employee-name e)
       (employee-number e)))

;; Template rules used:
;; - compound: 2 fields

(define-struct work [employee rate hours])
; A (piece of) Work is a structure: 
;   (make-work Employee String Number Number)
; interpretation (make-work e r h) combines the employee-info 
; with the pay rate r and the number of hours h
(define W1 (make-work (make-employee "Robby" 123) 11.95 39))
(define W2 (make-work (make-employee "Matthew" 124) 12.95 45))

(define (fn-for-work w)
  (... (fn-for-employee (work-employee w))
       (work-rate w)
       (work-hours w)))

;; Template rules used:
;; - compound: 3 fields
;; - reference: (work-employee w) is Employee

; Low (short for list of works) is one of: 
; – '()
; – (cons Work Low)
; interpretation an instance of Low represents the 
; hours worked for a number of employees
(define LOW1 '())
(define LOW2 (cons (make-work (make-employee "Robby" 123) 11.95 39) LOW1))
(define LOW3 (cons (make-work (make-employee "Matthew" 124) 12.95 45) LOW2))
(define LOW4 (cons (make-work (make-employee "John" 125) 13.95 43) LOW3))
(define LOW5 (cons (make-work (make-employee "Keanu" 126) 15 45) LOW4))

(define (fun-for-low low)
  (cond
    [(empty? low) (...)]
    [else
     (... (fn-for-work (first low))     ;Work
          (fn-for-low (rest low)))]))   ;Low

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Work Low)
;; - reference: (first low) is Work
;; - self-reference: (rest low) is Low

(define-struct paycheck [employee amount])
;; Paycheck is (make-paycheck Employee Number)
;; interp. an employee's paycheck with
;; the employee's info and amount to be payed

(define P1 (make-paycheck (make-employee "Robby" 123) 500))
(define P2 (make-paycheck (make-employee "Matthew" 124) 450))

(define (fn-for-paycheck p)
  (... (fn-for-employee (paycheck-employee p))
       (paycheck-amount p)))

;; Template rules used:
;; - compound: 2 fields
;; - reference: (paycheck-employee p) is Employee

;; ListOfPaycheck is one of:
;; - empty
;; - (cons Paycheck ListOfPaycheck)
;; interp. a list of paychecks

(define LOP1 empty)
(define LOP2 (cons (make-paycheck "Robby" 500) LOP1))
(define LOP3 (cons (make-paycheck "Matthew" 450) LOP2))

(define (fn-for-lop lop)
  (cond
    [(empty? lop) (...)]
    [else
     (... (fn-for-paycheck (first lop)) ;Paycheck
          (fn-for-lop (rest lop)))]))   ;ListOfPaycheck

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Paycheck ListOfPaycheck)
;; - reference: (first lop) is Paycheck
;; - self-reference: (rest lop) is ListOfPaycheck

;; Functions:

;; Low -> LOP
;; computes a list of paychecks from a list of work records
;(check-expect (wage*.v3 empty) empty)
;(check-expect (wage*.v3 (cons (make-work "Robby" 11.95 39) empty)) (cons (make-paycheck "Robby" (* 11.95 39)) empty))
;(check-expect (wage*.v3 (cons (make-work "Matthew" 12.95 45) (cons (make-work "Robby" 11.95 39) empty))) (cons (make-paycheck "Matthew" (* 12.95 45)) (cons (make-paycheck "Robby" (* 11.95 39)) empty)))

;(define (wage*.v3 low) empty) ;stub

(define (wage*.v3 low)
  (cond
    [(empty? low) empty]
    [else
     (cons (work-to-paycheck.v1 (first low))    
           (wage*.v3 (rest low)))]))

;; Work -> Paycheck
;; produces a paycheck from a work record
;(check-expect (work-to-paycheck.v1 (make-work "Matthew" 12.95 45)) (make-paycheck "Matthew" (* 12.95 45)))
;(check-expect (work-to-paycheck.v1 (make-work "Robby" 11.95 39))(make-paycheck "Robby" (* 11.95 39)))

;(define (work-to-paycheck.v1 w) (make-paycheck "" 0))


(define (work-to-paycheck.v1 w)
  (make-paycheck (work-employee w)
                 (* (work-rate w) (work-hours w))))

; Low -> List-of-numbers
; computes the weekly wages for the given records
;(check-expect (wage*.v2 empty) empty)
;(check-expect (wage*.v2 (cons (make-work "Robby" 11.95 39) empty)) (cons (* 11.95 39) empty))
;(check-expect (wage*.v2 (cons (make-work "Matthew" 12.95 45) (cons (make-work "Robby" 11.95 39) empty))) (cons (* 12.95 45)(cons (* 11.95 39) empty)))

;(define (wage*.v2 an-low) '()) ;stub

(define (wage*.v2 low)
  (cond
    [(empty? low) empty]
    [else
     (cons (wage.v2 (first low))  
           (wage*.v2 (rest low)))]))

;; Work -> Number
;; produces the weekly wage of the given worker
;(check-expect (wage.v2 (make-work "Robby" 11.95 39)) (* 11.95 39))

;(define (wage w) 0) ;stub

(define (wage.v2 w)
  (* (work-rate w) (work-hours w)))

;; Low -> LOP
;; computes a list of paychecks from a list of work records
(check-expect (wage*.v4 empty) empty)
(check-expect (wage*.v4 (cons (make-work (make-employee "Robby" 123) 11.95 39) empty)) (cons (make-paycheck (make-employee "Robby" 123) (* 11.95 39)) empty))
(check-expect (wage*.v4 (cons (make-work (make-employee "Matthew" 124) 12.95 45) (cons (make-work (make-employee "Robby" 123) 11.95 39) empty))) (cons (make-paycheck (make-employee "Matthew" 124) (* 12.95 45)) (cons (make-paycheck (make-employee "Robby" 123) (* 11.95 39)) empty)))

;(define (wage*.v4 low) empty) ;stub

(define (wage*.v4 low)
  (cond
    [(empty? low) empty]
    [else
     (cons (work-to-paycheck (first low))     
           (wage*.v4 (rest low)))]))

;; Work -> Paycheck
;; produces a paycheck from a work record
(check-expect (work-to-paycheck (make-work (make-employee "Robby" 123) 11.95 39)) (make-paycheck (make-employee "Robby" 123) (* 11.95 39)))
(check-expect (work-to-paycheck (make-work (make-employee "Matthew" 124) 12.95 45)) (make-paycheck (make-employee "Matthew" 124) (* 12.95 45)))

;(define (work-to-paycheck w) (make-paycheck (make-employee "" 0) 0)) ;stub

(define (work-to-paycheck w)
  (make-paycheck (work-employee w)
                 (* (work-rate w) (work-hours w))))

