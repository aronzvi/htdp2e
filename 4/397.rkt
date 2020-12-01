;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |397|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct employee (name number pay))
; An Employee is (make-employee String Number Number)
; interp. an employee record with:
;  - name. The name of the employee
; - number. His number
; - pay. The Hourly pay rate

(define E0 (make-employee "john" 123 20))

(define (fn-for-employee e)
  (... (employee-name e)
       (employee-number e)
       (employee-pay e)))

(define-struct time-card (employee hours))
; A TimeCard is (make-time-card Number Number)
; interp. An electronic time card with:
; - employee. The employee number
; - hours. the number of hours worked per week

(define TC0 (make-time-card 123 45))

(define (fn-for-time-card tc)
  (... (time-card-employee tc)
       (time-card-hours tc)))

(define-struct wage (name wage))
; A WageRecord is (make-wage String Number)
; interp. A weekly wage record with:
; - name. The name of the employee
; - wage. The weekly wage

;[List-of Employee] [List-of TimeCard] -> [List-of WageRecord]
; produces a list of wage records from loe and lotc
(check-expect (wages*.v3 empty empty) empty)
(check-error (wages*.v3 empty (list (make-time-card 123 50) (make-time-card 15 50))) "error")
(check-error (wages*.v3 (list (make-employee "John" 123 20) (make-employee "Jill" 4 15)) empty) "error")
(check-expect (wages*.v3 (list (make-employee "John" 123 20) (make-employee "Jill" 4 15))
                         (list (make-time-card 123 50) (make-time-card 4 45)))
              (list (make-wage "John" (* 50 20)) (make-wage "Jill" (* 45 15))))
(check-error (wages*.v3 (list (make-employee "John" 123 20) (make-employee "Jill" 4 15))
                        (list (make-time-card 123 50)))
             "error")
(check-error (wages*.v3 (list (make-employee "John" 123 20))
                        (list (make-time-card 123 50) (make-time-card 15 50)))
             "error")

;(define (wages*.v3 loe lotc) empty) ;stub

; not using cross-product table, not processing symoltanously. not exactly atomic. what is this???
#; 
(define (wages*.v3 loe lotc)
  (local ((define have-all-time-cards? ...)
          (define have-all-employees? ...)
          (define wages* (loe lotc) ...))
    (if (and have-all-time-cards? have-all-employees?)
        (wages* loe lotc)
        (error "error"))))

(define (wages*.v3 loe lotc)
  (local ((define have-all-time-cards?
            (andmap (lambda (e) (ormap (lambda (tc) (= (time-card-employee tc) (employee-number e))) lotc)) loe))
          (define have-all-employees?
            (andmap (lambda (tc) (ormap (lambda (e) (= (time-card-employee tc) (employee-number e))) loe)) lotc))

          ; Employee TimeCard -> Wage
          (define (weekly-wage e tc) (make-wage (employee-name e) (* (employee-pay e) (time-card-hours tc))))
          (define (wages* loe)
            (cond [(empty? loe) empty]
                  [else
                   (local ((define e (first loe))
                           (define employee-tc (first (filter (lambda (tc) (= (time-card-employee tc) (employee-number e))) lotc))))
                     (cons (weekly-wage e
                                        employee-tc)
                           (wages* (rest loe))))])))
    (if (and have-all-time-cards? have-all-employees?)
        (wages* loe)
        (error "error"))))