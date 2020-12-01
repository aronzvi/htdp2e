;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |388|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Data definitions

(define-struct employee [name ssn pay])
; An Employee is a structure (make-employee String Natural Number)
; interp. An employee record with:
; - name. The name
; - ssn. The social security number
; - pay. The hourly pay rate

(define E0 (make-employee "John" 12345116 16))
(define E1 (make-employee "Jake" 12342226 10.5))

(define (fn-for-employee e)
  (... (employee-name e)
       (employee-ssn e)
       (employee-pay e)))

(define-struct wr [name hours])
; A WR (WorkRecord) is a structure (make-wr String Number)
; interp. an employees wekly work record with:
; - name. The employee's name
; - hours. The number of hours worked in a week

(define WR0 (make-wr "John" 50))
(define WR1 (make-wr "Jack" 45))

(define (fn-for-wr r)
  (... (wr-name r)
       (wr-hours r)))

(define-struct ww [name wage])
; A WW (WeeklyWage) is a structure (make-ww String Number)
; interp. The weekly wage of an employee with:
; - name. The name of the employee
; - wage. The employees wekly wage

(define WW0 (make-ww "John" 500))
(define WW1 (make-ww "Jill" 1000))

(define (fn-for-ww w)
  (... (ww-name w)
       (ww-wage w)))


; [List-of Employee] [List-of WR] -> [List-of WW]
; produces list of employees weekly wages from loe and lowr
; assumes the two lists are of equal length 
(check-expect (wages*.v3 empty empty) empty)
(check-expect (wages*.v3 (list (make-employee "John" 123456 15)) (list (make-wr "John" 50)))
              (list (make-ww "John" (* 15 50))))
(check-expect (wages*.v3 (list (make-employee "John" 123456 15) (make-employee "Jack" 654321 20)) (list (make-wr "John" 50) (make-wr "Jack" 45)))
              (list (make-ww "John" (* 15 50)) (make-ww "Jack" (* 20 45))))
(check-expect (wages*.v3 (list (make-employee "John" 123456 15) (make-employee "Jack" 876543 20) (make-employee "Jill" 3333345 13)) (list (make-wr "John" 50) (make-wr "Jack" 45) (make-wr "Jill" 60)))
              (list (make-ww "John" (* 15 50)) (make-ww "Jack" (* 20 45)) (make-ww "Jill" (* 13 60))))
(check-expect (wages*.v3 (list (make-employee "John" 123456 15) (make-employee "Jack" 876543 20) (make-employee "Jill" 3333345 13)) (list (make-wr "Jill" 60) (make-wr "John" 50) (make-wr "Jack" 45)))
              (list (make-ww "John" (* 15 50)) (make-ww "Jack" (* 20 45)) (make-ww "Jill" (* 13 60))))


;(define (wages*.v3 loe lowr) empty) ;stub

#;
(define (wages*.v3 loe lowr)
  (cond [(empty? loe) (...)]
        [else
         (... (first loe)                             ;Employee 
              (first lowr)                            ;WR
              (wages*.v3 (rest loe) (rest lowr)))]))  ;[List-of Employee] ;[List-of WR]

;Fails if [List-of Employee] and [List-of WR] are not both ordered by employee 
(define (wages*.v3 loe lowr)  
  (cond [(empty? loe) empty]
        [else
         (cons (weekly-wage (first loe)               
                            (first lowr))              
               (wages*.v3 (rest loe) (rest lowr)))]))

; The produced [List-of WW] will be in differernt order than given [List-of Employee] so test needs to be modified or will fail over order difference
#;
(define (wages*.v3 loe lowr) 
  (cond [(empty? loe) empty]
        [else
         (local ((define loe-sorted-by-name (sort loe (lambda (e1 e2) (string<? (employee-name e1) (employee-name e2)))))
                 (define lowr-sorted-by-name (sort lowr (lambda (wr1 wr2) (string<? (wr-name wr1) (wr-name wr2)))))
                 (define (wages* loe-sorted lowr-sorted)
                   (cond [(empty? loe-sorted) empty]
                         [else
                          (cons (weekly-wage (first loe-sorted) (first lowr-sorted))
                                (wages* (rest loe-sorted) (rest lowr-sorted)))])))
           (wages* loe-sorted-by-name lowr-sorted-by-name))]))
           
; Employee WR -> WW
; computes the weekly wage from pay-rate and hours
(define (weekly-wage e wr)
  (make-ww (employee-name e)
           (* (employee-pay e)
              (wr-hours wr))))

