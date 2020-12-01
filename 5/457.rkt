;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |457|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Natural -> Natural
; computes how many months it takes to double the given amount of money when account pays rate percent interest on a monthly basis
; starting from the minimum amount of months to double possible - 1, we repeatedly add one more month until the given amount of months doubles the amount
; termination: we are done once (>= (expt (+ 1 (/ rate 100)) months) 2) is satisfied.
; since (+ 1 (/ rate 100)) > 0 and months is repeatedly incremented by 1, we are guaranteed that (expt (+ 1 (/ rate 100)) months) will eventually be >= 2
(check-expect (double-amount 100 100) 1)
(check-expect (double-amount 100 50) 2)
(check-within (double-amount 100 6) (ceiling (/ (log 2) (log (+ 1 (/ 6 100))))) 
              0.01)

;(define (double-amount rate) 0) ;stub

(define (double-amount amount rate)
  (local (; Natural -> Natural
          ; computes how many months it takes to double any amount of money at given monthly rate
          (define (double-amount months)
          (cond
            [(>= (expt (+ 1 (/ rate 100)) months) 2) months]
            [else
             (double-amount (+ months 1))])))
    (double-amount 1)))


