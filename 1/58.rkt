;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |58|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define BOTTOM-LOW-PRICE 0)
(define BOTTOM-MID-PRICE 1000)
(define BOTTOM-LUXURY-PRICE 10000)
(define LOW-TAX 0)
(define MID-TAX 0.05)
(define LUXURY-TAX 0.08)

; A Price falls into one of three intervals:
; - 0 through 999     inclusive, 0%
; - 1000 through 9999  inclusive, %5
; - 10000 and above    inclusive, %8
; interpretation: the price of an item

; Price -> Number
; computes the amount of tax for p
(check-expect (sales-tax BOTTOM-LOW-PRICE) 0)
(check-expect (sales-tax 537) (* LOW-TAX 537))
(check-expect (sales-tax 999) (* LOW-TAX 999))
(check-expect (sales-tax BOTTOM-MID-PRICE) (* MID-TAX BOTTOM-MID-PRICE))
(check-expect (sales-tax 5000) (* MID-TAX 5000))
(check-expect (sales-tax 9999) (* MID-TAX 9999))
(check-expect (sales-tax BOTTOM-LUXURY-PRICE) (* LUXURY-TAX BOTTOM-LUXURY-PRICE))
(check-expect (sales-tax 12017) (* LUXURY-TAX 12017))
(define (sales-tax p)
  (cond
    [(and (<= 0 p) (< p BOTTOM-MID-PRICE)) (* LOW-TAX p)]
    [(and (<= BOTTOM-MID-PRICE p) (< p BOTTOM-LUXURY-PRICE)) (* MID-TAX p)]
    [(>= p BOTTOM-LUXURY-PRICE) (* LUXURY-TAX p)]))
