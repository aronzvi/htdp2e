;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |164|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Constants:

(define USD-EURO-EXCHANGE-RATE 0.89)

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
    [(empty? lon) (...)]
    [else
     (... (first lon)                 ;Number
          (fn-for-lon (rest lon)))])) ;List-of-numbers

;; Functions:
;; List-of-numbers -> List-of-numbers
;; converts a list of US$ amounts into a list of € amounts
(check-expect (convert-euro empty) empty)
(check-expect (convert-euro (cons 1 empty)) (cons (* 1 USD-EURO-EXCHANGE-RATE) empty))
(check-expect (convert-euro (cons 2 (cons 1 empty))) (cons (* 2 USD-EURO-EXCHANGE-RATE)(cons (* 1 USD-EURO-EXCHANGE-RATE) empty)))

;(define (convert-euro lon) lon) ;stub

(define (convert-euro lod)
  (cond
    [(empty? lod) empty]
    [else
     (cons (usd->euro (first lod))              
           (convert-euro (rest lod)))]))

;; Number -> Number
;; converts USD amount to Euros
(check-expect (usd->euro 1) (* 1 USD-EURO-EXCHANGE-RATE))
(check-expect (usd->euro 100) (* 100 USD-EURO-EXCHANGE-RATE))

;(define (usd->euro n) n) ;stub

#;
(define (usd->euro usd)
  (... usd))

;; Template rules used:
;; - atomic non-distinct

(define (usd->euro usd)
  (* usd USD-EURO-EXCHANGE-RATE))

;; Functions:
;; Number List-of-numbers -> List-of-numbers
;; converts a list of US$ amounts into a list of € amounts
(check-expect (convert-euro* 0.75 empty) empty)
(check-expect (convert-euro*  0.50 (cons 1 empty)) (cons (* 1 0.50) empty))
(check-expect (convert-euro* 0.89 (cons 2 (cons 1 empty))) (cons (* 2 0.89)(cons (* 1 0.89) empty)))

;(define (convert-euro* rate lon) lon) ;stub

(define (convert-euro* rate lod)
  (cond
    [(empty? lod) empty]
    [else
     (cons (usd->euro* (first lod) rate)              
           (convert-euro* rate (rest lod)))]))

;; Number Number -> Number
;; converts USD amount to Euros using rate
(check-expect (usd->euro* 1 0.50) (* 1 0.50))
(check-expect (usd->euro* 100 0.89) (* 100 0.89))

;(define (usd->euro* rate amount) amount) ;stub

#;
(define (usd->euro rate amount)
  (... amount))

;; Template rules used:
;; - atomic non-distinct

(define (usd->euro* rate amount)
  (* amount rate))