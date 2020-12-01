;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |267|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define EXCHANGE-RATE 1.06)

;; [ListOf Number] -> [ListOf Number]
;; converts a list of US$ amounts into a list of € amounts based on EXCHANGE-RATE
(check-expect (convert-euro (list 1 2 3))
              (list (* 1 EXCHANGE-RATE) (* 2 EXCHANGE-RATE) (* 3 EXCHANGE-RATE)))

;(define (convert-euro lon) lon) ;stub

;(define (convert-euro lon) ;template
;     (Number -> Number)
;  (map   ...              lon))

(define (convert-euro lon)
  (map dollar->euro lon))

;; Number -> Number
;; converts a US$ amount to Euro amount based on EXCHANGE-RATE
(check-expect (dollar->euro 1) (* 1 EXCHANGE-RATE))
(check-expect (dollar->euro 3) (* 3 EXCHANGE-RATE))

;(define (dollar->euro n) n) ;stub

(define (dollar->euro n)
  (* n EXCHANGE-RATE))

;; [ListOf Number] -> [ListOf Number]
;; converts a list of Fahrenheit measurements to a list of Celsius measurements
(check-expect (convertFC (list 2 3 5)) (list (* (- 2 32) 5/9) (* (- 3 32) 5/9) (* (- 5 32) 5/9)))

;(define (convertFC lon) lon) ;stub

#;
(define (convertFC lon) ;template
  (map ... lon))

(define (convertFC lon)
  (map f->c lon))

;; Number -> Number
;; Fahrenheit measurement to Celsius
(check-expect (f->c 2) (* (- 2 32) 5/9))
(check-expect (f->c 5) (* (- 5 32) 5/9))

;(define (f->c n) n) ;stub

(define (f->c n)
  (* (- n 32) 5/9))

;; [ListOf Posn] -> [ListOf [List Number Number]] 
;; translates a list of Posns into a list of lists of pairs of numbers
(check-expect (translate (list (make-posn 1 2) (make-posn 8 5)))
              (list (list 1 2) (list 8 5)))

;(define (translate lop) empty) ;stub

(define (translate lop)
  (map posn->pair lop))

;; Posn -> [List Number Number]
;; translates a Posn to list of pair
(check-expect (posn->pair (make-posn 1 2)) (list 1 2))
(check-expect (posn->pair (make-posn 8 5)) (list 8 5))

;(define (posn->pair p) empty) ;stub

(define (posn->pair p)
  (list (posn-x p)
       (posn-y p)))