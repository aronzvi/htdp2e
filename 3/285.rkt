;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |285|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define EXCHANGE-RATE 1.06)

;; [List-of Number] -> [Listof Number]
;; converts list of US$ amounts to list of Euro amounts based on EXCHANGE-RATE
(check-expect (convert-euro empty) empty)
(check-expect (convert-euro (list 1 2 1.5)) (list (* 1 EXCHANGE-RATE) (* 2 EXCHANGE-RATE) (* 1.5 EXCHANGE-RATE)))

;(define (convert-euro lon) lon) ;stub

;; Template with lambda???

(define (convert-euro lon)
  (map (lambda (n)
         (* n EXCHANGE-RATE))
       lon))

;; [List-of Number] -> [List-of Number]
;; converts a list of Fahrenheit measurements to a list of Celsius measurements
(check-expect (convertFC empty) empty)
(check-expect (convertFC (list 32 20 50)) (list (* (- 32 32) (/ 5 9)) (* (- 20 32) (/ 5 9)) (* (- 50 32) (/ 5 9))))

;(define (convertFC lon) lon) ;stub

(define (convertFC lon)
  (map
   (lambda (n)
     (* (- n 32) (/ 5 9)))
   lon))

;; (listof Posn) -> (listof (listof Number))
;; translates list of Posns into a list of lists of pairs of numbers
(check-expect (translate empty) empty)
(check-expect (translate (list (make-posn 1 2) (make-posn 3 4)))
              (list (list 1 2) (list 3 4)))

;(define (translate lop) empty) ;stub

#;
(define (translate lop)
       ;(Posn -> (listof Number))
  (map ...                         lop))

(define (translate lop)
  (map (lambda (p)
         (list (posn-x p) (posn-y p)))
       lop))



