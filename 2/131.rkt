;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |131|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ListOfBoolean is one of:
;; - '()
;; - (cons Boolean ListOfBoolean)
;; interp. a list of Booleans

(define LOB1 '())
(define LOB2 (cons #true LOB1))
(define LOB3 (cons #false LOB2))

(define (fn-for-lob lob)
  (cond
    [(empty?) (...)]
    [else
     (... (first lob)                 ; Boolean
          (fn-for-lob (last lob)))])) ; ListOfBoolean 

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: '()
;; - compound: (cons Boolean ListOfBoolean)
;; - self-reference: (last lob) is ListOfBoolean
