;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |305|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

(define EXCHANGE-RATE 1.06)

; [List-of Number] -> [List-of Number]
; converts a list of US$ amounts into a list of € amounts based on EXCHANGE-RATE
(check-expect (convert-euro (list 1 2 3))
              (list (* 1 EXCHANGE-RATE) (* 2 EXCHANGE-RATE) (* 3 EXCHANGE-RATE)))

;(define (convert-euro lon) lon) ;stub

(define (convert-euro lon)
  (for/list ([amount lon])
    (* amount EXCHANGE-RATE)))