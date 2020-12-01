;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |309|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; [List-of [List-of String]] -> [List-of Natural]
; produces list of number of Strings per item in a llos
(check-expect (words-on-line empty) empty)
(check-expect (words-on-line (list empty))
              (list (length empty)))
(check-expect (words-on-line (list (list "22" "rr" "ff") (list "22" "rr")))
              (list (length (list "22" "rr" "ff")) (length (list "22" "rr"))))

;(define (words-on-line llos) empty) ;stub

(define (words-on-line llos)
  (match llos
    ['() empty]
    [(cons fst rst) (cons (length fst) (words-on-line rst))]))
