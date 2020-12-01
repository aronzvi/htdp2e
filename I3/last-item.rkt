;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname last-item) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; A [Non-empty-list X] is one of: 
; – (cons X '())
; – (cons X [Non-empty-list X])

#;
(define (fn-for-nelox nelox)
  (cond [(empty? (rest nelox)) (... (fn-for-x (first nelox)))]
        [else
         (... (fn-for-x (first nelox)) ; X
              (fn-for-nelox (rest-nelox)))])) ; [Non-empty-list X]

; [Non-empty-list X] -> X
; retrieves the last item on a non-empty list
(check-expect (last-item (list 1)) 1)
(check-expect (last-item (list 1 2 3)) 3)
(check-expect (last-item '(a b c)) 'c)
(check-error (last-item '()))

;(define (last-item nelox) (first nelox)) ;stub


(define (last-item nelox)
  (cond [(empty? (rest nelox)) (first nelox)]
        [else
         (last-item (rest nelox))])) 