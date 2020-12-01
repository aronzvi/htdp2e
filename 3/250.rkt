;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |250|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Number -> [List-of Number]
; tabulates sin between n 
; and 0 (incl.) in a list
(define (tab-sin n)
  (cond
    [(= n 0) (list (sin 0))]
    [else
     (cons
      (sin n)
      (tab-sin (sub1 n)))]))
  
; Number -> [List-of Number]
; tabulates sqrt between n 
; and 0 (incl.) in a list
(define (tab-sqrt n)
  (cond
    [(= n 0) (list (sqrt 0))]
    [else
     (cons
      (sqrt n)
      (tab-sqrt (sub1 n)))]))

; Number -> [List-of Number]
; tabulates sqr between n 
; and 0 (incl.) in a list
(check-expect (tab-sqr 0) (list (sqr 0)))
(check-expect (tab-sqr 2) (list (sqr 2) (sqr 1) (sqr 0)))

;(define (tab-sqr n) empty) ;stub

(define (tab-sqr n)
  (tabulate n sqr))

; Number -> [List-of Number]
; tabulates tan between n 
; and 0 (incl.) in a list
(check-within (tab-tan 0) (list (tan 0)) 0.1)
(check-within (tab-tan 2) (list (tan 2) (tan 1) (tan 0)) 0.1)

;(define (tab-tan n) empty) ;stub

(define (tab-tan n)
  (tabulate n tan))

; Number [Number -> X] -> [List-of X]
; tabulates f between n and 0 (incl.) in a list
(check-expect (tabulate 0 sqr) (list (sqr 0)))
(check-expect (tabulate 2 sqr) (list (sqr 2) (sqr 1) (sqr 0)))
(check-within (tabulate 2 tan) (list (tan 2) (tan 1) (tan 0)) 0.1)

(define (tabulate n f)
  (cond
    [(= n 0) (list (f 0))]
    [else
     (cons
      (f n)
      (tabulate (sub1 n) f))]))