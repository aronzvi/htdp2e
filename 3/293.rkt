;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |293|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; X [List-of X] -> [Maybe [List-of X]]
; returns the first sublist of l that starts
; with x, #false otherwise
(check-expect (find 1 empty) false)
(check-expect (find 1 (list 1 2 3)) (list 1 2 3))
(check-expect (find 1 (list 1 2 3 1 2)) (list 1 2 3 1 2))
(check-expect (find 2 (list 1 2 3)) (list 2 3))

(define (find x l)
  (cond
    [(empty? l) #false]
    [else
     (if (equal? (first l) x)
         l
         (find x (rest l)))]))

; X [List-of X] -> [[List-of X] -> Boolean]
; is l0 the first sublist of k that starts with x or false if no sublist that starts with x
; - l0 not empty
; - l0 starts with x
; - l0 is sublist of k
; - l0 is first sublist of k that sarts with x - how to test this - this is exactly what find finds??
(check-expect ((found? 1 (list 2 1 3 4)) (list 1 3 4))
              true)
(check-expect ((found? 1 (list 2 1 3 4)) (list 1 3))
              false)

(check-expect ((found? 1 (list 2 1 3 4 1 5)) (list 1 3 4 1 5))
              true)
(check-expect ((found? 1 (list 2 1 3 4 1 5)) (list 1 5))
              false)

#;
(check-expect ((found? 1 (list 2 1 3 4)) false) ; expecting (list 1 3 4) here
              false)

#;
(check-expect ((found? 5 (list 2 1 3 4)) false) ; no sublist here so expecting false 
              true)
#;
(check-expect ((found? 5 empty) false)
              true)

;(define (found? x k) (lambda (l0) false)) ;stub

(define (found? x k)
  (lambda (l0)
    (and (not (empty? l0))
         (equal? (first l0) x)))) 
