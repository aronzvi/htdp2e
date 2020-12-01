;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |146|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An NEList-of-temperatures is one of: 
; – (cons CTemperature '())
; – (cons CTemperature NEList-of-temperatures)
; interpretation non-empty lists of Celsius temperatures

; A CTemperature is a Number greater than -272.

(define NELOT1 (cons 100 '()))
(define NELOT2 (cons -1 NELOT1))
(define NELOT3 (cons -100 NELOT2))

(define (fn-for-nelot nelot)
  (cond
    [(empty? (rest nelot)) (... (first nelot))]
    [else
     (... (first nelot)                   ; CTemperature
          (fn-for-nelot (rest nelot)))])) ; NEList-of-temperatures

; NEList-of-temperatures -> Number
; computes the average temperature 
(check-expect (average (cons 1 (cons 2 (cons 3 '()))))
              2)
 
(define (average ne-l)
  (/ (sum ne-l)
     (how-many ne-l)))

; NEList-of-temperatures -> Number
; computes the sum of the given temperatures 
(check-expect
  (sum (cons 1 (cons 2 (cons 3 '())))) 6)

;(define (sum ne-l) 0) ;stub

#;
(define (sum ne-l)
  (cond
    [(empty? (rest ne-l)) (first ne-l)]
    [else
     (... (first ne-l) ... (sum (rest ne-l)) ...)]))


(define (sum ne-l)
  (cond
    [(empty? (rest ne-l)) (first ne-l)]
    [else (+ (first ne-l) (sum (rest ne-l)))]))

;; NEList-of-temperatures -> Number
;; determines how many strings are on nelot
(check-expect (how-many (cons 2 empty)) 1)
(check-expect (how-many (cons 3 (cons 2 empty))) 2)
(check-expect (how-many (cons 6 (cons 3 (cons 2 empty)))) 3)

;(define (how-many nelot) 1) ;stub

(define (how-many nelot)
  (cond
    [(empty? (rest nelot)) 1]
    [else
     (+ 1 (how-many (rest nelot)))]))
