;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |163|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
    [(empty? lon) ...]
    [else
     (... (first lon)                 ;Number
          (fn-for-lon (rest lon)))])) ;List-of-numbers

;; List-of-numbers -> List-of-numbers
;; converts a list of measurements in Fahrenheit to a list of Celsius measurements
(check-expect (convertFC empty) empty)
(check-expect (convertFC (cons 58 empty)) (cons (* (- 58 32) 5/9) empty))
(check-expect (convertFC (cons 100 (cons 58 empty))) (cons (* (- 100 32) 5/9) (cons (* (- 58 32) 5/9) empty)))

;(define (convertFC lof) lof) ; stub

(define (convertFC lof)
  (cond
    [(empty? lof) empty]
    [else
     (cons (f->c (first lof))               
           (convertFC (rest lof)))]))

;; Number -> Number
;; converts a Fahrenheit measurement to a Celsius measurement
(check-expect (f->c 100) (* (- 100 32) 5/9))

;(define (f->c f) f) ;stub

#;
(define (f->c f)
  (... f))

(define (f->c f)
  (* (- f 32) 5/9))

;; template rules used:
;; - atomic non-distinct: Number