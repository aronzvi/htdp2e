;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |459|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;(define ε 0.1)
;(define R 100)

(define ε 0.01)
(define R 159)

; N is one of:
; - 0
; - (add1 N)

#;
(define (f-for-n n)
  (cond [(zero? n) (...)]
        [else
         (...
          n
         (f-for-n (sub1 n)))]))

; [Number -> Number] Number Number -> Number
; produces an estimate of the area under f between a and b by dividing the area into R rectangles
(check-within (integrate-rectangles (lambda (x) 20) 12 22) 200 ε)
(check-within (integrate-rectangles (lambda (x) (* 2 x)) 0 10) 100 ε)
(check-within (integrate-rectangles (lambda (x) (* 3 (sqr x))) 0 10) 
              1000
              ε)

;(define (integrate-rectangles f a b) #i0.0) ;stub

(define (integrate-rectangles f a b)
  (local ((define W (/ (- b a) R))
          (define S (/ W 2))
          (define (rectangle-ith-area n)
            (* W (f (+ a (* (- n 1) W) S))))
          ; N -> Number
          (define (rectangles-sum-area n)
            (cond [(zero? n) 0]
                  [else
                   (+
                    (rectangle-ith-area n)
                    (rectangles-sum-area (sub1 n)))])))
   (rectangles-sum-area R)))