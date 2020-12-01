;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |398|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of Number] [List-of Number] -> Number
; produces the value of the combination of the linear combination and variable values
(check-expect (value empty empty) 0)
(check-expect (value (list 5) (list 10)) (+ (* 5 10) 0))
(check-expect (value (list 5 17) (list 10 1)) (+ (* 5 10) (+ (* 17 1) 0)))
(check-expect (value (list 5 17 3) (list 10 1 2)) (+ (* 5 10) (+ (* 17 1) (+ (* 3 2) 0))))

;(define (value linear-comb vars) 0) ;stub

; both have to be of same length. Will process together

(define (value linear-comb vars)
  (cond [(empty? linear-comb) 0]
        [else
         (+
          (* (first linear-comb)
             (first vars))
          (value (rest linear-comb) (rest vars)))]))

