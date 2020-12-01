;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |270|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; #### 1:

; Natural -> [List-of Natural]
; creates the list (list 0 ... (- n 1)) for any natural number n
(check-expect (zero-to-n-non-inc 4) (list 0 1 2 3))
(check-expect (zero-to-n-non-inc 0) empty)

;(define (zero-to-n-non-inc n) empty) ;stub

(define (zero-to-n-non-inc n)
  (build-list n identity))

; ### 2:

; Natural -> [List-of Natural]
; creates the list (list 1 ... n) for any natural number n
(check-expect (one-to-n-inc 4) (list 1 2 3 4))

;(define (one-to-n-inc n) empty) ;stub

(define (one-to-n-inc n)
  (build-list n add1))


; ### 3:

; Natural -> [List of Number]
; creates the list (list 1 1/2 ... 1/n) for any natural number n
(check-expect (n-fractions 4) (list 1/1 1/2 1/3 1/4))

;(define (n-fractions n) empty) ;stub

(define (n-fractions n)
  (local (; Natural -> Number
          ; produces 1/n+1 for given n
          (define (fraction-n-plus1 n) (/ 1 (add1 n))))
    (build-list n fraction-n-plus1)))


; ### 4:

; Natural -> [List-of Natural]
; creates the list of the first n even numbers
(check-expect (first-n-even 0) empty)
(check-expect (first-n-even 1) (list 0))
(check-expect (first-n-even 4) (list 0 2 4 6))

;(define (first-n-even n) empty) ;stub

(define (first-n-even n)
  (local (; Number -> Number
          ; produces 2n
          (define (2n n) (* n 2)))
    (build-list n 2n)))


; Natural -> [List-of [List-of Natural]]
; Creates diagonal squares of 0s and 1s of size n
(check-expect (identityM 1)
              (list (list 1)))
(check-expect (identityM 2)
              (list (list 1 0) (list 0 1)))
(check-expect (identityM 3)
              (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))

;(define (identityM n) empty) ;stub

#;
(define (identityM n)
                ;(Natural -> [List-of Natural]) 
  (build-list n ...))

#;
(define (identityM n)
  (local (;; Natural -> [List-of Natural]
          ;; creates list of n length with 1 at pos nx and rest 0s
          (define (zeros-and-1-at-index nx)
            (local (;; Natural -> Natural
                    ;; if n is equal to nx returns 1, else 0
                    (define (1-or-0 n) 0))
            (build-list n 1-or-0))))
(build-list n zeros-and-1-at-index)))


(define (identityM n)
  (local (;; Natural -> [List-of Natural]
          ;; creates list of n length with 1 at pos nx and rest 0s
          (define (zeros-and-1-at-index nx)
            (local (;; Natural -> Natural
                    ;; if n is equal to nx returns 1, else 0
                    (define (1-or-0 n) (if (= n nx)
                                           1
                                           0)))
            (build-list n 1-or-0))))
(build-list n zeros-and-1-at-index)))

; ### 6:

; Number [Number -> X] -> [List-of X]
; creates the list (list (f n), (f n-1) ... (f 0) for given n and f
(check-expect (tabulate 0 sqr) (list (sqr 0)))
(check-expect (tabulate 2 sqr) (list (sqr 2) (sqr 1) (sqr 0)))
(check-within (tabulate 0 tan) (list (tan 0)) 0.1)
(check-within (tabulate 2 tan) (list (tan 2) (tan 1) (tan 0)) 0.1)

;(define (tabulate n f) empty) ;stub

(define (tabulate n f)
  (cons (f n) (reverse (build-list n f))))