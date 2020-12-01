;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |288|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;### 1 ###

;; Natural -> [List-of Natural]
;; creates the list (list 0 ... (- n 1)) for any natural number n
(check-expect (list-0-to-n-1 0) empty)
(check-expect (list-0-to-n-1 3) (list 0 1 2))

;(define (list-0-to-n-1 n) empty) ;stub

#;
(define (list-0-to-n-1 n)
  ;(Natural -> Natural)
  (build-list n ...))

(define (list-0-to-n-1 n)
  (build-list n (lambda (n-1) n-1)))

;### 2 ###

;; Natural -> [List-of Natural]
;; creates the list (list 1 ... n) for any natural number n
(check-expect (list-1-to-n 0) empty)
(check-expect (list-1-to-n 1) (list 1))
(check-expect (list-1-to-n 3) (list 1 2 3))

;(define (list-1-to-n n) empty) ;stub

#;
(define (list-1-to-n n)
  ;(Natural -> Natural)
  (build-list n ...))

(define (list-1-to-n n)
  (build-list n (lambda (n-1)
                  (+ n-1 1))))

;### 3 ###

;; Natural -> [List-of Number]
;; creates the list (list 1 1/2 ... 1/n) for any natural number n
(check-expect (list-1-to-frac-n 0) empty)
(check-expect (list-1-to-frac-n 1) (list (/ 1 1)))
(check-expect (list-1-to-frac-n 2) (list (/ 1 1) (/ 1 2)))
(check-expect (list-1-to-frac-n 3) (list (/ 1 1) (/ 1 2) (/ 1 3)))

;(define (list-1-to-frac-n n) empty) ;stub

#;
(define (list-1-to-frac-n n)
  ;(Natural -> Number)
  (build-list n              ...))

(define (list-1-to-frac-n n)
  (build-list n  (lambda (n-1)
                   (/ 1 (+ n-1 1)))))

;### 4 ###

;; Natural -> [List-of Natural]
;; creates the list of the first n even numbers
(check-expect (list-even-n 0) empty)
(check-expect (list-even-n 1) (list 2))
(check-expect (list-even-n 3) (list 2 4 6))

;(define (list-even-n n) empty) ;stub

#;
(define (list-even-n n)
  ;(Natural -> Natural)
  (build-list n              ...))

(define (list-even-n n)
  (build-list n (lambda (n-1)
                  (* 2 (+ n-1 1)))))

;### 5 ###

; Natural -> [List-of [List-of Natural]]
; Creates diagonal squares of 0s and 1s of size n
(check-expect (identityM 1)
              (list (list 1)))
(check-expect (identityM 2)
              (list (list 1 0) (list 0 1)))
(check-expect (identityM 3)
              (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))
(check-expect (identityM 4)
              (list (list 1 0 0 0) (list 0 1 0 0) (list 0 0 1 0) (list 0 0 0 1)))

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
                    (define (1-or-0 n) (if (= n nx)
                                           1
                                           0)))
              (build-list n 1-or-0))))
    (build-list n zeros-and-1-at-index)))

(define (identityM n)
  (build-list n (lambda (nx1)
                  (build-list n (lambda (nx2)
                                  (if (= nx2 nx1)
                                      1
                                      0))))))

; ### 6:

; Number [Number -> X] -> [List-of X]
; creates the list (list (f n), (f n-1) ... (f 0) for given n and f
(check-expect (tabulate 0 sqr) (list (sqr 0)))
(check-expect (tabulate 2 sqr) (list (sqr 2) (sqr 1) (sqr 0)))
(check-within (tabulate 0 tan) (list (tan 0)) 0.1)
(check-within (tabulate 2 tan) (list (tan 2) (tan 1) (tan 0)) 0.1)

;(define (tabulate n f) empty) ;stub

#;
(define (tabulate n f)
  (cons (f n) (reverse (build-list n f))))

; How\why to use lambda here???

