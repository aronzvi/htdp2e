;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |262|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; A Nat is one of:
;; - 0
;; - (add1 Nat)

(define NAT0 0)
(define NAT1 (add1 0))
(define NAT2 (add1 (add1 0)))
(define NAT3 (add1 (add1 (add1 0))))

#;
(define (fn-for-nat n)
  (cond ((zero? n) (...))
        (... n
             (fn-for-nat (sub1 n))))) ;Nat

;; Template rules used:
;; - one of: 2 cases
;; - atomic-distinct: 0
;; - compound: 2 fields
;; - self-reference: (sub1 n) is Nat  

; Nat -> [List-of [List-of Nat]]
; Creates diagonal squares of 0s and 1s of size n
(check-expect (identityM 0)
              empty)
(check-expect (identityM 1)
              (list (list 1)))
(check-expect (identityM 2)
              (list (list 1 0) (list 0 1)))
(check-expect (identityM 3)
              (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))
(check-expect (identityM 4)
              (list (list 1 0 0 0) (list 0 1 0 0) (list 0 0 1 0) (list 0 0 0 1)))

;(define (identityM n) empty) ;stub

; Template from Nat

(define (identityM n)
  (local (;; Nat -> [List-of [List-of Nat]]
          ;; produces list with inner lists
          (define (make-outer-list ni)
            (local (;; Nat -> [List-of Nat]
                    ;; produces list of n length zeros with 1 at ni pos
                    (define (make-inner-list length)
                      (cond [(zero? length) empty]
                            [else
                             (cons (if (= ni length) 1 0)
                                   (make-inner-list (sub1 length)))])))
              (cond [(zero? ni) empty]
                    [else (cons (make-inner-list n)
                                (make-outer-list (sub1 ni)))]))))
    (make-outer-list n)))


(define (identityM-v1 n)
  (local (;; Nat Nat -> [List-of Nat]
          ;; produces list of length zeros with 1 at 1-index pos
          (define (make-inner-list length 1-index)
            (cond [(zero? length) empty]
                  [else
                   (cons (if (= length 1-index) 1 0)
                         (make-inner-list (sub1 length) 1-index))]))

          ;; Nat -> [List-of [List-of Nat]]
          ;; produces list with inner lists
          (define (make-outer-list ni)
            (cond [(zero? ni) empty]
                  [else (cons (make-inner-list n ni)
                              (make-outer-list (sub1 ni)))])))
    (make-outer-list n)))



