;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 412-inex+.v2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define RES-OUT-OF-RANGE-ERROR "result out of range")

; An S is one of:
; – 1
; – -1

; An N99 is an N between 0 and 99 (inclusive).

(define-struct inex [mantissa sign exponent])
; An Inex is a structure: 
;   (make-inex N99 S N99)

; N Number N -> Inex
; makes an instance of Inex after checking the arguments
(define (create-inex m s e)
  (cond
    [(and (<= 0 m 99) (<= 0 e 99) (or (= s 1) (= s -1)))
     (make-inex m s e)]
    [else (error "bad values given")]))

(define MAX-POSITIVE (create-inex 99 1 99))
(define MIN-POSITIVE (create-inex 1 -1 99))
 
; Inex -> Number
; converts an inex into its numeric equivalent 
(define (inex->number an-inex)
  (* (inex-mantissa an-inex)
     (expt
      10 (* (inex-sign an-inex) (inex-exponent an-inex)))))

; Inex Inex -> Inex
; adds inx1 to inx2
; exponents can differ by one in which case we need to adjust the inex with the smaller exponent to have the same exponent as the larger one.
; We do this by adding one to the exponent, dividing the manitssa by 10 and rounding.
; Once the exponents are equal we do exactly what we did in the previous version
(check-expect (inex+.v2 (create-inex 1 1 0) (create-inex 2 1 0))
              (create-inex 3 1 0))
(check-expect (inex+.v2 (create-inex 98 1 99) (create-inex 1 1 99)) ;result at max
              MAX-POSITIVE)
(check-expect (inex+.v2 (create-inex 55 1 0) (create-inex 55 1 0)) ;result contains too many digits. increase exponent
              (create-inex 11 1 1))
(check-expect (inex+.v2 (create-inex 56 1 0) (create-inex 56 1 0)) ;result contains too many digits. increase exponent and approximate (round)
              (create-inex 11 1 1))
(check-error (inex+.v2 MAX-POSITIVE (create-inex 1 1 99))          ;res passed max
             RES-OUT-OF-RANGE-ERROR)
(check-expect (inex+.v2 (create-inex 1 1 0) (create-inex 10 -1 1))  ;off by one. one negative
              (create-inex 2 1 0))
(check-expect (inex+.v2 (create-inex 10 1 0) (create-inex 1 1 1))   ;off by one. both positive 
              (create-inex 2 1 1))
(check-expect (inex+.v2 (create-inex 1 -1 1) (create-inex 10 -1 2)) ;off by one. both negative
              (create-inex 2 -1 1))
(check-expect (inex+.v2 (create-inex 10 1 2) (create-inex 10 1 1))   ;off by one. increase exponent of inx with smaller expt
              (create-inex 11 1 2))
(check-expect (inex+.v2 (create-inex 11 1 2) (create-inex 1 1 1))   ;off by one. increase exponent of inx with smaller expt and approximate (round)
              (create-inex 11 1 2))
(check-expect (inex+.v2 (create-inex 10 1 0) (create-inex 10 1 1)) ; From Ben - Should be handled. increase exponent of inx with smaller expt 
              (create-inex 11 1 1))

;(define (inex+.v2 inx1 inx2) inx1) ;stub

(define (inex+.v2 inx1 inx2)
  (local (; Inex Inex -> Boolean
          ; produces true if the exponents of the inexs differ by one
          (define (exponents-differ-by-1? inx1 inx2) (= (abs (- (inex-exponent inx1) (inex-exponent inx2))) 1))

          ; Inex Inex -> Boolean
          ; produces true if exponent of inx1 is smaller than inx2's. 
          (define (expt<? inx1 inx2)
            (< (* (inex-sign inx1) (inex-exponent inx1)) (* (inex-sign inx2) (inex-exponent inx2))))

          ; Inex -> Inex
          ; makes inx exponent larger by one keeping the same inx value. produce error if cannot 
          (define (expt-add1 inx)
            (local ((define expt-signed (* (inex-sign inx) (inex-exponent inx)))
                    (define expt-res-signed (add1 expt-signed))
                    (define expt-res (abs expt-res-signed))
                    (define mantissa-res (round (/ (inex-mantissa inx) 10))))
              (create-inex mantissa-res
                           (if (< expt-res-signed 0)
                               -1
                               1)
                           expt-res)))

          ; Inex Inex -> Inex
          ; adds inx1 to inx2
          ; assumes that both have equal exponent and sign
          (define (inex+ inx1 inx2)
            (local ((define mantissa-sum (+ (inex-mantissa inx1) (inex-mantissa inx2)))
                    (define exponent (inex-exponent inx1))
                    (define sign (inex-sign inx1)))
              (cond [(<= mantissa-sum 99)
                     (create-inex mantissa-sum
                                  sign
                                  exponent)]
                    [(< exponent 99)
                     (create-inex (round (/ mantissa-sum 10))
                                  sign
                                  (add1 exponent))]
                    [(= exponent 99) (error RES-OUT-OF-RANGE-ERROR)]))))
    (if (exponents-differ-by-1? inx1 inx2)
        (if (expt<? inx1 inx2)
            (inex+ (expt-add1 inx1) inx2)
            (inex+ inx1 (expt-add1 inx2)))
        (inex+ inx1 inx2))))