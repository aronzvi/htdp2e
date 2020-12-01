;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |264|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Nelon -> Number
; determines the largest 
; number on l
;(check-expect (sup-local (list 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1)) 25)
;(check-expect (sup-local (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25)) 25)

(define (sup-local nelop)
  (extract-local > nelop))

;; ??? ??? -> ??? 
;; ???
;(check-expect (extract-local < (list 5)) (inf (list 5)))
;(check-expect (extract-local < (list 6 5)) (inf (list 6 5)))
;(check-expect (extract-local < (list 5 6 9 3 10)) (inf (list 5 6 9 3 10)))
;(check-expect (extract-local > (list 5)) (sup (list 5)))
;(check-expect (extract-local > (list 6 7)) (sup (list 6 7)))
;(check-expect (extract-local > (list 5 6 9 3 8)) (sup (list 5 6 9 3 8)))

(define (extract-local R l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (local ((define ex (extract-local R (rest l))))
       (if (R (first l)
              ex)
           (first l)
           ex))]))

(sup-local (list 2 1 3))


