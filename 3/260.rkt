;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |260|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Nelon is one of:
;; - (cons Number '())
;; - (cons Number Nelon)
;; interp. A non-empty list of numbers
(define NELON1 (cons 4 '()))
(define NELON2 (cons 4 (cons 5 (cons 6 '()))))

(define (fn-for-nelon nelon)
  (cond [(empty? (rest nelon)) (... (first nelon))]
        [else
         (... (first nelon)
              (rest nelon))])) ;Nelon

;; Template rules used:
;; - one of: 2 cases
;; - compound: (cons Number '())
;; - compound: (cons Number Nelon)
;; - self-reference: (rest nelon) is NElon

; Nelon -> Number
; determines the smallest 
; number on l
(check-expect (inf (list 5)) 5)
(check-expect (inf (list 6 5)) 5)
(check-expect (inf (list 5 6 9 3 10)) 3)

(define (inf l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (< (first l)
            (inf (rest l)))
         (first l)
         (inf (rest l)))]))

; Nelon -> Number
; determines the smallest 
; number on l
(check-expect (inf-min (list 5)) 5)
(check-expect (inf-min (list 6 5)) 5)
(check-expect (inf-min (list 5 6 9 3 10)) 3)

(define (inf-min l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (min
      (first l)
      (inf-min (rest l)))]))

; Nelon -> Number
; determines the smallest 
; number on l
;(check-expect (inf-1 (list 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1)) 1)
;(check-expect (inf-1 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25)) 1)

(define (inf-1 nelon)
  (extract < nelon))
    
; Nelon -> Number
; determines the largest 
; number on l
(check-expect (sup (list 5)) 5)
(check-expect (sup (list 6 7)) 7)
(check-expect (sup (list 5 6 9 3 8)) 9)

(define (sup l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (> (first l)
            (sup (rest l)))
         (first l)
         (sup (rest l)))]))

; Nelon -> Number
; determines the largest 
; number on l
(check-expect (sup-max (list 5)) 5)
(check-expect (sup-max (list 6 7)) 7)
(check-expect (sup-max (list 5 6 9 3 8)) 9)

(define (sup-max l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (max
      (first l)
      (sup-max (rest l)))]))

; Nelon -> Number
; determines the largest 
; number on l
;(check-expect (sup-1 (list 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1)) 25)
;(check-expect (sup-1 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25)) 25)

(define (sup-1 nelop)
  (extract > nelop))


;; ??? ??? -> ??? 
;; ???
(check-expect (extract < (list 5)) (inf (list 5)))
(check-expect (extract < (list 6 5)) (inf (list 6 5)))
(check-expect (extract < (list 5 6 9 3 10)) (inf (list 5 6 9 3 10)))
(check-expect (extract > (list 5)) (sup (list 5)))
(check-expect (extract > (list 6 7)) (sup (list 6 7)))
(check-expect (extract > (list 5 6 9 3 8)) (sup (list 5 6 9 3 8)))

(define (extract R l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (R (first l)
            (extract R (rest l)))
         (first l)
         (extract R (rest l)))]))

;; This version is very slow because it can potentionally run a very large number of times (?)

; Nelon -> Number
; determines the largest 
; number on l
(check-expect (sup-local (list 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1)) 25)
(check-expect (sup-local (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25)) 25)

(define (sup-local nelop)
  (extract-local > nelop))

; Nelon -> Number
; determines the smallest 
; number on l
;(check-expect (inf-local (list 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1)) 1)
;(check-expect (inf-local (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25)) 1)

(define (inf-local nelon)
  (extract-local < nelon))

;; ??? ??? -> ??? 
;; ???
(check-expect (extract-local < (list 5)) (inf (list 5)))
(check-expect (extract-local < (list 6 5)) (inf (list 6 5)))
(check-expect (extract-local < (list 5 6 9 3 10)) (inf (list 5 6 9 3 10)))
(check-expect (extract-local > (list 5)) (sup (list 5)))
(check-expect (extract-local > (list 6 7)) (sup (list 6 7)))
(check-expect (extract-local > (list 5 6 9 3 8)) (sup (list 5 6 9 3 8)))

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


