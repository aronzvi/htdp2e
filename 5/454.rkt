;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |454|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Number [List-of Number] -> [List-of [List-of Number]]
; produces an n x n matrix from lon
; assumption l is of length n^2
; we get a list of n elements from l and cons it onto the list of groups of n elements generated  from the rest of the list
; we generate a new, smaller problem by dropping the n elements we got from the list 
; trivial case: when the list is empty for which we return empty
; termination: we repeatedly remove n elements from l until it is eventually empty
(check-expect
  (create-matrix 2 (list 1 2 3 4))
  (list (list 1 2)
        (list 3 4)))
(check-expect (create-matrix 1 (list 5)) (list (list 5)))
(check-expect (create-matrix 0 empty) empty)
(check-expect
  (create-matrix 3 (list 1 2 3 4 5 6 7 8 9))
  (list (list 1 2 3)
        (list 4 5 6)
        (list 7 8 9)))

;(define (create-matrix n l) empty) ;stub

(define (create-matrix n l)
  (cond [(empty? l) empty]
        [else
         (cons (take l n)
               (create-matrix n (drop l n)))]))

; taken from Figure 147
; [List-of X] N -> [List-of X]
; keeps the first n items from l if possible or everything
(define (take l n)
  (cond
    [(zero? n) '()]
    [(empty? l) '()]
    [else (cons (first l) (take (rest l) (sub1 n)))]))

; taken from Figure 147
; [List-of X] N -> [List-of X]
; removes the first n items from l if possible or everything
(define (drop l n)
  (cond
    [(zero? n) l]
    [(empty? l) l]
    [else (drop (rest l) (sub1 n))]))
