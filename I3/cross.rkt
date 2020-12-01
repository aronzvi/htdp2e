;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname cross) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; [List-of X] [List-of Y] -> [List-of [List X Y]]
; produces pairs of all items from l1 and l2
(check-expect (cross-existing (list 1 2 3) (list "a" "b" "c"))
              (list (list 1 "a") (list 1 "b") (list 1 "c")
                    (list 2 "a") (list 2 "b") (list 2 "c")
                    (list 3 "a") (list 3 "b") (list 3 "c")))

;(define (cross-existing l1 l2) empty) ;stub

(define (cross-existing l1 l2)
  (foldr append empty (map (lambda (x)
                             (map (lambda (y)
                                    (list x y))
                                  l2))
                           l1)))

; [List-of X] [List-of Y] -> [List-of [List X Y]]
; produces pairs of all items from l1 and l2
(check-satisfied (cross (list 1 2 3) (list "a" "b" "c"))
                 (lambda (l) (= (length l) 9)))

;(define (cross l1 l2) empty) ;stub

(define (cross l1 l2)
  (for*/list ([i l1]
              [j l2])
    (list i j)))


