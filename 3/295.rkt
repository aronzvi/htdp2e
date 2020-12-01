;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |295|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; distances in terms of pixels 
(define WIDTH 300)
(define HEIGHT 300)
 
; N -> [List-of Posn]
; generates n random Posns in [0,WIDTH) by [0,HEIGHT)
(check-satisfied (random-posns 3)
                 (n-inside-playground? 3))
(check-satisfied (random-posns/bad 3)
                 (n-inside-playground? 3)) ; Should not pass since not random
(define (random-posns n)
  (build-list
   n
   (lambda (i)
     (make-posn (random WIDTH) (random HEIGHT)))))

; N -> [[List-of Posn] -> Boolean]
; ensures that the length of lop is n and that all Posns in lop are within a WIDTH by HEIGHT rectangle:
(check-expect ((n-inside-playground? 3) (list (make-posn 20 20) (make-posn 30 60) (make-posn 40 60)))
              true)
(check-expect ((n-inside-playground? 3) (list (make-posn 20 20) (make-posn (- WIDTH 1) 60) (make-posn 40 60)))
              true)
(check-expect ((n-inside-playground? 3) (list (make-posn 20 (- HEIGHT 1)) (make-posn (- WIDTH 1) 60) (make-posn 40 60)))
              true)
(check-expect ((n-inside-playground? 3) (list (make-posn 20 20) (make-posn 30 60)))
              false)
(check-expect ((n-inside-playground? 3) (list (make-posn 20 20) (make-posn 30 HEIGHT) (make-posn 40 60)))
              false)
(check-expect ((n-inside-playground? 3) (list (make-posn 20 20) (make-posn 30 (+ HEIGHT 1)) (make-posn 40 60)))
              false)
(check-expect ((n-inside-playground? 3) (list (make-posn 20 20) (make-posn 30 60) (make-posn WIDTH 60)))
              false)
(check-expect ((n-inside-playground? 3) (list (make-posn 20 20) (make-posn 30 60) (make-posn (+ WIDTH 1) 60)))
              false)
#;
(define (n-inside-playground? n)
  (lambda (lop) false)) ;stub

(define (n-inside-playground? n)
  (lambda (lop)
    (and (= (length lop) n)
         (andmap (lambda (p)
                   (and (>= (posn-x p) 0)
                        (< (posn-x p) WIDTH)
                        (>= (posn-y p) 0)
                        (< (posn-y p) HEIGHT)))
                 lop))))

; N -> [List-os Posn]
; produces a list of n posn of (0 0)
(check-expect (random-posns/bad 3) (list (make-posn 0 0) (make-posn 0 0) (make-posn 0 0)))
(define (random-posns/bad n)
  (build-list n (lambda (i)
                  (make-posn 0 0))))

