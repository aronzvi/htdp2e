;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |430|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of Number] -> [List-of Number]
; produces a sorted version of alon 
(check-expect (quick-sort< empty) empty)
(check-expect (quick-sort< '(1 4 2 7 6 100 32 33 90 5))
              '(1 2 4 5 6 7 32 33 90 100))
(check-expect (quick-sort< '(1 1 1))
              '(1 1 1))
(check-expect (quick-sort< '(5 5 2 7 10 10 10 1))
              '(1 2 5 5 7 10 10 10))

;(define (quick-sort< alon) alon) ;stub

#;
(define (quick-sort< alon)
  (cond
    [(empty? alon) '()]
    [(= (length alon) 1) alon]
    [else (local (
                  (define pivot (first alon))
                  (define smallers (filter (lambda (n) (< n pivot)) alon))
                  (define largers-and-pivot (filter (lambda (n) (not (< n pivot))) alon))
                  (define largers (filter (lambda (n) (not (equal? pivot n))) largers-and-pivot)))
            (append (quick-sort< smallers)
                    (filter (lambda (n) (= n pivot)) largers-and-pivot)
                    (quick-sort< largers)))]))

;Ben's version. Cleaner. Handle each pivot individually. Will just do again for duplicates 
#; 
(define (quick-sort< alon)
  (cond
    [(empty? alon) '()]
    [(= (length alon) 1) alon]
    [else (local ((define pivot (first alon))
                  (define smallers (filter (lambda (n) (< n pivot)) (rest alon)))
                  (define largers-and-pivot (filter (lambda (n) (not (< n pivot))) (rest alon))))
            (append (quick-sort< smallers)
                    (list pivot)
                    (quick-sort< largers-and-pivot)))]))


(define (quick-sort< alon)
  (quick-sortx alon <))

; [List-of Number] [Number Number -> Boolean] -> [List-of Number]
; produces a sorted version of alon using f for comparison
; assume that numbers are distinct
(check-expect (quick-sort<.v2 empty <) empty)
(check-expect (quick-sort<.v2 '(1 4 2 7 6 100 32 33 90 5) <)
              '(1 2 4 5 6 7 32 33 90 100))
(check-expect (quick-sort<.v2 '(1 1 1) <)
              '(1 1 1))
(check-expect (quick-sort<.v2 '(5 5 2 7 10 10 10 1) <)
              '(1 2 5 5 7 10 10 10))

;(define (quick-sort<.v2 alon f) alon) ;stub

(define (quick-sort<.v2 alon f)
  (cond
    [(empty? alon) '()]
    [(= (length alon) 1) alon]
    [else (local ((define pivot (first alon))
                  (define smallers (filter (lambda (n) (f n pivot)) (rest alon)))
                  (define largers-with-pivot (filter (lambda (n) (not (f n pivot))) (rest alon))))
            (append (quick-sort<.v2 smallers f)
                    (list pivot)
                    (quick-sort<.v2 largers-with-pivot f)))]))

; [List-of Number] -> [List-of Number]
; produces a sorted version of alon in descending order
(check-expect (quick-sort> empty) empty)
(check-expect (quick-sort> '(1 4 2 7 6 100 32 33 90 5))
              '(100 90 33 32 7 6 5 4 2 1))
(check-expect (quick-sort> '(1 1 1))
              '(1 1 1))
(check-expect (quick-sort> '(5 5 2 7 10 10 10 5 1))
              '(10 10 10 7 5 5 5 2 1))

;(define (quick-sort> alon) alon) ;stub

(define (quick-sort> alon)
  (quick-sortx alon >))

; [List-of String] -> [List-of String]
; sorts los by string-length in ascending order
(check-expect (quick-sortstr< empty) empty)
(check-expect (quick-sortstr<  '("bb" "eeeee" "a" "ccc" "dddd"))
              '("a" "bb" "ccc" "dddd" "eeeee"))
(check-expect (quick-sortstr< '("bb" "eeeee" "a" "ccc" "dddd" "xxxxx"))
              '("a" "bb"  "ccc" "dddd" "eeeee" "xxxxx"))

;(define (quick-sortstr< l) l) ;stub

(define (quick-sortstr< l)
  (quick-sortx l
               (lambda (s1 s2)
                 (< (string-length s1) (string-length s2)))))

; [List-of X] [X X -> Boolean] -> [List-of X]
; sorts l by f
(check-expect (quick-sortx empty <) empty)
(check-expect (quick-sortx '(1 4 2 7 6 100 32 33 90 5) <)
              '(1 2 4 5 6 7 32 33 90 100))
(check-expect (quick-sortx '(1 1 1) <)
              '(1 1 1))
(check-expect (quick-sortx '(5 5 2 7 10 10 10 1) <)
              '(1 2 5 5 7 10 10 10))

;(define (quick-sortx l f) l) ;stub

#;
(define (quick-sortx alon f)
  (cond
    [(empty? alon) '()]
    [(= (length alon) 1) alon]
    [else (local ((define pivot (first alon))
                  (define smallers (filter (lambda (n) (f n pivot)) alon))
                  (define largers-and-pivot (filter (lambda (n) (not (f n pivot))) alon))
                  (define largers (filter (lambda (n) (not (equal? pivot n))) largers-and-pivot)))
            (append (quick-sortx smallers f)
                    (filter (lambda (n) (equal? n pivot)) alon)
                    (quick-sortx largers f)))]))

(define (quick-sortx alon f)
  (cond
    [(empty? alon) '()]
    [(= (length alon) 1) alon]
    [else (local ((define pivot (first alon))
                  (define smallers (filter (lambda (n) (f n pivot)) (rest alon)))
                  (define largers-and-pivot (filter (lambda (n) (not (f n pivot))) (rest alon))))
            (append (quick-sortx smallers f)
                    (list pivot)
                    (quick-sortx largers-and-pivot f)))]))

