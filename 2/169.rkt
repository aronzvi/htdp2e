;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |169|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Constants:

(define X-MIN 0)
(define X-MAX 100)
(define Y-MIN 0)
(define Y-MAX 200)


;; Data definitions:

#;
(define (fn-for-posn p)
  (... (posn-x p)
       (pos-y p)))

;; Template rules used:
;; - compound: 2 fields

;; ListOfPosn is one of:
;; - empty
;; (cons Posn ListOfPosn)
;; interp. a list of posns
(define LOP1 empty)
(define LOP2 (cons (make-posn 3 4) LOP1))
(define LOP3 (cons (make-posn 80 50) LOP2))

(define (fn-for-lop lop)
  (cond
    [(empty? lop) (...)]
    [else
     (... (fn-for-posn (first lop))   ;Posn
          (fn-for-lop (rest lop)))])) ;ListOfPosn

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - reference: (first lop) is Posn

;; Functions:

;; ListOfPosn -> ListOfPosn
;; contains all those Posns from lop whose x-coordinates are [0,100] and whose y-coordinates are [0,200]
(check-expect (legal empty) empty)
(check-expect (legal (cons (make-posn X-MIN 45) empty)) (cons (make-posn X-MIN 45) empty))     ;x - min
(check-expect (legal (cons (make-posn 50 45) empty)) (cons (make-posn 50 45) empty))           ;x - mid
(check-expect (legal (cons (make-posn X-MAX 45) empty)) (cons (make-posn X-MAX 45) empty))     ;x - max
(check-expect (legal (cons (make-posn (- X-MIN 1) 45) empty)) empty)                           ;x < min
(check-expect (legal (cons (make-posn (+ X-MAX 1) 45) empty)) empty)                           ;x > max
(check-expect (legal (cons (make-posn 50 Y-MIN) empty)) (cons (make-posn 50 Y-MIN) empty))     ;y - min
(check-expect (legal (cons (make-posn 50 50) empty)) (cons (make-posn 50 50) empty))           ;y - mid
(check-expect (legal (cons (make-posn 50 Y-MAX) empty)) (cons (make-posn 50 Y-MAX) empty))     ;y - max
(check-expect (legal (cons (make-posn 50  (- Y-MIN 1)) empty)) empty)                          ;y < min
(check-expect (legal (cons (make-posn 50 (+ Y-MAX 1)) empty))  empty)                          ;y > max

(check-expect (legal (cons (make-posn 40 90) (cons (make-posn 50 45) empty))) (cons (make-posn 40 90) (cons (make-posn 50 45) empty)))
(check-expect (legal (cons (make-posn -1 90) (cons (make-posn 50 45) empty))) (cons (make-posn 50 45) empty))
(check-expect (legal (cons (make-posn 40 90) (cons (make-posn 50 201) empty))) (cons (make-posn 40 90) empty))

;(define (legal lop) empty) ;stub

(define (legal lop)
  (cond
    [(empty? lop) empty]
    [else
     (if (legal-posn? (first lop))
         (cons (first lop)
               (legal (rest lop)))
         (legal (rest lop)))]))

;; Posn -> Boolean
;; produces true if given posn p's x is [0,100] and y is [0,200]
(check-expect (legal-posn? (make-posn X-MIN 45)) #true)
(check-expect (legal-posn? (make-posn (- X-MIN 1) 45)) #false)
(check-expect (legal-posn? (make-posn 50 (+ Y-MAX 1))) #false)

;(define (legal-posn? p) #false) ;stub

(define (legal-posn? p)
  (and (legal-posn-x? p) (legal-posn-y? p)))

;; Posn -> Boolean
;; produces true if given posn's x is [X-MIN,X-MAX]
(check-expect (legal-posn-x? (make-posn X-MIN 50)) #true)
(check-expect (legal-posn-x? (make-posn 50 50)) #true)
(check-expect (legal-posn-x? (make-posn X-MAX 50)) #true)
(check-expect (legal-posn-x? (make-posn (- X-MIN 1) 50)) #false)
(check-expect (legal-posn-x? (make-posn (+ X-MAX 1) 50)) #false)

;(define (legal-posn-x? p) #false) ;stub

(define (legal-posn-x? p)
  (<= X-MIN (posn-x p) X-MAX))

;; Posn -> Boolean
;; produces true if given posn's y is [Y-MIN,Y-MAX]
(check-expect (legal-posn-y? (make-posn 50 Y-MIN)) #true)
(check-expect (legal-posn-y? (make-posn 50 50)) #true)
(check-expect (legal-posn-y? (make-posn 50 Y-MAX)) #true)
(check-expect (legal-posn-y? (make-posn 50 (- Y-MIN 1))) #false)
(check-expect (legal-posn-y? (make-posn 50 (+ Y-MAX 1))) #false)

;(define (legal-posn-y? p) #false) ;stub

(define (legal-posn-y? p)
  (<= Y-MIN (posn-y p) Y-MAX))



