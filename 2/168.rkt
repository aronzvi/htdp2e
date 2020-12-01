;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |168|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

;; ListOfPosn -> ListOfPosn
;; for each (make-posn x y) in lop produces (make-posn x (+ y 1))
(check-expect (translate empty) empty)
(check-expect (translate (cons (make-posn 5 8) empty)) (cons (make-posn 5 (+ 8 1)) empty))
(check-expect (translate (cons (make-posn 40 57)(cons (make-posn 5 8) empty))) (cons (make-posn 40 (+ 57 1))(cons (make-posn 5 (+ 8 1)) empty)))

;(define (translate lop) empty) ;stub

(define (translate lop)
  (cond
    [(empty? lop) empty]
    [else
     (cons (translate-posn (first lop))   
           (translate (rest lop)))]))

;; Posn -> Posn
;; produces (make-posn x (+ y 1)) for given posn
(check-expect (translate-posn (make-posn 5 8)) (make-posn 5 (+ 8 1)))

;(define (translate-posn p) p) ;stub

#;
(define (translate-posn p) ; Template
  (... (posn-x p)
       (pos-y p)))

;; Template rules used:
;; - compound: 2 fields

(define (translate-posn p)
  (make-posn (posn-x p)
             (+ (posn-y p) 1)))

