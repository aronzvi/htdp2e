;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |170|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Constants:

(define OLD-AREA-CODE 713)
(define NEW-AREA-CODE 281)

;; Data definitions:

; A Three is a Number between 100 and 999. 
; A Four is a Number between 1000 and 9999. 

(define-struct phone [area switch four])
; A Phone is a structure: 
;   (make-phone Three Three Four)
; interp. a phone number
(define P1 (make-phone 100 500 1000))
(define P2 (make-phone 999 800 9999))

(define (fn-for-phone p)
  (... (phone-area p)
       (phone-switch p)
       (phone-four p)))

;; Template rules used:
;; - compound: 3 fields

;; ListOfPhone is one of:
;; - empty
;; - (cons Phone ListOfPhone)
;; interp. a list of phone numbers
(define LOP1 empty)
(define LOP2 (cons (make-phone 100 500 1000) empty))
(define LOP3 (cons (make-phone 999 800 9999) LOP2))

(define (fn-for-lop lop)
  (cond
    [(empty? lop) (...)]
    [else
     (... (fn-for-phone (first lop))   ;Phone
          (fn-for-lop (rest lop)))]))  ;ListOfPhone

;; Template rukes used
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Phone ListOfPhone)
;; - reference: (first lop)) is Phone
;; - self-reference: (rest lop) is ListOfPhone

;; Functions:

;; ListOfPhone -> ListOfPhone
;; replaces all occurrence in lop of area code OLD-AREA-CODE with NEW-AREA-CODE
(check-expect (replace empty) empty)
(check-expect (replace (cons (make-phone 999 800 9999) (cons (make-phone 100 500 1000) empty)))
              (cons (make-phone 999 800 9999) (cons (make-phone 100 500 1000) empty)))
(check-expect (replace (cons (make-phone OLD-AREA-CODE 800 9999) (cons (make-phone 100 500 1000) empty)))
              (cons (make-phone NEW-AREA-CODE 800 9999) (cons (make-phone 100 500 1000) empty)))
(check-expect (replace (cons (make-phone OLD-AREA-CODE 800 9999) (cons (make-phone OLD-AREA-CODE 500 1000) empty)))
              (cons (make-phone NEW-AREA-CODE 800 9999) (cons (make-phone NEW-AREA-CODE 500 1000) empty)))

;(define (replace lop) empty) ;stub

(define (replace lop)
  (cond
    [(empty? lop) empty]
    [else
     (cons (replace-area (first lop) OLD-AREA-CODE NEW-AREA-CODE)   
           (replace (rest lop)))]))

;; Phone Three Three -> Phone
;; produces phone replacing old area code with new one
(check-expect (replace-area (make-phone 123 500 1000) 123 456) (make-phone 456 500 1000))
(check-expect (replace-area (make-phone 777 500 1000) 777 1) (make-phone 1 500 1000))
(check-expect (replace-area (make-phone 100 500 1000) 123 456) (make-phone 100 500 1000))

;(define (replace-area p old new) p) ;stub

(define (replace-area p old new)
  (make-phone
   (if (= (phone-area p) old)
       new
       (phone-area p))
   (phone-switch p)
   (phone-four p)))
