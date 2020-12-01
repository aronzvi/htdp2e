;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |287|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct ir (name description acq-price sales-price))
;; an IR is (make-ir String String Number Number)
;; interp. an inventory record where
;; - name is the name of an inventory item
;; - description
;; - acq-price is the acquisition price
;; - sales-price is the recommended sales price

(define IR0 (make-ir "blah" "blah is blahhh" 15 25))
(define IR1 (make-ir "blee" "bleh is blehhhh" 25 45))

(define (fn-for-ir ir)
  (... (ir-name ir)
       (ir-description ir)
       (ir-acq-price ir)
       (ir-sales-price ir)))

;; Template rules used:
;; - compound: 4 fields

;; [List-of IR] Number -> [List-of IR]
;; produces a list of all IRs whose acquisition price is below ua
(check-expect (eliminate-exp empty 5) empty)
(check-expect (eliminate-exp (list (make-ir "blah" "blah blah" 14 67) (make-ir "blah2" "blah blafh" 25 67)) 25)
              (list (make-ir "blah" "blah blah" 14 67)))
              
;(define (eliminate-exp loir ua) loir) ;stub

#;
(define (eliminate-exp loir ua)
          ;(IR -> Boolean)
  (filter ...              loir))

(define (eliminate-exp loir ua)
  (filter (lambda (ir)
            (< (ir-acq-price ir) ua))
          loir))

;; [List-of IR] String -> [List-of IR]
;; produces a list of IRs that do not use the name ty
(check-expect (recall empty "blah") empty)
(check-expect (recall (list (make-ir "blah" "blah blah" 14 67) (make-ir "blah2" "blah blafh" 25 67)) "blah")
              (list (make-ir "blah2" "blah blafh" 25 67)))

;(define (recall loir ty) loir) ;stub

(define (recall loir ty)
  (filter (lambda (ir)
            (not (string=? (ir-name ir) ty)))
          loir))

;; [List-of String] [List-of String] -> [List-of String]
;; selects all names from los2 that are also in los1
(check-expect (selection empty (list "ww" "ee"))
              empty)
(check-expect (selection (list "ddd" "ff" "ww") (list "ww" "ee"))
              (list "ww"))
(check-expect (selection (list "ddd" "ff") (list "ww" "ee"))
              empty)
(check-expect (selection (list "ddd" "ee" "ff" "ww") (list "ww" "ee"))
              (list "ww" "ee"))

;(define (selection los1 los2) los2) ;stub

#;
(define (selection los1 los2)
          ;(String -> Boolean)
  (filter ...                  los2)) 

(define (selection los1 los2)
  (filter (lambda (s)
            (member? s los1))
          los2))