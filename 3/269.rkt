;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |269|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct ir [name desc acq-price sales-price])
; An IR is (make-ir String String Number Number)
; an inventory record with
; - name
; - description
; - acquisition price
; - recommended sales price

(define IR1 (make-ir "acer-ff" "blah blah" 120 150))

(define (fn-for-ir ir)
  (... (ir-name ir)
       (ir-desc ir)
       (ir-acq-price ir)
       (ir-sales-price ir)))

; Number [List-of IR] -> [List-of IR]
; produces a list of records whose sales price is below ua
(check-expect (eliminate-expensive 100 empty) empty)
(check-expect (eliminate-expensive 100 (list 
                                        (make-ir "acer-ff" "blah blah" 80 99)
                                        (make-ir "acer-ff" "blah blah" 80 100)
                                        (make-ir "acer-ff" "blah blah" 80 101)))
              (list (make-ir "acer-ff" "blah blah" 80 99)))

;(define (eliminate-expensive ua loir) loir) ;stub

#;
(define (eliminate-expensive ua loir) ;template
  ;[IR -> Boolean]         
  (filter    ...             loir))

(define (eliminate-expensive ua loir) 
  (local (; IR -> Boolean
          ; produces true if ir sales price less than ua 
          (define (sales-price-below-ua? ir) (< (ir-sales-price ir) ua)))     
    (filter sales-price-below-ua? loir)))

; String [List-of IR] -> [List-of IR]
; produces a list of inventory records that do not use the name ty
(check-expect (recall "acer-ff" (list (make-ir "acer-hh" "blah blah" 80 99)
                                      (make-ir "acer-ff" "blah blah" 80 100)
                                      (make-ir "acer-gg" "blah blah" 80 101)
                                      (make-ir "acer-ff" "blah blah" 80 106)))
              (list (make-ir "acer-hh" "blah blah" 80 99)
                    (make-ir "acer-gg" "blah blah" 80 101)))

;(define (recall ty loir) loir) ;stub

#;
(define (recall ty loir)   ;template
  ;[IR -> Boolean]   
  (filter ...              loir))

(define (recall ty loir)
  (local (; IR -> Boolean
          ; produces true if ir name does not equal ty
          (define (name-not-ty? ir) (not (string=? (ir-name ir) ty))))
    (filter name-not-ty? loir)))

; [List-of String] [List-of String] -> [List-of String]
; produces list of all names from los2 that are also on the los1.
(check-expect (selection (list "aa" "bb" "cc" "dd") (list "ff" "bb" "yy" "dd" "zz"))
              (list "bb" "dd"))
(check-expect (selection (list "aa" "bb" "cc" "dd") (list "ff" "vv" "yy" "oo" "zz"))
              empty)

;(define (selection los1 los2) los2) ;stub

#;
(define (selection los1 los2)       ;template
  ; String -> Boolean
  (filter ...                los2))

(define (selection los1 los2)
  (local (; String -> Boolean
          ; is s member of los1
          (define (in-los1? s) (member? s los1)))
    (filter in-los1? los2)))
