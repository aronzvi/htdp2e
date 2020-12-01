;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |286|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

;; [List-of IR] -> [List-of IR]
;; sorts a list of inventory records by the difference between the acq-price and ir-sales-price prices
(check-expect (sort-ir-by-sales-price empty) empty)
(check-expect (sort-ir-by-sales-price (list (make-ir "blah" "blah is blahhh" 15 25)
                                            (make-ir "blee" "bleh is blehhhh" 25 45)))
              (list (make-ir "blee" "bleh is blehhhh" 25 45)
                    (make-ir "blah" "blah is blahhh" 15 25)))
(check-expect (sort-ir-by-sales-price (list (make-ir "blee" "bleh is blehhhh" 25 45)
                                            (make-ir "blah" "blah is blahhh" 15 25)
                                            (make-ir "blahff" "vvv is blahhh" 15 50)))
              (list (make-ir "blahff" "vvv is blahhh" 15 50)
                    (make-ir "blee" "bleh is blehhhh" 25 45)
                    (make-ir "blah" "blah is blahhh" 15 25)))
                                            
;(define (sort-ir-by-sales-price loir) loir) ;stub

#;
(define (sort-ir-by-sales-price loir)
             ;(IR IR -> Boolean)
  (sort loir   ... ))

(define (sort-ir-by-sales-price loir)  
  (sort loir (lambda (ir1 ir2)
                 (local ((define (price-diff ir) (- (ir-sales-price ir) (ir-acq-price ir))))
                   (> (price-diff ir1) (price-diff ir2))))))