;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |268|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; [List-of IR] -> [List-of IR]
; sorts a list of inventory records by the difference between the two prices in descending order
(check-expect (sort-irs-by-price-diff (list  (make-ir "acer-ff" "blah blah" 100 150)
                                             (make-ir "acer-ff" "blah blah" 100 110)
                                             (make-ir "acer-ff" "blah blah" 90 250)))
              (list  (make-ir "acer-ff" "blah blah" 90 250)
                     (make-ir "acer-ff" "blah blah" 100 150)
                     (make-ir "acer-ff" "blah blah" 100 110)))

;(define (sort-irs-by-price-diff loir) loir) ;stub

#;
(define (sort-irs-by-price-diff loir) ;template
             ;[IR IR -> Boolean]
  (sort loir         ...    ))

(define (sort-irs-by-price-diff loir)
(local (;IR IR -> Boolean
        (define (price-diff ir) (- (ir-sales-price ir) (ir-acq-price ir)))
        (define (price-diff-larger? ir1 ir2) (> (price-diff ir1) (price-diff ir2))))
  (sort loir price-diff-larger?)))

