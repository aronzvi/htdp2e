;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |272|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; List List -> List
; concatenates the items of two lists
(check-expect (append-from-fold (list 1 2 3) (list 4 5 6 7 8))
              (list 1 2 3 4 5 6 7 8))

;(define (append-from-fold l1 l2) l1) ;stub

#;
(define (append-from-fold l1 l2)  ;template
  ;[X List -> List]    List
  (foldr ...                 ...     l1)) 

(define (append-from-fold l1 l2)
  (foldr cons l2 l1))

; Replacing foldr with foldl will result in appending the second list to a reveresd first list
(check-expect (append-from-foldl (list 1 2 3) (list 4 5 6 7 8))
              (list 3 2 1 4 5 6 7 8))

(define (append-from-foldl l1 l2)
  (foldl cons l2 l1))

; [List-of Number] -> Number
; computes the sum of lon
(check-expect (sum-from-fold empty) 0)
(check-expect (sum-from-fold (list 1 2 3 4)) (+ 1 2 3 4 0))

#;
(define (sum-from-fold lon)  ;template
  ;      [Number Number -> Number]   Number
  (foldr ...                         ...     lon))

(define (sum-from-fold lon)  
  (foldr + 0 lon))

; [List-of Number] -> Number
; computes the product of lon
(check-expect (product-from-fold empty) 1)
(check-expect (product-from-fold (list 1 2 3 4)) (* 1 2 3 4 1))

;(define (product-from-fold lon) 0) ;stub

#;
(define (product-from-fold lon)      ; template
;        [Number Number -> Number]   Number
  (foldl ...                         ...     lon))

(define (product-from-fold lon)      
  (foldl * 1 lon))

; [List-of Image] -> Image
; horizontally composes the images in loi
(check-expect (compose-images (list (square 20 "solid" "red") (circle 20 "solid" "blue") (square 20 "solid" "green")))
              (beside (square 20 "solid" "red") (circle 20 "solid" "blue") (square 20 "solid" "green")))

;(define (compose-images loi) empty-image) ;stub

(define (compose-images loi)
  (foldr beside empty-image loi))

; [List-of Image] -> Image
; stacks the images in loi vertically
(check-expect (stack-images (list (square 20 "solid" "red") (circle 20 "solid" "blue") (square 20 "solid" "green")))
              (above (square 20 "solid" "red") (circle 20 "solid" "blue") (square 20 "solid" "green")))

;(define (stack-images loi) empty-image) ;stub

(define (stack-images loi)
  (foldr above empty-image loi))
