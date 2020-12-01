;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |252|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; graphical constants:    
(define emt
  (empty-scene 100 100))
(define dot
  (circle 3 "solid" "red"))

; [List-of Number] -> Number
(check-expect (product empty) 1)
(check-expect (product (list 1 6 4)) 24)

(define (product l)
  (cond
    [(empty? l) 1]
    [else
     (* (first l)
        (product
          (rest l)))]))
  
; [List-of Posn] -> Image
(check-expect (image* empty) emt)
(check-expect (image* (list (make-posn 0 0) (make-posn 70 50)))
              (place-image dot 0 0 (place-image dot 70 50 emt)))
      
(define (image* l)
  (cond
    [(empty? l) emt]
    [else
     (place-dot
      (first l)
      (image* (rest l)))]))
 
; Posn Image -> Image 
(define (place-dot p img)
  (place-image
     dot
     (posn-x p) (posn-y p)
     img))

; [List-of Posn] -> Image
(check-expect (image*-fold2 empty) emt)
(check-expect (image*-fold2 (list (make-posn 0 0) (make-posn 70 50)))
              (place-image dot 0 0 (place-image dot 70 50 emt)))

(define (image*-fold2 l)
  (fold2 l place-dot emt))

; [List-of Number] -> Number
(check-expect (product-fold2 empty) 1)
(check-expect (product-fold2 (list 1 6 4)) 24)

(define (product-fold2 l)
  (fold2 l * 1))

; ? ? ? - > ?
; ???
; ???
(define (fold2 l f b)
  (cond
    [(empty? l) b]
    [else
     (f (first l)
        (fold2 (rest l) f b))]))

 
