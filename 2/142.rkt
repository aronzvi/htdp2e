;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |142|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; ImageOrFalse is one of:
; – Image
; – #false
;; interp. an image or #false

(define IOF1 (square 5 "solid" "black"))
(define IOF2 #false)

(define (fn-for-image-or-false iof)
  (cond
    [(image? iof) (... iof)]
    [else
     (...)]))

;; Template rules used:
;; - one of: 2 cases
;; - atomic non-distinct: Image
;; - atomic distinct: #false

;; ListOfImage is one of:
;; - empty
;; (cons Image ListOfImage)
;; interp. a list of images

(define LOI1 empty)
(define LOI2 (cons (square 5 "solid" "black") LOI1))
(define LOI3 (cons (circle 5 "solid" "red") LOI2))

(define (fn-for-loi loi)
  (cond
    [(empty? loi) (...)]
    [else
     (... (first loi)                 ;Image
          (fn-for-loi (rest loi)))])) ;ListOfImage

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Image ListOfImage)
;; - self-reference: (rest loi) is ListOfImage

;; ListOfImage Number -> ImageOrFalse
;; produces the first image on loi that is not an n by n square; if it cannot find such an image, it produces #false.
(check-expect (ill-sized? empty 5) #false)
(check-expect (ill-sized? (cons (circle 5 "solid" "red") empty) 4) (circle 5 "solid" "red"))
(check-expect (ill-sized? (cons (square 5 "solid" "red") empty) 5) #false)
(check-expect (ill-sized? (cons (square 5 "solid" "red") (cons (circle 5 "solid" "red") empty)) 5) (circle 5 "solid" "red"))
(check-expect (ill-sized? (cons (square 4 "solid" "red") (cons (rectangle 4 4 "solid" "green") empty)) 4) #false)
(check-expect (ill-sized? (cons (circle 5 "solid" "red") (cons (rectangle 10 10 "solid" "red") empty)) 4) (circle 5 "solid" "red"))             

;(define (ill-sized? loi n) #false) ;stub


(define (ill-sized? loi n)
  (cond
    [(empty? loi) #false]
    [else
     (if (or (= (image-width (first loi)) n) (= (image-height (first loi)) n))
         (ill-sized? (rest loi) n)
         (first loi))]))

