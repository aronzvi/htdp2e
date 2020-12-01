;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |134|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Data definitions:

;; ListOfString is one of:
;; - empty
;; (cons String ListOfString)
;; interp. a list of strings

(define LOS1 empty)
(define LOS2 (cons "Yo" LOS1))
(define LOS3 (cons "Mama" LOS2))
(define LOS4 (cons "Coco" LOS3))

#;
(define (fn-for-los los)
  (cond
    [(empty? los) (...)]
    [else
     (... (first los)                 ; String
          (fn-for-los (rest los)))])) ; ListOfString

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons String ListOfString)
;; - self-reference: (rest los) is ListOfString

;; Functions:

;; String ListOfString -> Boolean
;; produces true if s occurs in los
(check-expect (contains? "BLAH" empty) false)
(check-expect (contains? "Yo" (cons "Yo" empty)) true)
(check-expect (contains? "Gine" (cons "Yo" empty)) false)
(check-expect (contains? "Bo" (cons "Yo" (cons "Bo" empty))) true)
(check-expect (contains? "Bi" (cons "Yo" (cons "Bo" empty))) false)

;(define (contains? s los) #false) ;stub

(define (contains? s los)
  (cond
    [(empty? los) false]
    [else
     (or (string=? s (first los))                
         (contains? s (rest los)))])) 




