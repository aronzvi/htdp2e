;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |113|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; ===================
;; SIGS:

; A UFO is a Posn. 
; interpretation (make-posn x y) is the UFO's location 
; (using the top-down, left-to-right convention)
 
(define-struct tank [loc vel])
; A Tank is a structure:
;   (make-tank Number Number). 
; interpretation (make-tank x dx) specifies the position:
; (x, HEIGHT) and the tank's speed: dx pixels/tick 
 
; A Missile is a Posn. 
; interpretation (make-posn x y) is the missile's place

(define-struct aim [ufo tank])
(define-struct fired [ufo tank missile])

; A SIGS is one of: 
; – (make-aim UFO Tank)
; – (make-fired UFO Tank Missile)
; interpretation represents the complete state of a 
; space invader game

;; Any -> Boolean
;; is a an element of the SIGS collection
(check-expect (sigs? (make-aim (make-posn 30 20) (make-tank 30 3))) #true)
(check-expect (sigs? (make-fired (make-posn 30 20) (make-tank 30 3) (make-posn 70 20))) #true)
(check-expect (sigs? 1) #false)
(check-expect (sigs? "hello") #false)
(check-expect (sigs? empty-image) #false)

;(define (sigs? a) #false) ; stub

#;
(define (fn-for-sigs a) ; Template. Do I need to call fn-for-missile, fn-for-tank, fn-for-ufo?
  (cond
    [(aim? a) (... (fn-for-ufo (aim-ufo a))(fn-for-tank (aim-tank a)))]
    [(fired? a) (... (fn-for-ufo (fired-ufo a)) (fn-for-tank (fired-tank a)) (fn-for-missile (fired-missile a)))]
    [else #false]))

(define (sigs? a) 
  (or (aim? a) (fired? a)))

;; ===================
;; Coordinate:

; A Coordinate is one of: 
; – a NegativeNumber 
; interpretation on the y axis, distance from top
; – a PositiveNumber 
; interpretation on the x axis, distance from left
; – a Posn
; interpretation an ordinary Cartesian point

;; Any -> Boolean
;; is a an element of the Coordinate collection
(check-expect (collection? -8) #true)
(check-expect (collection? 10) #true)
(check-expect (collection? (make-posn 3 6)) #true)
(check-expect (collection? "Baba") #false)
(check-expect (collection? empty-image) #false)
(check-expect (collection? #false) #false)

;(define (collection? a) #false); stub

#;
(define (collection? a)  ; Template
  (cond
    [(and (number? a) (< a 0)) (... a)]
    [(and (number? a) (> a 0)) (... a)]
    [(posn? a) (... (posn-x a) (posn-y a))]
    [else #false]))

(define (collection? a) 
  (cond
    [(number? a) #true]
    [(posn? a) #true]
    [else #false]))

;; ===================
;; VAnimal

(define-struct vcat [x happiness])
; a VCat is a structure:
; (make-vcat Number Number)
; interpetation a cat's x position and happiness level where
; x is the center of the cat's image 
; hapiness level is between [0,100]

(define-struct vcham [x color happiness])
; a VCham is a structure:
; (make-vcham Number ChamColor Number)
; interpetation a cham's x position, color and happiness level
; x is the center of the cham's image
; happiness level is between [0, 100]
; color is one of ChamColor ???

; A VAnimal is either
; – a VCat
; – a VCham

;; Any -> Boolean
;; is a an element of VAnimal collection
(check-expect (vanimal? (make-vcat 50 5)) #true)
(check-expect (vanimal? (make-vcham 60 "red" 30)) #true)
(check-expect (vanimal? "fff") #false)
(check-expect (vanimal? 2) #false)
(check-expect (vanimal? empty-image) #false)

;(define (vanimal? a) #false) ; stub

#;
(define (vanimal? a) ; Template
  (cond
    [(vcat? a) (... (vcat-x a) (vcat-happiness a))]
    [(vcham? a) (... (vcham-x a) (vcham-color a) (vcham-happiness a))]
    [else #false]))

(define (vanimal? a) 
  (or (vcat? a) (vcham? a)))