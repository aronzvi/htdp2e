;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |103|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; =====================
;; Data definitions

(define-struct spider (legs space))
;; Spider is (make-spider Number Number)
;; interp. a spider where
;; - legs is the number of remaing legs
;; - space is the space they need in case of transport in meters cubed (length * width * height)

(define S1 (make-spider 8 0.001)) ; 0.10 * 0.10 * 0.10

#;
(define (fn-for-spider s)
  (... (spider-legs s)     ;Number
       (spider-space s)))  ;Number
;; Template rules used:
;; - compound: 2 fields

;; Elephant is Number
;; interp. An Elephant. The space needed in case of transport in meters cubed (length * width * height)

(define E1 125) ;5 * 5 * 5

#;
(define (fn-for-elephant e)
  (... e))

;; Template rules used:
;; - atomic non-distinct: Number

(define-struct boa (length girth))
;; Boa is (make-boa Number Number)
;; interp. a boa where
;; - length is the length in meters
;; - girth is the girth in meters

(define B1 (make-boa 10.2 0.4))

#;
(define (fn-for-boa b)
  (... (boa-length b)    ;Number
       (boa-girth b)))   ;Number

;; Template rules used:
;; - compound: 2 fields

(define-struct armadillo (space age))
;; Armadillo is (make-armadillo Number Number)
;; interp. an armadillo where
;; - space is the space they need in case of transport in meters cubed (length * width * height)
;; - age is the age of the armadillo

(define A1 (make-armadillo 1 4)) ; 1 * 1 * 1

#;
(define (fn-for-armadillo a)
  (... (armadillo-space a)  ;Number
       (armadillo-age a)))  ;Number

;; Template rules used:
;; - compound: 2 fields

;; ZooAnimal is one of:
;; - Spider
;; - Elephant
;; - Boa
;; - Armadillo
;; interp. A zoo animal

(define ZA1 S1)
(define ZA2 E1)

#;
(define (fn-for-za za)
  (cond [(spider? za) (... (fn-for-spider za))]      ;Spider
        [(number? za) (... (fn-for-elephant za))]    ;Elephant
        [(boa? za) (... (fn-for-boa za))]            ;Boa
        [else (... (fn-for-armadillo za))]           ;Armadillo
        ))

;; Template rules used:
;; one of: 4 cases
;; - reference: Spider
;; - reference: Elephant
;; - reference: Boa
;; - reference: Armadillo

(define-struct cage (h w l))
;; Cage is (make-cage Number Number Number)
;; interp. A cage where
;; - h is the height
;; - w is the width
;; -l is the length

(define C1 (make-cage 5 5 10))

#;
(define (fn-for-cage c)
  (... (cage-h c)
       (cage-w c)
       (cage-l c)))

;; Template rules used:
;; - compound: 3 cases

;; ===========================
;; Functions

;; ZooAnimal Cage -> Boolean
;; produces true if cage’s volume is larger or equal to the space of the animal
(check-expect (fits? (make-spider 8 0.027) (make-cage 5 5 5)) true)
(check-expect (fits? (make-spider 8 0.027) (make-cage 0.30 0.30 0.20)) false)
(check-expect (fits? 125 (make-cage 5 5 5)) true)
(check-expect (fits? 125 (make-cage 5 5 3)) false)
(check-expect (fits? (make-boa 10 0.5) (make-cage 1 1 1)) true)
(check-expect (fits? (make-boa 10 1) (make-cage 1 1 0.5)) false)
(check-expect (fits? (make-armadillo (* 2 1 1) 3) (make-cage 2 2 2)) true)
(check-expect (fits? (make-armadillo (* 2 2 1) 3) (make-cage 2 1 1)) false)

;(define (fits? za c) false) ; stub

; Template from ZooAnimal

(define (fits? za c)
  (cond [(spider? za) (>= (cage-vol c) (spider-space za))]      
        [(number? za) (>= (cage-vol c)  za)]    
        [(boa? za) (>= (cage-vol c) (boa-space za))]              
        [else (>= (cage-vol c) (armadillo-space za))]))

;; Boa -> Number
;; produces the space needed to transport the given boa
(check-within (boa-space (make-boa 10 0.5)) (* 10 (* pi (/ 0.5 (* 2 pi)) (/ 0.5 (* 2 pi)))) 0.0001)

;(define (boa-space b) 0) ; stub

; Template from Boa

(define (boa-space b)
  (* (boa-length b) (* pi (/ (boa-girth b) (* 2 pi)) (/ (boa-girth b) (* 2 pi)))))   

;; Cage -> Number
;; produces the volume of the given cage
(check-expect (cage-vol (make-cage 5 3 4)) (* 5 3 4))

;(define (cage-vol c) 0) ;stub

; Template from Cage

(define (cage-vol c)
  (* (cage-h c)
     (cage-w c)
     (cage-l c)))


