;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname russian_dolls_intro) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define HEAD (circle 10 "solid" "red"))
(define BODY (isosceles-triangle 60 45 "solid" "red"))
(define HEAD-AND-BODY (overlay/align "middle" "top" HEAD BODY))
(define LEGS (beside (above/align "right" (square 5 "solid" "red") (rectangle 15 5 "solid" "red"))
                     (square 10 "solid" "white")
                     (above/align "left" (square 5 "solid" "red") (rectangle 15 5 "solid" "red"))))
(define DOLL (above HEAD-AND-BODY LEGS))

(define-struct layer [color doll])
;; Layer is (make-layer String LayerOrString)
;; interp. A russian doll with one more inside. Not to be used for the last doll
;; - color is the color of the doll
;; - doll is the doll inside. Either a string for the one before last doll or a layer instance

(define L1 (make-layer "red" "green")) ;A red doll with a green doll inside
(define L2 (make-layer "pink" L1))     ;A pink doll containing the above red doll

(define (fn-for-layer l)
  (... (layer-color l)                 ;String
       (fn-for-layer (layer-doll l)))) ;LayerOrString???

;; Template rules used:
;; - compound: 2 fields
;; - self-reference:  (layer-doll l) is Layer (OrString??)


; An RD (short for Russian doll) is one of: 
; – String
; – (make-layer String RD)
; interp. a russian doll.
;   String is for the last doll - its color.
;   layer is for a doll containing another doll. Its color and its nested doll

(define RD1 (make-layer "green" "red"))  ; a red doll with a green doll inside
(define RD2 (make-layer "yellow" RD1))   ; a yellow doll with the above red doll inside

(define (fn-for-rd rd)
  (cond
    [(string? rd) (... rd)]
    [else
     (... (layer-color rd)                ;String
          (fn-for-rd (layer-doll rd)))])) ;RD

;; Template rules used:
;; - one of: 2 cases
;; - atomic non-distinct: String
;; - compound: (make-layer String RD)
;; - self-reference:  (layer-doll rd) is RD

; RD -> Number
; how many dolls are part of an-rd
(check-expect (depth "red") 1)
(check-expect (depth (make-layer "yellow" (make-layer "green" "red"))) 3)

;(define (depth rd) 0) ;stub

(define (depth rd)
  (cond
    [(string? rd) 1]
    [else
     (+ 1 (depth (layer-doll rd)))])) 

  


