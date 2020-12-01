;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |154|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

;; RD -> String
;; produces a string of all colors, separated by a comma and a space of the given russian doll
(check-expect (colors "red") "red")
(check-expect (colors (make-layer "yellow" (make-layer "green" "red"))) "yellow, green, red")

;(define (colors rd) "") ;stub

(define (colors rd)
  (cond
    [(string? rd) rd]
    [else
     (string-append (layer-color rd)
                    ", "               
                    (colors (layer-doll rd)))])) 