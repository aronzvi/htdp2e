;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname draw-polygon1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define MT (empty-scene 50 50))

(define (fn-for-posn p)
  (... (posn-x p)
       (posn-y p)))

;; Template rules used:
;; - compound: 2 fields

; A Polygon is one of:
; – (list Posn Posn Posn)
; – (cons Posn Polygon)
(define triangle-p
  (list
   (make-posn 20 10)
   (make-posn 20 20)
   (make-posn 30 20)))
(define square-p
  (list
   (make-posn 10 10)
   (make-posn 20 10)
   (make-posn 20 20)
   (make-posn 10 20)))

(define (fn-for-polygon p)
  (cond
    [(empty? (rest (rest (rest p))))
     (... (fn-for-posn (first p))   ;Posn
          (fn-for-posn (second p))  ;Posn
          (fn-for-posn (third p)))] ;Posn
    [else
     (... (fn-for-posn (first p))        ;Posn
          (fn-for-polygon (rest p)))]))  ;Polygon

;; Template rules used:
;; - one of: 2 cases
;; - compound: (list Posn Posn Posn)
;; - reference: (first p) is Posn
;; - reference: (second p) is Posn
;; - reference: (third p) is Posn
;; - compound: (cons Posn Polygon)
;; - reference: (first p) is Posn
;; - self-reference: (rest p) is Polygon

; Image Polygon -> Image
; renders the given polygon p into img
(check-expect (render-poly MT triangle-p)
              (scene+line
               (scene+line
                (scene+line MT 20 10 20 20 "red")
                20 20 30 20 "red")
               30 20 20 10 "red"))
(check-expect
 (render-poly MT square-p)
 (scene+line
  (scene+line
   (scene+line
    (scene+line MT 10 10 20 10 "red")
    20 10 20 20 "red")
   20 20 10 20 "red")
  10 20 10 10 "red"))

;(define (render-poly img p) img) ;stub

(define (render-poly img p)
  (cond
    [(empty? (rest (rest (rest p))))
     (render-line
      (render-line
       (render-line MT (first p) (second p))
       (second p) (third p))
      (third p) (first p))]   
    [else
     (render-line (render-poly img (rest p))
                  (first p)
                  (second p))]))   

; Image Posn Posn -> Image 
; draws a red line from Posn p to Posn q into im
(check-expect (render-line MT (make-posn 20 10) (make-posn 20 20))
              (scene+line MT 20 10 20 20 "red"))

;(define (render-line img p q) img) ;stub

(define (render-line img p q)
  (scene+line img
              (posn-x p) (posn-y p) (posn-x q) (posn-y q)
              "red"))
