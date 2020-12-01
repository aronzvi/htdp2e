;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |191|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; Constants:

(define MT (empty-scene 50 50))

;; Data definitions:

(define (fn-for-posn p)
  (... (posn-x p)
       (posn-y p)))

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

; An NELoP is one of: 
; – (cons Posn '())
; – (cons Posn NELoP)
(define NELOP1 (list (make-posn 20 10)))
(define NELOP2 (list (make-posn 20 10) (make-posn 20 20) (make-posn 30 20)))

(define (fn-for-nelop nelop)
  (cond
    [(empty? (rest nelop)) (... (fn-for-posn (first nelop)))]
    [else
     (... (fn-for-posn (first nelop))      ;Posn
          (fn-for-nelop (rest nelop)))]))  ;NELoP

;; Template rules used:
;; - one of: 2 cases
;; - compound: (cons Posn '())
;; - reference: (first nelop) is Posn
;; - compound: (cons Posn NELoP)
;; - reference: (first nelop) is Posn
;; - self-reference: (rest nelop) is NELoP

;; Functions:

; Image NELoP -> Image 
; connects the dots in p by rendering lines in img

(check-expect (connect-dots MT triangle-p)
              (scene+line
               (scene+line MT 20 10 20 20 "red")
               20 20 30 20 "red"))
(check-expect(connect-dots MT square-p)
             (scene+line
              (scene+line
               (scene+line MT 10 10 20 10 "red")
               20 10 20 20 "red")
              20 20 10 20 "red"))

(define (connect-dots img p) MT) ;stub


