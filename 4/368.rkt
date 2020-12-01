;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |368|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An Xexpr.v2 is a list: 
; – (cons Symbol Body)
; – (cons Symbol (cons [List-of Attribute] Body))

; where Body is short for [List-of Xexpr.v2]

; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

(define a0 '((initial "X")))
 
(define e0 '(machine))
(define e1 `(machine ,a0)) 
(define e2 '(machine (action)))
(define e3 '(machine () (action)))
(define e4 `(machine ,a0 (action) (action)))

(define (fn-for-xexpr xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) (... (first xe))]     ;Symbol
      [(list-of-attributes? (first optional-loa+content))
       (... (fn-for-loa (first optional-loa+content))      ;[List-of Attribute]
            (fn-for-loxexpr (rest optional-loa+content)))] ;[List-of Xexpr.v2] 
      [else 
       (...
        (fn-for-xexpr (first optional-loa+content))        ;Xexpr.v2
        (fn-for-loxexpr (rest optional-loa+content)))])))  ;[List-of Xexpr.v2]

(define (fn-for-loxexpr loxexpr)
  [(empty? loxexpr) (...)]
  [else
   (... (fn-for-xexpr (first loxexpr))     ;Xexpr.v2
        (fn-for-loxexpr (rest loxexpr)))]) ;[List-of Xexpr.v2]

; LOAorXexpr.v2 is one of:
; - [List-of Attribute]
; - Xexpr.v2
; interp. the second element of an Xexpr.v2 with more than one element

(define LOAorXexpr0 a0)
(define LOAorXexpr1 '(action))

(define (fn-for-loaorxexpr loaorxexpr)
  (cond [(list-of-attributes? loaorxexpr) (... (fn-for-loa loaorxexpr))]  ;[List-of Attribute] 
        [else
         (... (fn-for-xexpr loaorxexpr))])) ;Xexpr.v2 

; LOAorXexpr.v2 -> ???
; determines whether x is an element of [List-of Attribute]
; #false otherwise
;(define (list-of-attributes? x) #false) ;stub

; LOAorXexpr.v2 -> Boolean
; is x a list of attributes
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else
     (local ((define possible-attribute (first x)))
       (cons? possible-attribute))]))

