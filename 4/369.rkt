;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |369|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

(define a0 '(initial "X"))
(define a1 '(corona 3))

(define loa0 `(,a0 ,a1))

(define (fn-for-attribute a)
  (... (first a)
       (second a)))

; [List-of Attribute] Symbol -> String or false
(check-expect (find-attr empty 'dood) false)
(check-expect (find-attr loa0 'initial) "X")
(check-expect (find-attr loa0 'corona) false)
(check-expect (find-attr loa0 'dad) false)

;(define (find-attr loa s) false) ;stub

(define (find-attr loa s)
  (local ((define found-attrib (assq s loa)))
    (if (and (cons? found-attrib) (string? (second found-attrib)))
        (second found-attrib)
        false)))
