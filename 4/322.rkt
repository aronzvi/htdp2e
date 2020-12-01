;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |322|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct no-info [])
(define NONE (make-no-info))
 
(define-struct node [ssn name left right])
; A BT (short for BinaryTree) is one of:
; – NONE
; – (make-node Number Symbol BT BT)

(define BT0 NONE)
(define BT1 (make-node
             14
             'd
             NONE
             NONE))
(define BT2 (make-node
             15
             'd
             NONE
             (make-node
              24 'i NONE NONE)))
(define BT3 (make-node
             15
             'd
             (make-node
              87 'h NONE NONE)
             NONE))

(define (fn-for-bt bt)
  (cond [(no-info? bt) (...)]
        [else
         (... (node-ssn bt)                    ;Number
              (node-name bt)                   ;Symbol 
              (fn-for-bt (node-left bt))       ;BT 
              (fn-for-bt (node-right bt)))]))  ;BT

; Number BT -> Boolean
; determines whether number occurs in BT
(check-expect (contains-bt? 12 NONE) false)
(check-expect (contains-bt? 12 BT1) false)
(check-expect (contains-bt? 14 BT1) true)
(check-expect (contains-bt? 24 BT2) true)
(check-expect (contains-bt? 28 BT2) false)

;(define (contains-bt? n bt) false) ;stub

(define (contains-bt? n bt)
  (cond [(no-info? bt) false]
        [else
         (or (= (node-ssn bt) n)               
             (contains-bt? n (node-left bt))      
             (contains-bt? n (node-right bt)))]))  

