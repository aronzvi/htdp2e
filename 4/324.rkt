;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |324|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define BST10 (make-node 10 'a NONE NONE))
(define BST15 (make-node 15 'd BST10
                        (make-node 24 'f NONE NONE)))
(define BST29 (make-node 29 'x BST15 NONE))
(define BST95 (make-node 95 'l NONE
                        (make-node 99 'b NONE NONE)))
(define BST89 (make-node 89 'v  (make-node 77 'z NONE NONE)
                              BST95))
(define BST69 (make-node 63 'g BST29 BST89))

#;
(define (fn-for-bt bt)
  (cond [(no-info? bt) (...)]
        [else
         (... (node-ssn bt)                    ;Number
              (node-name bt)                   ;Symbol 
              (fn-for-bt (node-left bt))       ;BT 
              (fn-for-bt (node-right bt)))]))  ;BT

; BT -> [List-of Number]
; produces list of all the ssn numbers in the tree as they show up from left to right when looking at a tree drawing
(check-expect (inorder NONE) empty)
(check-expect (inorder BT1) (list 14))
(check-expect (inorder BT2) (list 15 24))
(check-expect (inorder BT3) (list 87 15))
(check-expect (inorder BST69) (list 10 15 24 29 63 77 89 95 99))

;(define (inorder bt) empty) ;stub

(define (inorder bt)
  (cond [(no-info? bt) empty]
        [else
         (append (inorder (node-left bt))
                 (list (node-ssn bt))                    
                 (inorder (node-right bt)))]))

; What does inorder produce for a binary search tree? - it produces a list of numbers sorted in ascending order?

