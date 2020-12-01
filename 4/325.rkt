;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |325|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; Number BST -> String or NONE
; produces the name of node with ssn n. if not found, produces NONE
(check-expect (search-bst 14 NONE) NONE)
(check-expect (search-bst 10 BST10) 'a)
(check-expect (search-bst 11 BST10) NONE)
(check-expect (search-bst 10 BST15) 'a)
(check-expect (search-bst 24 BST15) 'f)
(check-expect (search-bst 21 BST15) NONE)
(check-expect (search-bst 77 BST69) 'z)
(check-expect (search-bst 1 BST69) NONE)

;(define (search-bst n t) NONE) ;stub

(define (search-bst n bt)
  (cond [(no-info? bt) NONE]
        [else
         (cond [(= (node-ssn bt) n)  (node-name bt)]
               [(< n (node-ssn bt)) (search-bst n (node-left bt))]
               [(> n (node-ssn bt)) (search-bst n (node-right bt))])]))

;; Number ListOfNumber -> Boolean
;; produces true if number occurs in a sorted (ascending) list of numbers
(check-expect (sorted-search 1 empty) #false)
(check-expect (sorted-search 1 (list 1 2 3)) #true)
(check-expect (sorted-search 3 (list 1 2 3)) #true)
(check-expect (sorted-search 1 (list 2 3 5)) #false)

;(define (sorted-search n lon) #false) ;stub

(define (sorted-search n lon)
  (cond
    [(empty? lon) #false]
    [else
     (if (<= n (first lon))
         (= n (first lon))
          (sorted-search n (rest lon)))]))
                