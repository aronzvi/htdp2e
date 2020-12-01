;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |327|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct no-info [])
(define NONE (make-no-info))
 
(define-struct node [ssn name left right])
; A BST (short for BinarySortTree) is one of:
; – NONE
; – (make-node Number Symbol BST BST)
; Invariant:
; all ssn fields in L are smaller than ssn,
; all ssn fields in R are larger than ssn.

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
(define (fn-for-bst bst)
  (cond [(no-info? bst) (...)]
        [else
         (... (node-ssn bst)                    ;Number
              (node-name bst)                   ;Symbol 
              (fn-for-bst (node-left bst))       ;BST 
              (fn-for-bst (node-right bst)))]))  ;BST

; BST Number Symbol -> BST
; produces a BST with a new node (make-node n s NONE NONE) inserted properly into the tree
(check-expect (create-bst NONE 1 's) (make-node 1 's NONE NONE))
(check-expect (create-bst BST10 9 's) (make-node 10 'a (make-node 9 's NONE NONE) NONE))    ;None underneath, insert left
(check-expect (create-bst BST10 11 's) (make-node 10 'a NONE (make-node 11 's NONE NONE)))  ;None underneath, insert right
(check-expect (create-bst BST29 30 'h) (make-node 29 'x BST15 (make-node 30 'h NONE NONE))) ;1 down has left. insert right
(check-expect (create-bst BST95 94 'm) (make-node 95 'l (make-node 94 'm NONE NONE)         ;1 down has right. insert left
                                                  (make-node 99 'b NONE NONE)))
(check-expect (create-bst BST15 9 's) (make-node 15 'd (create-bst BST10 9 's)               ;2 down. add to left
                                                 (make-node 24 'f NONE NONE)))
(check-expect (create-bst BST15 25 'v) (make-node 15 'd BST10                                ;2 down. add to right
                                                  (create-bst (make-node 24 'f NONE NONE) 25 'v)))
(check-expect (create-bst BST69 94 'f)
              (make-node 63 'g BST29
                         (make-node 89 'v  (make-node 77 'z NONE NONE)
                                    (make-node 95 'l
                                               (make-node 94 'f NONE NONE)
                                               (make-node 99 'b NONE NONE)))))

;(define (create-bst b n s) b) ;stub

(define (create-bst bst n s)
  (cond [(no-info? bst) (make-node n s NONE NONE)]
        [else
         (if (< n (node-ssn bst))
             (make-node (node-ssn bst)                    
                        (node-name bst)                   
                        (create-bst (node-left bst) n s)
                        (node-right bst))
             (make-node (node-ssn bst)                    
                        (node-name bst)                   
                        (node-left bst)
                        (create-bst (node-right bst) n s)))])) 


