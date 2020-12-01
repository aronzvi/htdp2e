;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |473|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Node is a Symbol.
(define N0 'A)

; A NodeAndEdges is [List Node [List-of Node]]

(define NAE0 (list 'A (list 'B 'E)))

(define (fn-for-nae nae)
  (... (first nae)               ;Node
       (fn-for-lon (rest nae)))) ;[List-of Node]


; A Graph is one of:
; - '()
; (cons NodeAndEdges '()) Graph)

(define sample-graph
  '((A (B E))
    (B (E F))
    (C (D))
    (D ())
    (E (C F))
    (F (D G))
    (G ())))

(define cyclic-graph
  '((A (B E))
    (B (E F))
    (E (C F))
    (C (B D))
    (F (D G))
    (D ())
    (G ())))


(define (fn-for-graph g)
  (cond [(empty? g) (...)]
        [else
         (... (fn-for-noe (first g))           ;NodeAndEdges 
              (fn-for-graph (rest g)))]))      ;Graph

; A Path is a [List-of Node].
; interpretation The list of nodes specifies a sequence
; of immediate neighbors that leads from the first 
; Node on the list to the last one. 

; Node Node Graph -> [Maybe Path]
; finds a path from origination to destination in G
; if there is no path, the function produces #false

(check-expect (find-path 'C 'C sample-graph)
              '(C))
(check-expect (find-path 'C 'D sample-graph)
              '(C D))
(check-member-of (find-path 'E 'D sample-graph)
                 '(E F D) '(E C D))
(check-expect (find-path 'C 'G sample-graph)
              #false)
 
;(define (find-path origination destination G) #false) ;stub

#;
(define (find-path origination destination G)
  (cond
    [(symbol=? origination destination)
     (list destination)]
    [else
     (... origination ...
          ...(find-path/list (neighbors origination G)
                             destination G) ...)]))

#;
(define (find-path origination destination G)
  (cond
    [(symbol=? origination destination)
     (list destination)]
    [else
     (local ((define next (neighbors origination G))
             (define candidate
               (find-path/list next destination G)))
       (cond
         [(boolean? candidate) ...]
         [(cons? candidate) ...]))]))

(define (find-path origination destination G)
  (cond
    [(symbol=? origination destination) (list destination)]
    [else (local ((define next (neighbors origination G))
                  (define candidate
                    (find-path/list next destination G)))
            (cond
              [(boolean? candidate) #false]
              [else (cons origination candidate)]))]))
 
; [List-of Node] Node Graph -> [Maybe Path]
; finds a path from some node on lo-Os to D
; if there is no path, the function produces #false

;(define (find-path/list lo-originations destination G) #false) ;stub

(define (find-path/list lo-Os D G)
  (cond
    [(empty? lo-Os) #false]
    [else (local ((define candidate
                    (find-path (first lo-Os) D G)))
            (cond
              [(boolean? candidate)
               (find-path/list (rest lo-Os) D G)]
              [else candidate]))]))

; Node Graph -> [List-of Node]
; produces the list of immediate neighbors of n in g
(check-expect (neighbors 'A empty)
              empty)
(check-expect (neighbors 'Z sample-graph)
              empty)
(check-expect (neighbors 'A sample-graph)
              (list 'B 'E))
(check-expect (neighbors 'F sample-graph)
              (list 'D 'G))
(check-expect (neighbors 'D sample-graph)
              empty)

;(define (neighbors n g) empty) ;stub

(define (neighbors n g)
  (cond [(empty? g) empty]
        [else
         (if (symbol=? (get-node (first g)) n)
             (get-edges (first g))
             (neighbors n (rest g)))]))

; NodeAndEdge -> Node
; produces the node in noe
(check-expect (get-node (list 'Z (list 'B 'E)))
              'Z)

;(define (get-node noe) 'A) ;stub

(define (get-node nae)
  (first nae))

; NodeAndEdge -> [List-of Node]
; produces the edges in noe
(check-expect (get-edges (list 'Z (list 'B 'E)))
              (list 'B 'E))

;(define (get-edges noe) empty) ;stub

(define (get-edges nae)
  (first (rest nae)))


; Graph -> Boolean
; determines whether there is a path between any pair of nodes in g
(check-expect (test-on-all-nodes empty) false)
(check-expect (test-on-all-nodes '((A ()))) false)
(check-expect (test-on-all-nodes sample-graph) true)
(check-expect (test-on-all-nodes '((A ())
                                   (B ())
                                   (C ())))
              false)

;(define (test-on-all-nodes g) false) ;stub

(define (test-on-all-nodes g)
  (local ((define all-nodes (map get-node g))

          ; [List-of Node] -> [List-of [List Node Node]]
          (define (all-node-pairs lon)
            (cond 
              [(< (length lon) 2) empty]
              [else
               (append (list (list (first lon) (second lon))
                             (list (second lon) (first lon)))
                       (all-node-pairs (rest lon)))]))
          
          (define (pair-has-path? p) (cons? (find-path (first p) (second p) g))))
    (ormap pair-has-path? (all-node-pairs all-nodes))))

(find-path 'B 'C cyclic-graph)
(test-on-all-nodes cyclic-graph)