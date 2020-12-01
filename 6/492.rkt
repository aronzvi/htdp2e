;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |492|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Node is a Symbol.

(define N0 'A)


; An Edges is [List-of Node]
; interp: the edges of a Node in the Graph

(define E1 (list 'B 'E))

; A NodeAndEdges is [List-of Node Edges]

(define NAE0 (list 'A (list 'B 'E)))

(define (fn-for-nae nae)
  (... (first nae)                 ;Node
       (fn-for-lon (rest nae)))) ; Edges

; A Graph is [List-of NodeAndEdges]

(define sample-graph
  (list (list 'A (list 'B 'E))
        (list 'B (list 'E 'F))
        (list 'C (list 'D))
        (list 'D '())
        (list 'E (list 'C 'F))
        (list 'F (list 'D 'G))
        (list 'G '())))

(define cyclic-graph1
  '((A (B E))
    (B (E F))
    (E (C F))
    (C (B D))
    (F (D G))
    (D ())
    (G ())))

; Node Node Graph [List-of Node] -> [Maybe Path]
; finds a path from origination to destination in G
; if there is no path, the function produces #false
(check-expect (find-path 'C 'D sample-graph '())
              '(C D))
(check-member-of (find-path 'E 'D sample-graph '())
                 '(E F D) '(E C D))
(check-expect (find-path 'C 'G sample-graph '())
              #false)
(check-expect (find-path 'B 'D cyclic-graph1 '())
              (list 'B 'E 'C 'D))
(check-expect (find-path 'C 'G cyclic-graph1 '())
             (list 'C 'B 'E 'F 'G))

(define (find-path origination destination G seen)
  (cond
    [(symbol=? origination destination) (list destination)]
    [(member? origination seen) #false]
    [else (local ((define next (neighbors origination G))
                  (define candidate
                    (find-path/list next destination G (cons origination seen))))
            (cond
              [(boolean? candidate) #false]
              [else (cons origination candidate)]))]))
 
; [List-of Node] Node Graph [List-of Node] -> [Maybe Path]
; finds a path from some node on lo-Os to D
; if there is no path, the function produces #false
(define (find-path/list lo-Os D G seen)
  (cond
    [(empty? lo-Os) #false]
    [else (local ((define candidate
                    (find-path (first lo-Os) D G seen)))
            (cond
              [(boolean? candidate)
               (find-path/list (rest lo-Os) D G seen)]
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

; template from Graph

(define (neighbors n g)
  (cond [(empty? g) empty]
        [else
         (if (symbol=? (get-node (first g)) n)
         (get-edges (first g))
          (neighbors n (rest g)))]))

; NodeAndEdge -> Node
; produces the node in nae
(check-expect (get-node (list 'Z (list 'B 'E)))
              'Z)

;(define (get-node nae) 'A) ;stub

(define (get-node nae)
  (first nae))

; NodeAndEdge -> [List-of Node]
; produces the edges in noe
(check-expect (get-edges (list 'Z (list 'B 'E)))
              (list 'B 'E))

;(define (get-edges noe) empty) ;stub

(define (get-edges nae)
  (first (rest nae))) 