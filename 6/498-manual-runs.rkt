;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 468-manual-runs) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (height.v2 abt0)
  (local (; Tree N -> N
          ; measures the height of abt
          ; accumulator a is the number of steps 
          ; it takes to reach abt from abt0
          (define (height/a abt a)
            (cond
              [(empty? abt) a]
              [else
               (max
                (height/a (node-left abt)  (+ a 1))
                (height/a (node-right abt) (+ a 1)))])))
    (height/a abt0 0)))


(height.v2 (make-node
            (make-node '()
                       (make-node '() '()))
            '()))
== (height/a (make-node
            (make-node '()
                       (make-node '() '()))
            '()) 0)
==  (max
          (height/a (make-node '()
                       (make-node '() '())) 1)
          (height/a '() 1))
== (max (max
                (height/a '()  2)
                (height/a (make-node '() '()) 2))
        1)
== (max (max
                2
                (max
                (height/a '() 3)
                (height/a '() 3)))
        1)
== (max (max
                2
                (max
                3
                3))
        1)
== (max (max
              2
              3)
        1)
== (max 3
        1)
== 3