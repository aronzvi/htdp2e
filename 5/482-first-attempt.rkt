;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |482|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Board N -> [Maybe [List-of QP]]
; places n queens on board; otherwise, returns #false
;(define (place-queens a-board n) #false) ;stub

#;
(define (place-queens a-board n)
(local ((define open-spots (find-open-spots a-board))

; [List-of QP] Board N -> [Maybe [List-of QP]]
(define (place-queens--list loqp a-board n)
(cond [(empty? loqp) false]
[else
(... (place-queens (first loqp) a-board n)
(place-queens--list (rest loqp) a-board n))])))
    
(cond [(zero? n) empty]
[(= (length open-spots) 0) false]
[else
(local ((define open-spot (first open-spots))
(define try (place-queens (add-queen a-board open-spot) (sub1 n))))
(if (not (false? try))
(cons open-spot try)
(place-queens--list (rest open-spots) a-board n)))])))

#;
(define (place-queens a-board n)
  (cond [(zero? n) empty]
        [else
         (local ((define open-spots (find-open-spots a-board)))
           (if (= (length open-spots) 0)
               false
               (place-queens ... (sub1 n))))]))

#;
(define (place-queens a-board n)
  (cond [(zero? n) empty]
        [else
         (local ((define open-spots (find-open-spots a-board)))
           (if (= (length open-spots) 0)
               false
               (local ((define try (place-queens (add-queen a-board (first open-spots)) (sub1 n))))
                 (if (not (false? try))
                     (cons (first open-spots) try)
                     ...))))]))

(define (place-queens a-board n)
  (cond [(zero? n) empty]
        [else
         (local ((define open-spots (find-open-spots a-board))

                 ; [List-of QP] -> [Maybe QP]??
                 (define (find-first-spot loqp) false))
           (if (= (length open-spots) 0)
               false
               (find-first-spot open-spots)))]))


; N -> Board 
; creates the initial n by n board
(define (board0 n) ...)
 
; Board QP -> Board 
; places a queen at qp on a-board
(define (add-queen a-board qp)
  a-board)
 
; Board -> [List-of QP]
; finds spots where it is still safe to place a queen
(define (find-open-spots a-board)
  '())