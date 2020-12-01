;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |399|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of String] -> [List-of String] 
; picks a random non-identity arrangement of names

(define (gift-pick names)
  (random-pick
   (non-same names (arrangements names))))

; [NEList-of X] -> X 
; returns a random item from the list
(check-random (random-pick (list 1 2 3)) (list-pick (list 1 2 3) (random (length (list 1 2 3)))))

;(define (random-pick l) (first l)) ;stub

(define (random-pick l)
  (list-pick l (random (length l))))

(define (list-pick l n)
  (cond
    [(and (= n 0) (empty? l))
     (error "list too short")]
    [(and (> n 0) (empty? l))
     (error "list too short")]
    [(and (= n 0) (cons? l))
     (first l)]
    [(and (> n 0) (cons? l))
     (list-pick (rest l) (sub1 n))]))

; [List-of String] [List-of [List-of String]] -> [List-of [List-of String]]
; produces the list of those lists in ll that do 
; not agree with names at any place
(check-expect (non-same (list "Louise" "Jane" "Laura") empty) empty)
(check-expect (non-same (list "Louise") (list (list "Louise"))) empty)
(check-expect (non-same (list "Louise" "Jane" "Laura")
                        (arrangements (list "Louise" "Jane" "Laura")))
              (list (list "Jane" "Laura" "Louise")
                    (list "Laura" "Louise" "Jane")))

;(define (non-same names ll) ll) ;stub

; Not using cross product here
(define (non-same names ll)
  (local (;[List-of String] -> Boolean
          ; process both lists synchronously
          (define (agree-nowhere? l names)
            (cond [(empty? l) true]
                  [else
                   (if (string=? (first l) (first names))
                       false
                       (agree-nowhere? (rest l) (rest names)))])))
  (filter (lambda (l) (agree-nowhere? l names))
          ll)))

;; Word -> List-of-words
;; finds all rearrangements of word
(check-expect (arrangements empty) (list empty))
(check-expect (arrangements (list "d" "e")) (list (list "d" "e") (list "e" "d")))
(check-expect (arrangements (list "d" "e" "r")) (list (list "d" "e" "r")
                                                      (list "e" "d" "r")
                                                      (list "e" "r" "d")
                                                      (list "d" "r" "e")
                                                      (list "r" "d" "e")
                                                      (list "r" "e" "d")))

;(define (arrangements word) (list word)) ;stub

(define (arrangements w)
  (cond
    [(empty? w) (list '())]
    [else (insert-everywhere/in-all-words (first w)
                                          (arrangements (rest w)))]))

;; 1String List-of-words -> List-of-words
;; produces a list of words like low, but with s inserted at the beginning, between all letters, and at the end of all words of the given list
(check-expect (insert-everywhere/in-all-words "d" empty) empty)
(check-expect (insert-everywhere/in-all-words "d"
                                              (list (list "e" "r")))
              (list (list "d" "e" "r")
                    (list "e" "d" "r")
                    (list "e" "r" "d")))
(check-expect (insert-everywhere/in-all-words "d"
                                              (list (list "e" "r")
                                                    (list "r" "e")))
              (list (list "d" "e" "r")
                    (list "e" "d" "r")
                    (list "e" "r" "d")
                    (list "d" "r" "e")
                    (list "r" "d" "e")
                    (list "r" "e" "d")))
                                                          
;(define (insert-everywhere/in-all-words s low) low) ;stub

(define (insert-everywhere/in-all-words s low)
  (cond
    [(empty? low) empty]
    [else
     (append (insert-everywhere-in-word s (first low))   
             (insert-everywhere/in-all-words s (rest low)))]))

;; 1String Word -> List-of-words
;; produces a list of words from s inserted at the beginning, between all letters, and at the end of w
(check-expect (insert-everywhere-in-word "d" empty) (list (list "d")))
(check-expect (insert-everywhere-in-word "d"
                                         (list "r"))
              (list (list "d" "r")
                    (list "r" "d")))
(check-expect (insert-everywhere-in-word "d"
                                         (list "e" "r"))
              (list (list "d" "e" "r")
                    (list "e" "d" "r")
                    (list "e" "r" "d")))
(check-expect (insert-everywhere-in-word "d"
                                         (list "x" "e" "r"))
              (list (list "d" "x" "e" "r")
                    (list "x" "d" "e" "r")
                    (list "x" "e" "d" "r")
                    (list "x" "e" "r" "d")))

;(define (insert-everywhere-in-word s w) (list w)) ;stub

(define (insert-everywhere-in-word s w)
  (cond
    [(empty? w) (list (list s))]
    [else
     (cons (insert-first-in-word s w)  
           (insert-first/in-all-words (first w)
                                      (insert-everywhere-in-word s (rest w))))]))

;; 1String Word -> Word
;; inserts s at begining of w
(check-expect (insert-first-in-word "d" empty) (list "d"))
(check-expect (insert-first-in-word "d" (list "r")) (list "d" "r"))

;(define (insert-first-in-word s w) w) ;stub

(define (insert-first-in-word s w)
  (cons s w))

;; 1String List-of-words -> List-of-words
;; inserts s into the begining of all words in low
(check-expect (insert-first/in-all-words "r" empty) empty)
(check-expect (insert-first/in-all-words "r" (list (list "d")))
              (list (list "r" "d")))
(check-expect (insert-first/in-all-words "e" (list (list "d" "r")
                                                   (list "r" "d")))
              (list (list "e" "d" "r")
                    (list "e" "r" "d")))

;(define (insert-first/in-all-words s low) low) ;stub

(define (insert-first/in-all-words s low)
  (cond
    [(empty? low) empty]
    [else
     (cons (insert-first-in-word s (first low))
           (insert-first/in-all-words s (rest low)))])) 

