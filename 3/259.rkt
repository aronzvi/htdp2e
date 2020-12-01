;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |259|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
  (local [;; 1String List-of-words -> List-of-words
          ;; produces a list of words like low, but with s inserted at the beginning, between all letters, and at the end of all words of the given list
          (define (insert-everywhere/in-all-words s low)
            (cond
              [(empty? low) empty]
              [else
               (append (insert-everywhere-in-word s (first low))   
                       (insert-everywhere/in-all-words s (rest low)))]))
          
          ;; 1String Word -> List-of-words
          ;; produces a list of words from s inserted at the beginning, between all letters, and at the end of w
          (define (insert-everywhere-in-word s w)
            (cond
              [(empty? w) (list (list s))]
              [else
               (cons (insert-first-in-word s w)  
                     (insert-first/in-all-words (first w)
                                                (insert-everywhere-in-word s (rest w))))]))

          ;; 1String Word -> Word
          ;; inserts s at begining of w
          (define (insert-first-in-word s w)
            (cons s w))
          
          ;; 1String List-of-words -> List-of-words
          ;; inserts s into the begining of all words in low
          (define (insert-first/in-all-words s low)
            (cond
              [(empty? low) empty]
              [else
               (cons (insert-first-in-word s (first low))
                     (insert-first/in-all-words s (rest low)))])) ]
    (cond
      [(empty? w) (list '())]
      [else (insert-everywhere/in-all-words (first w)
                                            (arrangements (rest w)))])))



