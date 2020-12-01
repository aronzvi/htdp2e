;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |292|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [NEList-of X] is one of:
; (cons X empty)
; (cons X [NEList-of X])

(define NELX1 (list 1))
(define NELX2 (list "d" "f"))

#;
(define (fn-for-nelox nelox)
  (cond [(empty? (rest nelox)) (... (first nelox))]
        [else
         (... (fn-for-x (first nelox))        ;X
              (fn-for-nelox (rest nelox)))])) ;[NEList-of X] 

; Template rules used:
; - one of 2 cases:
; - compound: (cons X empty)
; - compound: (cons X [NEList-of X])
; - reference: (first nelox) is X
; - self-reference: (rest nelox) is [NEList-of X]

; [X X -> Boolean] [NEList-of X] -> Boolean 
; determines whether l is sorted according to cmp
 
(check-expect (sorted? < '(1 2 3)) #true)
(check-expect (sorted? < '(2 1 3)) #false)

;(define (sorted? cmp l) #false) ;stub

(define (sorted? cmp nelox)
  (cond [(empty? (rest nelox)) true]
        [else
         (and
          (cmp (first nelox) (second nelox))       
          (sorted? cmp (rest nelox)))]))

; [X X -> Boolean] -> [[List-of X] -> Boolean]
; is the given list l0 sorted according to cmp
(check-expect [(sorted string<?) '("b" "c")] #true)
(check-expect [(sorted <) '(1 2 3 4 5 6)] #true)

(define (sorted cmp)
  (lambda (l0)
      (if (empty? l0) #true (sorted? cmp l0))))

#;
(define (sorted cmp)
  (lambda (l0)
    (local (; [NEList-of X] -> Boolean 
            ; is l sorted according to cmp
            (define (sorted/l cmp l)
              (cond
                [(empty? (rest l)) #true]
                [else (and (cmp (first l) (second l))
                           (sorted/l cmp (rest l)))])))
      (if (empty? l0) #true (sorted/l cmp l0)))))