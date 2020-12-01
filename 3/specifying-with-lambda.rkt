;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname specifying-with-lambda) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [X] [List-of X] [X X -> Boolean] -> [List-of X]
; sorts alon0 according to cmp
(check-expect (sort-cmp '("c" "b") string<?) '("b" "c"))
(check-expect (sort-cmp '(2 1 3 4 6 5) <) '(1 2 3 4 5 6))
 
(define (sort-cmp alon0 cmp)
  (local (; [List-of X] -> [List-of X]
          ; produces a variant of alon sorted by cmp
          (define (isort alon)
            (cond [(empty? alon) empty]
                  [else
                   (insert (first alon)
                           (isort (rest alon)))]))
 
          ; X [List-of X] -> [List-of X]
          ; inserts n into the sorted list of numbers alon 
          (define (insert n alon)
            (cond [(empty? alon) (list n)]
                  [else
                   (if (cmp n (first alon))
                       (cons n alon)
                        (cons (first alon)
                              (insert n (rest alon))))])))
    (isort alon0)))

; [List-of X] [X X -> Boolean] -> [[List-of X] -> Boolean]
; is l0 sorted according to cmp
; are all items in list k members of list l0
(check-expect [(sorted-variant-of '(3 2) <) '(2 3)]
              #true)
(check-expect [(sorted-variant-of '(3 2) <) '(3)]
              #false)

;(define (sorted-variant-of k cmp) (lambda (l0) #false)) ; stub

(define (sorted-variant-of k cmp)
  (lambda (l0)
    (and (sorted? cmp l0)
         (contains? l0 k))))

; [X X -> Boolean] [NEList-of X] -> Boolean 
; determines whether l is sorted according to cmp
(check-expect (sorted? < '(1 2 3)) #true)
(check-expect (sorted? < '(1 2 50 60 70 70)) #true) 
(check-expect (sorted? < '(1 2 50 60 70 71)) #true)
(check-expect (sorted? < '(2 1 3)) #false)

;(define (sorted? cmp l) #false) ;stub

(define (sorted? cmp nelox)
  (cond [(empty? (rest nelox)) true]
        [else
         (and
          (or (cmp (first nelox) (second nelox))
              (equal? (first nelox) (second nelox)))       
          (sorted? cmp (rest nelox)))]))

; [List-of X] [List-of X] -> Boolean 
; are all items in list k members of list l
(check-expect (contains? '(1 2 3) '(1 4 3)) #false)
(check-expect (contains? '(1 2 3 4) '(1 3)) #true)
 
(define (contains? l k)
  (andmap (lambda (in-k) (member? in-k l)) k))

(define (sorted-variant-of.v2 k cmp)
  (lambda (l0)
    (and (sorted? cmp l0)
         (contains? l0 k)
         (contains? k l0))))

(define MAX-RAND 20000)

;; Natural -> [List-of Natural]
;; generats list on n MAX-RAND random numbers
(check-random (build-list-of-random-numbers 3)
              (list (random MAX-RAND) (random MAX-RAND) (random MAX-RAND)))

;(define (build-list-of-random-numbers n) empty) ;stub

(define (build-list-of-random-numbers n)
  (build-list n (lambda (ni)
                (random MAX-RAND))))

(define a-list (build-list-of-random-numbers 500))

(check-satisfied (sort-cmp a-list <)
                 (sorted-variant-of.v2 a-list <))

(check-satisfied (sort-cmp (list 2 1 3) <)
                 (sorted-variant-of.v2 (list 1 2 2 3) <)) ; still flawed in case of duplicates. this should return false but will return true 

