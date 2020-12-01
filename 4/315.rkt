;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |315|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct no-parent [])
(define NP (make-no-parent))

(define-struct child [father mother name date eyes])

; An FT is one of:
; - NP
; - (make-child FT FT String N String)

; Oldest Generation:
(define Carl (make-child NP NP "Carl" 1926 "green"))
(define Bettina (make-child NP NP "Bettina" 1926 "green"))
 
; Middle Generation:
(define Adam (make-child Carl Bettina "Adam" 1950 "hazel"))
(define Dave (make-child Carl Bettina "Dave" 1955 "black"))
(define Eva (make-child Carl Bettina "Eva" 1965 "blue"))
(define Fred (make-child NP NP "Fred" 1966 "pink"))
 
; Youngest Generation: 
(define Gustav (make-child Fred Eva "Gustav" 1988 "brown"))

;#
; FT -> ???
(define (fn-for-ft ft)
  (cond [(no-parent? ft) (...)]
        [else
         (... (fn-for-ft (child-father ft))   ;FT
              (fn-for-ft (child-mother ft))   ;FT
              (child-name ft)                 ;String
              (child-date ft)                 ;N
              (child-eyes ft))]))             ;String

; An FF (short for family forest) is [List-of FT]
; interpretation a family forest represents several
; families (say, a town) and their ancestor trees

(define ff1 (list Carl Bettina))
(define ff2 (list Fred Eva))
(define ff3 (list Fred Eva Carl))

; [List-of FT] Natural -> Number
; produces the average age of all child instances in the forest
;(check-expect (average-age empty 2020) 0)
(check-expect (average-age ff1 2020)
              (/ (+ (- 2020 1926)
                    (- 2020 1926))
                 2))
(check-expect (average-age ff2 2020)
              (/ (+ (- 2020 1966)
                    (- 2020 1965)
                    (- 2020 1926)
                    (- 2020 1926))
                 4))

(check-expect (average-age ff3 2020)
              (/
               (+ (- 2020 1966)
                  (- 2020 1965)
                  (- 2020 1926)
                  (- 2020 1926)
                  (- 2020 1926))
               5))

;(define (average-age ff n) 0) ;stub

(define (average-age ff n)
  (local ((define sum-age-ff
            (foldl (lambda (ft s) (+ (sum-age ft n) s))
                   0
                   ff))
          (define people-count-ff
            (foldl (lambda (ft c) (+ (count-persons ft) c))
                   0
                   ff)))
    (/ sum-age-ff people-count-ff)))


; FT -> Natural
; counts the child structures in the tree.
(check-expect (count-persons NP) 0)
(check-expect (count-persons Carl) 1)
(check-expect (count-persons Adam) 3)
(check-expect (count-persons Gustav) 5)

;(define (count-persons ft) 0) ;stub

(define (count-persons ft)
  (cond [(no-parent? ft) 0]
        [else
         (+ 1
            (count-persons (child-father ft))   
            (count-persons (child-mother ft)))]))

; FT Natural -> Natrual
; produces sum of age of ft with given current year n
(check-expect (sum-age NP 2020) 0)
(check-expect (sum-age Carl 2020) (- 2020 1926))
(check-expect (sum-age Dave 2020)
              (+ (- 2020 1955)
                 (- 2020 1926)
                 (- 2020 1926)))
(check-expect (sum-age Gustav 2020)
              (+ (- 2020 1988)
                 (- 2020 1966)
                 (- 2020 1965)
                 (- 2020 1926)
                 (- 2020 1926)))
              


;(define (sum-age ft n) 0) ;stub

(define (sum-age ft n)
  (cond [(no-parent? ft) 0]
        [else
         (+ (- n (child-date ft))
            (sum-age (child-father ft) n)   
            (sum-age (child-mother ft) n))]))        
