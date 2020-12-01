;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |313|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; FT -> Boolean
; produces true if an ancestor has blue eyes
(check-expect (blue-eyed-ancestor? Eva) #false)
(check-expect (blue-eyed-ancestor? Gustav) #true)

;(define (blue-eyed-ancestor? ft) false) ;stub

#;
(define (blue-eyed-ancestor? an-ftree)
  (cond
    [(no-parent? an-ftree) #false]
    [else
     (or
      (blue-eyed-ancestor?
       (child-father an-ftree))
      (blue-eyed-ancestor?
       (child-mother an-ftree)))]))

; looks correct but we never produce true anywhere and will always recurse to the false which is the only value we produce

(define (blue-eyed-ancestor? an-ftree)
  (cond
    [(no-parent? an-ftree) #false]
    [else
     (or (blue-eyed-child? (child-father an-ftree))
         (blue-eyed-child? (child-mother an-ftree)))]))

(define (blue-eyed-child? ft)
  (cond [(no-parent? ft) false]
        [else
         (or (string=? (child-eyes ft) "blue")
             (blue-eyed-child? (child-father ft))   
             (blue-eyed-child? (child-mother ft)))]))


;