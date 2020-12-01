;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |160|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Son.L is one of: 
; – empty 
; – (cons Number Son.L)
; 
; Son is used when it 
; applies to Son.L and Son.R

(define SL1 empty)
(define SL2 (cons 1 SL1))
(define SL3 (cons 1 SL2))

; A Son.R is one of: 
; – empty 
; – (cons Number Son.R)
; 
; Constraint If s is a Son.R, 
; no number occurs twice in s

(define SR1 empty)
(define SR2 (cons 1 SR1))
(define SR3 (cons 2 SR2))

#;
(define (fn-for-son son)
  (cond
    [(empty? son) (...)]
    [else
     (... (first son)                 ;Number
          (fn-for-son (rest son)))])) ;Son

;; Template rules used:
;; - atomic distinct: empty
;; - compound: (cons Number Son)
;; - self-reference: (rest son) is Son


; Number Son.L -> Son.L
; adds x to s 
(check-satisfied (set+.L 1 (cons 1 (cons 2 (cons 3 '())))) member-1?)
(check-satisfied (set+.L 4 (cons 1 (cons 2 (cons 3 '())))) member-4?)
 
;(define (set+.L x s) s) ;stub

(define (set+.L x son)
  (cons x son))

; Number Son.R -> Son.R
; adds x to s
(check-satisfied (set+.R 1 (cons 1 (cons 2 (cons 3 '())))) member-1-once?)
(check-satisfied (set+.R 4 (cons 1 (cons 2 (cons 3 '())))) member-4-once?)

;(define (set+.R x s) s) ;stub

(define (set+.R x s)
  (cons x (set-.R x s)))

; Number Son.R -> Son.R
; removes x from s
(check-satisfied (set-.R 1 (cons 2 (cons 1 empty))) not-member-1?)

(define (set-.R x s)
  (remove x s))

; Son -> Boolean
; #true if 1 is not a member of s;  #false otherwise
(define (not-member-1? s)
  (not (member? 1 s)))

;; Son -> Boolean
;; #true if 1 is a member of s and only appears once; #false otherwise
(check-expect (member-1-once? empty) #false)
(check-expect (member-1-once? (cons 2 (cons 1 empty))) #true)
(check-expect (member-1-once? (cons 3 (cons 2 empty))) #false)
(check-expect (member-1-once? (cons 1 (cons 1 empty))) #false)

;(define (member-1-once? s) #false) ;stub

(define (member-1-once? s)
  (= (count s 1) 1))

;; Son -> Boolean
;; #true if 4 is a member of s and only appears once; #false otherwise
(check-expect (member-4-once? (cons 2 (cons 4 empty))) #true)
(check-expect (member-4-once? (cons 3 (cons 2 empty))) #false)
(check-expect (member-4-once? (cons 4 (cons 4 empty))) #false)

;(define (member-4-once? s) #false) ; stub

(define (member-4-once? s)
  (= (count s 4) 1))

; Son Number -> N
; determines how often n occurs in son
(check-expect (count empty 1) 0)
(check-expect (count (cons 1 empty) 1) 1)
(check-expect (count (cons 1 empty) 5) 0)
(check-expect (count (cons 6 (cons 5 empty)) 1) 0)
(check-expect (count (cons 2 (cons 7 (cons 2 empty))) 2) 2)

;(define (count son n) 0) ;stub

(define (count son n)
  (cond
    [(empty? son) 0]
    [else
     (if (= (first son) n)
         (+ 1 (count (rest son) n))
         (count (rest son) n))]))
  
; Son -> Boolean
; #true if 1 is a member of s; #false otherwise
(check-expect (member-1? empty) #false)
(check-expect (member-1? (cons 6 (cons 1 (cons 3 '())))) #true)
(check-expect (member-1? (cons 6 (cons 2 (cons 3 '())))) #false)
(check-expect (member-1? (cons 6 (cons 1 (cons 1 '())))) #true)

;(define (member-1? s) #false) ; stub

(define (member-1? s)
  (member? 1 s))

; Son -> Boolean
; #true if 1 is a member of s; #false otherwise
(check-expect (member-4? empty) #false)
(check-expect (member-4? (cons 6 (cons 4 (cons 3 '())))) #true)
(check-expect (member-4? (cons 6 (cons 2 (cons 3 '())))) #false)
(check-expect (member-4? (cons 6 (cons 4 (cons 4 '())))) #true)

;(define (member-4? s) #false) ; stub

(define (member-4? s)
  (member? 4 s))