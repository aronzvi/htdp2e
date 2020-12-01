;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |82|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Letter is one of:
; - 1String [a,z] 
; - #false

(define LETTER1 "a")
(define LETTER2 #false)

#;
(define (fn-for-letter let)
  (cond
    [(string? let) (.. let)]
    [else (...)]))

(define-struct word [let1 let2 let3])
; A Word is a structure
;  (make-word Letter Letter Letter)
; interpetation a 3 letter word

(define WORD1 (make-word "a" "b" "c"))
(define WORD2 (make-word "a" "s" "b"))
(define WORD3 (make-word "a" #false "b"))
(define WORD4 (make-word #false #false #false))
(define WORD5(make-word  "e" "r" "t"))

#;
(define (fn-for-word word)
  (... (word-let1 word) ; Letter
       (word-let2 word) ; Letter
       (word-let3 word))) ; Letter

; Word Word -> Word
; produces a word that indicates where the given word1 and word2 agree and disagree.
; Retains the contents if agree, otherwise, sets the field to false
(check-expect (compare-word WORD1 WORD1) WORD1) ; all agree
(check-expect (compare-word WORD1 WORD5) (make-word #false #false #false)) ; none agree
(check-expect (compare-word WORD1 WORD2) (make-word "a" #false #false)) ; First letter agree

; (define (compare-word word1 word2) WORD4) ; stub

(define (compare-word word1 word2)
  (make-word (compare-letter (word-let1 word1)
                             (word-let1 word2))
             (compare-letter (word-let2 word1)
                             (word-let2 word2))
             (compare-letter (word-let3 word1)
                             (word-let3 word2))))

; Letter Letter -> Letter
; produces given letter if both are equal else produces #false
(check-expect (compare-letter "a" "a") "a")
(check-expect (compare-letter "a" "b") #false)

; (define (compare-letter l1 l2) #false) ; stub

(define (compare-letter l1 l2)
  (if (string=? l1 l2) l2 #false))
