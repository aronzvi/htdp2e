;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |235|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; String Los -> Boolean
; determines whether l contains the string s
(define (contains? s l)
  (cond
    [(empty? l) #false]
    [else (or (string=? (first l) s)
              (contains? s (rest l)))]))

; Los -> Boolean
; determines whether l contains the string "atom"
(check-expect (contains-atom? (list "mmooo" "atom")) #true)
(check-expect (contains-atom? (list "mmooo" "gggg")) #false)

(define (contains-atom? l)
  (contains? "atom" l))

; Los -> Boolean
; determines whether l contains the string "basic"
(check-expect (contains-basic? (list "mmooo" "basic")) #true)
(check-expect (contains-basic? (list "mmooo" "gggg")) #false)

(define (contains-basic? l)
  (contains? "basic" l))

; Los -> Boolean
; determines whether l contains the string "zoo"
(check-expect (contains-zoo? (list "mmooo" "zoo")) #true)
(check-expect (contains-zoo? (list "mmooo" "gggg")) #false)

(define (contains-zoo? l)
  (contains? "zoo" l))