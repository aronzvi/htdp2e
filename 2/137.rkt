;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |137|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(define (how-many alos)
  (cond
    [(empty? alos) ...]
    [else
     (... (first alos) ...
      ... (how-many (rest alos)) ...)]))

(define (contains-flatt? alon)
  (cond
    [(empty? alon) ...]
    [else
     (... (first alon) ...
          (contains-flatt? (rest alon)) ...)]))

; Both templates have two clauses. One for for empty and the other for when not empty.
; In the second case, when not empty, both templates combine some appliction of the first selector with a natural recursive application of the the rest selector


