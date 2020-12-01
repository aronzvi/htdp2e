;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |79|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Color is one of:
; — "white"
; — "yellow"
; — "orange"
; — "green"
; — "red"
; — "blue"
; — "black"

(define WHITE "white")
(define ORANGE "orange")
(define BLACK "black")

; H is a Number between 0 and 100.
; interpretation represents a happiness value

(define SAD 0)
(define MEH 50)
(define EXTATIC 100)

(define-struct person [fstname lstname male?])
; A Person is a structure:
;   (make-person String String Boolean)

(define JOHN (make-person "John" "Jones" #true))
(define JADE (make-person "Jade" "Jix" #false))

; I don't see a problem using a field name that looks like a predicate

(define-struct dog [owner name age happiness])
; A Dog is a structure:	;
;  (make-dog Person String PositiveInteger H)
; interpretation a dog has an owner, name, age and hapiness value

(define DOG1 (make-dog "Jake" "Pit" 9 MEH))
(define DOG2 (make-dog "Bella" "Friz" 4 SAD))

; A Weapon is one of:
; — #false
; — Posn
; interpretation #false means the missile hasn't
; been fired yet; a Posn means it is in flight

(define ON-GROUND #false)
(define FIRED1 (make-posn 10 10))