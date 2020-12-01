;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |187|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Data definitions:

(define-struct gp [name score])
; A GamePlayer is a structure: 
;    (make-gp String Number)
; interpretation (make-gp p s) represents player p who 
; scored a maximum of s points
(define P1 (make-gp "Moshe Zuchi" 3))
(define P2 (make-gp "Aron Croto" 2))
(define P3 (make-gp "Carlitos Gorditos" 1))

(define (fn-for-gp p)
  (... (gp-name p)
       (gp-score p)))

;; Template rules used:
;; - compound: 2 fields

;; ListOfGamePlayer is one of:
;; - empty
;; (cons GamePlayer ListOfGamePlayer)
;; interp. a list of game players
(define LOGP1 empty)
(define LOGP2 (cons P1 (cons P2 empty)))

(define (fn-for-logp logp)
  (cond
    [(empty? logp) (...)]
    [else
     (... (fn-for-gp (first logp))      ;GamePlayer
          (fn-for-logp (rest logp)))])) ;ListOfGamePlayer

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons GamePlayer ListOfGamePlayer)
;; - reference:  (first logp) is GamePlayer
;; - self-reference: (rest-logp) is ListOfGamePlayer

;; Functions:

;; ListOfGamePlayers -> ListOfGamePlayers
;; sorts lists of game players by score in descending order
(check-expect (sort-players empty) empty)
(check-expect (sort-players (list P1 P2)) (list P1 P2))
(check-expect (sort-players (list P2 P1)) (list P1 P2))
(check-expect (sort-players (list P3 P2 P1)) (list P1 P2 P3))

;(define (sort-players logp) logp) ;stub

(define (sort-players logp)
  (cond
    [(empty? logp) empty]
    [else
     (insert-player (first logp)     
                    (sort-players (rest logp)))]))

;; GamePlayer ListOfGamePlayers -> ListOfGamePlayers
;; inserts player into proper position in sorted list of players (descending order)
(check-expect (insert-player P1 empty) (list P1))
(check-expect (insert-player P1 (list P2)) (list P1 P2))
(check-expect (insert-player P2 (list P1)) (list P1 P2))
(check-expect (insert-player P1 (list P2 P3)) (list P1 P2 P3))
(check-expect (insert-player P2 (list P1 P3)) (list P1 P2 P3))
(check-expect (insert-player P3 (list P1 P2)) (list P1 P2 P3))

;(define (insert-player p logp) logp) ;stub

(define (insert-player p logp)
  (cond
    [(empty? logp) (list p)]
    [else
     (if (higher-score? p (first logp))
         (cons p logp)
         (cons (first logp)      
               (insert-player p (rest logp))))]))

;; GamePlayer GamePlayer -> Boolean
;; produces true if p1 has higher score than p2
(check-expect (higher-score? P1 P2) #true)
(check-expect (higher-score? P2 P1) #false)

;(define (higher-score? p1 p2) #false) ;stub

(define (higher-score? p1 p2)
  (> (gp-score p1) (gp-score p2)))