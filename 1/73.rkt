;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |73|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Posn Number -> Posn
; produces a Posn like p with n in the x field
(check-expect (posn-up-x (make-posn 5 4) 3) (make-posn 3 4))
(check-expect (posn-up-x (make-posn 3 4) 3) (make-posn 3 4))
; (define (posn-up-x p n ) p) ; stub

; (define (posn-up-x p n ) ;template
;  (... (posn-x p) (posn-y p) n ...))

(define (posn-up-x p n ) 
  (make-posn n (posn-y p))) 