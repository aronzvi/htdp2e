;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |54|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; (string=?) expects a string as its second parameter. Since LRCD can also be a number, the function call will fail (if it is not the last condition)
; (and (string? x) (string=? x "resting"))

(define LRCD-RESTING "resting")
(define LRCD-COUNTDOWN -3)
(define LRCD-FLYING 53)

; LRCD -> Boolean
; returns true if lrcs is in "resting" state
(check-expect (resting? LRCD-RESTING) true)
(check-expect (resting? LRCD-COUNTDOWN) false)
(check-expect (resting? LRCD-FLYING) false)
(check-expect (resting? "gggg") false)
(define (resting? lrcd)
  (and (string? lrcd) (string=? lrcd "resting")))