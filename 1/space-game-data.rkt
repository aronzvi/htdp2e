;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname space-game-data) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct space-game [ufo tank])
; A Space-game is a structure
; (make-space-game Number Number)
; interpetation the state of the space game
; ufo is the y position of the ufo
; tank is the x position of the tank
