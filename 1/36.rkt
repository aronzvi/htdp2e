;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |36|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; Image -> Number
; counts the number of pixels in img
; given (rectangle 40 20 "outline" "black"), expect: 800
; given (circle 30 "outline" "red"), expect: 3600
; given (square 40 "outline" "black"), expect 1200
(define (image-area img)
  (* (image-height img) (image-width img)))