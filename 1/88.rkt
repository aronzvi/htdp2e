;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |88|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct vcat [x happiness])
; a VCat is a structure:
; (make-vcat Number Number)
; interpetation a cat's x position and happiness level where
; x is the center of the cat's image 
; hapiness level is between [0,100]

(define SUPER-SAD-CAT (make-vcat 30 0))
(define PERFECT-HAPPY-CAT (make-vcat 50 100))
