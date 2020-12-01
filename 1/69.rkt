;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |69|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct movie [title producer year])

------
movie|
------------------------
title | producer | year
-------------------------
"Die" "Jhon Doe"  "1945"

(define-struct person [name hair eyes phone])

------
person|
------------------------
name | hair | eyes| phone
-------------------------
"Moe" "black" "blue"  "55555555"


(define-struct pet [name number])

pet|
---------------
name | number 
---------------
"pinky" "54"

(define-struct CD [artist title price])

------
CD|
------------------------
artist | title | price
-------------------------
"Malik" "Die now"  80

(define-struct sweater [material size producer])

------
sweater|
------------------------
material | size | producer
-------------------------
"silk" "large"  "Snow daddy"