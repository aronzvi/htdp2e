;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |66|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct movie [title producer year])
(make-movie "My movie" "Jhon Jones" "1990")

(define-struct person [name hair eyes phone])
(make-person "Moshe blou" "Red" "Blue" "44444444")

(define-struct pet [name number])
(make-pet "Alpha" "54")

(define-struct CD [artist title price])
(make-CD "Hoe Moe" "Why" 35)