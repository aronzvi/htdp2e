;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |76|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct movie [title producer year])
; A Movie is a structure
;  (make-movie String String Number)
; interpretation a movie record
; title is the movie title
; producer is the movie producer
; year is th year the movie was released

; A HairColor is one of:
; - "brown"
; - "black"
; - "grey"
; - "white"
; - "blond"

; An EyeColor is one of:
; - "brown"
; - "green"
; - "blue"

(define-struct phone [area number])
; A Phone is a structure
;  (make-phone Number Number)
; interpretation a phone number
; area is the area code
; number is the phone number

(define-struct person [name hair eyes phone])
; A Person is a structure:
;  (make-person String HairColor EyeColor Phone)
; interpretation a person record
; name is the name of the person
; hair is person's hair color
; eyes is the person's eye color
; phone is the person's phone number

(define-struct pet [name number])
; A Pet is a structure:
;  (make-pet String Number)
; interpretation a pet
; name is the pet's name
; number is the pet's number

(define-struct CD [artist title price])
; A CD is a structure:
;  (make-CD String String Number)
; interpretation a CD
; artist is the CD's artist
; title is the title of the CD
; price is the price od the CD in USD

; Material is one of:
; - "cotton"
; - "wool"

; Size is one of:
; - "small"
; - "medium"
; - "large"
; - "extra-large"

(define-struct sweater [material size producer])
; A sweater is a structure:
;  (make-sweater Material Size String)
; interpretation a sweater
; material is the sweater's material
; size is the sweater size
; producer is the sweater producer
