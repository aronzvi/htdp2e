;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |185|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; .1
(check-expect (first (list 1 2 3))
              1)

; .2
(check-expect (rest (list 1 2 3))
              (list 2 3))

; .3
(check-expect (second (list 1 2 3))
              2)

; .4
(check-expect (third (list 1 2 3))
              3)

; .5
(check-expect (fourth (list 1 2 3 4))
              4)
