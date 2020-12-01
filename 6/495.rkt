;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |495|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(sum.v1 '(10 4))
== (+ 10 (sum.v1 '(4)))
== (+ 10 (+ 4 (sum.v1 '())))
== (+ 10 (+ 4 0))
== (+ 10 4)
== 14
 
; 4 + 0 + 10

(sum.v2 '(10 4))
== (sum/a '(10 4) 0)
== (sum/a '(4) (+ 10 0))
== (sum/a '(4) 10)
== (sum/a '() (+ 4 10))
== (sum/a '() 14)
== 14

; 10 + 0 + 4