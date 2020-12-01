;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |184|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; .1
(check-expect (list (string=? "a" "b") #false)
              (list #false #false))

; .2
(check-expect (list (+ 10 20) (* 10 20) (/ 10 20))
              (list 30 200 1/2))


; .3
(check-expect (list "dana" "jane" "mary" "laura")
              (list "dana" "jane" "mary" "laura"))
