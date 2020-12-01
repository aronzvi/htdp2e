;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |307|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; String [List-of String] -> String
; retrieves the first name in los that is equal to, or an extension of s
(check-expect (find-name "blah" (list "fff" "blah"))
              "blah")
(check-expect (find-name "blah" (list "fff" "gggblah"))
              "gggblah")
(check-expect (find-name "blah" (list "fff" "blahggg"))
              "blahggg")
(check-expect (find-name "blah" (list "fff" "gggblah" "blahggg"))
              "gggblah")
(check-expect (find-name "blah" (list "fff" "yyy"))
              false)

;(define (find-name s los) s) ;stub

(define (find-name s los)
  (for/or ([i los])
    (if (string-contains? s i) i false)))

; [List-of String] Natural -> Boolean
; produces true if no name in los exceeds width
(check-expect (none-exceed-width? empty 3) true)
(check-expect (none-exceed-width? (list "www" "ff" "g") 3) true)
(check-expect (none-exceed-width? (list "www" "ff" "g") 2) false)

;(define (none-exceed-width? los width) false) ;stub

(define (none-exceed-width? los width)
  (for/and ([s los])
  (<= (string-length s) width)))

#;
(define (none-exceed-width? los width)
  (local ((define (does-not-exceed-width? s) (<= (string-length s) width)))
    (andmap does-not-exceed-width? los)))
