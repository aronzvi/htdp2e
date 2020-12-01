;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |271|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; String [List-of String] -> Boolean
; determines whether any of the names in los are equal to or an extension of s
(check-expect (find-name "hola" (list "fff" "hola")) #true)
(check-expect (find-name "hola" (list "fff" "holaff")) #true)
(check-expect (find-name "hola" (list "fff" "ddd")) #false)

;(define (find-name s los) #false) ;stub

(define (find-name s los)
  (local (; String -> Boolean
          ; is sx equal to or an extension of s
          (define (equal-or-ext sx) (string-contains? s sx)))
    (ormap equal-or-ext los)))

; [List-of String] -> Boolean
; checks that all names in los start with the letter "a"
(check-expect (all-start-with-a (list "a" "aass" "addd")) #true)
(check-expect (all-start-with-a (list "a" "ss" "addd")) #false)

;(define (all-start-with-a los) #false) ;stub

(define (all-start-with-a los)
  (local (; String -> Boolean
          ; produces true if s starts with "a"
          (define (starts-with-a? s) (string=? (string-ith s 0) "a")))
  (andmap starts-with-a? los)))

; We should use andmap to define a function that ensures that no name on some list exceeds a given width

; [List-of String] Natural -> Boolean
; produces true if no string in los exceeds width
(check-expect (none-exceed-width? empty 3) true)
(check-expect (none-exceed-width? (list "www" "ff" "g") 3) true)
(check-expect (none-exceed-width? (list "www" "ff" "g") 2) false)

;(define (none-exceed-width? los width) false) ;stub

(define (none-exceed-width? los width)
  (local ((define (does-not-exceed-width? s) (<= (string-length s) width)))
    (andmap does-not-exceed-width? los)))
