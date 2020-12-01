;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |242|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Data definitions:

; A [List-of ITEM] is one of:
; - '()
; - (cons ITEM [List-of ITEM])

; [List-of String]
; interp. a list of String
(define LOS1 empty)
(define LOS2 (cons "hello" (cons "world" '())))

(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (first los)
              (fn-for-los (rest los)))]))

(define (fn-for-nelos nelos)
  (cond [(empty? (rest nelos)) (... (first nelos))]
        [else
         (... (first nelos)
              (fn-for-nelos (rest nelos)))]))

; A [Maybe X] is one of: 
; – #false 
; – X

; [Maybe String]
; interp. false or String

; [Maybe [List-of String]]
; interp. false or [List-of String]


; [List-of [Maybe String]]
; interp. a list of [Maybe String]

; Functions:

; String [List-of String] -> [Maybe [List-of String]]
; returns the remainder of los starting with s 
; #false otherwise
(check-expect (occurs "d" '()) #f)
(check-expect (occurs "a" (list "b" "a" "d" "e"))
              (list "d" "e"))
(check-expect (occurs "a" (list "b" "c" "d")) #f)

;(define (occurs s los) los) ;stub

(define (occurs s los)
  (if (member? s los)
      (list-remainder s los)
      #false))

;; String [List-of String] -> [List-of String]
;; returns the remainder of los starting with s
(check-expect (list-remainder "d" (list "d")) empty)
(check-expect (list-remainder "b" (list "b" "a" "d" "e")) (list "a" "d" "e"))

;(define (list-remainder s los) los) ;stub

(define (list-remainder s los)
  (cond [(empty? los) empty]
        [else
        (if (string=? (first los) s)
            (rest los)
            (list-remainder s (rest los)))]))

