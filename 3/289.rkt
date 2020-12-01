;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |289|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; String [List-of String] -> Boolean
;; determines whether any of the names in los are equal to or an extension of s
(check-expect (find-name "blah" empty) false)
(check-expect (find-name "blah" (list "ff" "blah")) true)
(check-expect (find-name "blah" (list "ff" "blahffff")) true)
(check-expect (find-name "blah" (list "ff" "ffffblah")) true) ; is this really an extension??
(check-expect (find-name "blah" (list "ff" "ggg")) false)

;(define (find-name s los) false) ;stub

#;
(define (find-name s los)
  ;(String -> Boolean)
  (ormap ... los))

(define (find-name s los)
  (ormap (lambda (si)
           (string-contains? s si))
         los))

;; [List-of String] -> Boolean
;; checks all if all names on a list of names start with the letter "a"
(check-expect (all-start-with-a? empty) false) ; this fails!
(check-expect (all-start-with-a? (list "asa" "add")) true)
(check-expect (all-start-with-a? (list "ssa" "add")) false)

;(define (all-start-with-a? los) false) ;stub

#;
(define (all-start-with-a? los)
  ;(String -> Boolean)
  (andmap ...                 los))

(define (all-start-with-a? los)
  (andmap (lambda (s)
            (string=? (string-ith s 0) "a"))
          los))

;; [List-of String] Natural -> Boolean
;; checks if all widths of names in los do not exceed n
(check-expect (all-within-width (list "d" "22" "333" "fgfg") 4) true)
(check-expect (all-within-width (list "d" "22" "333" "fgfg") 3) false)

;(define (all-within-width los n) false) ;stub

#;
(define (all-within-width los n)
  ;(String -> Boolean)
  (andmap ...                 los))

(define (all-within-width los n)
  (andmap (lambda (s)
            (<= (string-length s) n))
          los))
