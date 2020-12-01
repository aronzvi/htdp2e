;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |452|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; File -> [List-of Line]
; converts a file into a list of lines 
 (check-expect (file->list-of-lines
                (list "a" "b" "c" "\n"
                      "d" "e" "\n"
                      "f" "g" "h" "\n"))
              (list (list "a" "b" "c")
                    (list "d" "e")
                    (list "f" "g" "h")))
(check-expect (file->list-of-lines '()) empty)
(check-expect (file->list-of-lines (list "\n"))  (list empty))
(check-expect (file->list-of-lines (list "\n" "\n"))  (list empty empty))

;(define (file->list-of-lines afile) '()) ;stub

(define (file->list-of-lines afile)
  (cond
    [(empty? afile) '()]
    [else
     (cons (first-line afile)
           (file->list-of-lines (remove-first-line afile)))]))
 
; File -> Line
; collects all 1Strings in afile up to, but excluding, the first occurrence of "\n" or the end of the list 
(define (first-line afile)
  (cond
    [(empty? afile) '()]
    [(string=? (first afile) NEWLINE) '()]
    [else (cons (first afile) (first-line (rest afile)))]))
 
; File -> Line
; removes all the 1Strings up to and including the first occurrence of "\n" or the end of the list  
(define (remove-first-line afile)
  (cond
    [(empty? afile) '()]
    [(string=? (first afile) NEWLINE) (rest afile)]
    [else (remove-first-line (rest afile))]))
 
(define NEWLINE "\n") ; the 1String 
