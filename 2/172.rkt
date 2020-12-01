;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |172|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

;; Data definitions:

;; ListOfString is one of:
;; - empty
;; - (cons String ListOfString)
;; interp. a list of strings
(define LINE0 empty)
(define LINE1 (cons "BOOO" empty))
(define LINE2 (cons "hello" (cons "world" empty)))
(define LINE3 (cons "my" (cons "name" (cons "is" (cons "Aron" empty)))))



;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons String ListOfString)
;; - self-reference: (rest los) is ListOfString

(define (fn-for-los los)
  (cond
    [(empty? los) (...)]
    [else
     (... (first los)                 ;String
          (fn-for-los (rest los)))])) ;ListOfString

;; LLS is one of:
;; - empty
;; (cons ListOfString LLS)
;; interp. a list of ListOfString
(define LLS0 empty)
(define LLS1 (cons LINE1 (cons LINE0 (cons LINE2 (cons LINE3 empty)))))

(define (fn-for-lls lls)
  (cond
    [(empty? lls) (...)]
    [else
     (... (fn-for-los (first lls))    ;ListOfString
          (fn-for-lls (rest lls)))])) ;LLS

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons ListOfString ListOfListOfString)
;; - reference:  (first lolos) is ListOfString
;; - self-reference: (rest lolos) is LLS

;; LLS -> String
;; converts a list of lines into a string. The strings are separated " ". The lines are separated with "\n"
(check-expect (collapse empty) "")
(check-expect (collapse LLS1) "BOOO\n\nhello world\nmy name is Aron")
remove-articles
;(define (collapse lines) "") ;stub

(define (collapse lls)
  (cond
    [(empty? lls) ""]
    [else
     (if (= (length lls) 1)
         (string-append (collapse-line (first lls))     
                        (collapse (rest lls)))    
         (string-append (collapse-line (first lls)) "\n"    
                        (collapse (rest lls))))]))

;; ListOfString -> String
;; converts a list of words into a string. The strings are separated " "
(check-expect (collapse-line empty) "")
(check-expect (collapse-line LINE1) "BOOO")
(check-expect (collapse-line LINE2) "hello world")
(check-expect (collapse-line LINE3) "my name is Aron")

;(define (collapse-line words) "") ;stub

(define (collapse-line los)
  (cond
    [(empty? los) ""]
    [else
     (if (= (length los) 1)
         (string-append (first los)          
                        (collapse-line (rest los)))
         (string-append (first los) " "                 
                        (collapse-line (rest los))))]))

(write-file "ttt.dat"
            (collapse (read-words/line "ttt.txt")))
(check-expect (read-file "ttt.dat") (read-file "ttt.txt"))
