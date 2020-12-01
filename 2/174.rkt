;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |174|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

;; =======================
;; Data definitions:

;; ListOfString is one of:
;; - empty
;; - (cons String ListOfString)
;; interp. a list of strings
(define LINE0 empty)
(define LINE1 (cons "BOOO" empty))
(define LINE2 (cons "hello" (cons "world" empty)))
(define LINE3 (cons "my" (cons "name" (cons "is" (cons "Aron" empty)))))

(define (fn-for-los los)
  (cond
    [(empty? los) (...)]
    [else
     (... (first los)                 ;String
          (fn-for-los (rest los)))])) ;ListOfString

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons String ListOfString)
;; - self-reference: (rest los) is ListOfString

;; ListOf1String is one of:
;; - empty
;; - (cons 1String ListOf1String)
;; interp. a list of strings
(define LO1S1 empty)
(define LO1S2 (cons "B" (cons "O" (cons "o" empty))))

(define (fn-for-lo1s lo1s)
  (cond
    [(empty? lo1s) (...)]
    [else
     (... (first lo1s)                  ;1String
          (fn-for-lo1s (rest lo1s)))])) ;ListOf1String

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons 1String ListOf1String)
;; - self-reference: (rest lo1s) is ListOf1String

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

;; =======================
;; Functions:

;; String -> String
;; encodes given text file n numerically. Each letter in a word is encoded as a numeric three-letter string with a value between 0 and 256.
;; outputs encoded file to "encoded-" n.
(check-expect (encode-file "ttt.txt") "encoded-ttt.txt")

;(define (encode-file n) "") ;stub

(define (encode-file n)
  (write-file (string-append "encoded-" n)
              (collapse (encode-lines (read-words/line n)))))

;; LLS -> LLS
;; encodes lines numerically. Each letter in a word is encoded as a numeric three-letter string with a value between 0 and 256.
(check-expect (encode-lines empty) empty)
(check-expect (encode-lines (cons LINE1 (cons LINE0 (cons LINE2 empty))))
              (cons (cons "066079079079" empty)
                    (cons empty
                          (cons (cons "104101108108111"
                                      (cons "119111114108100" empty))
                                empty))))

; (define (encode-lines lines) empty) ;stub

(define (encode-lines lines)
  (cond
    [(empty? lines) empty]
    [else
     (cons (encode-line (first lines))    
           (encode-lines (rest lines)))]))

;; ListOfString -> ListOfString
;; encodes a line numerically.  Each letter in a word is encoded as a numeric three-letter string with a value between 0 and 256.
(check-expect (encode-line empty) empty)
(check-expect (encode-line LINE2) (cons "104101108108111"
                                        (cons "119111114108100" empty)))

;(define (encode-line line) empty) ;stub

(define (encode-line line)
  (cond
    [(empty? line) empty]
    [else
     (cons (encode-string (first line))                
           (encode-line (rest line)))]))

;; String -> String
;; encodes a string numerically. Each letter is encoded as a numeric three-letter string with a value between 0 and 256.
(check-expect (encode-string "BOOO") "066079079079")
(check-expect (encode-string "hello") "104101108108111")

;(define (encode-string str) "") ;stub

;(define (encode-string str) (... str)) ;Template

(define (encode-string str)
  (strings-to-string (encode-letters (explode str))))

;; ListOf1String -> ListOfString
;; encodes a list of 1Strings. Each letter is encoded as a numeric three-letter string with a value between 0 and 256.
(check-expect (encode-letters empty) empty)
(check-expect (encode-letters LO1S2) (cons "066" (cons "079" (cons "111" empty))))

;(define (encode-letters letters) empty) ;stub

(define (encode-letters letters)
  (cond
    [(empty? letters) empty]
    [else
     (cons (encode-letter (first letters))                  
           (encode-letters (rest letters)))])) 

;; ListOfString -> String
;; produces a single string from a list of strings
(check-expect (strings-to-string empty) "")
(check-expect (strings-to-string (cons "hello" (cons "boob" empty))) "helloboob")

;(define (strings-to-string strs) "") ;stub

(define (strings-to-string strings)
  (cond
    [(empty? strings) ""]
    [else
     (string-append (first strings)                 
                    (strings-to-string (rest strings)))])) 

; 1String -> String
; converts the given 1String to a 3-letter numeric String
 
(check-expect (encode-letter "z") (code1 "z"))
(check-expect (encode-letter "\t")
              (string-append "00" (code1 "\t")))
(check-expect (encode-letter "a")
              (string-append "0" (code1 "a")))
 
(define (encode-letter s)
  (cond
    [(>= (string->int s) 100) (code1 s)]
    [(< (string->int s) 10)
     (string-append "00" (code1 s))]
    [(< (string->int s) 100)
     (string-append "0" (code1 s))]))
 
; 1String -> String
; converts the given 1String into a String
 
(check-expect (code1 "z") "122")
 
(define (code1 c)
  (number->string (string->int c)))

;; LLS -> String
;; converts a list of lines of string into a string. The strings are separated " ". The lines are separated with "\n"
(check-expect (collapse empty) "")
(check-expect (collapse LLS1) "BOOO\n\nhello world\nmy name is Aron")

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