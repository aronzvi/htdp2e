;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |175|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define-struct wc-res [letters words lines])
;; WCRes is (make-wc-res Number Number Number)
;; interp. The number of letters, words and lines in a file
(define WCR1 (make-wc-res 10 2 1))

(define (fn-for-wc-res wcr)
  (... (wc-res-letters wcr)
       (wc-res-words wcr)
       (wc-res-lines wcr)))

;; Template rules used:
;; - compound: 3 fields

;; =======================
;; Functions:

;; String -> WCRes
;; simulates the Unix command wc. produces the number of characters, words, and lines in a given file
(check-expect (wc "ttt.txt") (make-wc-res 183 33 13))

;(define (wc n) (make-wc-res 0 0 0)) ;stub

;(define (wc n)  (...)) ;Template

#;
(define (wc n) 
  (make-wc-res (count-letters-in-lines (read-words/line n))
               (count-words-in-lines (read-words/line n))
               (length (read-words/line n))))


(define (wc n) 
  (make-wc-res (length (read-1strings n))
               (length (read-words n))
               (length (read-words/line n))))

;; LLS -> Number
;; produces the number of letters in given lines
(check-expect (count-letters-in-lines empty) 0)
(check-expect (count-letters-in-lines LLS1) (+ 4 5 5 2 4 2 4))

;(define (count-letters lines) 0) ;stub

(define (count-letters-in-lines lines)
  (cond
    [(empty? lines) 0]
    [else
     (+ (count-letters-in-line (first lines))  
        (count-letters-in-lines (rest lines)))]))

;; ListOfString -> Number
;; produces the number of letters in given line not including \n
(check-expect (count-letters-in-line empty) 0)
(check-expect (count-letters-in-line LINE2) (+ 5 5))

;(define (count-letters-in-line line) 0) ;stub

(define (count-letters-in-line line)
  (cond
    [(empty? line) 0]
    [else
     (+ (string-length (first line))               
        (count-letters-in-line (rest line)))]))

;; LLS -> Number
;; produces the number of words in given lines
(check-expect (count-words-in-lines empty) 0)
(check-expect (count-words-in-lines LLS1) (+ 1 2 4))

;(define (count-words-in-lines lines) 0) ;stub

(define (count-words-in-lines lines)
  (cond
    [(empty? lines) 0]
    [else
     (+ (length (first lines)) 
        (count-words-in-lines (rest lines)))])) 

