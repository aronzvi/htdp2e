;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 453-v3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Line is a [List-of 1String]

; A LineLeadingAlphabetic is a Line whose first element is an alphabetic character
(define LineLeadingAlphabetic0 (explode "hello\n"))
(define LineLeadingAlphabetic1 (explode "a, hello\n"))

; a 1StringToken is a non-letter, non-whitespace 1String

(define 1StringToken0 ",")
(define 1StringToken1 "1")

; a Word is a String whose characters are letters and nothing else

(define Word0 "hello")
(define Word1 "h")

; a Token is one of:
; - 1StringToken 
; - Word

; Line -> [List-of Token]
; turns a Line into a list of tokens using generative recursion
; - all white-space is dropped
; - all other non-letters remain as is
; - all consecutive letters are bundled into “words.”
; we get a 1StringToken or Word and cons it onto the rest of the tokens
; we generate a new, smaller Line by removing the token that we got from the original Line and tokenizing this new smaller string
; trivial case:
;    1. when the given Line is empty for which we produce '()
;    2. when the given Line is all whitespace characters for which we produce '()
; termination: we collect all tokens (and drop whitespace) and make the Line smaller as we go along until the Line is eventually empty or all whitespace characters
(check-expect (tokenize empty) '())
(check-expect (tokenize (explode "\t\r\n")) '())
(check-expect (tokenize (explode "\thello world\n")) '("hello" "world"))
(check-expect (tokenize (explode "\rhello, \t$ world!\n")) '("hello" "," "$" "world" "!"))
(check-expect (tokenize (explode "\t\rhello123world 45 !\n"))  '("hello" "1" "2" "3" "world" "4" "5" "!"))
(check-expect (tokenize (explode "\tEhello world\n"))  '("Ehello" "world"))

;(define (tokenize l) empty) ;stub

(define (tokenize l)
  (local ((define l-no-leading-whitespace (drop-leading-whitespace l)))
    (cond [(empty? l) '()]
          [(empty? l-no-leading-whitespace) '()] ; all whitespace
          [(1stringtoken? (first l-no-leading-whitespace))
           (cons (first l-no-leading-whitespace)
                 (tokenize (rest l-no-leading-whitespace)))]
          [else
           (cons (get-next-word l-no-leading-whitespace)
                 (tokenize (remove-next-word l-no-leading-whitespace)))])))

; Line -> Line
; removes all leading whitespace until the first non-whitespace character
(check-expect (drop-leading-whitespace empty) empty)
(check-expect (drop-leading-whitespace (explode "\t\r\n")) empty)
(check-expect (drop-leading-whitespace (explode "\t\r\nhell")) (explode "hell"))
(check-expect (drop-leading-whitespace (explode "\t\r\nhell123\n")) (explode "hell123\n"))

;(define (drop-leading-whitespace l) l) ;stub

(define (drop-leading-whitespace l)
  (cond [(empty? l) empty]
        [(not (string-whitespace? (first l))) l]
        [else
         (drop-leading-whitespace (rest l))]))

; 1String -> Boolean
; produces true if given 1String is a 1StringToken
; any character that is neither whitespace, nor alphabetic
(check-expect (1stringtoken? "A") false)
(check-expect (1stringtoken? "1") true)
(check-expect (1stringtoken? ",") true)
(check-expect (1stringtoken? "a") false)
(check-expect (1stringtoken? "\n") false)

;(define (1stringtoken? s) false) ;stub

(define (1stringtoken? s)
  (not (or (string-whitespace? s) (string-alphabetic? s))))

; LineLeadingAlphabetic -> Word
; produces a Word from the begining of the list until first whitespace or non lowercase character
(check-expect (get-next-word empty) "")
(check-expect (get-next-word '("d")) "d")
(check-expect (get-next-word (explode "d1")) "d")
(check-expect (get-next-word (explode "d\n")) "d")
(check-expect (get-next-word (explode "hello world\n")) "hello")
(check-expect (get-next-word (explode "hello2 world\n")) "hello")

;(define (get-next-word l) "") ;stub

(define (get-next-word l)
  (local ((define (get-next-word l)
            (cond [(empty? l) empty]
                  [(word-ended? (first l)) empty]
                  [else
                   (cons (first l)
                         (get-next-word (rest l)))])))
    (implode (get-next-word l))))

; LineLeadingAlphabetic -> Line
; removes consecutive lowercase letters from the begining of the list until first whitespace or non lowercase character
(check-expect (remove-next-word empty) empty)
(check-expect (remove-next-word '("d")) empty)
(check-expect (remove-next-word (explode "d1")) '("1"))
(check-expect (remove-next-word (explode "d\n")) '("\n"))
(check-expect (remove-next-word (explode "hello world\n")) (explode " world\n"))
(check-expect (remove-next-word (explode "hello2 world\n")) (explode "2 world\n"))

;(define (remove-next-word l) empty) ;stub

(define (remove-next-word l)
  (cond [(empty? l) empty]
        [(word-ended? (first l)) l] 
        [else
         (remove-next-word (rest l))]))

; 1String -> Boolean
; produces true if we have reached the end of a Word
; we encountered either whitespace or a non alphabetic character
(check-expect (word-ended? "\n") true)
(check-expect (word-ended? "1") true)
(check-expect (word-ended? "a") false)
(check-expect (word-ended? "A") false)

;(define (word-ended? s) false) ;stub

(define (word-ended? s)
  (or (string-whitespace? s) (1stringtoken? s)))