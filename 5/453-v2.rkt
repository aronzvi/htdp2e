;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 453-v2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Line is a [List-of 1String]

; A LineLeadingWord is a Line whose first element is an alphabetic character
(define LineLeadingWord0 (explode "hello\n"))
(define LineLeadingWord1 (explode "a, hello\n"))

; a 1StringToken is a non-letter, non-whitespace 1String

(define 1StringToken0 ",")
(define 1StringToken1 "1")

; a Word is a [List-of 1String] of letters and nothing else

(define Word0 (explode "hello"))
(define Word1 '("h"))

; a Token is one of:
; - 1StringToken 
; - Word

; Line -> [List-of Token]
; turns a Line into a list of tokens using generative recursion
; - all white-space 1Strings are dropped
; - all other non-letters remain as is
; - all consecutive letters are bundled into “words.”
; we get the next string or 1string token and cons it onto the rest of the tokens
; we generate a new, smaller string by removing the token that we got from the original string and tokenizing this new smaller string
; trivial case: when the given string is whitespace. ("" is also whitespace) for which we produce '()
; termination: we collect all tokens (and drop whitespace) and make the string smaller as we go along until the string is eventually whitespace ("" is also white space)
(check-expect (tokenize empty) '())
(check-expect (tokenize (explode "\t\r\n")) '())
(check-expect (tokenize (explode "\thello world\n")) `(,(explode "hello") ,(explode "world")))
(check-expect (tokenize (explode "\rhello, \t$ world!\n")) `(,(explode "hello") "," "$" ,(explode "world") "!"))
(check-expect (tokenize (explode "\t\rhello123world 45 !\n")) `(,(explode "hello") "1" "2" "3" ,(explode "world") "4" "5" "!"))
;(check-expect (tokenize "\tEhello world\n") '("E" "hello" "world"))

;(define (tokenize l) empty) ;stub

(define (tokenize l)
  (cond [(string-whitespace? (implode l)) '()]
        [else
         (cons (get-next-token l)
               (tokenize (remove-next-token l)))]))

; Line -> Token
; produces next Token
; drops any leading whitespace 
(check-expect (get-next-token '()) '())
(check-expect (get-next-token (explode "\n\t\r")) '())
(check-expect (get-next-token (explode "\thello world\n")) (explode "hello"))
(check-expect (get-next-token (explode "\t,2hello world\n")) ",")
(check-expect (get-next-token (explode "\t2hello world\n")) "2")

;(define (get-next-token l) "") ;stub

(define (get-next-token l)
  (drop-whitespace-and-do-for-next-token l
                                         first
                                         get-next-word))

; 2do: sugnature, purpose, headers and tests

;(check-expect (get-next-token '() ) '())
;(check-expect (get-next-token (explode "\n\t\r") ) '())
;(check-expect (get-next-token (explode "\thello world\n")) (explode "hello"))
;(check-expect (get-next-token (explode "\t,2hello world\n")) ",")
;(check-expect (get-next-token (explode "\t2hello world\n")) "2")
;(check-expect (remove-next-token '()) '())
;(check-expect (remove-next-token (explode "\n\t\r")) '())
;(check-expect (remove-next-token  (explode "\thello world\n")) (explode " world\n"))
;(check-expect (remove-next-token  (explode "\t,2hello world\n")) (explode "2hello world\n"))
;(check-expect (remove-next-token  (explode "\t2hello world\n")) (explode "hello world\n"))

(define (drop-whitespace-and-do-for-next-token l fn-1stringtoken fn-word)
  (local ((define l-no-leading-whitespace (drop-leading-whitespace l)))
    (cond [(empty? l-no-leading-whitespace) '()]
          [(1stringtoken? (first l-no-leading-whitespace)) (fn-1stringtoken l-no-leading-whitespace)]
          [else (fn-word l-no-leading-whitespace)])))

; Line -> Line
; removes all leading whitespace until the first non-whitespace character
(check-expect (drop-leading-whitespace empty) empty)
(check-expect (drop-leading-whitespace (explode "\t\r\n")) empty)
(check-expect (drop-leading-whitespace (explode "\t\r\nhell")) (explode "hell"))
(check-expect (drop-leading-whitespace (explode "\t\r\nhell123\n")) (explode "hell123\n"))

;(define (remove-all-leading-whitespace l) l) ;stub

(define (drop-leading-whitespace l)
  (cond [(empty? l) empty]
        [(not (string-whitespace? (first l))) l]
        [else
         (drop-leading-whitespace (rest l))]))

; 1String -> Boolean
; produuces true if given 1String is a 1StringToken
; any character that is neither whitespace, nor alphabetic
(check-expect (1stringtoken? "A") true)
(check-expect (1stringtoken? "1") true)
(check-expect (1stringtoken? ",") true)
(check-expect (1stringtoken? "a") false)
(check-expect (1stringtoken? "\n") false)

;(define (1stringtoken? s) false) ;stub

(define (1stringtoken? s)
  (not (or (string-whitespace? s) (string-lower-case? s))))

; LineLeadingWord -> [List-of 1String]
; produces list of consecuteve letters from the begining of the list until first whitespace or non lowercase character
(check-expect (get-next-word empty) empty)
(check-expect (get-next-word '("d")) '("d"))
(check-expect (get-next-word (explode "d1")) '("d"))
(check-expect (get-next-word (explode "d\n")) '("d"))
(check-expect (get-next-word (explode "hello world\n")) (explode "hello"))
(check-expect (get-next-word (explode "hello2 world\n")) (explode "hello"))

;(define (get-next-word l) l) ;stub

(define (get-next-word l)
  (cond [(empty? l) empty]
        [(not (string-alphabetic? (first l))) empty]
        [else
         (cons (first l)
               (get-next-word (rest l)))]))

; Line -> Line
; produces a Line like l without the first Token.
; drops all leading whitespace
(check-expect (remove-next-token '()) '())
(check-expect (remove-next-token (explode "\n\t\r")) '())
(check-expect (remove-next-token  (explode "\thello world\n")) (explode " world\n"))
(check-expect (remove-next-token  (explode "\t,2hello world\n")) (explode "2hello world\n"))
(check-expect (remove-next-token  (explode "\t2hello world\n")) (explode "hello world\n"))

;(define (remove-next-token l) "") ;stub

(define (remove-next-token l)
  (drop-whitespace-and-do-for-next-token l
                                         rest
                                         remove-next-word))

; LineLeadingWord -> Line
; removes consecuteve lowercase letters from the begining of the list until first whitespace or non lowercase character
(check-expect (remove-next-word empty) empty)
(check-expect (remove-next-word '("d")) empty)
(check-expect (remove-next-word (explode "d1")) '("1"))
(check-expect (remove-next-word (explode "d\n")) '("\n"))
(check-expect (remove-next-word (explode "hello world\n")) (explode " world\n"))
(check-expect (remove-next-word (explode "hello2 world\n")) (explode "2 world\n"))

;(define (remove-next-word l) empty) ;stub

(define (remove-next-word l)
  (cond [(empty? l) empty]
        [(or (string-whitespace? (first l)) (1stringtoken? (first l))) l]
        [else
         (remove-next-word (rest l))]))

