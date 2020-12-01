;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |453|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; a Token is one of:
; - 1String (not white space)
; - String (lower-case letters and nothing else)

; String -> [List-of Token]
; turns a Line into a list of tokens
; all white-space 1Strings are dropped
; all other non-letters remain as is
; and all consecutive letters are bundled into “words.”
(check-expect (tokenize "") '())
(check-expect (tokenize "\t\r\n") '())
(check-expect (tokenize "\thello world\n") '("hello" "world"))
(check-expect (tokenize "\rhello, \t$ world!\n") '("hello" "," "$" "world" "!"))
(check-expect (tokenize "\t\rhello123world 45 !\n") '("hello" "1" "2" "3" "world" "4" "5" "!"))
(check-expect (tokenize "\tEhello world\n") '("E" "hello" "world"))

;(define (tokenize s) empty) ;stub

(define (tokenize s)
  (cond [(string-whitespace? s) '()]
        [else
         (cons (get-next-1string-or-string s)
               (tokenize (remove-next-1string-or-string s)))]))

; String -> 1String or String
; produces next 1String or String of consecutive letters.
; drops any leading whitespace 
(check-expect (get-next-1string-or-string "") "")
(check-expect (get-next-1string-or-string "\n\t\r") "")
(check-expect (get-next-1string-or-string  "\thello world\n") "hello")
(check-expect (get-next-1string-or-string  "\t,2hello world\n") ",")
(check-expect (get-next-1string-or-string  "\t2hello world\n") "2")

;(define (get-next-1string-or-string s) s) ;stub

(define (get-next-1string-or-string s)
  (local ((define s-list (explode s))
          (define s-list-no-leading-whitespace (remove-all-leading-whitespace s-list)))
    (cond [(empty? s-list-no-leading-whitespace) ""]
          [(1string-token? (first s-list-no-leading-whitespace)) (first s-list-no-leading-whitespace)]
          [else (implode (get-next-string s-list-no-leading-whitespace))])))

; [List-of 1String] -> String
; removes all leading whitespace until the first non-whitespace character
(check-expect (remove-all-leading-whitespace empty) empty)
(check-expect (remove-all-leading-whitespace (explode "\t\r\n")) empty)
(check-expect (remove-all-leading-whitespace (explode "\t\r\nhell")) (explode "hell"))
(check-expect (remove-all-leading-whitespace (explode "\t\r\nhell123\n")) (explode "hell123\n"))

;(define (remove-all-leading-whitespace lo1s) lo1s) ;stub

(define (remove-all-leading-whitespace lo1s)
  (cond [(empty? lo1s) empty]
        [(not (string-whitespace? (first lo1s))) lo1s]
        [else
         (remove-all-leading-whitespace (rest lo1s))]))

#;
(define (remove-all-leading-whitespace lo1s)
  (cond [(empty? lo1s) empty]
        [else
         (if (string-whitespace? (first lo1s))
             (remove-all-leading-whitespace (rest lo1s))
             lo1s)]))

; 1String -> Boolean
; produuces true if s is a 1String token
; any charachter that is neither whitespace, nor lowercase letter
(check-expect (1string-token? "A") true)
(check-expect (1string-token? "1") true)
(check-expect (1string-token? ",") true)
(check-expect (1string-token? "a") false)
(check-expect (1string-token? "\n") false)

;(define (1string-token? s) false) ;stub

(define (1string-token? s)
  (not (or (string-whitespace? s) (string-lower-case? s))))

; [List-of 1String] -> [List-of 1String]
; produces list of consecuteve lowercase letters from the begining of the list until first whitespace or non lowercase character
; assumes that lo1s starts with niether whitespace nor non lowercase letter 
(check-expect (get-next-string empty) empty)
(check-expect (get-next-string '("d")) '("d"))
(check-expect (get-next-string (explode "d1")) '("d"))
(check-expect (get-next-string (explode "d\n")) '("d"))
(check-expect (get-next-string (explode "hello world\n")) (explode "hello"))
(check-expect (get-next-string (explode "hello2 world\n")) (explode "hello"))

;(define (get-next-string lo1s) lo1s) ;stub

; need 3 clauses here??
(define (get-next-string lo1s)
  (cond [(empty? lo1s) empty]
        [(not (string-lower-case? (first lo1s))) empty]
        [else
         (cons (first lo1s)
               (get-next-string (rest lo1s)))]))

; String -> String
; produces a string from s without the first 1String or String of consecutive letters.
; drops all leading whitespace
(check-expect (remove-next-1string-or-string "") "")
(check-expect (remove-next-1string-or-string "\n\t\r") "")
(check-expect (remove-next-1string-or-string  "\thello world\n") " world\n")
(check-expect (remove-next-1string-or-string  "\t,2hello world\n") "2hello world\n")
(check-expect (remove-next-1string-or-string  "\t2hello world\n") "hello world\n")

;(define (remove-next-1string-or-string s) "") ;stub

(define (remove-next-1string-or-string s)
  (local ((define s-list (explode s))
          (define s-list-no-leading-whitespace (remove-all-leading-whitespace s-list)))
    (cond [(empty? s-list-no-leading-whitespace) ""]
          [(1string-token? (first s-list-no-leading-whitespace)) (implode (rest s-list-no-leading-whitespace))]
          [else (implode (remove-next-string s-list-no-leading-whitespace))])))

; [List-of 1String] -> [List-of 1String]
; removes consecuteve lowercase letters from the begining of the list until first whitespace or non lowercase character
; assumes that lo1s starts with niether whitespace nor non lowercase letter
(check-expect (remove-next-string empty) empty)
(check-expect (remove-next-string '("d")) empty)
(check-expect (remove-next-string (explode "d1")) '("1"))
(check-expect (remove-next-string (explode "d\n")) '("\n"))
(check-expect (remove-next-string (explode "hello world\n")) (explode " world\n"))
(check-expect (remove-next-string (explode "hello2 world\n")) (explode "2 world\n"))

;(define (remove-next-string lo1s) empty) ;stub

(define (remove-next-string lo1s)
  (cond [(empty? lo1s) empty]
        [(or (string-whitespace? (first lo1s)) (1string-token? (first lo1s))) lo1s]
        [else
         (remove-next-string (rest lo1s))]))

        