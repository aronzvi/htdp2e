;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |396|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/batch-io)

(define LOCATION "/usr/share/dict/words") ; on OS X
(define AS-LIST (read-lines LOCATION))
(define SIZE (length AS-LIST))

; A Letter is a member of the below LETTERS list
(define LETTERS (explode "abcdefghijklmnopqrstuvwxyz"))

; An HM-Word is a [List-of Letter or "_"]
; interpretation "_" represents a letter to be guessed 
 
; String N -> String
; runs a simplistic hangman game, produces the current state
(define (play the-pick time-limit)
  (local ((define the-word  (explode the-pick))
          (define the-guess (make-list (length the-word) "_"))
          ; HM-Word -> HM-Word
          (define (do-nothing s) s)
          ; HM-Word KeyEvent -> HM-Word 
          (define (checked-compare current-status ke)
            (if (member? ke LETTERS)
                (compare-word the-word current-status ke)
                current-status)))
    (implode
     (big-bang the-guess ; HM-Word
       [to-draw render-word]
       [on-tick do-nothing 1 time-limit]
       [on-key  checked-compare]))))
 
; HM-Word -> Image
(define (render-word w)
  (text (implode w) 22 "black"))

; HM-Word HM-Word Letter -> HM-Word
; produces s, replacing the "_" with guess where guess revealed a letter in current
(check-expect (compare-word empty empty "b")
              empty)
(check-expect (compare-word (list "b") (list "_") "b")
              (list "b"))
(check-expect (compare-word (list "b") (list "_") "z")
              (list "_"))
(check-expect (compare-word (list "b") (list "b") "b")
              (list "b"))
(check-expect (compare-word (list "b" "o" "o") (list "_" "_" "_") "o")
              (list "_" "o" "_"))
(check-expect (compare-word (list "b" "o" "o") (list "_" "_" "_") "z")
              (list "_" "_" "_"))
(check-expect (compare-word (list "b" "o" "o") (list "b" "o" "o") "o")
              (list "b" "o" "o"))
(check-expect (compare-word (list "b" "o" "o") (list "b" "_" "o") "o")
              (list "b" "o" "o"))

;(define (compare-word word current guess) word) ;stub

; using regular list template on HM-Word and processing word and current simultaneously
#;
(define (compare-word word current guess)
  (cond [(empty? word) (...)]
        [else
         (... (first word)    ;Letter
              (first current) ;Letter or "_"
              (compare-word (rest word) ... guess)
              (compare-word (rest current) ... guess))]))

(define (compare-word word current guess)
  (cond [(empty? word) empty]
        [else
         (if (and (string=? (first current) "_")
                  (string=? (first word) guess))
             (cons guess (rest current))
             (cons (first current) (compare-word (rest word) (rest current) guess)))]))
