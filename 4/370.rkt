;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |370|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

(define a0 '(initial "X"))
(define a1 '(corona 3))

(define loa0 `(,a0 ,a1))

; An XWord is '(word ((text String)))

(define XW0 '(word ((text "test1"))))
(define XW1 '(word ((text "test2"))))
(define XW2 '(word ((text "test3"))))

; Template ???

; ??? -> Boolean
; is x an  XWord
(check-expect (word? XW0) true)
(check-expect (word? 1) false)
(check-expect (word? "d") false)
(check-expect (word? 'd) false)
(check-expect (word? '(wrd)) false)
(check-expect (word? '(wrd ((text "test1")))) false)
(check-expect (word? '(word ((txt "test1")))) false)
(check-expect (word? '(word ((text 1)))) false)
(check-expect (word? '(word ((text dd)))) false)

;(define (word? x) false) ;stub

(define (word? x)
  (and (cons? x)
       (= 2 (length x))
       (local ((define word-tag (first x)))
         (and (symbol? word-tag)
              (symbol=? word-tag 'word)))
       (local ((define attrib-list (first (rest x))))
         (and (cons? attrib-list)
              (= 1 (length attrib-list))))
       (local ((define attrib (first (first (rest x)))))
         (and (cons? attrib)
              (= 2 (length attrib))
              (symbol=? (first attrib) 'text)
              (string? (second attrib))))))

; XWord -> String
; extracts the value of the only attribute of xw
(check-expect (word-text XW0) "test1")
(check-expect (word-text XW1) "test2")

;(define (word-text xw) "") ;stub

(define (word-text xw)
  (find-attr (second xw) 'text))

; [List-of Attribute] Symbol -> String or false
(check-expect (find-attr empty 'dood) false)
(check-expect (find-attr loa0 'initial) "X")
(check-expect (find-attr loa0 'corona) false)
(check-expect (find-attr loa0 'dad) false)

;(define (find-attr loa s) false) ;stub

(define (find-attr loa s)
  (local ((define found-attrib (assq s loa)))
    (if (and (cons? found-attrib) (string? (second found-attrib)))
        (second found-attrib)
        false)))
