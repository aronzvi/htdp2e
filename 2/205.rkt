;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |205|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/itunes)

(define ITUNES-LOCATION "library.xml")
 
; LLists
(define list-tracks
  (read-itunes-as-lists ITUNES-LOCATION))

; A BSDN is one of: 
; – Boolean
; – Number
; – String
; – Date
 
(define BSDN1 #true)
(define BSDN2 12)
(define BSDN3 "File")
(define BSDN4 (create-date 1996 3 1 16 54 53))

(define (fn-for-bsdn bsdn)
  (cond
    [(boolean? bsdn) (... bsdn)]
    [(number? bsdn) (... bsdn)]
    [(string? bsdn) (... bsdn)]
    [else
      (... (date-year bsdn)
           (date-month bsdn)
           (date-day bsdn)
           (date-hour bsdn)
           (date-minute bsdn))]))

;; Template rules used:
;; - one of: 4 cases
;; - atomic non-distinct: Boolean
;; - atomic non-distinct: Number
;; - atomic non-distinct: String
;; - compound: 5 fields

; An Association is a list of two items: 
;   (cons String (cons BSDN '()))

(define ASSOC1 (list "Protected" #true ))
(define ASSOC2 (list "Play Count" 12)) 
(define ASSOC3 (list "Track Type" "File"))
(define ASSOC4 (list "Date Added" (create-date 1996 3 1 16 54 53)))
(define ASSOC5 (list "Protected" #false))
(define ASSOC6 (list "Play Count" 32))
(define ASSOC7 (list "Track Type" "File"))
(define ASSOC8 (list "Date Added" (create-date 2000 5 1 16 54 53)))

(define (fn-for-assoc assoc)
  (... (first assoc)
       (fn-for-bsdn (second assoc)))) ;BSDN

;; Template rules used:
;; - compound: 2 fields
;; - reference: (second assoc) is BSDN

; An LAssoc is one of: 
; – '()
; – (cons Association LAssoc)

(define LASSOC1 empty)
(define LASSOC2 (list ASSOC1 ASSOC2 ASSOC3 ASSOC4))
(define LASSOC3 (list ASSOC5 ASSOC6 ASSOC7 ASSOC8))

(define (fn-for-lassoc lassoc)
  (cond
     [(empty? lassoc) (...)]
     [else
      (... (fn-for-assoc (first lassoc))     ;Association
           (fn-for-lassoc (rest lassoc)))])) ;LAssoc

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Association LAssoc)
;; - reference: (first lassoc is Association)
;; - self-reference: (rest lassoc) is LAssoc

(define LLISTS1 empty)
(define LLISTS2 (list LASSOC2 LASSOC3))

(define (fn-for-llists llists)
  (cond
    [(empty? llists) (...)]
    [else
     (... (fn-for-lassoc (first llists))    ;LAssoc
          (fn-for-llists (rest llists)))])) ;LLists

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons LAssoc LLists)
;; - reference: (first lassoc is LAssoc)
;; - self-reference: (rest llists) is LLists
