;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |308|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

(define-struct phone [area switch four])
; A PR is (make-phone Natural Natural Natural)

(define PR0 (make-phone 713 664 9993))

; [List-of PR] -> PR
; substitutes the area code 713 with 281 in a list of phone records
(check-expect (replace empty) empty)
(check-expect (replace (list (make-phone 123 456 777) (make-phone 713 456 777)))
              (list (make-phone 123 456 777) (make-phone 281 456 777)))
(check-expect (replace (list (make-phone 123 456 777) (make-phone 714 456 777)))
              (list (make-phone 123 456 777) (make-phone 714 456 777)))
              
;(define (replace lopr) lopr) ;stub

#;
(define (replace lopr)
  (for/list ((p lopr))
    (match p
      [(phone 713 s f) (make-phone 281 s f)]
      [(phone a s f) (make-phone a s f)])))

(define (replace lopr)
  (for/list ((p lopr))
    (match p
      [(phone 713 s f) (make-phone 281 s f)]
      [x p]))) 