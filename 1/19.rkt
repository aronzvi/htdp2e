;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |19|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define INSERTED_STR "_")
(define (string-insert str i) (if ( = (string-length str) 0) INSERTED_STR (string-append (substring str 0 i) INSERTED_STR (substring str i))))