;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |199|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/itunes)

(define DATE1 (create-date 2019 1 12 23 56 0))
(define DATE2 (create-date 2019 7 4 16 28 0))
(define DATE3 (create-date 2019 5 6 15 28 0))
(define DATE4 (create-date 1983 2 11 12 1 23))

(define TRACK1 (create-track "Bobojangles" "Bad bunny" "Bunny's worst" (* 1000 60 3) 4 DATE1 43 DATE2))
(define TRACK2 (create-track "Bunnies are forever" "Bad bunny" "Bunny's worst" (* 1000 60 2.5) 2 DATE1 43 DATE3))
(define TRACK3 (create-track "What to do next" "Azee" "Lost" (* 1000 60 2.7) 1 DATE4 43 DATE1))

(define LTRACKS1 empty)
(define LTRACKS2 (list TRACK1 TRACK2 TRACK3))


