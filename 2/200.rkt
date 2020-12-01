;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |200|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/itunes)

(define DATE1 (create-date 2019 1 12 23 56 0))
(define DATE2 (create-date 2019 7 4 16 28 0))
(define DATE3 (create-date 2019 5 6 15 28 0))
(define DATE4 (create-date 1983 2 11 12 1 23))

(define (fn-for-date d)
  (... (date-year d)
       (date-month d)
       (date-day d)
       (date-hour d)
       (date-minute d)
       (date-second d)))

;; Template rules used:
;; - compound: 6 fields

(define TRACK1 (create-track "Bobojangles" "Bad bunny" "Bunny's worst" 180000 4 DATE1 43 DATE2))
(define TRACK2 (create-track "Bunnies are forever" "Bad bunny" "Bunny's worst" 150000 2 DATE1 43 DATE3))
(define TRACK3 (create-track "What to do next" "Azee" "Lost" 162000 1 DATE4 43 DATE1))

(define (fn-for-track t)
  (... (track-name t)
       (track-artist t)
       (track-album t)
       (track-time t)
       (track-track# t)
       (fn-for-date (track-added t))   ;Date
       (track-play# t)
       (fn-for-date (track-played t)))) ;Date

;; Template rules used:
;; - compound: 8 fields
;; - reference: added field is Date
;; - reference: played field is Date

(define LTRACKS1 empty)
(define LTRACKS2 (list TRACK1 TRACK2))
(define LTRACKS3 (list TRACK1 TRACK2 TRACK3))

(define (fn-for-ltracks ltracks)
  (cond
    [(empty? ltracks) (...)]
    [else
     (... (fn-for-track (first ltracks))     ;Track
          (fn-for-ltracks (rest ltracks)))])) ;Ltrack

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Track LTrack)
;; - reference: (first ltrack) is Track
;; - self-reference: (rest ltrack) is LTrack

;; Functions:

;; LTracks -> N
;; produces the total amount of play time of tracks in ltracks
(check-expect (total-time empty) 0)
(check-expect (total-time LTRACKS2) (+ 180000 150000 0))
(check-expect (total-time LTRACKS3) (+ 180000 150000 162000 0))

;(define (total-time ltracks) 0) ;stub

(define (total-time ltracks)
  (cond
    [(empty? ltracks) 0]
    [else
     (+ (track-time (first ltracks))   
        (total-time (rest ltracks)))])) 