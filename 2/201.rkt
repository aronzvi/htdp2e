;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |201|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/itunes)

(define ITUNES-LOCATION "library.xml")
 
; LTracks
(define itunes-tracks
  (read-itunes-as-tracks ITUNES-LOCATION))

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

(define (fn-for-los los)
  (cond
    [(empty? los) (...)]
    [else
     (... (first los)
          (fn-for-los (rest los)))])) ;List-of-strings

;; Template rules used:
;; - atomic distinct: empty
;; - compound: (cons String List-of-strings)
;; - self reference: (rest los) is List-of-strings

;; Functions:

;; LTracks -> List-of-strings
;; produces the list of album titles from ltracks
(check-expect (select-all-album-titles empty) empty)
(check-expect (select-all-album-titles LTRACKS2) (list "Bunny's worst" "Bunny's worst"))
(check-expect (select-all-album-titles LTRACKS3) (list "Bunny's worst" "Bunny's worst" "Lost"))

;(define (select-all-album-titles ltracks) empty) ;stub

(define (select-all-album-titles ltracks)
  (cond
    [(empty? ltracks) empty]
    [else
     (cons (track-album (first ltracks))     
           (select-all-album-titles (rest ltracks)))]))

;; List-of-strings -> List-of-strings
;; produces a list of strings that contains every String from los exactly once
(check-expect (create-set empty) empty)
(check-expect (create-set (list "Bunny's worst")) (list "Bunny's worst"))
(check-expect (create-set (list "Bunny's worst" "Bunny's worst")) (list "Bunny's worst"))
(check-expect (create-set (list "Bunny's worst" "Bunny's worst" "Lost")) (list "Bunny's worst" "Lost"))

;(define (create-set los) los) ;stub

(define (create-set los)
  (cond
    [(empty? los) empty]
    [else
     (if (member? (first los) (create-set (rest los)))
         (create-set (rest los))
         (cons (first los) (create-set (rest los))))]))

;; LTracks -> List-of-strings
;; produces a list of unique album titles from given ltracks
(check-expect (select-album-titles/unique empty) empty)
(check-expect (select-album-titles/unique LTRACKS2) (list "Bunny's worst"))
(check-expect (select-album-titles/unique LTRACKS3) (list "Bunny's worst" "Lost"))

;(define (select-album-titles/unique ltracks) empty) ;stub

(define (select-album-titles/unique ltracks)
  (create-set (select-all-album-titles ltracks)))

(select-album-titles/unique itunes-tracks)
(length (select-album-titles/unique itunes-tracks))