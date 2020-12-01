;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |202|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

;; String LTracks -> LTracks
;; extracts list of tracks from ltracks that belong to the given album
(check-expect (select-album "Bunny's worst" empty) empty)
(check-expect (select-album "Bunny's worst" LTRACKS3) LTRACKS2)
(check-expect (select-album "Lost" LTRACKS3) (list TRACK3))
(check-expect (select-album "Boobee" LTRACKS3) empty)

;(define (select-album album ltracks) ltracks) ;stub

(define (select-album album ltracks)
  (cond
    [(empty? ltracks) empty]
    [else
     (if (track-album=? (first ltracks) album)
         (cons (first ltracks) (select-album album (rest ltracks)))
         (select-album album (rest ltracks)))]))

;; Track String -> Boolean
;; produces true if track's album is equal to album
(check-expect (track-album=? TRACK1 "Bunny's worst") #true)
(check-expect (track-album=? TRACK1 "Googy's worst") #false)

;(define (track-album=? t album) #false) ;stub

(define (track-album=? t album)
  (string=? (track-album t) album))