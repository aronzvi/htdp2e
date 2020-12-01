;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |203|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

;; String Date LTracks -> LTracks
;; extracts the list of tracks from ltracks that belong to album and have been played after date
(check-expect (select-album-date "Bunny's worst" DATE4 empty) empty)
(check-expect (select-album-date "Bunny's worst" DATE4 LTRACKS3) (list TRACK1 TRACK2))
(check-expect (select-album-date "Bunny's worst" DATE3 LTRACKS3) (list TRACK1))

;(define (select-album-date album date ltracks) ltracks) ;stub

(define (select-album-date album date ltracks)
  (select-played-after date (select-album album ltracks)))

;; Date LTracks -> LTracks
;; extracts the list of tracks from ltracks that have been played after date
(check-expect (select-played-after DATE1 empty) empty)
(check-expect (select-played-after (create-date 2019 1 12 23 55 59) LTRACKS3) LTRACKS3)
(check-expect (select-played-after (create-date 2019 1 12 23 56 0) LTRACKS3) (list TRACK1 TRACK2))
(check-expect (select-played-after DATE2 LTRACKS3) empty)

;(define (select-played-after date ltracks) ltracks) ;stub

(define (select-played-after date ltracks)
  (cond
    [(empty? ltracks) empty]
    [else
     (if (track-played-after? date (first ltracks))
         (cons (first ltracks) (select-played-after date (rest ltracks)))
         (select-played-after date (rest ltracks)))]))

;; Date Track -> Boolean
;; produces true if track was played after date
(check-expect (track-played-after? DATE3 TRACK1) #true)
(check-expect (track-played-after? DATE2 TRACK1) #false)

;(define (track-played-after? date t) #false) ;stub

(define (track-played-after? date t)
  (before? date (track-played t)))

;; Date Date -> Boolean
;; produces true if d1 is before d2
(check-expect (before? (create-date 2019 1 12 23 55 59) (create-date 2019 1 12 23 56 0)) #true)
(check-expect (before? (create-date 2019 1 12 23 56 0) (create-date 2019 1 12 23 55 59) ) #false)
(check-expect (before? (create-date 2019 1 12 23 56 0) (create-date 2019 1 12 23 56 0) ) #false)
(check-expect (before? (create-date 2015 7 4 16 28 0) (create-date 2019 1 12 23 56 0)) #true)
(check-expect (before? (create-date 2019 7 4 16 28 0) (create-date 2019 1 12 23 56 0)) #false)
(check-expect (before? (create-date 2019 7 4 16 28 0)  (create-date 2019 5 6 15 28 0)) #false)
(check-expect (before? (create-date 2020 7 4 16 28 0)  (create-date 2019 5 6 15 28 0)) #false)
(check-expect (before? (create-date 2019 5 8 16 28 0)  (create-date 2019 5 6 15 28 0)) #false)
(check-expect (before? (create-date 2019 5 5 16 28 0)  (create-date 2019 5 6 15 28 0)) #true)
(check-expect (before? (create-date 2019 5 6 16 28 0)  (create-date 2019 5 6 15 28 0)) #false)
(check-expect (before? (create-date 2019 5 6 14 28 0)  (create-date 2019 5 6 15 28 0)) #true)
(check-expect (before? (create-date 2019 5 6 14 28 0)  (create-date 2019 5 6 14 28 1)) #true)

;(define (before? d1 d2) #false) ;stub


#;
(define (before? d1 d2)
  (or
   (< (date-year d1) (date-year d2))
   (and (= (date-year d1) (date-year d2))
        (< (date-month d1) (date-month d2)))
   (and (= (date-year d1) (date-year d2))
        (= (date-month d1) (date-month d2))
        (< (date-day d1) (date-day d2)))
   (and (= (date-year d1) (date-year d2))
        (= (date-month d1) (date-month d2))
        (= (date-day d1) (date-day d2))
        (< (date-hour d1) (date-hour d2)))
   (and (= (date-year d1) (date-year d2))
        (= (date-month d1) (date-month d2))
        (= (date-day d1) (date-day d2))
        (= (date-hour d1) (date-hour d2))
        (< (date-minute d1) (date-minute d2)))
   (and (= (date-year d1) (date-year d2))
        (= (date-month d1) (date-month d2))
        (= (date-day d1) (date-day d2))
        (= (date-hour d1) (date-hour d2))
        (= (date-minute d1) (date-minute d2))
        (< (date-second d1) (date-second d2)))))

(define (before? d1 d2)
  (cond
    [(> (date-year d1) (date-year d2)) #false]
    [(< (date-year d1) (date-year d2)) #true]
    [(> (date-month d1) (date-month d2)) #false]   ; (date-year d1) == (date-year d2) 
    [(< (date-month d1) (date-month d2)) #true] 
    [(> (date-day d1) (date-day d2)) #false]       ; (date-month d1) == (date-month d2)
    [(< (date-day d1) (date-day d2)) #true]
    [(> (date-hour d1)  (date-hour d2)) #false]    ; (date-day d1) == (date-day d2)
    [(< (date-hour d1)  (date-hour d2)) #true]
    [(< (date-minute d1) (date-minute d2)) #true]  ; (date-hour d1)  == (date-hour d2)
    [(> (date-minute d1) (date-minute d2)) #false]
    [(< (date-second d1)  (date-second d2)) #true] 
    [else #false]))                                ; (date-minute d1) == (date-minute d2) 



    


