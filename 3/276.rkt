;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |276|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/itunes)

(define ITUNES-LOCATION "library.xml")
 
; LTracks
(define itunes-tracks
  (read-itunes-as-tracks ITUNES-LOCATION))

(define DATE1 (create-date 2019 1 12 23 56 0))
(define DATE2 (create-date 2019 7 4 16 28 0))
(define DATE3 (create-date 2019 5 6 15 28 0))
(define DATE4 (create-date 1983 2 11 12 1 23))
(define DATE5 (create-date 2019 2 12 23 56 0))

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
(define TRACK4 (create-track "Oh dawggy" "Azee" "Lost" 162000 1 DATE4 43 DATE5))
(define TRACK5 (create-track "Little monkey" "Azee" "Lost Again" 162000 1 DATE4 43 DATE5))

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
(define LTRACKS4 (list TRACK1 TRACK2 TRACK3 TRACK4 TRACK5))

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

;; String Date LTracks -> LTracks
;; extracts the list of tracks from ltracks that belong to album and have been played after date
(check-expect (select-album-date "Bunny's worst" DATE4 empty) empty)
(check-expect (select-album-date "Bunny's worst" DATE4 LTRACKS3) (list TRACK1 TRACK2))
(check-expect (select-album-date "Bunny's worst" DATE3 LTRACKS3) (list TRACK1))

;(define (select-album-date album date ltracks) ltracks) ;stub

#;
(define (select-album-date album date ltracks)
  ;         (Track -> Boolean)
  (filter      ...                ltracks))

#;
(define (select-album-date album date ltracks)
  (local ((define albums-by-name ...) ;tracks that belong to album
          ;; (Track -> Boolean)
          ;; produces true if track was played after date
          (define (track-played-after-date? t) false))
    (filter    track-played-after-date?    albums-by-name)))


(define (select-album-date album date ltracks)
  (local (;; Track -> Boolean
          ;; produces true if track album is album
          (define (track-album=? t) (string=? (track-album t) album))

          ;tracks that belong to album
          (define tracks-by-album (filter track-album=? ltracks)) 

          ;; (Track -> Boolean)
          ;; produces true if track was played after date
          (define (track-played-after-date? t) (after? (track-played t) date)))
    (filter track-played-after-date? tracks-by-album)))

;; Date Date -> Boolean
;; produces true if d1 is after d2
(check-expect (after? (create-date 2019 1 12 23 56 0) (create-date 2019 1 12 23 55 59)) #true)
(check-expect (after? (create-date 2019 2 12 23 56 0) (create-date 2019 1 12 23 55 59)) #true)
(check-expect (after? (create-date 2019 2 12 23 56 0) (create-date 2018 1 12 23 55 59)) #true)
(check-expect (after? (create-date 2019 1 12 23 55 59) (create-date 2019 2 12 23 56 0)) #false)
(check-expect (after? (create-date 2018 1 12 23 55 59) (create-date 2019 2 12 23 56 0)) #false)
(check-expect (after? (create-date 2019 1 12 23 55 59) (create-date 2019 1 12 23 56 0)) #false)
(check-expect (after? (create-date 2019 1 12 23 56 0) (create-date 2019 1 12 23 56 50)) #false)

;(define (after? d1 d2) #false) ;stub

(define (after? d1 d2)
  (cond
    [(> (date-year d1) (date-year d2)) #true]
    [(< (date-year d1) (date-year d2)) #false]
    [(> (date-month d1) (date-month d2)) #true]
    [(< (date-month d1) (date-month d2)) #false]
    [(> (date-day d1) (date-day d2)) #true]
    [(< (date-day d1) (date-day d2)) #false]
    [(> (date-hour d1) (date-hour d2)) #true]
    [(< (date-hour d1) (date-hour d2)) #false]
    [(> (date-minute d1) (date-minute d2)) #true]
    [(< (date-minute d1) (date-minute d2)) #false]
    [(> (date-second d1) (date-second d2)) #true]
    [(< (date-second d1) (date-second d2)) #false]
    [(= (date-second d1) (date-second d2)) #false]))

;; [List-of Track] -> [List-of [List-of Track]]
;; produces a list of LTracks, one per album
(check-expect (select-albums empty) empty)
(check-expect (select-albums LTRACKS2) (list LTRACKS2))
(check-expect (select-albums LTRACKS4) (list (list TRACK1 TRACK2) (list TRACK3 TRACK4) (list TRACK5)))

;(define (select-albums ltracks) empty) ;stub

#;
(define (select-albums ltracks)
  ;       (Track -> [List-of Track])
  (map    ...                       ltracks))

#;
(define (select-albums ltracks)
  (local (
          ;; [List-of Track] -> [List-of String]
          ;; produces list of all albums in ltracks
          (define (all-albums ltracks) empty)
          
          ;; [List-of string] -> [List-of String]
          ;; produces the the unique strings in los                     
          (define (unique-strings los) los)

          ;; Unique albums in ltracks
          (define unique-albums (unique-strings (all-albums ltracks)))
          
          ;; Track -> [List-of Track]
          ;; produces list of all tracks from album 
          (define (tracks-from-album album) empty))
    (map tracks-from-album unique-albums)))

(define (select-albums ltracks)
  (local (
          ;;[List-of Track] -> [List-of String]
          ;; produces list of all albums in ltracks
          (define (all-albums ltracks) (map track-album ltracks))
          
          ;; Unique albums in ltracks
          (define unique-albums (create-set (all-albums ltracks)))

          ;; String -> [List-of Track]
          ;; produces list of all tracks from album 
          (define (tracks-from-album album)
            (local (;; Track -> Boolean
                    ;; produces true if t album is album
                    (define (track-album=? t) (string=? (track-album t) album)))
              (filter track-album=? ltracks))))
    (map tracks-from-album unique-albums)))

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
     (local ((define set-rest (create-set (rest los))))
       (if (member? (first los) set-rest)
           set-rest
           (cons (first los) set-rest)))]))