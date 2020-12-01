;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |208|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/itunes)
(require 2htdp/image) ;just for BSDN predicate test

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

(define ASSOC1 (list "Protected" #true))
(define ASSOC2 (list "Play Count" 12)) 
(define ASSOC3 (list "Track Type" "File"))
(define ASSOC4 (list "Date Added" (create-date 1996 3 1 16 54 53)))
(define ASSOC5 (list "Protected" #false))
(define ASSOC6 (list "Play Count" 32))
(define ASSOC7 (list "Track Type" "File"))
(define ASSOC8 (list "Date Added" (create-date 2000 5 1 16 54 53)))
(define ASSOC9 (list "Total Time" 150000))
(define ASSOC10 (list "Total Time" 200000))
(define ASSOC11 (list "Bool1" #true))
(define ASSOC12 (list "Bool2" #true))

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
(define LASSOC2 (list ASSOC1 ASSOC2 ASSOC3 ASSOC4 ASSOC9 ASSOC11))
(define LASSOC3 (list ASSOC5 ASSOC6 ASSOC7 ASSOC8 ASSOC10 ASSOC12))

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

; An LLists is one of:
; – '()
; – (cons LAssoc LLists)

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

;; AssociationOrAny is one of:
;; - Association
;; - Any

(define AssociationOrAny0 (list "Protected" #true))
(define AssociationOrAny1 #false)
(define AssociationOrAny2 "default")

(define (fn-for-assoc-or-any aoa)
  (cond [(association? aoa) (... (first aoa)
                                 (fn-for-bsdn (second aoa)))] ;BDSN
        [else (...)]))

;; Functions:

; Any -> Boolean
; predicate for Association
(check-expect (association? (list "Protected" #true)) #true)
(check-expect (association? (list "Protected")) #false)
(check-expect (association? (list "Protected" #true 1)) #false)
(check-expect (association? (list "Protected" #true (list 1))) #false)
(check-expect (association? (list "Protected" #true (square 5 "solid" "red"))) #false)

;(define (association? x) #false) ;stub

(define (association? x)
  (and (cons? x)
       (= (length x) 2)
       (bsdn? (second x))))

; Any -> Boolean
; predicate for BSDN

; – Boolean
; – Number
; – String
; – Date
 
(check-expect (bsdn? #true) #true)
(check-expect (bsdn? #false) #true)
(check-expect (bsdn? 23) #true)
(check-expect (bsdn? "hello") #true)
(check-expect (bsdn? (create-date 2011 2 27 21 45 47)) #true)
(check-expect (bsdn? (list 1)) #false)
(check-expect (bsdn? (square 5 "solid" "red")) #false)

;(define (bsdn? x) #false) ;stub

(define (bsdn? x)
  (or (boolean? x)
      (number? x)
      (string? x)
      (date? x)))

;; LLists -> List-of-strings
;; produces the Strings that are associated with a Boolean attribute. Does not repeat the Strings.
(check-expect (boolean-attributes empty) empty)
(check-satisfied (boolean-attributes LLISTS2) all-boolean-attributes-llist2?)

;(define (boolean-attributes llists) empty) ;stub

(define (boolean-attributes llists)
  (cond
    [(empty? llists) empty]
    [else
     (create-set (append (lassoc-all-boolean-attributes (first llists)) 
                         (boolean-attributes (rest llists))))]))


;(define (boolean-attributes llists)
;  (cond
;    [(empty? llists) empty]
;    [else
;     (create-set (all-boolean-attributes llists))]))

;; LLists -> List-of-strings
;; produces all the Strings that are associated with a Boolean attribute. May include duplicates
;(check-expect (all-boolean-attributes empty) empty)
;(check-expect (all-boolean-attributes LLISTS2) (list "Protected" "Bool1" "Protected" "Bool2"))

;(define (all-boolean-attributes llists) empty) ;stub

;(define (all-boolean-attributes llists)
; (cond
;    [(empty? llists) empty]
;    [else
;     (append (lassoc-all-boolean-attributes (first llists)) 
;                         (boolean-attributes (rest llists)))]))


;; LAssoc -> List-of-strings
;; produces all Strings in lassoc that are associated with a Boolean attribute
(check-expect (lassoc-all-boolean-attributes empty) empty)
(check-expect (lassoc-all-boolean-attributes LASSOC2) (list "Protected" "Bool1"))
(check-expect (lassoc-all-boolean-attributes LASSOC3) (list "Protected" "Bool2"))

;(define (lassoc-all-boolean-attributes lassoc) empty) ;stub

(define (lassoc-all-boolean-attributes lassoc)
  (cond
    [(empty? lassoc) empty]
    [else
     (if (assoc-boolean? (first lassoc))
         (cons (first (first lassoc)) (lassoc-all-boolean-attributes (rest lassoc)))
         (lassoc-all-boolean-attributes (rest lassoc)))]))

;; Association -> Boolean
;; produces true if assoc is a boolean attribute
(check-expect (assoc-boolean? ASSOC1) #true)
(check-expect (assoc-boolean? ASSOC2) #false)

;(define (boolean-attrib? assoc) #false) ;stub

(define (assoc-boolean? assoc)
  (bsdn-boolean? (second assoc)))

;; BSDN -> Boolean
;; produces true if bsdn is a Boolean
(check-expect (bsdn-boolean? BSDN1) #true)
(check-expect (bsdn-boolean? BSDN2) #false)
(check-expect (bsdn-boolean? BSDN3) #false)
(check-expect (bsdn-boolean? BSDN4) #false)

;(define (bsdn-boolean? bsdn) #false) ;stub

(define (bsdn-boolean? bsdn)
  (boolean? bsdn))

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

; List-of-strings -> Boolean
; produces true if los is expected list of attributes for LLIST2
(check-expect (all-boolean-attributes-llist2? (list "Bool1"  "Protected" "Bool2")) #true)
(check-expect (all-boolean-attributes-llist2? (list "Protected" "Bool1" "Bool2")) #true)
(check-expect (all-boolean-attributes-llist2? (list "blah" "blee" "blue")) #false)
(check-expect (all-boolean-attributes-llist2? (list "Bool1" "Bool2")) #false)

(define (all-boolean-attributes-llist2? los)
  (and (= (length los) 3)
       (member? "Bool1" los)
       (member? "Protected" los)
       (member? "Bool2" los)))

;; LAssoc -> Track Or #false
;; converts an LAssoc to a Track when possible
(check-expect(track-as-struct empty) #false)
(check-expect (track-as-struct (list (list "Name" "Shiz")
                                     (list "Artist" "Azeee")
                                     (list "Album" "The shiz album")
                                     (list "Total Time" 300000)
                                     (list "Track Number" 10)
                                     (list "Date Added" (create-date 2011 2 27 21 45 47))
                                     (list "Play Count" 1)
                                     (list "Play Date UTC" (create-date 2011 2 27 21 51 59))))
              (create-track "Shiz" "Azeee" "The shiz album" 300000 10 (create-date 2011 2 27 21 45 47) 1 (create-date 2011 2 27 21 51 59)))
(check-expect (track-as-struct (list (list "Name" "Shit2")
                                     (list "Artist" "Azeee")
                                     (list "Album" "The shit album 2")
                                     (list "Total Time" 301000)
                                     (list "Track Number" 11)
                                     (list "Date Added" (create-date 2013 2 27 21 45 47))
                                     (list "Play Count" 3)
                                     (list "Play Date UTC" (create-date 2015 2 27 21 51 59))))
              (create-track "Shit2" "Azeee" "The shit album 2" 301000 11 (create-date 2013 2 27 21 45 47) 3 (create-date 2015 2 27 21 51 59)))
(check-expect (track-as-struct (list (list "Album" "The shiz album") ; Missing "Name"
                                     (list "Artist" "Azeee")
                                     (list "Total Time" 300000)
                                     (list "Track Number" 10)
                                     (list "Date Added" (create-date 2011 2 27 21 45 47))
                                     (list "Play Count" 1)
                                     (list "Play Date UTC" (create-date 2011 2 27 21 51 59))))
              #false)
(check-expect (track-as-struct (list (list "Name" "Shiz")  ; Missing "Artist"
                                     (list "Album" "The shit album")
                                     (list "Total Time" 300000)
                                     (list "Track Number" 10)
                                     (list "Date Added" (create-date 2011 2 27 21 45 47))
                                     (list "Play Count" 1)
                                     (list "Play Date UTC" (create-date 2011 2 27 21 51 59))))
              #false)
(check-expect (track-as-struct (list (list "Name" "Shiz") ; Missing "Play Count"
                                     (list "Artist" "Azeee")
                                     (list "Album" "The shiz album")
                                     (list "Total Time" 300000)
                                     (list "Track Number" 10)
                                     (list "Date Added" (create-date 2011 2 27 21 45 47))
                                     (list "Play Date UTC" (create-date 2011 2 27 21 51 59))))
              #false)
(check-expect (track-as-struct (list (list "Name" "Shiz")  ; Invalid value for "Total Time"
                                     (list "Artist" "Azeee")
                                     (list "Album" "The shit album")
                                     (list "Total Time" "ererer")
                                     (list "Track Number" 10)
                                     (list "Date Added" (create-date 2011 2 27 21 45 47))
                                     (list "Play Count" 1)
                                     (list "Play Date UTC" (create-date 2011 2 27 21 51 59))))
              #false)

;(define (track-as-struct lassoc) #false) ;stub

(define (track-as-struct lassoc)
  (create-track (assoc-val (find-association "Name" lassoc #false))              ;name	 	 	 	 
                (assoc-val (find-association "Artist" lassoc #false))	     ;artist	 	 	 	 
                (assoc-val (find-association "Album" lassoc #false))	     ;album	 	 	 	 
                (assoc-val (find-association "Total Time" lassoc #false))	     ;time
                (assoc-val (find-association "Track Number" lassoc #false))    ;track#
                (assoc-val (find-association "Date Added" lassoc #false))       ;added
                (assoc-val (find-association "Play Count" lassoc #false))       ;play#
                (assoc-val (find-association "Play Date UTC" lassoc #false))))  ;played

; AssociationOrAny -> BSDN or #false
; produces the association's (BSDN) value or #false if not given an association
(check-expect (assoc-val (list "Name" "Shiz")) "Shiz")
(check-expect (assoc-val (list "Track Number" 10)) 10)
(check-expect (assoc-val (list "Date Added" (create-date 2011 2 27 21 45 47))) (create-date 2011 2 27 21 45 47))
(check-expect (assoc-val 1) #false)
(check-expect (assoc-val #true) #false)
(check-expect (assoc-val "hello") #false)
(check-expect (assoc-val (list 1 2 3)) #false)

;(define (assoc-val a) #false) ;stub

(define (assoc-val aoa)
  (cond [(association? aoa) (second aoa)] 
        [else #false]))


;; String LAssoc Any -> AssociationOrAny
;; produces the first Association whose first item is equal to key, or default if there is no such Association
(check-expect (find-association "Protected" empty "default") "default")
(check-expect (find-association "Protected" (list ASSOC1) "default") ASSOC1)
(check-expect (find-association "Track Type" LASSOC2 "default") ASSOC3)
(check-expect (find-association "Ganas" LASSOC2 "default") "default")

;(define (find-association key lassoc default) default) ;stub

(define (find-association key lassoc default)
  (cond
    [(empty? lassoc) default]
    [else
     (if (key=? (first lassoc) key)
         (first lassoc)
         (find-association key (rest lassoc) default))]))

;; Association String -> Boolean
;; produces true if Associations first item is equal to key
(check-expect (key=? ASSOC1 "Protected") #true)
(check-expect (key=? ASSOC1 "Ganas") #false)

;(define (key=? assoc key) #false) ;stub

(define (key=? assoc key)
  (string=? (first assoc) key))



