;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |198|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

;; Constants

(define LOCATION "/usr/share/dict/words")
(define AS-LIST (read-lines LOCATION))

; A Letter is one of the following 1Strings: 
; – "a"
; – ... 
; – "z"
; or, equivalently, a member? of this list: 
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))
(define L1 "a")
(define L2 "z")

;; ListOfLetter is one of:
;; - empty
;; - (cons Letter ListOfLetter)
;; interp. a list of letters
(define LOT1 empty)
(define LOT2 (list "a" "b" "c" "z"))

(define (fn-for-lol lol)
  (cond
    [(empty? lol) (...)]
    [else
     (... (fn-for-letter (first lol)) ;Letter
          (fn-for-lol (rest lol)))])) ;ListOfLetter

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Letter ListOfLetter)
;; - reference: (first lot) is Letter
;; - self-reference: (rest lot) is ListOfLetter


;; ListOfString is one of:
;; - empty
;; - (cons String ListOfString)
;; interp. a lit of strings
(define LOS1 empty)
(define LOS2 (list "deeee" "doooo"))

(define (fn-for-los los)
  (cond
    [(empty? los) (...)]
    [else
     (... (first los)  
          (fn-for-los (rest los)))])) ;ListOfString

;; Template rules used:
;; one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons String ListOfString)
;; - self-reference: (rest los) is List-of-strings

; A Dictionary is a ListOfString.

(define-struct letter-count [letter count])
;; A LetterCount is (make-letter-count Letter Number)
;; interp. the number of occurences for a letter
(define LC1 (make-letter-count "a" 1))
(define LC2 (make-letter-count "z" 5))

(define (fn-for-letter-count lc)
  (... (fn-for-letter (letter-count-letter lc)) ;Letter
       (letter-count-count lc)))

;; Template rules used:
;; - compound: 2 fields
;; - reference: (letter-count-letter lc)) is Letter

;; ListOfLetterCount is one of:
;; - empty
;; - (cons LetterCount ListOfLetterCount)
;; interp. A list of letter-counts
(define LOLC1 empty)
(define LOLC2 (list (make-letter-count "a" 1)
                    (make-letter-count "z" 5)))

(define (fn-for-lolc lolc)
  (cond
    [(empty? lolc) (...)]
    [else
     (... (fn-for-letter-count (first lolc)) ;LetterCount
          (fn-for-lolc (rest lolc)))]))      ;ListOfLetterCount

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons LetterCount ListOfLetterCount)
;; - reference: (first lolc) is LetterCount
;; - self-reference: (rest lolc) is ListOfLetterCount

;; ListOfDictionary is one of:
;; - empty
;; - (cons Dictionary ListOfDictionary)
;; interp. a list of dictionaries
(define LOD1 empty)
(define LOD2 (list (list "aaa" "asss") empty (list "xxx" "xdfdf" "xff")))

(define (fn-for-lod lod)
  (cond
    [(empty? lod) (...)]
    [else
     (... (fn-for-los (first lod))    ;Dictionary
          (fn-for-lod (rest lod)))])) ;ListOfDictionary

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Dictionary ListOfDictionary)
;; - reference: (first lod) is Dictionary
;; - self-reference: (rest lod) is ListOfDictionary

;; Functions V2

;; Dictionary -> LetterCount
;; produces the LetterCount for the letter that occurs most often as the first one in the given Dictionary
(check-expect (most-frequent.v2 empty) (make-letter-count "a" 0))
(check-expect (most-frequent.v2 (list "bdd")) (make-letter-count "b" 1))
(check-expect (most-frequent.v2 (list "addd" "axx" "hhh" "qww" "rfff" "tf;f" "zx"))
              (make-letter-count "a" 2))
(check-expect (most-frequent.v2 (list "addd" "axx" "hhh" "qww" "tfff" "tff" "xxx" "xvv" "xccc" "zx"))
              (make-letter-count "x" 3))

;(define (most-frequent.v2 d) (make-letter-count "a" 0)) ;stub

(define (most-frequent.v2 d)
  (letter-count-from-dict (most-words (words-by-first-letter d))))

;; ListOfDictionary -> Dictionary
;; produces the Dictionary with most words in lod
(check-expect (most-words empty) empty)
(check-expect (most-words (list (list "add")))
              (list "add"))
(check-expect (most-words (list (list "add") empty))
              (list "add"))
(check-expect (most-words (list (list "add" "axx") (list "bdd")))
              (list "add" "axx"))
(check-expect (most-words (list (list "add" "axx") (list "bdd" "bvv")))
              (list "add" "axx"))
(check-expect (most-words (list (list "addd" "axx") (list "hhh") (list "qww") (list "rfff" "rfff" "rggg") (list "tf;f") (list "zx")))
              (list "rfff" "rfff" "rggg"))

;(define (most-words lod) empty) ;stub

(define (most-words lod)
  (cond
    [(empty? lod) empty]
    [else
     (first (sort-lod lod))]))

;; Dictionary -> LetterCount
;; produces a LetterCount from given los which contains only words starting with the same letter
(check-expect (letter-count-from-dict empty) (make-letter-count "a" 0))
(check-expect (letter-count-from-dict (list "aa" "ass" "axx")) (make-letter-count "a" 3))

;(define (letter-count-from-dict los) (make-letter-count "a" 0)) ;stub

(define (letter-count-from-dict los)
  (cond
    [(empty? los) (make-letter-count "a" 0)]
    [else
     (make-letter-count (first-letter (first los))
                        (length los))]))

;; String -> Letter
;; produces the first letter of s. Assumes s is not empty
(check-expect (first-letter "asas") "a")
(check-expect (first-letter "sas") "s")

;(define (first-letter s) "a") ;stub

;(define (first-letter s) (... s)) ;Template

;; Template rules used:
;; - atomic non-distinct

(define (first-letter s)
  (string-ith s 0))

;; ListOfDictionary -> ListOfDictionary
;; sorts lod by Dictionary length in descending order
(check-expect (sort-lod empty) empty)
(check-expect (sort-lod (list (list "add"))) (list (list "add")))
(check-expect (sort-lod (list (list "add") (list "bdd" "bvv")))
              (list (list "bdd" "bvv") (list "add")))
(check-expect (sort-lod (list (list "add" "axx") (list "bdd" "bvv")))
              (list (list "add" "axx") (list "bdd" "bvv")))
(check-expect (sort-lod (list (list "add" "axx") (list "xcc" "xvv" "xbb")(list "bdd")))
              (list (list "xcc" "xvv" "xbb") (list "add" "axx") (list "bdd")))
(check-expect (sort-lod (list (list "add") (list "xcc" "xvv" "xbb")(list "bdd" "bvv")))
              (list (list "xcc" "xvv" "xbb") (list "bdd" "bvv") (list "add")))

;(define (sort-lod lod) lod) ;stub

(define (sort-lod lod)
  (cond
    [(empty? lod) empty]
    [else
     (insert-dict (first lod)
                  (sort-lod (rest lod)))]))

;; Dictionary ListOfDictionary -> ListOfDictionary
;; inserts d into sorted lod at proper postion in descending order of length
(check-expect (insert-dict (list "add") empty)
              (list (list "add")))
(check-expect (insert-dict (list "add") (list (list "bdd" "bvv")))
              (list (list "bdd" "bvv") (list "add")))
(check-expect (insert-dict (list "add" "axx") (list (list "bdd" "bvv")))
              (list (list "add" "axx") (list "bdd" "bvv")))
(check-expect (insert-dict (list "add" "axx") (list (list "xcc" "xvv" "xbb") (list "bdd")))
              (list (list "xcc" "xvv" "xbb") (list "add" "axx") (list "bdd")))

;(define (insert-dict d lod) lod) ;stub

(define (insert-dict d lod)
  (cond
    [(empty? lod) (list d)]
    [else
     (if (longer-or-equal? d (first lod))
         (cons d lod)
          (cons (first lod) (insert-dict d (rest lod))))]))

;; Dictionary -> Dictionary -> Boolean
;; produces true if d1 is longer or equal than d2
(check-expect (longer-or-equal? empty empty) #true)
(check-expect (longer-or-equal? (list "aaaa") empty) #true)
(check-expect (longer-or-equal? (list "aaaa" "sdsds") (list "ddd")) #true)
(check-expect (longer-or-equal? (list "ddd") (list "aaaa" "sdsds")) #false)
(check-expect (longer-or-equal? (list "aaaa" "sdsds") (list "ddd" "dfdf")) #true)

;(define (longer-or-equal? d1 d2) #false) ;stub

(define (longer-or-equal? d1 d2)
  (>= (length d1) (length d2)))

;; Dictionary -> ListOfDictionary
;; produces a list of list of words, one per first letter. empty list if words not found for a letter
(check-expect (words-by-first-letter empty)
              (list empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty))
(check-expect (words-by-first-letter (list "aaa" "aaa" "d" "dfdf" "fdd" "ggg" "yfdf" "yyy" "yyff" "z"))
              (list (list "aaa" "aaa")
                    empty
                    empty
                    (list "d" "dfdf")
                    empty
                    (list "fdd")
                    (list "ggg")
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    empty
                    (list  "yfdf" "yyy" "yyff")
                    (list "z")))

;(define (words-by-first-letter d) empty) ;stub

(define (words-by-first-letter d)
  (words-by-first-letter-with-letters LETTERS d))

;; ListOfLetter Dictionary -> ListOfDictionary
;; produces a list of list of words, one per starting letter in lol. empty list if words not found for letter
(check-expect (words-by-first-letter-with-letters empty (list "aaa" "aaa" "d" "dfdf" "fdd" "ggg" "yfdf" "yyy" "yyff" "z"))
              empty)
(check-expect (words-by-first-letter-with-letters (list "a" "d" "f" "g" "y" "z")
                                                  (list "aaa" "aaa" "d" "dfdf" "fdd" "ggg" "yfdf" "yyy" "yyff" "z"))
              (list (list "aaa" "aaa")
                    (list "d" "dfdf")
                    (list "fdd")
                    (list "ggg")
                    (list  "yfdf" "yyy" "yyff")
                    (list "z")))
(check-expect (words-by-first-letter-with-letters (list "a" "b" "d" "f" "g" "h" "j" "k" "y" "z")
                                                  (list "aaa" "aaa" "d" "dfdf" "fdd" "ggg" "yfdf" "yyy" "yyff" "z"))
              (list (list "aaa" "aaa")
                    empty
                    (list "d" "dfdf")
                    (list "fdd")
                    (list "ggg")
                    empty
                    empty
                    empty
                    (list  "yfdf" "yyy" "yyff")
                    (list "z")))

;(define (words-by-first-letter-with-letters lol d) empty) ;stub

(define (words-by-first-letter-with-letters lol d)
  (cond
    [(empty? lol) empty]
    [else
     (cons (words-by-first-letter-with-letter (first lol) d) 
           (words-by-first-letter-with-letters (rest lol) d))]))

;; Letter Dictionary -> Dictionary
;; produces a list of words from d starting with l
(check-expect (words-by-first-letter-with-letter "a" empty) empty)
(check-expect (words-by-first-letter-with-letter "a" (list "fff")) empty)
(check-expect (words-by-first-letter-with-letter "y" (list "aaa" "aaa" "d" "dfdf" "fdd" "ggg" "yfdf" "yyy" "yyff" "z"))
              (list "yfdf" "yyy" "yyff"))
              
;(define (words-by-first-letter-with-letter l d) empty) ;stub

(define (words-by-first-letter-with-letter l los)
  (cond
    [(empty? los) empty]
    [else
     (if (starts-with? (first los) l)
         (cons (first los) (words-by-first-letter-with-letter l (rest los)))
         (words-by-first-letter-with-letter l (rest los)))]))

;; String 1String -> Boolean
;; produces true if string s starts with letter l
(check-expect (starts-with? "afff" "a") #true)
(check-expect (starts-with? "zfff" "z") #true)
(check-expect (starts-with? "dooo" "b") #false)

;(define (starts-with? s l) #false) ; stub

;(define (starts-with? s l) (... s l)) ;Template

;; Template rules used:
;; - atomic non-distinct

(define (starts-with? s l)
  (string=? (string-ith s 0) l))

;; Functions from 197 sort version

;; Dictionary -> ListOfLetterCount
;; counts how often each letter is used as the first one of a word in the given dictionary
(check-expect (count-by-letter empty)
              (list (make-letter-count "a" 0)
                    (make-letter-count "b" 0)
                    (make-letter-count "c" 0)
                    (make-letter-count "d" 0)
                    (make-letter-count "e" 0)
                    (make-letter-count "f" 0)
                    (make-letter-count "g" 0)
                    (make-letter-count "h" 0)
                    (make-letter-count "i" 0)
                    (make-letter-count "j" 0)
                    (make-letter-count "k" 0)
                    (make-letter-count "l" 0)
                    (make-letter-count "m" 0)
                    (make-letter-count "n" 0)
                    (make-letter-count "o" 0)
                    (make-letter-count "p" 0)
                    (make-letter-count "q" 0)
                    (make-letter-count "r" 0)
                    (make-letter-count "s" 0)
                    (make-letter-count "t" 0)
                    (make-letter-count "u" 0)
                    (make-letter-count "v" 0)
                    (make-letter-count "w" 0)
                    (make-letter-count "x" 0)
                    (make-letter-count "y" 0)
                    (make-letter-count "z" 0)))
(check-expect (count-by-letter (list "addd" "axx" "hhh" "qww" "tfff" "tff" "zx"))
              (list (make-letter-count "a" 2)
                    (make-letter-count "b" 0)
                    (make-letter-count "c" 0)
                    (make-letter-count "d" 0)
                    (make-letter-count "e" 0)
                    (make-letter-count "f" 0)
                    (make-letter-count "g" 0)
                    (make-letter-count "h" 1)
                    (make-letter-count "i" 0)
                    (make-letter-count "j" 0)
                    (make-letter-count "k" 0)
                    (make-letter-count "l" 0)
                    (make-letter-count "m" 0)
                    (make-letter-count "n" 0)
                    (make-letter-count "o" 0)
                    (make-letter-count "p" 0)
                    (make-letter-count "q" 1)
                    (make-letter-count "r" 0)
                    (make-letter-count "s" 0)
                    (make-letter-count "t" 2)
                    (make-letter-count "u" 0)
                    (make-letter-count "v" 0)
                    (make-letter-count "w" 0)
                    (make-letter-count "x" 0)
                    (make-letter-count "y" 0)
                    (make-letter-count "z" 1)))

;(define (count-by-letter d) empty) ;stub

(define (count-by-letter d)
  (count-by-letters LETTERS d))

;; ListOfLetter Dictionary -> ListOfLetterCount
;; counts how often each letter in lot is used as the first one of a word in d
(check-expect (count-by-letters empty (list "addd" "axx" "hhh" "qww" "tfff" "tff" "zx")) empty)
(check-expect (count-by-letters (list "a") (list "addd" "axx" "hhh" "qww" "tfff" "tff" "zx"))
              (list (make-letter-count "a" 2)))
(check-expect (count-by-letters (list "a" "t" "x") (list "addd" "axx" "hhh" "qww" "tfff" "tff" "zx"))
              (list (make-letter-count "a" 2)
                    (make-letter-count "t" 2)
                    (make-letter-count "x" 0)))

;(define (count-by-letters lot d) empty) ;stub

(define (count-by-letters lot d)
  (cond
    [(empty? lot) empty]
    [else
     (cons (make-letter-count (first lot) (starts-with# (first lot) d))
           (count-by-letters (rest lot) d))]))

;; Letter Dictionary -> Number
;; counts how many words in d start with the l
(check-expect (starts-with# "a" empty) 0)
(check-expect (starts-with# "a" (list "afff")) 1)
(check-expect (starts-with# "a" (list "dooo" "ass")) 1)
(check-expect (starts-with# "a" (list "dooo" "bass")) 0)
(check-expect (starts-with# "a" (list "apple" "dooo" "are")) 2)

;(define (starts-with# l d) 0) ;stub

(define (starts-with# l los)
  (cond
    [(empty? los) 0]
    [else
     (if (starts-with? (first los) l)
         (+ 1 (starts-with# l (rest los)))
         (starts-with# l (rest los)))]))

;; String 1String -> Boolean
;; produces true if string s starts with letter l
(check-expect (starts-with? "afff" "a") #true)
(check-expect (starts-with? "zfff" "z") #true)
(check-expect (starts-with? "dooo" "b") #false)

;(define (starts-with? s l) #false) ; stub

;(define (starts-with? s l) (... s l)) ;Template

;; Template rules used:
;; - atomic non-distinct

;; Dictionary -> LetterCount
;; produces the LetterCount for the letter that occurs most often as the first one in the given Dictionary
(check-expect (most-frequent empty) (make-letter-count "a" 0))
(check-expect (most-frequent (list "add")) (make-letter-count "a" 1))
(check-expect (most-frequent (list "addd" "axx" "hhh" "qww" "rfff" "tf;f" "zx"))
              (make-letter-count "a" 2))
(check-expect (most-frequent (list "addd" "axx" "hhh" "qww" "tfff" "tff" "zx"))
              (make-letter-count "a" 2))

;(define (most-frequent d) (make-letter-count "a" 0)) ;stub is (make-letter-count "a" 0) a good value here?

(define (most-frequent d)
  (max-count (count-by-letter d)))

;; ListOfLetterCount -> LetterCount
;; produces the lc with the maximum count from given lolc
(check-expect (max-count empty)
              (make-letter-count "a" 0))
(check-expect (max-count (list (make-letter-count "a" 0)
                                  (make-letter-count "b" 1)
                                  (make-letter-count "c" 3)))
              (make-letter-count "c" 3))
(check-expect (max-count (list (make-letter-count "a" 3)
                                  (make-letter-count "b" 3)
                                  (make-letter-count "c" 3)))
              (make-letter-count "a" 3))

;(define (max-count lolc) (make-letter-count "a" 0)) ;stub

(define (max-count lolc)
  (cond
    [(empty? lolc) (make-letter-count "a" 0)]
    [else
     (first (sort-lolc lolc))]))     

;; ListOfLetterCount -> ListOfLetterCount
;; sorts lolc in descending order by count
(check-expect (sort-lolc empty) empty)
(check-expect (sort-lolc (list (make-letter-count "v" 40))) (list (make-letter-count "v" 40)))
(check-expect (sort-lolc (list (make-letter-count "a" 0)
                               (make-letter-count "b" 1)
                               (make-letter-count "c" 3)))
              (list (make-letter-count "c" 3)
                    (make-letter-count "b" 1)
                    (make-letter-count "a" 0)))
(check-expect (sort-lolc (list (make-letter-count "a" 3)
                               (make-letter-count "b" 3)
                               (make-letter-count "c" 3)))
              (list (make-letter-count "a" 3)
                    (make-letter-count "b" 3)
                    (make-letter-count "c" 3)))

;(define (sort-lolc lolc) lolc) ;stub

(define (sort-lolc lolc)
  (cond
    [(empty? lolc) empty]
    [else
     (insert-lc (first lolc) (sort-lolc (rest lolc)))]))

;; LetterCount ListOfLetterCount -> ListOfLetterCount
;; inserts lc into sorted lolc in proper place in descending order
(check-expect (insert-lc (make-letter-count "a" 0) empty) (list (make-letter-count "a" 0)))
(check-expect (insert-lc (make-letter-count "a" 5) (list (make-letter-count "v" 6)))
              (list (make-letter-count "v" 6) (make-letter-count "a" 5)))
(check-expect (insert-lc (make-letter-count "a" 7) (list (make-letter-count "v" 6)))
              (list (make-letter-count "a" 7) (make-letter-count "v" 6)))
(check-expect (insert-lc (make-letter-count "c" 3) (list (make-letter-count "b" 4) (make-letter-count "a" 0)))
              (list (make-letter-count "b" 4) (make-letter-count "c" 3) (make-letter-count "a" 0)))

;(define (insert-lc lc lolc) lolc) ;stub

(define (insert-lc lc lolc)
  (cond
    [(empty? lolc) (list lc)]
    [else
     (if (lc-larger-or-equal? lc (first lolc))
         (cons lc lolc)
         (cons (first lolc) (insert-lc lc (rest lolc))))]))      

;; LetterCount LetterCount -> Boolean
;; produces true if lc1 count is larger or equal than lc2 count
(check-expect (lc-larger-or-equal? (make-letter-count "a" 3) (make-letter-count "v" 4)) #false)
(check-expect (lc-larger-or-equal? (make-letter-count "v" 4) (make-letter-count "a" 3)) #true)
(check-expect (lc-larger-or-equal? (make-letter-count "v" 4) (make-letter-count "a" 4)) #true)

;(define (lc-larger-or-equal? lc1 lc2) #false) ;stub

(define (lc-larger-or-equal? lc1 lc2)
  (>= (letter-count-count lc1)
      (letter-count-count lc2)))

(check-expect
  (most-frequent AS-LIST)
  (most-frequent.v2 AS-LIST))

