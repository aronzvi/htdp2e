;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |132|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List-of-names is one of: 
; – '()
; – (cons String List-of-names)
; interpretation a list of friends


(define LON1 (cons "Fagan"
                   (cons "Findler"
                         (cons "Fisler"
                               (cons "Flanagan"
                                     (cons "Flatt"
                                           (cons "Felleisen"
                                                 (cons "Friedman" '())))))))
  )

; List-of-names -> Boolean
; determines whether "Flatt" is on a-list-of-names
(check-expect (contains-flatt? '()) #false)
(check-expect (contains-flatt? (cons "Find" '()))
              #false)
(check-expect (contains-flatt? (cons "Flatt" '()))
              #true)
(check-expect
 (contains-flatt?
  (cons "A" (cons "Flatt" (cons "C" '()))))
 #true)
(check-expect
 (contains-flatt?
  (cons "B" (cons "D" (cons "C" '()))))
 #false)
(check-expect (contains-flatt? LON1) #true)

;(define (contains-flatt? alon) #false) ; stub

#;
(define (contains-flatt? alon)
  (cond
    [(empty? alon) ...]
    [else
     (... (first alon)    ; String
          (rest alon))])) ; List-of-names

(define (contains-flatt? alon)
  (cond
    [(empty? alon) #false]
    [else
     (or (string=? (first alon) "Flatt")
         (contains-flatt? (rest alon)))]))




