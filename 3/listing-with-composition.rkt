;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname listing-with-composition) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct address [first-name last-name street])
; An Addr is a structure: 
;   (make-address String String String)
; interpretation associates an address with a person's name
(define ADDR1 (make-address "Aron" "Weiner" "Frias"))

(define (fn-for-address a)
  (... (address-first-name a)
       (address-last-name a)
       (address-street a)))

(define ex0
  (list (make-address "Robert"   "Findler" "South")
        (make-address "Matthew"  "Flatt"   "Canyon")
        (make-address "Shriram"  "Krishna" "Yellow")))

; [List-of Addr] -> String 
; creates a string from first names, 
; sorted in alphabetical order,
; separated and surrounded by blank spaces
(check-expect (listing empty) " ")
(check-expect (listing ex0) " Matthew Robert Shriram ")

;(define (listing loa) "") ;stub

(define (listing loa)
  (concatenate-strings (sort-names (first-names loa))))

; [List-of Addr] -> [List-of String]
; extracts the first names from the given list of Addr
(check-expect (first-names empty) empty)
(check-expect (first-names ex0) (list "Robert" "Matthew"  "Shriram"))

;(define (first-names loa) empty) ;stub

#;
(define (first-names loa)      ;Template
  (cond [(empty? loa) (...)]
        [else
         (... (fn-for-address (first loa))   ;Addr
              (first-names (rest loa)))]))

(define (first-names loa)     
  (cond [(empty? loa) empty]
        [else
         (cons (address-first-name (first loa))   
               (first-names (rest loa)))]))  

; [List-of String] -> [List-of String]
; sorts list of names in alphabetical order
(check-expect (sort-names empty) empty)
(check-expect (sort-names (list "Robert")) (list "Robert"))
(check-expect (sort-names (list "Robert" "Matthew"  "Shriram")) (list "Matthew" "Robert" "Shriram"))

;(define (sort-names los) los) ;stub

#; 
(define (sort-name los)       ;Template
  (cond [(empty? los) (...)]
        [else
         (... (first los)
              (sort-name (rest los)))]))
        
(define (sort-names los)      
  (cond [(empty? los) empty]
        [else
         (insert (first los)
                 (sort-names (rest los)))]))

; String [List-of String] -> [List-of String]
; inserts s into correct position of sorted los
(check-expect (insert "Robert" empty) (list "Robert"))
(check-expect (insert "Robert"  (list "Matthew"  "Shriram"))
              (list "Matthew" "Robert" "Shriram"))

;(define (insert s los) los) ;stub

#;
(define (insert s los)       ;Template
  (cond [(empty? los) (... s)]
        [else
         (... s
              (first los)
              (insert s (rest los)))]))

(define (insert s los)      
  (cond [(empty? los) (list s)]
        [else
         (if (string<=? s (first los))
             (cons s los)
             (cons (first los)
                   (insert s (rest los))))]))


; [List-of String] - > String
; concatenates strings in los, separated and surrounded by blank spaces
(check-expect (concatenate-strings empty) " ")
(check-expect (concatenate-strings (list "Matthew")) " Matthew ")
(check-expect (concatenate-strings (list "Matthew" "Robert"))  " Matthew Robert ")
(check-expect (concatenate-strings (list "Matthew" "Robert" "Shriram"))  " Matthew Robert Shriram ")

;(define (concatenate-strings los) "") ;stub

#;
(define (concatenate-strings los)       ;Template
  (cond [(empty? los) (...)]
        [else
         (... (first los)
              (concatenate-strings (rest los)))]))

(define (concatenate-strings los)       ;Template
  (cond [(empty? los) " "]
        [else
         (string-append
          " "
          (first los)
          (concatenate-strings (rest los)))]))

