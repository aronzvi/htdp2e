;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |389|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct phone-record [name number])
; A PhoneRecord is a structure:
;   (make-phone-record String String)

(define PR0 (make-phone-record "John Doe" "2225556"))

; [List-of String] [List-of String] -> [List-of PhoneRecord]
; combines equally long list of names and list of phone numbers into a list of phone records
; assume that the corresponding list items belong to the same person
(check-expect (zip empty empty) empty)
(check-expect (zip (list "John Doe") (list "4445556"))
              (list (make-phone-record "John Doe" "4445556")))
(check-expect (zip (list "John Doe" "Joe Shmoe") (list "4445556" "9654444"))
              (list (make-phone-record "John Doe" "4445556")
                    (make-phone-record  "Joe Shmoe" "9654444")))

;(define (zip names numbers) empty) ;stub

#;
(define (zip names numbers)
  (cond [(empty? names) (...)]
        [else
         (... (first names)      ;String
              (first numbers)    ;String
              (zip (rest names) (rest numbers)))]))   ;[List-of String] ;[List-of String]

(define (zip names numbers)
  (cond [(empty? names) empty]
        [else
         (cons (make-phone-record (first names)     
                                  (first numbers))   
               (zip (rest names) (rest numbers)))]))  

