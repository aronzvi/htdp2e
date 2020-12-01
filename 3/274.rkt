;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |274|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; [List-of 1String] -> [List-of [List-of 1String]]
;; produces the list of all prefixes of given list
(check-expect (prefixes empty) empty)
(check-expect (prefixes (list "a")) (list (list "a")))
(check-expect (prefixes (list "a" "b")) (list (list "a") (list "a" "b")))
(check-expect (prefixes (list "a" "b" "c")) (list (list "a") (list "a" "b") (list "a" "b" "c")))
(check-expect (prefixes (list "a" "b" "c" "d")) (list (list "a") (list "a" "b") (list "a" "b" "c") (list "a" "b" "c" "d")))

;(define (prefixes lo1s) empty) ;stub

(define (prefixes lo1s)
  (local (; Natural -> 1String
          ; produces element from lo1s at index i
          (define (get-element-at-index i) (list-ref lo1s i))

          ; Natural -> [List-of 1String]
          ; produces first n elements from lo1s
          (define (get-n-elements n) (build-list n get-element-at-index))
          
          (define list-num-elements-to-get (build-list (length lo1s) add1)))
    
    (map get-n-elements list-num-elements-to-get)))

;; [List-of 1String] -> [List-of [List-of 1String]]
;; produces the list of all suffixes of given lo1s
(check-expect (suffixes empty) empty)
(check-expect (suffixes (list "a")) (list (list "a")))
(check-expect (suffixes (list "a" "b")) (list (list "b") (list "a" "b")))
(check-expect (suffixes (list "a" "b" "c")) (list (list "c") (list "b" "c") (list "a" "b" "c")))
(check-expect (suffixes (list "a" "b" "c" "d")) (list (list "d") (list "c" "d") (list "b" "c" "d") (list "a" "b" "c" "d")))

;(define (suffixes lo1s) empty) ;stub

(define (suffixes lo1s)
  (local ((define element-idxs-from (reverse (build-list (length lo1s) identity)))

          ; Natural -> [List-of 1String]
          ; produces all elements of lo1s starting from index idx
          (define (get-all-elements-from-idx idx)))
    (map get-all-elements-from-idx element-idxs-from)))
