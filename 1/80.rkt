;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |80|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct movie [title director year])

(define (fn-for-movie movie)
  (... (movie-title movie)
   ... (movie-director movie)
   ... (movie-year movie)))

(define-struct pet [name number])

(define (fn-for-pet pet)
  (... (pet-name pet)
   ... (pet-number pet)))

(define-struct CD [artist title price])

(define (fn-for-CD cd)
  (... (CD-artist cd)
   ... (CD-title cd)
   ... (CD-price cd)))

(define-struct sweater [material size color])

(define (fn-for-sweater sweater)
  (... (sweater-material sweater)
   ... (sweater-size sweater)
   ... (sweater-color sweater)))