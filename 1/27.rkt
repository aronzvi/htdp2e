;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |27|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define FIXED_PERFORMANCE_COST 180)
(define PER_ATTENDEE_VARIABLE_COST 0.04)
(define NUM_ATTENDEES_FOR_BASE_PRICE 120)
(define BASE_PRICE 5.0)
(define PRICE_CHANGE 0.1)
(define AVERAGE_ATTENDANCE_CHANGE 15)

(define (attendees ticket-price)
  (- NUM_ATTENDEES_FOR_BASE_PRICE (* (- ticket-price BASE_PRICE) (/ AVERAGE_ATTENDANCE_CHANGE PRICE_CHANGE))))

(define (revenue ticket-price)
  (* ticket-price (attendees ticket-price)))

(define (cost ticket-price)
  (+  FIXED_PERFORMANCE_COST (*  PER_ATTENDEE_VARIABLE_COST (attendees ticket-price))))

(define (profit ticket-price)
  (- (revenue ticket-price)
     (cost ticket-price)))


