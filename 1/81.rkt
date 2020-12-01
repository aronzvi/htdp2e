;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |81|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct time [hours minutes seconds])
; A Time is a structure
;  (make-time Number Number Number)
; interpretation the amount of hours, minutes and seconds since midnight

(define MIDNIGHT (make-time 0 0 0))
(define T1 (make-time 5 40 2))
(define T2 (make-time 12 30 2))

; Time -> Number
; produces the number of seconds that have passed since midnight since given time
(check-expect (time->seconds MIDNIGHT) 0)
(check-expect (time->seconds T2) 45002)
(check-expect (time->seconds  T1) (+ (* 3600 5) (* 60 40) 2))

; (define (time->seconds time) 0) ; stub

#;
(define (time->seconds time) ; template
  (... (time-hours time)
       ... (time-minutes time)
       ... (time-seconds time)))

(define (time->seconds time) 
  (+ (* 3600 (time-hours time))
     (* 60 (time-minutes time))
     (time-seconds time)))