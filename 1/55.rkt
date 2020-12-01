;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |55|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define HEIGHT 300) ; distances in pixels 
(define WIDTH  100)
(define YDELTA 3)
 
(define BACKG  (empty-scene WIDTH HEIGHT))
(define ROCKET (rectangle 5 30 "solid" "red"))
 
(define CENTER (/ (image-height ROCKET) 2))

(define (show x)
  (cond
    [(string? x)
     (render-rocket-scene HEIGHT)]
    [(<= -3 x -1)
     (place-image (text (number->string x) 20 "red")
                  10 (* 3/4 WIDTH)
                  (render-rocket-scene HEIGHT))]
    [(>= x 0)
     (render-rocket-scene x)]))

; Number -> Image
; render rocket on scene at given height
(check-expect
 (show HEIGHT)
 (place-image ROCKET 10 (- HEIGHT CENTER) BACKG))
(check-expect
 (show 53)
 (place-image ROCKET 10 (- 53 CENTER) BACKG))
(check-expect
 (show 0)
 (place-image ROCKET 10 (- 0 CENTER) BACKG))
(define (render-rocket-scene height)
  (place-image ROCKET 10 (- height CENTER) BACKG))
