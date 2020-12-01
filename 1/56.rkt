;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |56|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; We are stuck in an endless loop - after going off screen we are launched again. This happens since we subtract by -3 to make the rocket go higher, and when we reach hight 0 - off the screen, our state becomes -3 which is handled agian by the tick handler as launch countdown

(require 2htdp/image)
(require 2htdp/universe)

(define HEIGHT 300) ; distances in pixels 
(define WIDTH  100)
(define YDELTA 3)
 
(define BACKG  (empty-scene WIDTH HEIGHT))
(define ROCKET (rectangle 5 30 "solid" "red"))
 
(define CENTER (/ (image-height ROCKET) 2))

; An LRCD (for launching rocket countdown) is one of:
; – "resting"
; – a Number between -3 and -1
; – a NonnegativeNumber 
; interpretation a grounded rocket, in countdown mode,
; a number denotes the number of pixels between the
; top of the canvas and the center of the rocket (its height)

(check-expect
 (show "resting")
 (place-image ROCKET 10 (- HEIGHT CENTER) BACKG)) ; resting
(check-expect
 (show -2)
 (place-image (text "-2" 20 "red")
              10 (* 3/4 WIDTH)
              (place-image ROCKET 10 (- HEIGHT CENTER) BACKG))) ; in middle of countdown
(check-expect
 (show 53)
 (place-image ROCKET 10 (- 53 CENTER) BACKG)) ; in flight
(check-expect
 (show 0)
 (place-image ROCKET 10 (- 0 CENTER) BACKG)) ; off screen
(check-expect
 (show HEIGHT)
 (place-image ROCKET 10 (- HEIGHT CENTER) BACKG)) ; on ground about to take off
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
 (place-image ROCKET 10 (- HEIGHT CENTER) BACKG)) ; on ground
(check-expect
 (show 53)
 (place-image ROCKET 10 (- 53 CENTER) BACKG)) ; in flight
(check-expect
 (show 0)
 (place-image ROCKET 10 (- 0 CENTER) BACKG)) ; off screen
(define (render-rocket-scene height)
  (place-image ROCKET 10 (- height CENTER) BACKG))

; LRCD KeyEvent -> LRCD
; starts the countdown when the rocket is "resting" and the space key is pressed. Otherwise return lrcd
(check-expect (launch "resting" " ") -3)
(check-expect (launch "resting" "a") "resting")
(check-expect (launch -3 " ") -3)
(check-expect (launch -1 " ") -1)
(check-expect (launch 33 " ") 33)
(check-expect (launch 33 "a") 33)
(define (launch x ke)
  (cond
    [(string? x) (if (string=? " " ke) -3 x)]
    [(<= -3 x -1) x]
    [(>= x 0) x]))

; LRCD -> LRCD
; raises the rocket by YDELTA if it is moving already 
(check-expect (fly "resting") "resting")
(check-expect (fly -3) -2)
(check-expect (fly -2) -1)
(check-expect (fly -1) HEIGHT)
(check-expect (fly 10) (- 10 YDELTA))
(check-expect (fly 22) (- 22 YDELTA)) 
(define (fly x)
  (cond
    [(string? x) x]
    [(<= -3 x -1) (if (= x -1) HEIGHT (+ x 1))]
    [(>= x 0) (- x YDELTA)]))

; LRCD -> Boolean
; returns true when rocket is off screen. Otherwise, returns false
(check-expect (off-screen? "resting") false)
(check-expect (off-screen? -3) false)
(check-expect (off-screen? HEIGHT) false)
(check-expect (off-screen? 53) false)
(check-expect (off-screen? 0) true)
(define (off-screen? lrcd)
  (cond
    [(string? lrcd) false]
    [(<= -3 lrcd -1) false]
    [(>= lrcd 0) (if (= lrcd 0) true false)]))

; LRCD -> LRCD
(define (main1 s)
  (big-bang s
    [to-draw show]
    [on-key launch]))



; LRCD -> LRCD
(define (main2 s)
  (big-bang s
    [to-draw show]
    [on-key launch]
    [on-tick fly 0.3]
    [stop-when off-screen?]))
