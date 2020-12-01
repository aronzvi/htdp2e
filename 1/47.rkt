;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |47|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define SCENE-WIDTH 300)
(define SCENE-HEIGHT 200)
(define MTSCN (empty-scene SCENE-WIDTH SCENE-HEIGHT))
(define GAUGE-HEIGHT SCENE-HEIGHT) ;(- SCENE-HEIGHT 100)) 
(define GAUGE-WIDTH SCENE-WIDTH) ;(- SCENE-WIDTH 100))
(define GAUGE-FRAME (rectangle GAUGE-WIDTH GAUGE-HEIGHT "outline" "black"))
(define GAUGE-Y (/ SCENE-HEIGHT 2))
(define GAUGE-X (/ SCENE-WIDTH 2))
(define MIN-HAPPINESS 0)
(define MAX-HAPPINESS 100)
(define UP-JUMP (/ MAX-HAPPINESS 3))
(define DOWN-JUMP (/ MAX-HAPPINESS 5))

; Happiness is a Number [0,100]

; Happiness -> Happiness
; starts the gauge with the maximum score
; start with (gauge-prog MAX-HAPPINESS)
(define (gauge-prog h)
  (big-bang h                     ; Hapiness
    (on-tick   tock)              ; Happines -> Happines
    (to-draw   render)            ; Happines -> Image
    (on-key increase-happiness))) ; Happines KeyEvent -> Happiness

; Happiness -> Happiness
; decreases happiness by -0.1 with each clock tick. it never falls below 0
(check-expect (tock 5) 4.9)
(check-expect (tock 0.1) 0)
(check-expect (tock 0) 0)

(define (tock h)
  (if (= h 0) h
      (- h 0.1)))

; Happiness -> Image
; Renders the gauge on the scene with given happiness level as percentage of the gauge to fill
(check-expect (render 0) (place-image (overlay/align "left" "middle" GAUGE-FRAME (rectangle (* (/ 0 100) GAUGE-WIDTH) GAUGE-HEIGHT "solid" "red")) GAUGE-X GAUGE-Y MTSCN))
(check-expect (render 50) (place-image (overlay/align "left" "middle" GAUGE-FRAME (rectangle (* (/ 50 100) GAUGE-WIDTH) GAUGE-HEIGHT "solid" "red")) GAUGE-X GAUGE-Y MTSCN))
(check-expect (render 100) (place-image (overlay/align "left" "middle" GAUGE-FRAME (rectangle (* (/ 100 100) GAUGE-WIDTH) GAUGE-HEIGHT "solid" "red")) GAUGE-X GAUGE-Y MTSCN))

(define (render h)
  (place-image (overlay/align "left" "middle" GAUGE-FRAME (rectangle  (* (/ h 100) GAUGE-WIDTH) GAUGE-HEIGHT "solid" "red"))
               GAUGE-X
               GAUGE-Y
               MTSCN))

; Happines KeyEvent -> Happiness
; Every time the down arrow key is pressed, happiness increases by 1/5;
; every time the up arrow is pressed, happiness jumps by 1/3
(check-expect (increase-happiness 0 "up") (+ 0 UP-JUMP))
(check-expect (increase-happiness 0 "down") (+ 0 DOWN-JUMP))
(check-expect (increase-happiness (- MAX-HAPPINESS 1) "down") MAX-HAPPINESS)
(check-expect (increase-happiness (- MAX-HAPPINESS 1) "up") MAX-HAPPINESS)

(define (increase-happiness h ke)
  (cond
    [(string=? ke "up") (limit-to-max (+ h UP-JUMP))]
    [(string=? ke "down") (limit-to-max (+ h DOWN-JUMP))]
    [else h]))

; Happiniess -> Happiniess
; returns MAX-HAPPINESS if h is larger than MAX-HAPPINESS. Else, returns h
(check-expect (limit-to-max MAX-HAPPINESS) MAX-HAPPINESS)
(check-expect (limit-to-max (- MAX-HAPPINESS 1)) (- MAX-HAPPINESS 1))
(check-expect (limit-to-max (+ MAX-HAPPINESS 1)) MAX-HAPPINESS)

(define (limit-to-max h)
  (if (> h MAX-HAPPINESS) MAX-HAPPINESS
      h))