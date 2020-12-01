;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |101|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; ===========================
;; Constants

(define SCENE-WIDTH 400)
(define SCENE-HEIGHT 200)
(define SCENE (empty-scene SCENE-WIDTH SCENE-HEIGHT))
(define SCENE-MID-X (/ SCENE-WIDTH 2))

(define TANK-COLOR "black")
(define TANK-WIDTH 25)
(define TANK-HEIGHT (/ TANK-WIDTH 2))
(define TANK (rectangle TANK-WIDTH TANK-HEIGHT "solid" TANK-COLOR))
(define TANK-Y (- SCENE-HEIGHT (/ (image-height TANK) 2)))
(define TANK-SPEED 1)

(define UFO-COLOR "green")
(define UFO-WIDTH 40)
(define UFO-HEIGHT (/ UFO-WIDTH 2))
(define UFO-MID-HEIGHT (/ UFO-HEIGHT 2))
(define UFO-MID-WIDTH (/ UFO-WIDTH 2))
(define UFO-R UFO-MID-HEIGHT)
(define UFO (overlay (circle UFO-R "solid" UFO-COLOR) (rectangle UFO-WIDTH UFO-R "solid" UFO-COLOR)))
(define UFO-SPEED 1)
(define UFO-MAX-JUMP-LEN 3)
(define UFO-LANDED (- SCENE-HEIGHT UFO-MID-HEIGHT))

(define MISSILE-COLOR "red")
(define MISSILE-HEIGHT (/ TANK-HEIGHT 1.5))
(define MISSILE-MID-HEIGHT (/ MISSILE-HEIGHT 2))
(define MISSILE (triangle MISSILE-HEIGHT "solid" MISSILE-COLOR))
(define MISSILE-SPEED (* 2 UFO-SPEED))
(define MISSILE-LAUNCH-HEIGHT (- SCENE-HEIGHT TANK-HEIGHT))

;; ===========================
;; Data definitions

(define-struct tank [loc vel])
; A Tank is a structure:
;   (make-tank Number Number). 
; interpretation (make-tank x dx) specifies the position:
; (x, TANK-Y) and the tank's speed: dx pixels/tick

(define T1 (make-tank 30 3)) ; Moving to the right at 3 pixels/tick
(define T2 (make-tank 80 -6)) ; Moving to the left at 6 pixels/tick

#;
(define (fn-for-tank t)
  (... (tank-loc t)   ;Number
       (tank-vel t))) ;Number 

;; Template rules used:
;; - compound: 2 fields

(define-struct sigs [ufo tank missile])
; A SIGS.v2 (short for SIGS version 2) is a structure:
;   (make-sigs UFO Tank MissileOrNot)
; interp: represents the complete state of a
; space invader game

(define S1 (make-sigs (make-posn 50 90) (make-tank 40 -3) #false))           ; No missile
(define S2 (make-sigs (make-posn 54 60) (make-tank 90 2) (make-posn 50 50))) ; With missile

#;
(define (fn-for-sigs s)
  (... (fn-for-ufo (sigs-ufo s))           ; UFO
       (fn-for-tank (sigs-tank s))         ; Tank
       (fn-for-missile (sigs-missile s)))) ; Missile

; Template rules used:
; - compound: 3 fields
; - reference: (sigs-ufo s) is UFO
; - reference: (sigs-tank s) is Tank
; - reference: (sigs-missile s) is Missile


; A MissileOrNot is one of: 
; – #false
; – Posn
; interp: #false means the missile is in the tank;
; Posn says the missile is at that location

(define M1 #false) ; missile is in the tank
(define M2 (make-posn 40 60))

#;
(define (fn-for-missile m)
  (cond
    [(false? m) (...)]
    [else (... (posn-x m)     ; Number
               (posn-y m))])) ; Number

; Template rules used:
; one-of: 2 cases
; - atomic distinct: false
; - compound: 2 fields

;;====================
;; Functions

; MissileOrNot Image -> Image 
; adds an image of missile m to scene s
(check-expect (missile-render.v2 #false SCENE) SCENE)
(check-expect (missile-render.v2 (make-posn 32 (- SCENE-HEIGHT TANK-HEIGHT 10)) SCENE) (place-image MISSILE 32 (- SCENE-HEIGHT TANK-HEIGHT 10) SCENE))
              
#;
(define (missile-render.v2 m s)
  (cond
    [(boolean? m) ...]
    [(posn? m) (... (posn-x m) ... (posn-y m) ...)]))

(define (missile-render.v2 m s)
  (cond
    [(boolean? m) s]
    [(posn? m)
     (place-image MISSILE (posn-x m) (posn-y m) s)]))





