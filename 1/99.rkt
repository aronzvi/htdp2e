;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |99|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; ======================
;; Constants:

(define SCENE-WIDTH 400)
(define SCENE-HEIGHT 200)
(define SCENE (empty-scene SCENE-WIDTH SCENE-HEIGHT))
(define SCENE-MID-X (/ SCENE-WIDTH 2))

(define TANK-COLOR "black")
(define TANK-WIDTH 25)
(define TANK-HEIGHT (/ TANK-WIDTH 2))
(define TANK (rectangle TANK-WIDTH TANK-HEIGHT "solid" TANK-COLOR))
(define TANK-Y (- SCENE-HEIGHT (/ (image-height TANK) 2)))

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

(define GAME-OVER-X SCENE-MID-X)
(define GAME-OVER-Y (/ SCENE-HEIGHT 2))


;; ======================
;; Data definitions:

; A UFO is a Posn. 
; interpretation (make-posn x y) is the UFO's location 
; (using the top-down, left-to-right convention)

(define UFO1 (make-posn 20 30))

#;
(define (fn-for-ufo u)
  (... (posn-x u)   ;Number
       (posn-y u))) ;Number

;; Template rules used:
;; - compound: 2 fields
 
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
 
; A Missile is a Posn. 
; interpretation (make-posn x y) is the missile's place

(define M1 (make-posn 30 20))

#;
(define (fn-for-MISSILE m)
  (... (posn-x m)   ;Number
       (posn-y m))) ;Number

;; Template rules used:
;; - compound: 2 fields

(define-struct aim [ufo tank])
; An aim is a structure:
;    (make-aim UFO Tank)
; interp. The state when the player is trying to get the tank in position for a shot

(define AIM1 (make-aim (make-posn 10 20) (make-tank 28 -3)))

#;
(define (fn-for-aim a)
  (... (fn-for-ufo (aim-ufo a))     ;UFO
       (fn-for-tank (aim-tank a)))) ;Tank

;; Template rules used:
;; - compound: 2 fields
;; - reference: (aim-ufo a) is UFO
;; - reference: (aim-tank a) is Tank

(define-struct fired [ufo tank missile])
; A fired is a structure:
;   (make-fired UFO Tank MISSILE)
; interp. The state after the missile is fired

(define F1 (make-fired
            (make-posn 20 100)
            (make-tank 100 3)
            (make-posn 22 103)))
(define F2 (make-fired
            (make-posn 10 20)
            (make-tank 28 -3)
            (make-posn 32 (- SCENE-HEIGHT TANK-HEIGHT 10))))

#;
(define (fn-for-fired f)
  (... (fn-for-ufo (fired-ufo f))         ;UFO
       (fn-for-tank (fired-tank f))       ;Tank
       (fn-for-missile (fired-missile f)))) ;Missile

;; Template rules used:
;; - compound: 3 fields
;; - reference: (fired-ufo f) is UFO
;; - reference: (fired-tank f) is Tank
;; - reference: (fired-missile f) is Missile

; A SIGS is one of: 
; – (make-aim UFO Tank)
; – (make-fired UFO Tank Missile)
; interpretation represents the complete state of a 
; space invader game(make-posn 10 20)

(define S1 AIM1)
(define S2 F2)

#;
(define (fn-for-sigs s)
  (cond [(aim? s) (... (fn-for-ufo (aim-ufo s))         ; UFO
                       (fn-for-tank (aim-tank s)))]     ; Tank 
        [else (... (fn-for-ufo (fired-ufo s))           ; Ufo
                   (fn-for-tank (fired-tank s))         ; Tank
                   (fn-for-missile (fired-missile s)))])) ; Missile 

;; Template rules used:
;; - one of: 2 cases
;; – compound: (make-aim UFO Tank)
;; - compound: (make-fired UFO Tank Missile)
;; - reference: (aim-ufo s) is UFO
;; - reference: (aim-tank s) is Tank
;; - reference: (fired-ufo s) is UFO
;; - reference: (fired-tank s) is Tank
;; - reference: (fired-missile s) is Missile

;; ====================================
;; Functions

;; SIGS -> SIGS
;; moves the objects
;; No test since we use random???

; not using template

(define (si-move s)
  (si-move-proper s (ufo-random-x UFO-MAX-JUMP-LEN)))

;; Number -> Number
;; creates a random x-coordinate for the ufo with max. Passing in this max since we cannot create function with zero parameters
(check-random (ufo-random-x 3) (- (random (* 3 2)) 3))
(check-random (ufo-random-x 5) (- (random (* 5 2)) 5))

(define (ufo-random-x max)
  (- (random (* max 2)) max))

;; SIGS Number -> SIGS
;; moves the space-invader objects predictably by ufo-delta
(check-expect (si-move-proper (make-aim (make-posn 10 20) (make-tank 28 -3)) 3)
              (make-aim (make-posn (+ 10 3) (+ 20 UFO-SPEED)) (make-tank (+ 28 -3) -3)))
(check-expect (si-move-proper (make-fired (make-posn 20 100) (make-tank 100 3) (make-posn 22 103)) -1)
              (make-fired (make-posn (+ 20 -1) (+ 100 UFO-SPEED)) (make-tank (+ 100 3) 3) (make-posn 22 (- 103 MISSILE-SPEED))))

;(define (si-move-proper s ufo-delta) s) ; stub

; Template from SIGS

(define (si-move-proper s ufo-delta)
  (cond [(aim? s) (make-aim (move-ufo (aim-ufo s) ufo-delta)         
                            (move-tank (aim-tank s)))]     
        [else (make-fired (move-ufo (fired-ufo s) ufo-delta)           
                          (move-tank (fired-tank s))         
                          (move-missile (fired-missile s)))]))

;; UFO Number -> UFO
;; moves given ufo by delta-x and UFO-SPEED
(check-expect (move-ufo (make-posn 10 20) 3) (make-posn (+ 10 3) (+ 20 UFO-SPEED)))
(check-expect (move-ufo (make-posn 10 20) -2) (make-posn (+ 10 -2) (+ 20 UFO-SPEED)))
; (define (move-ufo u delta-x) u) ; stub

; Template from UFO

(define (move-ufo u delta-x)
  (make-posn (+ (posn-x u) delta-x)
             (+ (posn-y u) UFO-SPEED))) 

;; Tank -> Tank
;; moves the given tank by its speed
(check-expect (move-tank (make-tank 100 3)) (make-tank (+ 100 3) 3))
(check-expect (move-tank (make-tank 100 -3)) (make-tank (+ 100 -3) -3))

;(define (move-tank t) t) ; stub

; Template from Tank

(define (move-tank t)
  (make-tank (+ (tank-loc t) (tank-vel t))
             (tank-vel t)))

;; Missile -> Missile
;; moves the given missile by MISSILE-SPEED
(check-expect (move-missile (make-posn 22 103)) (make-posn 22 (- 103 MISSILE-SPEED)))

;(define (move-missile m) m) ; stub

; Template from Missile

(define (move-missile m)
  (make-posn (posn-x m)   
       (- (posn-y m) MISSILE-SPEED))) 




